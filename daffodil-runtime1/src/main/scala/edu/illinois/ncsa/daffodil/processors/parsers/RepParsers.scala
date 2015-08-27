/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.api._
import java.nio._
import java.nio.charset._
import scala.collection.JavaConversions._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.Parser
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.Success
import edu.illinois.ncsa.daffodil.processors.WithParseErrorThrowing
import edu.illinois.ncsa.daffodil.processors.Infoset
import edu.illinois.ncsa.daffodil.processors.InfosetElement
import edu.illinois.ncsa.daffodil.dpath.AsIntConverters
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.processors.Failure

abstract class RepParser(n: Long, rParser: Parser, context: ElementRuntimeData, baseName: String)
  extends Parser(context) {

  override lazy val childProcessors = Seq(rParser)

  val intN = n.toInt

  def checkN(pstate: PState, n: Long): Boolean = {
    if (n > DaffodilTunableParameters.maxOccursBounds) {
      // TODO: how can we go after bigger than max int bytes? We have 64-bit computers
      // after all....
      PE(pstate, "Occurs count %s exceeds implementation maximum of %s.", n, DaffodilTunableParameters.maxOccursBounds)
      false
    } else true
  }

  final def parse(pstate: PState): Unit = {
    if (!checkN(pstate, n)) return
    parseAllRepeats(pstate)
    // pstate.mpstate.clearDelimitedText
  }

  protected def parseAllRepeats(pstate: PState): Unit

  override def toString = "Rep" + baseName + "(" + rParser.toString + ")"

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<Rep" + baseName + " name='" + context.name + "' n='" + n + "'>" + rParser.toBriefXML(depthLimit - 1) +
        "</Rep" + baseName + ">"
  }
}

/**
 * This object is so that we can share the iteration idioms between situations
 * where we know N statically, and where dynamic evaluation computes N.
 *
 * In these cases, there are no new points of uncertainty because computed or
 * otherwise, we know N.
 */
object Rep {
  def loopExactlyTotalN(intN: Int, rParser: Parser, pstate: PState, context: RuntimeData, iParser: Parser): Unit = {
    while (pstate.mpstate.arrayPos <= intN) {
      pstate.dataProc.beforeRepetition(pstate, iParser)
      rParser.parse1(pstate)
      pstate.dataProc.afterRepetition(pstate, iParser)
      if (pstate.status != Success) return // fail if we don't get them all 
      pstate.mpstate.moveOverOneArrayIndexOnly
    }
  }
}

class RepExactlyNParser(n: Long, rParser: Parser, context: ElementRuntimeData)
  extends RepParser(n, rParser, context, "ExactlyN") {

  def parseAllRepeats(pstate: PState): Unit = {
    var i = 0
    while (i < intN) {
      i += 1
      pstate.dataProc.beforeRepetition(pstate, rParser)
      rParser.parse1(pstate)
      pstate.dataProc.afterRepetition(pstate, rParser)
      if (pstate.status != Success) {
        val cause = pstate.status.asInstanceOf[Failure].cause
        PE(pstate, "Failed to populate %s:%s[%s].  Expected %s item(s). Cause: %s.",
          context.thisElementsNamespacePrefix, context.name, pstate.mpstate.arrayPos, n,
          cause) // they all must succeed, otherwise we fail here.
        return
      }
      pstate.mpstate.moveOverOneArrayIndexOnly
    }
  }
}

class RepAtMostTotalNParser(n: Long, rParser: Parser, erd: ElementRuntimeData)
  extends RepParser(n, rParser, erd, "AtMostTotalN") {

  def parseAllRepeats(initialState: PState): Unit = {
    val startState = initialState.mark
    var priorState = initialState.mark
    var pstate = initialState
    while (pstate.mpstate.arrayPos <= intN) {
      // Since each one could fail, each is a new point of uncertainty.
      // 
      // save the state of the infoset
      //
      val cloneNode = pstate.captureInfosetElementState
      val infosetElement = pstate.infoset

      pstate.pushDiscriminator

      pstate.dataProc.beforeRepetition(pstate, this)
      rParser.parse1(pstate)
      pstate.dataProc.afterRepetition(pstate, this)

      if (pstate.status != Success) {
        // 
        // Did not succeed
        // 
        // Was a discriminator set?
        // 
        if (pstate.discriminator == true) {
          // we fail the whole RepUnbounded, because there was a discriminator set 
          // before the failure.
          // pstate.popDiscriminator // pop happens when we reset.
          pstate.reset(startState)
          return
        }
        //
        // backout any element appended as part of this attempt.
        //
        pstate.infoset = infosetElement
        pstate.restoreInfosetElementState(cloneNode)
        pstate.reset(priorState)
        pstate.discard(startState)
        return // success at prior state. 
      }
      //
      // Success
      //
      pstate.discard(priorState)
      priorState = pstate.mark
      pstate.mpstate.moveOverOneArrayIndexOnly
      pstate.popDiscriminator
    }
    pstate.discard(startState)
  }
}

class RepExactlyTotalNParser(n: Long, rParser: Parser, context: ElementRuntimeData)
  extends RepParser(n, rParser, context, "ExactlyTotalN") {

  def parseAllRepeats(pstate: PState): Unit = {
    Rep.loopExactlyTotalN(intN, rParser, pstate, context, this)

    if (pstate.status != Success) {
      PE(pstate, "Failed to populate %s:%s[%s].  Expected %s item(s).",
        context.thisElementsNamespacePrefix, context.name, pstate.mpstate.arrayPos, n) // they all must succeed, otherwise we fail here.
    }
  }
}

class RepUnboundedParser(occursCountKind: OccursCountKind.Value, rParser: Parser, erd: ElementRuntimeData)
  extends RepParser(-1, rParser, erd, "Unbounded") {

  def parseAllRepeats(initialState: PState): Unit = {
    Assert.invariant(initialState.status =:= Success)
    val startState = initialState.mark
    var pstate = initialState
    var priorState = initialState.mark

    while (pstate.status == Success) {

      //      erd.maxOccurs.foreach { maxOccurs =>
      //        if ((occursCountKind == OccursCountKind.Implicit) &&
      //          (maxOccurs == -1)) {
      //          erd.minOccurs.foreach { minOccurs =>
      //            if (pstate.mpstate.arrayPos - 1 <= minOccurs) {
      //              // Is required element
      //              // Need to trigger default value creation 
      //              // in right situations (like the element is defaultable)
      //              // This is relatively easy for simple types
      //              // for complex types, defaulting is trickier as one
      //              // must recursively traverse the type, and then determine one
      //              // has not advanced the data at all.
      //            }
      //          }
      //        }
      //      }

      val cloneNode = pstate.captureInfosetElementState
      val infosetElement = pstate.infoset
      //
      // Every parse is a new point of uncertainty.
      pstate.pushDiscriminator
      pstate.dataProc.beforeRepetition(pstate, this)
      rParser.parse1(pstate)
      pstate.dataProc.afterRepetition(pstate, this)
      if (pstate.status != Success) {
        // 
        // Did not succeed
        // 
        // Was a discriminator set?
        // 
        if (pstate.discriminator == true) {
          // we fail the whole RepUnbounded, because there was a discriminator set 
          // before the failure.
          pstate.popDiscriminator
          pstate.reset(startState)
          return
        }
        // 
        // no discriminator, so suppress the failure. Loop terminated with prior element.
        //
        pstate.infoset = infosetElement
        pstate.restoreInfosetElementState(cloneNode)
        log(LogLevel.Debug, "Failure suppressed. This is normal termination of a occursCountKind='parsed' array.")
        pstate.reset(priorState)
        pstate.discard(startState)
        return // note that it has the prior point of uncertainty. No restore needed.
      }
      // Success
      // Need to check for forward progress
      if (pstate.bitPos =#= priorState.bitPos0b) {
        val bytePos = pstate.bytePos
        pstate.discard(priorState) // didn't move, but might have assigned variables.
        pstate.discard(startState)
        PE(pstate,
          "RepUnbounded - No forward progress at byte %s. Attempt to parse %s " +
            "succeeded but consumed no data.\nPlease re-examine your schema to correct this infinite loop.",
          pstate.bytePos, erd.prettyName)
        return
      }
      pstate.discard(priorState)
      priorState = pstate.mark
      pstate.mpstate.moveOverOneArrayIndexOnly // was pstate, not pNext....why?
      pstate.popDiscriminator // point of uncertainty has been resolved.
    }
    Assert.invariantFailed("Unbounded loop terminated wrong")
  }
}

class OccursCountExpressionParser(occursCount: CompiledExpression, erd: ElementRuntimeData)
  extends Parser(erd) with WithParseErrorThrowing {

  override lazy val childProcessors = Nil

  def parse(pstate: PState): Unit = withParseErrorThrowing(pstate) {
    try {
      val oc = occursCount.evaluate(pstate)
      val ocLong = AsIntConverters.asLong(oc)
      if (ocLong < 0 ||
        ocLong > DaffodilTunableParameters.maxOccursBounds) {
        PE(pstate, "Evaluation of occursCount expression %s returned out of range value %s.", occursCount.prettyExpr, ocLong)
        return
      }
      pstate.mpstate.updateBoundsHead(ocLong)
    } catch {
      case s: scala.util.control.ControlThrowable => throw s
      case u: UnsuppressableException => throw u
      case r: RuntimeException => throw r
      case e: Exception =>
        // TODO: get rid of this. Really we shouldn't have this general catch.
        // parsers or the expression subsystem should be catching the narrow
        // set of things that are "real" and allowing anything else to ripple to
        // the top as a bug.
        PE(pstate, "Evaluation of occursCount expression %s threw exception %s", occursCount.prettyExpr, e)
        return
    }
  }

  override def toString = toBriefXML() // "OccursCount(" + e.occursCount.prettyExpr + ")"

  override def toBriefXML(depthLimit: Int = -1) = {
    "<OccursCount>" + occursCount.prettyExpr + "</OccursCount>"
  }
}

class RepAtMostOccursCountParser(rParser: Parser, intN: Long, erd: ElementRuntimeData)
  extends RepParser(intN, rParser, erd, "AtMostOccursCount") {
  def parseAllRepeats(pstate: PState): Unit = {
    // repeat either n times, or occursCount times if that's less than n.
    val n = math.min(pstate.mpstate.occursBounds, erd.minOccurs.get)
    Rep.loopExactlyTotalN(intN.toInt, rParser, pstate, erd, this)
    if (pstate.status != Success) {
      PE(pstate, "Failed to populate %s:%s[%s].  Expected at most %s items.",
        erd.thisElementsNamespacePrefix, erd.name, pstate.mpstate.arrayPos, n) // they all must succeed, otherwise we fail here.
      return
    }
  }
}

class RepExactlyTotalOccursCountParser(rParser: Parser, erd: ElementRuntimeData)
  extends RepParser(-1, rParser, erd, "ExactlyTotalOccursCount") {
  def parseAllRepeats(pstate: PState): Unit = {
    val ocInt = pstate.mpstate.occursBounds.toInt
    Rep.loopExactlyTotalN(ocInt, rParser, pstate, erd, this)
    if (pstate.status != Success) {
      PE(pstate, "Failed to populate %s:%s[%s].  Expected %s item(s).",
        erd.thisElementsNamespacePrefix, erd.name, pstate.mpstate.arrayPos, ocInt) // they all must succeed, otherwise we fail here.
      return
    }
  }
}
