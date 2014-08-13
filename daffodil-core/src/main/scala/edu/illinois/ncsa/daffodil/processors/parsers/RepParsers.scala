package edu.illinois.ncsa.daffodil.processors.parsers

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.grammar._
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
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.Parser
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.Success
import edu.illinois.ncsa.daffodil.processors.WithParseErrorThrowing
import edu.illinois.ncsa.daffodil.processors.Infoset
import edu.illinois.ncsa.daffodil.processors.InfosetElement

abstract class RepParser(n: Long, rParser: Parser, context: ElementRuntimeData, baseName: String)
  extends Parser(context) {

  val intN = n.toInt

  def checkN(pstate: PState, n: Long): Option[PState] = {
    if (n > DaffodilTunableParameters.maxOccursBounds) {
      // TODO: how can we go after bigger than max int bytes? We have 64-bit computers
      // after all....
      Some(PE(pstate, "Occurs count %s exceeds implementation maximum of %s.", n, DaffodilTunableParameters.maxOccursBounds))
    } else None
  }

  final def parse(pstate: PState): PState = {
    checkN(pstate, n).map { perr => return perr }
    val res = parseAllRepeats(pstate)
    pstate.mpstate.clearDelimitedText
    res
  }

  protected def parseAllRepeats(pstate: PState): PState

  override def toString = "Rep" + baseName + "(" + rParser.toString + ")"

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<Rep" + baseName + ">" + rParser.toBriefXML(depthLimit - 1) +
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
  def loopExactlyTotalN(intN: Int, rParser: Parser, pstate: PState, context: RuntimeData, iParser: Parser): PState = {
    var pResult = pstate
    while (pResult.mpstate.arrayPos <= intN) {
      Debugger.beforeRepetition(pResult, iParser)
      val pNext = rParser.parse1(pResult, context)
      Debugger.afterRepetition(pResult, pNext, iParser)
      if (pNext.status != Success) return pNext // fail if we don't get them all 
      pstate.mpstate.moveOverOneArrayIndexOnly
      pResult = pNext
    }
    pResult
  }
}

class RepExactlyNParser(n: Long, rParser: Parser, context: ElementRuntimeData)
  extends RepParser(n, rParser, context, "ExactlyN") {

  def parseAllRepeats(pstate: PState): PState = {
    var pResult = pstate // TODO: find perfect monad functional programming idiom to eliminate this var
    1 to intN foreach { _ =>
      {
        Debugger.beforeRepetition(pResult, this)
        val pNext = rParser.parse1(pResult, context)
        Debugger.afterRepetition(pResult, pNext, this)
        if (pNext.status != Success) return pNext // they all must succeed, otherwise we fail here.
        pstate.mpstate.moveOverOneArrayIndexOnly
        pResult = pNext
      }
    }
    pResult
  }
}

class RepAtMostTotalNParser(n: Long, rParser: Parser, erd: ElementRuntimeData)
  extends RepParser(n, rParser, erd, "AtMostTotalN") {

  def parseAllRepeats(pstate: PState): PState = {
    var pResult = pstate
    while (pResult.mpstate.arrayPos <= intN) {
      // Since each one could fail, each is a new point of uncertainty.
      val newpou = pResult.withNewPointOfUncertainty
      // 
      // save the state of the infoset
      //
      val cloneNode = newpou.captureInfosetElementState

      Debugger.beforeRepetition(newpou, this)
      val pNext = rParser.parse1(newpou, context)
      Debugger.afterRepetition(newpou, pNext, this)

      if (pNext.status != Success) {
        // 
        // Did not succeed
        // 
        // Was a discriminator set?
        // 
        if (pNext.discriminator == true) {
          // we fail the whole RepUnbounded, because there was a discriminator set 
          // before the failure.
          return pNext.withRestoredPointOfUncertainty
        }
        //
        // backout any element appended as part of this attempt.
        //
        pResult.restoreInfosetElementState(cloneNode)
        return pResult // success at prior state. 
      }
      pstate.mpstate.moveOverOneArrayIndexOnly
      pResult = pNext.withRestoredPointOfUncertainty
    }
    pResult
  }
}

class RepExactlyTotalNParser(n: Long, rParser: Parser, context: ElementRuntimeData)
  extends RepParser(n, rParser, context, "ExactlyTotalN") {

  def parseAllRepeats(pstate: PState): PState = {
    Rep.loopExactlyTotalN(intN, rParser, pstate, context, this)
  }
}

class RepUnboundedParser(occursCountKind: OccursCountKind.Value, rParser: Parser, erd: ElementRuntimeData)
  extends RepParser(-1, rParser, erd, "Unbounded") {

  def parseAllRepeats(pstate: PState): PState = {

    var pResult = pstate.duplicate()
    var priorResult = pstate
    while (pResult.status == Success) {

      erd.maxOccurs.foreach { maxOccurs =>
        if ((occursCountKind == OccursCountKind.Implicit) &&
          (maxOccurs == -1)) {
          erd.minOccurs.foreach { minOccurs =>
            if (pResult.mpstate.arrayPos - 1 <= minOccurs) {
              // Is required element
              // Somehow need to return that this element is required
            }
          }
        }
      }

      val cloneNode = pResult.captureInfosetElementState
      //
      // Every parse is a new point of uncertainty.
      val newpou = pResult.withNewPointOfUncertainty
      Debugger.beforeRepetition(newpou, this)
      val pNext = rParser.parse1(newpou, erd)
      Debugger.afterRepetition(newpou, pNext, this)
      if (pNext.status != Success) {
        // 
        // Did not succeed
        // 
        // Was a discriminator set?
        // 
        if (pNext.discriminator == true) {
          // we fail the whole RepUnbounded, because there was a discriminator set 
          // before the failure.
          return pNext.withRestoredPointOfUncertainty
        }
        // 
        // no discriminator, so suppress the failure. Loop terminated with prior element.
        //
        pResult.restoreInfosetElementState(cloneNode)
        log(LogLevel.Debug, "Failure suppressed. This is normal termination of a occursCountKind='parsed' array.")
        return priorResult // note that it has the prior point of uncertainty. No restore needed.
      }
      // Success
      // Need to check for forward progress
      if (priorResult.bitPos == pNext.bitPos) {
        return PE(pNext,
          "RepUnbounded - No forward progress at byte %s. Attempt to parse %s " +
            "succeeded but consumed no data.\nPlease re-examine your schema to correct this infinite loop.",
          pResult.bytePos, erd.prettyName)
      }
      priorResult = pNext.duplicate()
      pNext.mpstate.moveOverOneArrayIndexOnly // was pstate, not pNext....why?
      pResult = pNext.withRestoredPointOfUncertainty // point of uncertainty has been resolved.

    }
    Assert.invariantFailed("Unbounded loop terminated wrong")
  }
}

class OccursCountExpressionParser(occursCount: CompiledExpression, erd: ElementRuntimeData)
  extends Parser(erd) with WithParseErrorThrowing {

  def parse(pstate: PState): PState = withParseErrorThrowing(pstate) {

    val res = try {
      val (oc, newVMap) = occursCount.evaluate(pstate)
      val postEvalState = pstate.withVariables(newVMap)
      val ocLong = oc match {
        case s: String => s.toInt.toLong
        case i: Int => i.toLong
        case l: Long => l
        case bi: BigInt => bi.toLong
        case bi: java.math.BigInteger => BigInt(bi).toLong
        case _ => Assert.invariantFailed("not an integer-type number: " + oc + " of type " + oc.getClass().getName())
      }
      if (ocLong < 0 ||
        ocLong > DaffodilTunableParameters.maxOccursBounds) {
        return PE(postEvalState, "Evaluation of occursCount expression %s returned out of range value %s.", occursCount.prettyExpr, ocLong)
      }
      pstate.mpstate.updateBoundsHead(ocLong)
      postEvalState
    } catch {
      case u: UnsuppressableException => throw u
      case r: RuntimeException => throw r
      case e: Exception =>
        // TODO: get rid of this. Really we shouldn't have this general catch.
        // parsers or the expression subsystem should be catching the narrow
        // set of things that are "real" and allowing anything else to ripple to
        // the top as a bug.
        PE(pstate, "Evaluation of occursCount expression %s threw exception %s", occursCount.prettyExpr, e)
    }
    res
  }

  override def toString = toBriefXML() // "OccursCount(" + e.occursCount.prettyExpr + ")"

  override def toBriefXML(depthLimit: Int = -1) = {
    "<OccursCount>" + occursCount.prettyExpr + "</OccursCount>"
  }
}

class RepAtMostOccursCountParser(rParser: Parser, intN: Long, erd: ElementRuntimeData)
  extends RepParser(intN, rParser, erd, "AtMostOccursCount") {
  def parseAllRepeats(pstate: PState): PState = {
    // repeat either n times, or occursCount times if that's less than n.
    val n = math.min(pstate.mpstate.occursBounds, erd.minOccurs.get)
    Rep.loopExactlyTotalN(intN.toInt, rParser, pstate, erd, this)
  }
}

class RepExactlyTotalOccursCountParser(rParser: Parser, erd: ElementRuntimeData)
  extends RepParser(-1, rParser, erd, "ExactlyTotalOccursCount") {
  def parseAllRepeats(pstate: PState): PState = {
    val ocInt = pstate.mpstate.occursBounds.toInt
    Rep.loopExactlyTotalN(ocInt, rParser, pstate, erd, this)
  }
}
