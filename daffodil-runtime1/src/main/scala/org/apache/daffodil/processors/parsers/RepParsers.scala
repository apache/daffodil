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

package org.apache.daffodil.processors.parsers

import java.lang.{ Long => JLong }

import org.apache.daffodil.dsom.SchemaDefinitionDiagnosticBase
import org.apache.daffodil.equality.ViewEqual
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.processors.Failure
import org.apache.daffodil.processors.RuntimeData
import org.apache.daffodil.processors.Success
import org.apache.daffodil.schema.annotation.props.gen.OccursCountKind
import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.util.Numbers

abstract class RepParser(n: Long, rParser: Parser, context: ElementRuntimeData, baseName: String)
  extends ParserObject(context) {

  override lazy val childProcessors = Seq(rParser)

  val intN = n.toInt

  def checkN(pstate: PState, n: Long): Boolean = {
    if (n > pstate.tunable.maxOccursBounds) {
      // TODO: how can we go after bigger than max int bytes? We have 64-bit computers
      // after all....
      PE(pstate, "Occurs count %s exceeds implementation maximum of %s.", n, pstate.tunable.maxOccursBounds)
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
      if (pstate.dataProc.isDefined) pstate.dataProc.get.beforeRepetition(pstate, iParser)
      rParser.parse1(pstate)
      if (pstate.dataProc.isDefined) pstate.dataProc.get.afterRepetition(pstate, iParser)
      if (pstate.processorStatus ne Success) {
        return // fail if we don't get them all
      }
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
      if (pstate.dataProc.isDefined) pstate.dataProc.get.beforeRepetition(pstate, rParser)
      rParser.parse1(pstate)
      if (pstate.dataProc.isDefined) pstate.dataProc.get.afterRepetition(pstate, rParser)
      if (pstate.processorStatus ne Success) {
        val cause = pstate.processorStatus.asInstanceOf[Failure].cause
        PE(pstate, "Failed to populate %s[%s].  Expected %s item(s). Cause: %s.",
          context.prefixedName, pstate.mpstate.arrayPos, n,
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
    if (initialState.mpstate.arrayPos <= intN) {
      val startState = initialState.mark("RepAtMostTotalNParser1")
      var priorState = initialState.mark("RepAtMostTotalNParser2")
      val pstate = initialState
      var returnFlag = false
      while (!returnFlag && (pstate.mpstate.arrayPos <= intN)) {
        // Since each one could fail, each is a new point of uncertainty.

        pstate.pushDiscriminator

        if (pstate.dataProc.isDefined) pstate.dataProc.get.beforeRepetition(pstate, this)

        try {
          rParser.parse1(pstate)
        } catch {
          case sde: SchemaDefinitionDiagnosticBase => {
            pstate.discard(startState)
            throw sde
          }
        }

        if (pstate.dataProc.isDefined) pstate.dataProc.get.afterRepetition(pstate, this)

        if (pstate.processorStatus ne Success) {
          //
          // Did not succeed
          //
          // Was a discriminator set?
          //
          if (pstate.discriminator == true) {
            // we fail the whole RepUnbounded, because there was a discriminator set
            // before the failure.
            pstate.reset(startState)
            // no need discard priorState, that is implicitly discarded by resetting the startState
            returnFlag = true
          } else {
            //
            // backout any element appended as part of this attempt.
            //
            pstate.reset(priorState)
            pstate.discard(startState)
            returnFlag = true // success at prior state.
          }
        } else {
          //
          // Success
          //
          pstate.discard(priorState)
          priorState = pstate.mark("RepAtMostTotalNParser3")
          pstate.mpstate.moveOverOneArrayIndexOnly
          returnFlag = false
        }

        pstate.popDiscriminator
      }
      if (returnFlag == false) {
        // we exited the loop due to arrayPos hitting the upper limit
        pstate.discard(priorState)
        pstate.discard(startState)
      }
    }
  }
}

class RepExactlyTotalNParser(n: Long, rParser: Parser, context: ElementRuntimeData)
  extends RepParser(n, rParser, context, "ExactlyTotalN") {

  def parseAllRepeats(pstate: PState): Unit = {
    Rep.loopExactlyTotalN(intN, rParser, pstate, context, this)

    if (pstate.processorStatus ne Success) {
      PE(pstate, "Failed to populate %s[%s].  Expected %s item(s).",
        context.prefixedName, pstate.mpstate.arrayPos, n) // they all must succeed, otherwise we fail here.
    }
  }
}

class RepUnboundedParser(occursCountKind: OccursCountKind.Value, rParser: Parser, erd: ElementRuntimeData)
  extends RepParser(-1, rParser, erd, "Unbounded") {

  def parseAllRepeats(initialState: PState): Unit = {
    Assert.invariant(initialState.processorStatus eq Success)
    val startState = initialState.mark("RepUnboundedParser1")
    val pstate = initialState
    var priorState = initialState.mark("RepUnboundedParser2")
    var returnFlag = false
    while (!returnFlag && (pstate.processorStatus eq Success)) {

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

      //
      // Every parse is a new point of uncertainty.
      pstate.pushDiscriminator
      if (pstate.dataProc.isDefined) pstate.dataProc.get.beforeRepetition(pstate, this)

      try {
        rParser.parse1(pstate)
      } catch {
        case sde: SchemaDefinitionDiagnosticBase => {
          pstate.discard(startState)
          throw sde
        }
      }

      if (pstate.dataProc.isDefined) pstate.dataProc.get.afterRepetition(pstate, this)
      if (pstate.processorStatus ne Success) {
        //
        // Did not succeed
        //
        // Was a discriminator set?
        //
        if (pstate.discriminator == true) {
          // we fail the whole RepUnbounded, because there was a discriminator set
          // before the failure.
          pstate.reset(startState)
          // no need discard priorState, that is implicitly discarded by resetting the startState
        } else {
          //
          // no discriminator, so suppress the failure. Loop terminated with prior element.
          //

          log(LogLevel.Debug, "Failure suppressed. This is normal termination of a occursCountKind='parsed' array.")
          pstate.reset(priorState)
          pstate.discard(startState)
        }
        returnFlag = true
      } else {
        // Success
        // Need to check for forward progress
        if (pstate.bitPos =#= priorState.bitPos0b) {
          pstate.discard(priorState) // didn't move, but might have assigned variables, have to undo those.
          pstate.discard(startState)
          PE(pstate,
            "RepUnbounded - No forward progress at byte %s. Attempt to parse %s " +
              "succeeded but consumed no data.\nPlease re-examine your schema to correct this infinite loop.",
            pstate.bytePos, erd.diagnosticDebugName)
          returnFlag = true
        } else {
          pstate.discard(priorState)
          priorState = pstate.mark("RepUnboundedParser3")
          pstate.mpstate.moveOverOneArrayIndexOnly
          returnFlag = false
        }
      }
      pstate.popDiscriminator
    }
    Assert.invariant(returnFlag == true)
  }
}

class OccursCountExpressionParser(occursCountEv: Evaluatable[JLong], erd: ElementRuntimeData)
  extends ParserObject(erd) {

  override lazy val childProcessors = Nil

  def parse(pstate: PState): Unit = {
    val oc = occursCountEv.evaluate(pstate)
    val ocLong = Numbers.asLong(oc)
    if (ocLong < 0 ||
      ocLong > pstate.tunable.maxOccursBounds) {
      PE(pstate, "Evaluation of occursCount expression %s returned out of range value %s.", occursCountEv, ocLong)
      return
    }
    pstate.mpstate.updateBoundsHead(ocLong)
  }

  override def toString = toBriefXML() // "OccursCount(" + e.occursCount.prettyExpr + ")"

  override def toBriefXML(depthLimit: Int = -1) = {
    "<OccursCount>" + occursCountEv.toString + "</OccursCount>"
  }
}

class RepAtMostOccursCountParser(rParser: Parser, intN: Long, erd: ElementRuntimeData)
  extends RepParser(intN, rParser, erd, "AtMostOccursCount") {
  def parseAllRepeats(pstate: PState): Unit = {
    // repeat either n times, or occursCount times if that's less than n.
    val n = math.min(pstate.mpstate.occursBounds, erd.minOccurs.get)
    Rep.loopExactlyTotalN(intN.toInt, rParser, pstate, erd, this)
    if (pstate.processorStatus ne Success) {
      PE(pstate, "Failed to populate %s[%s].  Expected at most %s items.",
        erd.prefixedName, pstate.mpstate.arrayPos, n) // they all must succeed, otherwise we fail here.
      return
    }
  }
}

class RepExactlyTotalOccursCountParser(rParser: Parser, erd: ElementRuntimeData)
  extends RepParser(-1, rParser, erd, "ExactlyTotalOccursCount") {
  def parseAllRepeats(pstate: PState): Unit = {
    val ocInt = pstate.mpstate.occursBounds.toInt
    Rep.loopExactlyTotalN(ocInt, rParser, pstate, erd, this)
    if (pstate.processorStatus ne Success) {
      PE(pstate, "Failed to populate %s[%s].  Expected %s item(s).",
        erd.prefixedName, pstate.mpstate.arrayPos, ocInt) // they all must succeed, otherwise we fail here.
      return
    }
  }
}
