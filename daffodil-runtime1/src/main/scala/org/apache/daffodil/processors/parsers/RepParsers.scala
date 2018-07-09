/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.processors.parsers

import java.lang.{ Long => JLong }

import org.apache.daffodil.processors.{ ElementRuntimeData, Evaluatable }
import org.apache.daffodil.util.Numbers

//trait RepParser { self: SequenceChildParser =>
//
//  def rParser: Parser
//  def srd: SequenceRuntimeData
//  def erd: ElementRuntimeData
//  def baseName: String
//
//  override lazy val runtimeDependencies = Nil
//
//  def checkN(pstate: PState, n: Long): Boolean = {
//    if (n > pstate.tunable.maxOccursBounds) {
//      PE(pstate, "Occurs count %s exceeds implementation maximum of %s.", n, pstate.tunable.maxOccursBounds)
//      false
//    } else true
//  }
//
//  override def toString = "Rep" + baseName + "(" + rParser.toString + ")"
//
//  override def toBriefXML(depthLimit: Int = -1): String = {
//    if (depthLimit == 0) "..." else
//      "<Rep" + baseName + " name='" + erd.name + "'>" + rParser.toBriefXML(depthLimit - 1) +
//        "</Rep" + baseName + ">"
//  }
//
//  def startArray(state: PState): Unit = {
//
//    state.mpstate.arrayIndexStack.push(1L) // one-based indexing
//    state.mpstate.occursBoundsStack.push(state.tunable.maxOccursBounds)
//  }
//
//  def endArray(state: PState): Unit = {
//    val actualOccurs = state.mpstate.arrayIndexStack.pop()
//    state.mpstate.occursBoundsStack.pop()
//
//    if (state.processorStatus ne Success) return
//
//    val shouldValidate =
//      state.dataProc.isDefined && state.dataProc.value.getValidationMode != ValidationMode.Off
//
//    if (shouldValidate && erd.minOccurs.isDefined && erd.maxOccurs.isDefined) {
//      val minO = erd.minOccurs.get
//      val maxO = erd.maxOccurs.get
//      val isUnbounded = maxO == -1
//      val occurrence = actualOccurs - 1
//
//      if (isUnbounded && occurrence < minO)
//        state.validationError("%s occurred '%s' times when it was expected to be a " +
//          "minimum of '%s' and a maximum of 'UNBOUNDED' times.", erd.diagnosticDebugName,
//          occurrence, minO)
//      else if (!isUnbounded && (occurrence < minO || occurrence > maxO))
//        state.validationError("%s occurred '%s' times when it was expected to be a " +
//          "minimum of '%s' and a maximum of '%s' times.", erd.diagnosticDebugName,
//          occurrence, minO, maxO)
//      else {
//        //ok
//      }
//    }
//  }
//}
//
//abstract class RepUnseparatedParser(
//  val repeatCount: Long,
//  val rParser: Parser,
//  srd: SequenceRuntimeData,
//  val erd: ElementRuntimeData,
//  val baseName: String)
//  extends UnseparatedSequenceChildParser(srd, erd, Seq(rParser)) with RepParser {
//
//  override protected def parse(pstate: PState): Unit = {
//    startArray(pstate)
//    parseAllRepeats(pstate)
//    endArray(pstate)
//  }
//
//  protected def parseAllRepeats(pstate: PState): Unit
//}
//
///**
// * This object is so that we can share the iteration idioms between situations
// * where we know N statically, and where dynamic evaluation computes N.
// *
// * In these cases, there are no new points of uncertainty because computed or
// * otherwise, we know N.
// */
//object Rep {
//  def loopExactlyTotalN(intN: Long, rParser: Parser, pstate: PState, context: TermRuntimeData, iParser: Parser): Unit = {
//    while (pstate.mpstate.arrayPos <= intN) {
//      if (pstate.dataProc.isDefined) pstate.dataProc.get.beforeRepetition(pstate, iParser)
//      rParser.parse1(pstate)
//      if (pstate.dataProc.isDefined) pstate.dataProc.get.afterRepetition(pstate, iParser)
//      if (pstate.processorStatus ne Success) {
//        return // fail if we don't get them all
//      }
//      pstate.mpstate.moveOverOneArrayIndexOnly
//    }
//  }
//}
//
//class RepExactlyNParser(repeatCount: Long, rParser: Parser, sContext: SequenceRuntimeData, eContext: ElementRuntimeData)
//  extends RepUnseparatedParser(repeatCount, rParser, sContext, eContext, "ExactlyN") {
//
//  /**
//   * Overridden for bounds check.
//   */
//  override protected def parse(pstate: PState): Unit = {
//    if (!checkN(pstate, repeatCount)) return
//    super.parse(pstate)
//  }
//
//  def parseAllRepeats(pstate: PState): Unit = {
//    var i = 0
//    while (i < repeatCount) {
//      i += 1
//      if (pstate.dataProc.isDefined) pstate.dataProc.get.beforeRepetition(pstate, rParser)
//      rParser.parse1(pstate)
//      if (pstate.dataProc.isDefined) pstate.dataProc.get.afterRepetition(pstate, rParser)
//      if (pstate.processorStatus ne Success) {
//        val cause = pstate.processorStatus.asInstanceOf[Failure].cause
//        PE(pstate, "Failed to populate %s[%s].  Expected %s item(s). Cause: %s.",
//          eContext.prefixedName, pstate.mpstate.arrayPos, repeatCount,
//          cause) // they all must succeed, otherwise we fail here.
//        return
//      }
//      pstate.mpstate.moveOverOneArrayIndexOnly
//    }
//  }
//}
//
//class RepAtMostTotalNParser(repeatCount: Long, rParser: Parser, srd: SequenceRuntimeData, erd: ElementRuntimeData)
//  extends RepUnseparatedParser(repeatCount, rParser, srd, erd, "AtMostTotalN") {
//
//  def parseAllRepeats(initialState: PState): Unit = {
//    var startState: PState.Mark = null
//    var priorState: PState.Mark = null
//    var markLeakCausedByException = false
//
//    try {
//      if (initialState.mpstate.arrayPos <= repeatCount) {
//        startState = initialState.mark("RepAtMostTotalNParser1")
//        priorState = initialState.mark("RepAtMostTotalNParser2")
//        val pstate = initialState
//        var returnFlag = false
//        while (!returnFlag && (pstate.mpstate.arrayPos <= repeatCount)) {
//          // Since each one could fail, each is a new point of uncertainty.
//
//          pstate.pushDiscriminator
//
//          if (pstate.dataProc.isDefined) pstate.dataProc.get.beforeRepetition(pstate, this)
//
//          try {
//            rParser.parse1(pstate)
//          } catch {
//            case sde: SchemaDefinitionDiagnosticBase => {
//              pstate.discard(startState)
//              startState = null
//              priorState = null
//              throw sde
//            }
//          }
//
//          if (pstate.dataProc.isDefined) pstate.dataProc.get.afterRepetition(pstate, this)
//
//          if (pstate.processorStatus ne Success) {
//            //
//            // Did not succeed
//            //
//            // Was a discriminator set?
//            //
//            if (pstate.discriminator == true) {
//              // we fail the whole RepUnbounded, because there was a discriminator set
//              // before the failure.
//              pstate.reset(startState)
//              startState = null
//              priorState = null
//
//              // no need discard priorState, that is implicitly discarded by resetting the startState
//              returnFlag = true
//            } else {
//              //
//              // backout any element appended as part of this attempt.
//              //
//              pstate.reset(priorState)
//              pstate.discard(startState)
//              priorState = null
//              startState = null
//
//              returnFlag = true // success at prior state.
//            }
//          } else {
//            //
//            // Success
//            //
//            pstate.discard(priorState)
//            priorState = pstate.mark("RepAtMostTotalNParser3")
//            pstate.mpstate.moveOverOneArrayIndexOnly
//            returnFlag = false
//          }
//
//          pstate.popDiscriminator
//        }
//        if (returnFlag == false) {
//          // we exited the loop due to arrayPos hitting the upper limit
//          pstate.discard(priorState)
//          pstate.discard(startState)
//          priorState = null
//          startState = null
//        }
//      }
//    } catch {
//      // Similar try/catch/finally logic for returning marks is also used in
//      // the AltCompParser and RepUnboundedParser. The logic isn't
//      // easily factored out so it is duplicated. Changes made here should also
//      // be made there. Only these parsers deal with taking marks, so this logic
//      // should not be needed elsewhere.
//      case t: Throwable => {
//        if (priorState != null || startState != null) {
//          markLeakCausedByException = true
//          if (!t.isInstanceOf[SchemaDefinitionDiagnosticBase] && !t.isInstanceOf[UnsuppressableException]) {
//            val stackTrace = new StringWriter()
//            t.printStackTrace(new PrintWriter(stackTrace))
//            Assert.invariantFailed("Exception thrown with mark not returned: " + t + "\nStackTrace:\repeatCount" + stackTrace)
//          }
//        }
//        throw t
//      }
//    } finally {
//      var markLeak = false;
//      if (priorState != null) {
//        initialState.discard(priorState)
//        markLeak = true;
//      }
//      if (startState != null) {
//        initialState.discard(startState)
//        markLeak = true;
//      }
//
//      if (markLeak && !markLeakCausedByException) {
//        // likely a logic bug, throw assertion
//        Assert.invariantFailed("mark not returned, likely a logic bug")
//      }
//    }
//  }
//}
//
//class RepExactlyTotalNParser(repeatCount: Long, rParser: Parser, srd: SequenceRuntimeData, erd: ElementRuntimeData)
//  extends RepUnseparatedParser(repeatCount, rParser, srd, erd, "ExactlyTotalN") {
//
//  def parseAllRepeats(pstate: PState): Unit = {
//    Rep.loopExactlyTotalN(repeatCount, rParser, pstate, erd, this)
//
//    if (pstate.processorStatus ne Success) {
//      PE(pstate, "Failed to populate %s[%s].  Expected %s item(s).",
//        erd.prefixedName, pstate.mpstate.arrayPos, repeatCount) // they all must succeed, otherwise we fail here.
//    }
//  }
//}
//
//class RepUnboundedParser(occursCountKind: OccursCountKind.Value, rParser: Parser, srd: SequenceRuntimeData, erd: ElementRuntimeData)
//  extends RepUnseparatedParser(-1, rParser, srd, erd, "Unbounded") {
//
//  def parseAllRepeats(initialState: PState): Unit = {
//    var startState: PState.Mark = null
//    var priorState: PState.Mark = null
//    var markLeakCausedByException = false
//
//    Assert.invariant(initialState.processorStatus eq Success)
//
//    try {
//      val pstate = initialState
//      startState = initialState.mark("RepUnboundedParser1")
//      priorState = initialState.mark("RepUnboundedParser2")
//      var returnFlag = false
//      while (!returnFlag && (pstate.processorStatus eq Success)) {
//
//        // Every parse is a new point of uncertainty.
//        pstate.pushDiscriminator
//        if (pstate.dataProc.isDefined) pstate.dataProc.get.beforeRepetition(pstate, this)
//
//        try {
//          rParser.parse1(pstate)
//        } catch {
//          case sde: SchemaDefinitionDiagnosticBase => {
//            pstate.discard(startState)
//            startState = null
//            priorState = null
//            throw sde
//          }
//        }
//
//        if (pstate.dataProc.isDefined) pstate.dataProc.get.afterRepetition(pstate, this)
//        if (pstate.processorStatus ne Success) {
//          //
//          // Did not succeed
//          //
//          // Was a discriminator set?
//          //
//          if (pstate.discriminator == true) {
//            // we fail the whole RepUnbounded, because there was a discriminator set
//            // before the failure.
//            pstate.reset(startState)
//            startState = null
//            priorState = null
//            // no need discard priorState, that is implicitly discarded by resetting the startState
//          } else {
//            //
//            // no discriminator, so suppress the failure. Loop terminated with prior element.
//            //
//
//            log(LogLevel.Debug, "Failure suppressed. This is normal termination of a occursCountKind='parsed' array.")
//            pstate.reset(priorState)
//            pstate.discard(startState)
//            startState = null
//            priorState = null
//          }
//          returnFlag = true
//        } else {
//          // Success
//          // Need to check for forward progress
//          if (pstate.bitPos =#= priorState.bitPos0b) {
//            pstate.discard(priorState) // didn't move, but might have assigned variables, have to undo those.
//            pstate.discard(startState)
//            startState = null
//            priorState = null
//            PE(pstate,
//              "RepUnbounded - No forward progress at byte %s. Attempt to parse %s " +
//                "succeeded but consumed no data.\nPlease re-examine your schema to correct this infinite loop.",
//              pstate.bytePos, erd.diagnosticDebugName)
//            returnFlag = true
//          } else {
//            pstate.discard(priorState)
//            priorState = pstate.mark("RepUnboundedParser3")
//            pstate.mpstate.moveOverOneArrayIndexOnly
//            returnFlag = false
//          }
//        }
//        pstate.popDiscriminator
//      }
//      Assert.invariant(returnFlag == true)
//    } catch {
//      // Similar try/catch/finally logic for returning marks is also used in
//      // the AltCompParser and RepAtMostTotalNParser. The logic isn't
//      // easily factored out so it is duplicated. Changes made here should also
//      // be made there. Only these parsers deal with taking marks, so this logic
//      // should not be needed elsewhere.
//      case t: Throwable => {
//        if (priorState != null || startState != null) {
//          markLeakCausedByException = true
//          if (!t.isInstanceOf[SchemaDefinitionDiagnosticBase] && !t.isInstanceOf[UnsuppressableException]) {
//            val stackTrace = new StringWriter()
//            t.printStackTrace(new PrintWriter(stackTrace))
//            Assert.invariantFailed("Exception thrown with mark not returned: " + t + "\nStackTrace:\n" + stackTrace)
//          }
//        }
//        throw t
//      }
//    } finally {
//      var markLeak = false;
//      if (priorState != null) {
//        initialState.discard(priorState)
//        markLeak = true;
//      }
//      if (startState != null) {
//        initialState.discard(startState)
//        markLeak = true;
//      }
//
//      if (markLeak && !markLeakCausedByException) {
//        // likely a logic bug, throw assertion
//        Assert.invariantFailed("mark not returned, likely a logic bug")
//      }
//    }
//  }
//}

class OccursCountExpressionParser(occursCountEv: Evaluatable[JLong],
  override val context: ElementRuntimeData)
  extends PrimParserNoData {
  override lazy val runtimeDependencies = Nil

  override lazy val childProcessors = Nil

  override def parse(pstate: PState): Unit = {
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

//class RepAtMostOccursCountParser(rParser: Parser, intN: Long, srd: SequenceRuntimeData, erd: ElementRuntimeData)
//  extends RepUnseparatedParser(intN, rParser, srd, erd, "AtMostOccursCount") {
//  def parseAllRepeats(pstate: PState): Unit = {
//    // repeat either n times, or occursCount times if that's less than n.
//    val n = math.min(pstate.mpstate.occursBounds, erd.minOccurs.get)
//    Rep.loopExactlyTotalN(intN.toInt, rParser, pstate, erd, this)
//    if (pstate.processorStatus ne Success) {
//      PE(pstate, "Failed to populate %s[%s].  Expected at most %s items.",
//        erd.prefixedName, pstate.mpstate.arrayPos, n) // they all must succeed, otherwise we fail here.
//      return
//    }
//  }
//}
//
//class RepExactlyTotalOccursCountParser(rParser: Parser, ocParser: Parser, srd: SequenceRuntimeData, erd: ElementRuntimeData)
//  extends RepUnseparatedParser(-1, rParser, srd, erd, "ExactlyTotalOccursCount") {
//  def parseAllRepeats(pstate: PState): Unit = {
//    ocParser.parse1(pstate)
//    val ocInt = pstate.mpstate.occursBounds.toInt
//    Rep.loopExactlyTotalN(ocInt, rParser, pstate, erd, this)
//    if (pstate.processorStatus ne Success) {
//      PE(pstate, "Failed to populate %s[%s].  Expected %s item(s).",
//        erd.prefixedName, pstate.mpstate.arrayPos, ocInt) // they all must succeed, otherwise we fail here.
//      return
//    }
//  }
//}
