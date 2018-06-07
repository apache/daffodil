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

import org.apache.daffodil.processors.Evaluatable
import java.io.PrintWriter
import org.apache.daffodil.exceptions.UnsuppressableException
import java.io.StringWriter
import org.apache.daffodil.dsom.SchemaDefinitionDiagnosticBase
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.Success
import org.apache.daffodil.processors.SequenceRuntimeData
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.MaybeBoolean
import org.apache.daffodil.api.ValidationMode
import org.apache.daffodil.processors.Failure

sealed trait ArrayIndexStatus
sealed trait GoArrayIndexStatus extends ArrayIndexStatus
sealed trait StopArrayIndexStatus extends ArrayIndexStatus

object ArrayIndexStatus {

  /**
   * Indicates the array element is between 0 and minOccurs in position
   * so is required. However, for occursCountKind 'parsed' or 'stopValue'
   * this is never returned, as the min/max bounds are only advisory
   * for validation purposes in that case.
   */
  case object Required extends GoArrayIndexStatus

  /**
   * Indicates that the array element index is between minOccurs and maxOccurs
   * so isOptional, or that occursCountKind is 'parsed' or 'stopValue' so
   * all elements are optional.
   */
  case object Optional extends GoArrayIndexStatus

  /**
   * Indicates that maxOccurs bound has been reached and the occursCountKind
   * is such that one must stop after that many. (Not unbounded behavior)
   *
   * If this status is returned then one should NOT attempt to parse an
   * additional array element.
   */
  case object MaxExceeded extends StopArrayIndexStatus

  /**
   * Indicates that pstate status is failed, that is, we
   * are unable to continue parsing.
   */
  case object Failed extends StopArrayIndexStatus
}

/**
 * TODO: Performance Stack allocate these objects (stack will be in PState to avoid
 * overhead of another threadlocal)
 */
trait ParseLoopState {

  /**
   * Tells us whether to attempt another array element at the current index,
   * and how we should interpret the existence of an element
   * or empty/zero-length based on the array index.
   */
  def arrayIndexStatus(parser: SequenceChildParser, state: PState): ArrayIndexStatus

  /**
   * Must advance array position for arrays, plus any
   * incrementation.
   */
  def nextArrayIndex(state: PState): Long

}

abstract class OrderedSequenceParserBase(rd: SequenceRuntimeData, childParsers: Seq[Parser])
  extends CombinatorParser(rd) {
  override def nom = "Sequence"

  override lazy val runtimeDependencies: Seq[Evaluatable[AnyRef]] = Nil

  override lazy val childProcessors = childParsers

  /**
   * Parses one iteration of an array/optional element, and returns
   * * MaybeBoolean.One(true) - indicates the child parse was zero length
   * * MaybeBoolean.One(false) - indicates the child parse was not zero length or failed
   * * MaybeBoolean.Nope - indicates that the array loop should terminate due to discriminator failure. in which case the pstate will indicate failure.
   */
  protected def parseOne(
    parser: SequenceChildParser,
    trd: TermRuntimeData,
    pstate: PState,
    priorState: PState.Mark,
    maybeStartState: Maybe[PState.Mark],
    ais: GoArrayIndexStatus): MaybeBoolean

  final protected def tryParseDetectMarkLeaks(
    parser: SequenceChildParser,
    pstate: PState,
    maybeStartState: Maybe[PState.Mark],
    ais: GoArrayIndexStatus): MaybeBoolean = {

    var markLeakCausedByException = false
    var priorState: PState.Mark = null
    val result =
      try {
        priorState = pstate.mark("OrderedSeparatedSequence_beforeSeparator")
        val res = parseOne(parser, parser.trd, pstate, priorState, maybeStartState, ais)
        priorState = null
        res
      } catch {
        // Similar try/catch/finally logic for returning marks is also used in
        // the AltCompParser and RepUnboundedParser. The logic isn't
        // easily factored out so it is duplicated. Changes made here should also
        // be made there. Only these parsers deal with taking marks, so this logic
        // should not be needed elsewhere.
        case t: Throwable => {
          if (priorState != null) {
            markLeakCausedByException = true
            if (!t.isInstanceOf[SchemaDefinitionDiagnosticBase] && !t.isInstanceOf[UnsuppressableException]) {
              val stackTrace = new StringWriter()
              t.printStackTrace(new PrintWriter(stackTrace))
              Assert.invariantFailed("Exception thrown with mark not returned: " + t + "\nStackTrace:\n" + stackTrace)
            }
          }
          throw t
        }
      } finally {
        var markLeak = false;
        if (priorState != null) {
          pstate.discard(priorState)
          markLeak = true;
        }
        if (markLeak && !markLeakCausedByException) {
          // likely a logic bug, throw assertion
          Assert.invariantFailed("mark not returned, likely a logic bug")
        }
      } // end try/catch/finally
    result
  }

}
object SequenceChildParser {
  type SeparatedChildParser = SequenceChildParser with Separated
  type RepSeparatedChildParser = SeparatedChildParser with RepParser

  type UnseparatedChildParser = SequenceChildParser with Unseparated
  type RepUnseparatedChildParser = UnseparatedChildParser with RepParser
}

abstract class SequenceChildParser(
  val childParser: Parser,
  val srd: SequenceRuntimeData,
  val trd: TermRuntimeData)
  extends CombinatorParser(srd) {

  override def runtimeDependencies = Nil
}

trait RepParser { self: SequenceChildParser =>

  def childParser: Parser
  def srd: SequenceRuntimeData
  def erd: ElementRuntimeData
  def baseName: String
  def minRepeats: Long
  def maxRepeats: Long

  override protected def parse(pstate: PState): Unit = {
    childParser.parse1(pstate)
    if (pstate.processorStatus ne Success) {
      val cause = pstate.processorStatus.asInstanceOf[Failure].cause
      PE(pstate, "Failed to populate %s[%s].  Expected from %s to %s item(s). Cause: %s.",
        erd.prefixedName, pstate.mpstate.arrayPos, minRepeats,
        (if (maxRepeats == Long.MaxValue) "unbounded" else maxRepeats),
        cause) // they all must succeed, otherwise we fail here.
      return
    }
  }

  def loopState(state: PState): ParseLoopState

  override lazy val runtimeDependencies = Nil

  def checkN(pstate: PState, n: Long): Boolean = {
    if (n > pstate.tunable.maxOccursBounds) {
      PE(pstate, "Occurs count %s exceeds implementation maximum of %s.", n, pstate.tunable.maxOccursBounds)
      false
    } else true
  }

  override def toString = "Rep" + baseName + "(" + childParser.toString + ")"

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<Rep" + baseName + " name='" + erd.name + "'>" + childParser.toBriefXML(depthLimit - 1) +
        "</Rep" + baseName + ">"
  }

  def startArray(state: PState): Unit = {

    state.mpstate.arrayIndexStack.push(1L) // one-based indexing
    state.mpstate.occursBoundsStack.push(state.tunable.maxOccursBounds)
  }

  def endArray(state: PState): Unit = {
    val actualOccurs = state.mpstate.arrayIndexStack.pop()
    state.mpstate.occursBoundsStack.pop()

    if (state.processorStatus ne Success) return

    val shouldValidate =
      state.dataProc.isDefined && state.dataProc.value.getValidationMode != ValidationMode.Off

    if (shouldValidate) {
      val minO = erd.minOccurs
      val maxO = erd.maxOccurs
      val isUnbounded = maxO == -1
      val occurrence = actualOccurs - 1

      if (isUnbounded && occurrence < minO)
        state.validationError("%s occurred '%s' times when it was expected to be a " +
          "minimum of '%s' and a maximum of 'UNBOUNDED' times.", erd.diagnosticDebugName,
          occurrence, minO)
      else if (!isUnbounded && (occurrence < minO || occurrence > maxO))
        state.validationError("%s occurred '%s' times when it was expected to be a " +
          "minimum of '%s' and a maximum of '%s' times.", erd.diagnosticDebugName,
          occurrence, minO, maxO)
      else {
        //ok
      }
    }
  }
}

