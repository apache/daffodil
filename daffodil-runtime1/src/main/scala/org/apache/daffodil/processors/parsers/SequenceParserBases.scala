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

  /**
   * Indicates that we are done iterating, and should stop parsing more
   * array. Used to indicate that the end of the array was identified
   * by speculative parsing.
   */
  case object Done extends StopArrayIndexStatus
}

sealed trait ParseAttemptStatus
sealed trait SuccessParseAttemptStatus extends ParseAttemptStatus
sealed trait FailedParseAttemptStatus extends ParseAttemptStatus

object ParseAttemptStatus {

  case object Uninitialized extends ParseAttemptStatus

  case object Success_ZeroLength extends SuccessParseAttemptStatus

  case object Success_NotZeroLength extends SuccessParseAttemptStatus

  case object Success_LengthUndetermined extends SuccessParseAttemptStatus

  case object Success_EndOfArray extends SuccessParseAttemptStatus

  case object FailedWithDiscriminatorSet extends FailedParseAttemptStatus

  case object FailedSpeculativeParse extends FailedParseAttemptStatus

  case object FailedEntireArray extends FailedParseAttemptStatus

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
  def arrayIndexStatus(parser: SequenceChildParser, state: PState,
    resultOfPriorTry: ParseAttemptStatus): ArrayIndexStatus

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
    ais: GoArrayIndexStatus): ParseAttemptStatus

  final protected def tryParseDetectMarkLeaks(
    parser: SequenceChildParser,
    pstate: PState,
    maybeStartState: Maybe[PState.Mark],
    ais: GoArrayIndexStatus): ParseAttemptStatus = {

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

  protected def failedChild(pstate: PState, priorState: PState.Mark): ParseAttemptStatus = {
    val cause = pstate.processorStatus.asInstanceOf[Failure].cause
    pstate.reset(priorState)
    PE(pstate, "Failed to parse sequence child. Cause: %s.", cause)
    ParseAttemptStatus.FailedSpeculativeParse
  }

  protected def processFailedChildParseResults(
    pstate: PState,
    priorState: PState.Mark,
    maybeStartState: Maybe[PState.Mark]): ParseAttemptStatus = {
    val isFixedOccurs = maybeStartState.isEmpty
    val isVariableOccurs = !isFixedOccurs
    //
    // we failed to parse a child
    //
    // failedChild(pstate, priorState) // discards prior state
    if (isFixedOccurs) {
      //
      // in fixed occurs, there are no points of uncertainty for the
      // individual elements, so a failure is a failure of the whole loop.
      // And any discriminator that has been set is the discriminator
      // of some surrounding scope.
      //
      ParseAttemptStatus.FailedEntireArray
    } else {
      Assert.invariant(isVariableOccurs)
      val startState = maybeStartState.get
      //
      // variable occurs case, there is a PoU per array element
      //
      // Parsing the array element may be a deep recursive walk
      // somewhere in there a discriminator may be set indicating
      // this array element is known to exist
      //
      // Hence, if discriminator is set, we fail the whole array
      // because the discriminator says this array element *is here*, but
      // the parse failed, so it isn't.
      //
      if (pstate.discriminator == true) {
        pstate.discard(priorState) // deallocate
        pstate.reset(startState)
        ParseAttemptStatus.FailedWithDiscriminatorSet
      } else {
        Assert.invariant(pstate.discriminator == false)
        //
        // discriminator false, variable occurs case, we failed the element, but that
        // just means we back out to prior element.
        //
        pstate.reset(priorState) // no need discard priorState, that is implicitly discarded by resetting the startState
        pstate.discard(startState) // finished with array, so cleanup
        ParseAttemptStatus.Success_EndOfArray // not zero length (in this case with success at prior element)
      }
    } // end if fixed/variable
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

  override protected def parse(pstate: PState): Unit = {
    childParser.parse1(pstate)
    if (pstate.processorStatus ne Success) {
      val cause = pstate.processorStatus.asInstanceOf[Failure].cause
      PE(pstate, "Failed to populate %s[%s]. Cause: %s.",
        erd.prefixedName, pstate.mpstate.arrayPos, cause) // they all must succeed, otherwise we fail here.
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

