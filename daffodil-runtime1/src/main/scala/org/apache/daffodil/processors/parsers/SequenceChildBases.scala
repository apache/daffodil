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
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.Success
import org.apache.daffodil.processors.SequenceRuntimeData
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.api.ValidationMode
import org.apache.daffodil.processors.Failure
import org.apache.daffodil.processors.OccursCountEv
import org.apache.daffodil.schema.annotation.props.gen.OccursCountKind
import org.apache.daffodil.processors.ParseOrUnparseState

/**
 * Enables various sub-kinds of success/failure of a parse to be distinguished
 * easily. These are statuses of parsing of an occurrence of an array/optional
 * element, or when implied, the entire array/optional that contains that occurrence.
 * This applies to both specified and speculative number of occurrences.
 */
sealed trait ParseAttemptStatus {
  def isSuccess: Boolean = false
}
sealed trait SuccessParseAttemptStatus extends ParseAttemptStatus {
  override def isSuccess = true
}
sealed trait FailedParseAttemptStatus extends ParseAttemptStatus {
  override def isSuccess = false
}

object ParseAttemptStatus {

  /**
   * State that we initialize the variable to. Only exists until the first
   * parse attempt.
   */
  case object Uninitialized extends ParseAttemptStatus

  /**
   * The parse succeeded. The parse consumed no bits - i.e., was zero length.
   */
  case object Success_ZeroLength extends SuccessParseAttemptStatus

  /**
   * The parse succeeded. The parse consumed a separator that is being skipped.
   *
   * This is used only by separated sequences, and only for optional occurrences
   * when a separator is being passed over and ignored, without an occurrence
   * being added to the infoset.
   */
  case object Success_SkippedSeparator extends SuccessParseAttemptStatus

  /**
   * The parse succeeded. The parse consumed some bits - was not zero length.
   */
  case object Success_NotZeroLength extends SuccessParseAttemptStatus

  /**
   * The parse succeeded. We did not keep track of whether it consumed bits or not.
   */
  case object Success_LengthUndetermined extends SuccessParseAttemptStatus

  /**
   * The parse succeeded. We have reached the end of the variable occurrences.
   *
   * Ultimately all parses of variable-occurrence/PoU elements must end with
   * either Success_EndOfArray, or FailedWholeArray status. However, in some cases
   * a failure of an occurrence gets turned into a Success_EndOfArray (at a
   * prior occurrence) or a FailedWholeArray.
   */
  case object Success_EndOfArray extends SuccessParseAttemptStatus

  /**
   * The parse failed, and a discriminator was set, indicating that the
   * PoU was resolved first, and subsequently a failure occurred.
   */
  case object Failed_WithDiscriminatorSet extends FailedParseAttemptStatus

  /**
   * The parse failed. This was a forward-speculative parse, and it failed.
   */
  case object Failed_SpeculativeParse extends FailedParseAttemptStatus

  /**
   * The parse failed. This was a forward speculative parse, and it failed, but furthermore,
   * no forward progress was made.
   *
   * This status is only created by unseparated sequences.
   */
  case object Failed_NoForwardProgress extends FailedParseAttemptStatus

  /**
   * The parse failed. The entire array/optional element failed.
   *
   * When the number of occurrences is specified, then any failure results in
   * a failure of the entire array.
   *
   * When the number of occurrences is determined by speculative parsing,
   * then in some cases a failure of an occurrence gets turned into a
   * Success_EndOfArray (at a prior occurrence) or a Failed_WholeArray.
   */
  case object Failed_EntireArray extends FailedParseAttemptStatus

}

/**
 * An encapsulating parser for a term parser that is a child of a sequence.
 *
 * This class provides support for the iteration of the sequence over the occurrences
 * of the children, which must distinguish scalars from optional and array elements,
 * and must distinguish situations with specified numbers of occurrences from
 * those with points-of-uncertainty.
 */
abstract class SequenceChildParser(
  val childParser: Parser,
  val srd: SequenceRuntimeData,
  val trd: TermRuntimeData)
  extends CombinatorParser(srd) {

  override def runtimeDependencies: Vector[Evaluatable[AnyRef]] = Vector()
}

/**
 * Base for SequenceChildParsers that are repeating.
 *
 * This mixes in the interface. Implementations of this enable the
 * driver loop in OrderedSequenceParserBase to iterate over the occurrences
 * with a common interation pattern.
 */
abstract class RepeatingChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  val erd: ElementRuntimeData,
  baseName: String)
  extends SequenceChildParser(childParser, srd, erd)
  with MinMaxRepeatsMixin {

  def hasPoU: Boolean

  /**
   *  Invokes the child parser. Once, only. Does NOT do iterations of it.
   */
  override protected def parse(pstate: PState): Unit = {
    childParser.parse1(pstate)
    //
    // This is retained because tests look for this wording in error messages.
    // It isn't really necessary to re-encapsulate the failure like this, but
    // we otherwise have to chase down tests that depend on this wording.
    //
    if (pstate.processorStatus ne Success) {
      val cause = pstate.processorStatus.asInstanceOf[Failure].cause
      PE(pstate, "Failed to populate %s[%s]. Cause: %s.",
        erd.prefixedName, pstate.mpstate.arrayPos, cause)
      return
    }
  }

  /**
   * Tells us whether to attempt another array element at the current index,
   * and how we should interpret the existence of an element
   * or empty/zero-length based on the array index.
   *
   * NOTE: must be stateless. State must be passed in, and returned for
   * assignment to a loop var, or held in pstate.
   */
  def arrayIndexStatus(minRepeats: Long, maxRepeats: Long,
    pstate: PState,
    resultOfPriorTry: ParseAttemptStatus): ArrayIndexStatus = {
    import ParseAttemptStatus._
    import ArrayIndexStatus._
    val result =
      if (pstate.processorStatus ne Success)
        Failed
      else
        resultOfPriorTry match {
          case Success_EndOfArray => Done
          case _: SuccessParseAttemptStatus | Uninitialized =>
            if (pstate.arrayPos <= minRepeats)
              Required
            else if (pstate.arrayPos < maxRepeats)
              OptionalMiddle
            else if (pstate.arrayPos == maxRepeats)
              OptionalLast
            else
              Done
          // case FailedSpeculativeParse => Assert.invariantFailed("Should already be handled.")
          case _: FailedParseAttemptStatus => Failed
        }
    result
  }

  override def toString = "Rep" + baseName + "(" + childParser.toString + ")"

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<Rep" + baseName + " name='" + erd.name + "'>" + childParser.toBriefXML(depthLimit - 1) +
        "</Rep" + baseName + ">"
  }

  /**
   * Do things that are done at the start of an array-element.
   *
   * This applies to both variable-occurrence and fixed-occurrence array elements,
   * as well as optional elements.
   *
   * This applies for optional elements as well because expressions can access them
   * by way of index: e.g., fn:exists( optElement[dfdl:currentIndex()]  )
   *
   * This makes more sense if you consider that an "optional" element (minOccurs 0,
   * maxOccurs 1) when occursCountKind is 'parsed' is treated as an array with
   * an unbounded number of possible occurrences. Similarly, if occursCountKind is
   * 'expression', then minOccurs/maxOccurs are ignored (used only for validation), and
   * there can be more than 1 occurrence.
   */
  def startArray(state: PState): Unit = {

    state.mpstate.arrayIndexStack.push(1L) // one-based indexing
  }

  /**
   * Do things that must be done at the end of an array.
   *
   * This applies to both variable-occurrence and fixed-occurrence array elements,
   * as well as optional elements.
   *
   * This applies for optional elements as well because expressions can access them
   * by way of index: e.g., fn:exists( optElement[dfdl:currentIndex()]  )
   */
  def endArray(state: PState): Unit = {
    val actualOccurs = state.mpstate.arrayIndexStack.pop()

    if (state.processorStatus eq Success) {

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

}

/**
 * Indicates the status of an array index vis a vis whether the
 * element occurrence at that index is required, optional, etc.
 */
sealed trait ArrayIndexStatus

/**
 * Indicates that parsing of an element occurrence for that index
 * should be attempted.
 */
sealed trait GoArrayIndexStatus extends ArrayIndexStatus

/**
 * Indicates that the parsing of an element occurrence for that index
 * should be attempted, and that the index is for an optional occurrence.
 */
sealed trait OptionalArrayIndexStatus extends GoArrayIndexStatus

/**
 * Indicates that the parsing of an element occurence for that index should
 * not be attempted.
 */
sealed trait StopArrayIndexStatus extends ArrayIndexStatus

/**
 * Object to provide namespace for these constant values.
 */
object ArrayIndexStatus {

  /**
   * Indicates the array element occurrence index is less than or equal to
   * minOccurs, and so is required. However, for occursCountKind 'parsed' or 'stopValue'
   * this is never returned, as the min/max bounds are only advisory
   * for validation purposes in that case.
   */
  case object Required extends GoArrayIndexStatus

  /**
   * Indicates that the array element index is minOccurs or greater, and strictly less than maxOccurs.
   *
   * When maxOccurs is unbounded, this is always returned.
   */
  case object OptionalMiddle extends OptionalArrayIndexStatus

  /**
   * Indicates that the array element index is maxOccurs exactly, for any
   * element which has bounded occurrences.
   *
   * This is needed so we can decide NOT to consume a separator if we
   * fail on a zero-length string. This is used in some situations where we
   * tolerate redundant separators.
   */
  case object OptionalLast extends OptionalArrayIndexStatus

  /**
   * Indicates that pstate status is failed, that is, we
   * are unable to continue parsing. No parse attempt should be done for this
   * index.
   */
  case object Failed extends StopArrayIndexStatus

  /**
   * Indicates that we are done iterating, and should stop parsing more
   * array. Used to indicate that the end of the array was identified
   * by speculative parsing.
   */
  case object Done extends StopArrayIndexStatus
}

/**
 * Parser is for a non-scalar with a specific number of occurrences.
 *
 * There are no points-of-uncertainty (PoU).
 */
abstract class OccursCountExactParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData)
  extends RepeatingChildParser(childParser, srd, erd, "ExactN") {

  final override def isBoundedMax(max: Long) = true

  final override def minRepeats(pstate: ParseOrUnparseState) = maxRepeats(pstate)

  final override def maxRepeats(pstate: ParseOrUnparseState): Long = erd.maxOccurs match {
    case -1 => Long.MaxValue
    case _ => erd.maxOccurs
  }

  final override def hasPoU = false
}

/**
 * Parser is for a non-scalar with a specific number of occurrences given by
 * an occursCount expression.
 *
 * There are no points-of-uncertainty (PoU).
 */
abstract class OccursCountExpressionParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData,
  val occursCountEv: OccursCountEv)
  extends RepeatingChildParser(childParser, srd, erd, "Expression") {

  final override def hasPoU = false

  final override lazy val runtimeDependencies = Vector(occursCountEv)

  final override def isBoundedMax(max: Long) = true

  final override def minRepeats(pstate: ParseOrUnparseState) = maxRepeats(pstate)

  final override def maxRepeats(pstate: ParseOrUnparseState): Long = {
    val ocInt = occursCountEv.evaluate(pstate)
    ocInt
  }
}

/**
 * Trait shared by both repeating sequence child parsers and unparsers
 */
trait MinMaxRepeatsMixin {

  def erd: ElementRuntimeData

  private val ock = erd.maybeOccursCountKind.get

  private val minRepeats_ = {
    val mr =
      if (ock eq OccursCountKind.Parsed) 0
      else erd.minOccurs
    mr
  }

  /**
   * The digestion of minOccurs with the occursCountKind results in minRepeats.
   * For example, when occursCountKind is parsed, then minRepeats is 0, regardless
   * of the value of minOccurs.
   */
  def minRepeats(state: ParseOrUnparseState): Long = minRepeats_

  /**
   * True if the loop has a finite upper bound on number of iterations.
   * Meaning either it's specified occurrence count, or
   * for speculative parsing cases, it's not OCK parsed, or OCK implicit with
   * maxOccurs unbounded.
   */
  private val maxRepeats_ = {
    if (ock eq OccursCountKind.Parsed) Long.MaxValue
    else if (erd.maxOccurs == -1) Long.MaxValue
    else erd.maxOccurs
  }

  /**
   * The digestion of maxOccurs with the occursCountKind results in maxRepeats.
   * For example, when occursCountKind is parsed, then maxRepeats is -1 (meaning unbounded)
   * regardless of the value of maxOccurs.
   */
  def maxRepeats(state: ParseOrUnparseState): Long = maxRepeats_

  private val isBoundedMax_ = maxRepeats_ < Long.MaxValue

  def isBoundedMax(maxRepeats: Long) = isBoundedMax_

}

/**
 * Base class for parsers for terms which are parsed speculatively.
 *
 * These may respect min/maxOccurs, or may not depending on the occursCountKind.
 * (e.g., parsed uses 0 for min, unbounded for max, implicit does use the
 * min and max occurs values.)
 */
abstract class OccursCountMinMaxParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData)
  extends RepeatingChildParser(childParser, srd, erd, "MinMax") {

  Assert.invariant(erd.maybeOccursCountKind.isDefined)

  private val ock = erd.maybeOccursCountKind.get

  Assert.invariant(ock == OccursCountKind.Implicit ||
    ock == OccursCountKind.Parsed)

  final override def hasPoU = true
}
