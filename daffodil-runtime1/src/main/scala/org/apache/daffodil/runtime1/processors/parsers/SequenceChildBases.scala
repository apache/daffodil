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
package org.apache.daffodil.runtime1.processors.parsers

import org.apache.daffodil.lib.iapi.ValidationMode
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.OccursCountKind
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Evaluatable
import org.apache.daffodil.runtime1.processors.OccursCountEv
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.Processor
import org.apache.daffodil.runtime1.processors.SequenceRuntimeData
import org.apache.daffodil.runtime1.processors.Success
import org.apache.daffodil.runtime1.processors.TermRuntimeData

/**
 * Enables various sub-kinds of success/failure of a parse to be distinguished
 * easily. These are statuses of parsing of an occurrence of an array/optional
 * element, or when implied, the entire array/optional that contains that occurrence.
 * This applies to both specified and speculative number of occurrences.
 */
sealed trait ParseAttemptStatus {
  def isSuccess: Boolean = false
}

object ParseAttemptStatus {
  type Type = ParseAttemptStatus
  sealed trait SuccessParseAttemptStatus extends Type {
    override def isSuccess = true
  }
  sealed trait FailedParseAttemptStatus extends Type {
    override def isSuccess = false
  }

  /**
   * State that we initialize the variable to. Only exists until the first
   * parse attempt.
   */
  case object Uninitialized extends Type

  /**
   * Means the Nil representation was found in the data stream.
   *
   * Used only on nillable elements.
   *
   * The NilRep has priority over other representations. Due to
   * dfdl:nilValueDelimiterPolicy, the NilRep can be zero-length or non-zero length.
   */
  case object NilRep extends SuccessParseAttemptStatus

  /**
   * Empty rep is second in priority to NilRep. Due to
   * dfdl:emptyValueDelimiterPolicy, the EmptyRep can be zero-length or non-zero-length, which
   * allows the schema author to arrange for it to be distinguished from AbsentRep.
   *
   * An example would be dfdl:emptyValueDelimiterPolicy="both" dfdl:initiator='"' dfdl:terminator='"'.
   * Then in comma separated data, if you want to specify that a field contains an empty string,
   * the data must contain ....,"",.... i.e., open-close quotes to indicate a literal empty string.
   *
   * The EmptyRep for simpleTypes enables default values to be substituted at parse time.
   *
   * For simple types xs:string and xs:hexBinary, the property dfdl:emptyElementParsePolicy controls
   * whether the EmptyRep is allowed for strings and hexBinary. In required positions, when
   * dfdl:emptyElementParsePolicy is 'treatAsAbsent', a required string/hexBinary that has EmptyRep
   * causes a Parse Error, and an optional EmptyRep causes nothing to be added to the infoset (the empty string
   * or hexBinary value is suppressed). When dfdl:emptyElementParsePolicy is 'treatAsEmpty', a required
   * string/hexBinary with EmptyRep creates an empty string or zero-length byte array in the infoset.
   * An optional EmptyRep behaves differently depending on whether the EmptyRep is truly zero-length, or
   * dfdl:emptyValueDelimiterPolicy is such that EmptyRep is non-zero-length. When truly zero-length, no
   * value is added to the infoset. When non-zero-length, an empty string or zero-length byte array is added
   * to the infoset at the current index.
   *
   * An element may have no EmptyRep. For example, a fixed-length data element has no EmptyRep.
   *
   * An element of complex type can have EmptyRep, but dfdl:emptyValueDelimiterPolicy does not apply.
   * TBD: CONFIRM THIS. When a complex type element is parsed, and zero data is consumed, but the parse is successful,
   * then any infoset created is the "empty value" for this complex type element. When the element is required,
   * this infoset is retained. When the element is optional, this infoset is discarded, any side-effects that occurred
   * in its creation are backtracked.
   */
  case object EmptyRep extends SuccessParseAttemptStatus

  /**
   * When the parse is successful, and the data did not match NilRep(if nillable) or
   * EmptyRep(if defined/meaningful.)
   *
   * Normal means the data in the data stream matches the representation required for the
   * type of the element. For all simple types other than string and hexBinary, this requires
   * some representation in the data stream. For string and hexBinary it is possible for "normal"
   * data to be empty string, in which case normalRep is the same thing as emptyRep, and so can be
   * ambiguous with absentRep.
   */
  case object NormalRep extends SuccessParseAttemptStatus

  /**
   * Means the representation is zero-length, but if a separated
   * sequence, the separator was found. It also is lower priority
   * than the NilRep or the EmptyRep, if either of those can contain
   * zero-length.
   *
   * AbsentRep is only a concept when there is a way to distinguish
   * an occurrence of AbsentRep from a situation where there is just
   * a parse failure, for example when there are separators.
   *
   * This status influences when separatorSuppressionPolicy of trailingEmpty or
   * trailingEmptyStrict accepts and moves past extra adjacent separators.
   */
  case object AbsentRep extends FailedParseAttemptStatus

  /**
   * Base for statuses that indicate data is missing, which means that
   * we are able to find where it should have been, and isolate the length of
   * that area, but it is zero length. Typically these are used in delimited
   * formats where it is possible to recognize where data might have been located,
   * but can determine that it isn't present because it is zero-length, or
   * lacks a distinguishing separator.
   */
  trait Missing extends FailedParseAttemptStatus

  /**
   * Means the separator was not found for a separated sequence.
   * This is different from AbsentRep, which requires the separator to
   * be successfully parsed in a separated sequence.
   *
   * This status is obtained when parsing the sequence children and
   * you run out of them, and encounter data that does not have a separator
   * at all. (Typically an out of scope delimiter, or end-of-data.)
   */
  case object MissingSeparator extends Missing

  /**
   * Means that the representation was simply not found, but still there is
   * a way to determine where it should have been, and nothing is there. That
   * is, parsing failed, but zero data was consumed. Typically this would be
   * finding an out-of-scope delimiter or end-of-data *without* having found
   * a separator.
   */
  case object MissingItem extends Missing

  /**
   * Means that a failure has occurred after succesfully parsing a discriminator.
   * For example, in a choice with initiated content if an initiator is
   * successfully parsed, but the content of the choice branch fails, we should
   * not continue to attempt to parse the other branches.
   */
  case object UnorderedSeqDiscriminatedFailure extends FailedParseAttemptStatus

  /**
   * Means the parsing failed but no particular information about
   * separators or the length the data was consuming is available.
   *
   * As an example, if a fixed length field length 8 fails, then that
   * could be because there weren't 8 units of data available, or the 8
   * units didn't produce data of the right type, etc.
   *
   * This avoids overloading the term Missing to mean "just didn't parse successfully",
   * allowing us to give the term Missing to mean a more specific notion where
   * zero data was available.
   */
  case object FailureUnspecified extends FailedParseAttemptStatus
}

/**
 * Strong typing, not a bunch of booleans that can be
 * mixed up with each other.
 */
sealed abstract class PoUStatus
object PoUStatus {
  case object HasPoU extends PoUStatus
  case object NoPoU extends PoUStatus
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
  val trd: TermRuntimeData
) extends CombinatorParser(trd) {

  override def childProcessors: Vector[Processor] = Vector(childParser)

  override def runtimeDependencies: Vector[Evaluatable[AnyRef]] = Vector()

  final override def parse(pstate: PState): Unit =
    Assert.usageError("Not to be called on sequence child parsers")

  def parseOne(pstate: PState, requiredOptional: RequiredOptionalStatus): ParseAttemptStatus

  def maybeStaticRequiredOptionalStatus: Maybe[RequiredOptionalStatus]

  def isPositional: Boolean

  def pouStatus: PoUStatus

  def arrayCompleteChecks(
    pstate: PState,
    resultOfTry: ParseAttemptStatus,
    priorResultOfTry: ParseAttemptStatus
  ): Unit = {
    // does nothing by default.
    // overridden in separated sequence child parsers in some cases
  }

  def sequenceCompleteChecks(
    pstate: PState,
    resultOfTry: ParseAttemptStatus,
    priorResultOfTry: ParseAttemptStatus
  ): Unit = {
    // does nothing by default.
    // overridden in separated sequence child parsers in some cases
  }

}

trait NonRepeatingSequenceChildParser { self: SequenceChildParser =>

  def pouStatus: PoUStatus = PoUStatus.NoPoU

  def maybeStaticRequiredOptionalStatus: Maybe[RequiredOptionalStatus] =
    Maybe(RequiredOptionalStatus.Required)

}

/**
 * For computed elements, and for groups (which commonly will be sequences)
 * which contain only other non-represented entities, or executable
 * statements like asserts or setVar, and which have no
 * syntax of their own. These have no representation, their parsers just need
 * to be called for side-effect.
 */
final class NonRepresentedSequenceChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  trd: TermRuntimeData
) extends SequenceChildParser(childParser, srd, trd) {

  def pouStatus = PoUStatus.NoPoU

  def isPositional = false

  def maybeStaticRequiredOptionalStatus: Maybe[RequiredOptionalStatus] =
    Assert.usageError("not to be used for non-represented terms.")

  def parseOne(pstate: PState, ignored_roStatus: RequiredOptionalStatus): ParseAttemptStatus = {
    childParser.parse1(pstate)
    if (pstate.processorStatus eq Success)
      ParseAttemptStatus.NormalRep
    else
      ParseAttemptStatus.FailureUnspecified
  }
}

/**
 * Base for SequenceChildParsers that are repeating.
 *
 * This mixes in the interface. Implementations of this enable the
 * driver loop in OrderedSequenceParserBase to iterate over the occurrences
 * with a common iteration pattern.
 */
abstract class RepeatingChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  val erd: ElementRuntimeData,
  baseName: String
) extends SequenceChildParser(childParser, srd, erd)
  with MinMaxRepeatsMixin
  with EndArrayChecksMixin {

  final def maybeStaticRequiredOptionalStatus: Maybe[RequiredOptionalStatus] = Maybe.Nope

  /**
   * Tells us whether to attempt another array element at the current index,
   *
   * NOTE: must be stateless. State must be passed in, and returned for
   * assignment to a loop var, or held in pstate.
   */
  def arrayIndexStatus(minRepeats: Long, maxRepeats: Long, pstate: PState): ArrayIndexStatus = {
    import ArrayIndexStatus._
    Assert.invariant(pstate.processorStatus eq Success)
    val apos = pstate.arrayIterationPos
    val result: ArrayIndexStatus =
      if (apos <= minRepeats)
        Required
      else if (apos < maxRepeats)
        OptionalMiddle
      else if (apos == maxRepeats) {
        OptionalLast
      } else {
        Assert.invariant(apos == (maxRepeats + 1))
        Done
      }
    result
  }

  override def toString = "Rep" + baseName + "(" + childParser.toString + ")"

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..."
    else
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
    state.mpstate.arrayIterationIndexStack.push(1L) // one-based indexing
    state.mpstate.occursIndexStack.push(1L)
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
    state.mpstate.arrayIterationIndexStack.pop()
    val occurrences = state.mpstate.occursIndexStack.pop() - 1
    super.endArray(state, occurrences)
  }

}

/**
 * Base for Required/Optional information about any sequence child.
 */
sealed trait RequiredOptionalStatus

object RequiredOptionalStatus {
  type Type = RequiredOptionalStatus
  sealed trait Required extends Type
  object Required extends Required

  sealed trait Optional extends Type
  object Optional extends Optional
}

/**
 * Indicates the status of an array index vis a vis whether the
 * element occurrence at that index is required or variants on optional.
 */
sealed trait ArrayIndexStatus

object ArrayIndexStatus {
  trait Type extends ArrayIndexStatus

  case object Uninitialized extends Type

  /**
   * Indicates that we are done iterating, and should stop parsing more
   * array. Used to indicate that the end of the array was identified
   * by speculative parsing, or that we reached and finished the parse
   * of the element at index maxOccurs and are stepping past that.
   */
  case object Done extends Type

  /**
   * Indicates the array element occurrence index is less than or equal to
   * minOccurs, and so is required. However, for occursCountKind 'parsed' or 'stopValue'
   * this is never returned, as the min/max bounds are only advisory
   * for validation purposes in that case.
   */
  case object Required extends Type with RequiredOptionalStatus.Required

  /**
   * Indicates that the array element index is minOccurs or greater, and strictly less than maxOccurs.
   *
   * When maxOccurs is unbounded, this is always returned.
   */
  case object OptionalMiddle extends Type with RequiredOptionalStatus.Optional

  /**
   * Indicates that the array element index is maxOccurs exactly, for any
   * element which has bounded occurrences.
   *
   * This is needed so we can decide NOT to consume a separator if we
   * fail on a zero-length string. This is used in some situations where we
   * tolerate redundant separators.
   */
  case object OptionalLast extends Type with RequiredOptionalStatus.Optional
}

/**
 * Parser is for a non-scalar with a specific number of occurrences.
 *
 * There are no points-of-uncertainty (PoU).
 */
abstract class OccursCountExactParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData
) extends RepeatingChildParser(childParser, srd, erd, "ExactN") {

  final override def isBoundedMax = true

  final override def minRepeats(pstate: ParseOrUnparseState) = maxRepeats(pstate)

  final override def maxRepeats(pstate: ParseOrUnparseState): Long = erd.maxOccurs match {
    case -1 => Long.MaxValue
    case _ => erd.maxOccurs
  }

  final override def pouStatus = PoUStatus.NoPoU
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
  val occursCountEv: OccursCountEv
) extends RepeatingChildParser(childParser, srd, erd, "Expression") {

  final override def pouStatus = PoUStatus.NoPoU

  final override def runtimeDependencies = Vector(occursCountEv)

  final override def isBoundedMax = true

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

  final val ock = erd.maybeOccursCountKind.get

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

  def isBoundedMax: Boolean = isBoundedMax_

}

/**
 * Trait shared by both repeating sequence child parser and unparsers
 * for things that must be done at the end of an array
 */
trait EndArrayChecksMixin {

  def erd: ElementRuntimeData

  def endArray(state: ParseOrUnparseState, occurrences: Long): Unit = {
    if (state.processorStatus eq Success) {

      // if we should do limited validation
      val shouldValidate =
        state.dataProc.isDefined &&
          state.dataProc.value.validationMode == ValidationMode.Limited

      if (shouldValidate) {
        val minO = erd.minOccurs
        val maxO = erd.maxOccurs
        val isUnbounded = maxO == -1

        if (isUnbounded && occurrences < minO)
          state.validationError(
            "%s occurred '%s' times when it was expected to be a " +
              "minimum of '%s' and a maximum of 'UNBOUNDED' times.",
            erd.diagnosticDebugName,
            occurrences,
            minO
          )
        else if (!isUnbounded && (occurrences < minO || occurrences > maxO)) {
          state.validationError(
            "%s occurred '%s' times when it was expected to be a " +
              "minimum of '%s' and a maximum of '%s' times.",
            erd.diagnosticDebugName,
            occurrences,
            minO,
            maxO
          )
        } else {
          // ok
        }
      }
    }
  }
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
  erd: ElementRuntimeData
) extends RepeatingChildParser(childParser, srd, erd, "MinMax") {

  Assert.invariant(erd.maybeOccursCountKind.isDefined)

  Assert.invariant(
    ock == OccursCountKind.Implicit ||
      ock == OccursCountKind.Parsed
  )

  final override def pouStatus = PoUStatus.HasPoU
}
