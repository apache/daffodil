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

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.EmptyElementParsePolicy
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.ModelGroupRuntimeData

/**
 * Define the kinds of behaviors for terms in a sequence.
 *
 * Positional and Non-Positional are concepts that apply only to the children
 *  of separated sequences. They are not characteristics of the entire sequence,
 *  but of each child term of the sequence.
 *
 *  Positional Behavior - means that separators are required in order to determine
 *  which child of the sequence group is being parsed/unparsed.
 *
 *  Positional-Trailing Behavior - Like positional, but with special treatment for
 *  things at the end of a sequence, where extra separators for absentRep occurrences
 *  may be tolerated when parsing, but are not created when unparsing. A strict behavior
 *  applies only for parsing. In this case, there can be extra separators for absentRep,
 *  but only if they are subsequently followed by a non-absentRep (meaning nilRep, emptyRep
 *  that is distinct from absentRep, or normalRep). For Positional-Trailing behavior to
 *  apply, the sequence child must be potentially trailing.
 *
 *  Non-Positional Behavior - means that separators are not required to determine which
 *  child of the sequence group is being parsed/unparsed. The data must make this
 *  ambiguous with initiators, or with simply the accepted syntax of the data type.
 *
 *  A sequence child which is an array element with dfdl:occursCountKind fixed or expression
 *  this has positional behavior regardless of the separatorSuppressionPolicy of the sequence.
 *  For the occurrences of that element, the behavior is as if the sequence had
 *  separatorSuppressionPolicy 'never'.
 *
 *  A sequence child which is an array element with dfdl:occursCountKind parsed has
 *  non-positional behavior regardless of the separatorSuppressionPolicy of the sequence.
 *  For the occurrences of that element the behavior is as if the sequence had
 *  separatorSuppressionPolicy 'anyEmpty'
 *
 *  A sequence child which is an array element with dfdl:occursCountKind implicit has
 *  positional, positional-trailing, or non-positional behavior based on several other factors.
 */
sealed abstract class SeparatedSequenceChildBehavior
object SeparatedSequenceChildBehavior {
  type Type = SeparatedSequenceChildBehavior
  sealed abstract class PositionalLike extends Type
  sealed abstract class PositionalTrailing extends PositionalLike
  case object Positional extends PositionalLike
  case object PositionalNever extends PositionalLike
  case object PositionalTrailingLax extends PositionalTrailing
  case object PositionalTrailingStrict extends PositionalTrailing
  case object NonPositional extends Type
}

trait SeparatedSequenceChildParseResultHelper extends SequenceChildParseResultHelper {
  import SeparatedSequenceChildBehavior._

  def separatedSequenceChildBehavior: SeparatedSequenceChildBehavior
  protected final def sscb = separatedSequenceChildBehavior

  /**
   * Used for postfix separator case.
   *
   * Always false for model groups
   */
  def isSimpleDelimited: Boolean

  def computeFailedSeparatorParseAttemptStatus(
    parser: SequenceChildParser,
    prevBitPosBeforeChild: Long,
    pstate: PState,
    isZL: Boolean,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus = {
    ParseAttemptStatus.MissingSeparator
  }

  /**
   * Define this as final here so we aren't creating proliferation of
   * traits/classes just for this one little issue.
   */
  final def arrayCompleteChecks(
    parser: SequenceChildParser,
    pstate: PState,
    resultOfTry: ParseAttemptStatus,
    priorResultOfTry: ParseAttemptStatus
  ): Unit = {
    if ((sscb eq PositionalNever)) {
      val resultToTest = resultOfTry
      resultToTest match {
        case ParseAttemptStatus.FailureUnspecified | ParseAttemptStatus.MissingSeparator =>
          parser.PE(
            pstate,
            "maxOccurs instances and their separators are required when dfdl:separatorSuppressionPolicy='never'"
          )
        case _ => // ok
      }
    }
  }

  /**
   * Define this as final here so we aren't creating proliferation of
   * traits/classes just for this one little issue.
   */
  final def sequenceCompleteChecks(
    parser: SequenceChildParser,
    pstate: PState,
    resultOfTry: ParseAttemptStatus,
    priorResultOfTry: ParseAttemptStatus
  ): Unit = {

    if ((sscb eq PositionalTrailingStrict)) {
      val resultToTest = priorResultOfTry
      resultToTest match {
        case ParseAttemptStatus.AbsentRep | ParseAttemptStatus.EmptyRep =>
          parser.PE(
            pstate,
            "Empty trailing optional elements are not allowed when dfdl:separatorSuppressionPolicy='trailingEmptyStrict'"
          )
        case _ => // ok
      }
    }
  }
}

trait ScalarElementSeparatedSequenceChildParseResultHelper
  extends SeparatedSequenceChildParseResultHelper
  with ScalarElementSequenceChildParseResultHelper {

  def erd: ElementRuntimeData
}

trait PositionalLikeElementSeparatedSequenceChildParseResultMixin
  extends ElementSequenceChildParseResultHelper {

  final override protected def anyTypeElementFailedParseAttemptStatus(
    pstate: PState,
    isZL: Boolean,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus = {
    requiredOptional match {
      case _: RequiredOptionalStatus.Optional if isZL => {
        //
        // optional and zero-length
        // if we just said AbsentRep, that generally triggers backtracking an element
        //
        // Correction - if you set success here, then failures like when you parse
        // an int, but hit end of data, and get unable to convert empty string to int....
        // those should cause a failure, but those would be masked here.
        // pstate.setSuccess()
        //
        // Missing/Failed is certainly correct for NonPositional(anyEmpty)separated sequences.
        // Also for Positional, and Unseparated.
        //
        // PositionalTrailing is the question. There, a failure that is ZL we want to have
        // just be Absent.
        ParseAttemptStatus.AbsentRep
      }
      case _ if isZL =>
        ParseAttemptStatus.MissingItem
      case _ =>
        ParseAttemptStatus.FailureUnspecified
    }
  }

}

class PositionalScalarElementSeparatedSequenceChildParseResultHelper(
  override val separatedSequenceChildBehavior: SeparatedSequenceChildBehavior,
  override val erd: ElementRuntimeData,
  override val isSimpleDelimited: Boolean,
  override val emptyElementParsePolicy: EmptyElementParsePolicy,
  override val isEmptyRepZeroLength: Boolean,
  override val isEmptyRepNonZeroLength: Boolean
) extends ScalarElementSeparatedSequenceChildParseResultHelper
  with ScalarElementSequenceChildParseResultHelper
  with PositionalLikeElementSeparatedSequenceChildParseResultMixin

class NonPositionalScalarElementSeparatedSequenceChildParseResultHelper(
  override val separatedSequenceChildBehavior: SeparatedSequenceChildBehavior,
  override val erd: ElementRuntimeData,
  override val isSimpleDelimited: Boolean,
  override val emptyElementParsePolicy: EmptyElementParsePolicy,
  override val isEmptyRepZeroLength: Boolean,
  override val isEmptyRepNonZeroLength: Boolean
) extends ScalarElementSeparatedSequenceChildParseResultHelper
  with NonPositionalLikeElementSequenceChildParseResultMixin

class PositionalTrailingScalarElementSeparatedSequenceChildParseResultHelper(
  override val separatedSequenceChildBehavior: SeparatedSequenceChildBehavior,
  override val erd: ElementRuntimeData,
  override val isSimpleDelimited: Boolean,
  override val emptyElementParsePolicy: EmptyElementParsePolicy,
  override val isEmptyRepZeroLength: Boolean,
  override val isEmptyRepNonZeroLength: Boolean
) extends ScalarElementSeparatedSequenceChildParseResultHelper
  with PositionalLikeElementSeparatedSequenceChildParseResultMixin

trait RepElementSeparatedSequenceChildParseResultHelper
  extends SeparatedSequenceChildParseResultHelper
  with RepElementSequenceChildParseResultHelper

class PositionalTrailingRepElementSeparatedSequenceChildParseResultHelper(
  override val separatedSequenceChildBehavior: SeparatedSequenceChildBehavior,
  override val erd: ElementRuntimeData,
  override val isSimpleDelimited: Boolean,
  override val emptyElementParsePolicy: EmptyElementParsePolicy,
  override val isEmptyRepZeroLength: Boolean,
  override val isEmptyRepNonZeroLength: Boolean
) extends PositionalRepElementSeparatedSequenceChildParseResultHelper(
    separatedSequenceChildBehavior,
    erd,
    isSimpleDelimited,
    emptyElementParsePolicy,
    isEmptyRepZeroLength,
    isEmptyRepNonZeroLength
  )

class PositionalRepElementSeparatedSequenceChildParseResultHelper(
  override val separatedSequenceChildBehavior: SeparatedSequenceChildBehavior,
  override val erd: ElementRuntimeData,
  override val isSimpleDelimited: Boolean,
  override val emptyElementParsePolicy: EmptyElementParsePolicy,
  override val isEmptyRepZeroLength: Boolean,
  override val isEmptyRepNonZeroLength: Boolean
) extends RepElementSeparatedSequenceChildParseResultHelper
  with PositionalLikeElementSeparatedSequenceChildParseResultMixin {}

class NonPositionalRepElementSeparatedSequenceChildParseResultHelper(
  override val separatedSequenceChildBehavior: SeparatedSequenceChildBehavior,
  override val erd: ElementRuntimeData,
  override val isSimpleDelimited: Boolean,
  override val emptyElementParsePolicy: EmptyElementParsePolicy,
  override val isEmptyRepZeroLength: Boolean,
  override val isEmptyRepNonZeroLength: Boolean
) extends RepElementSeparatedSequenceChildParseResultHelper
  with NonPositionalLikeElementSequenceChildParseResultMixin

trait GroupSeparatedSequenceChildParseResultHelper
  extends SeparatedSequenceChildParseResultHelper
  with ModelGroupSequenceChildParseResultHelper {

  final override def isSimpleDelimited = false // only relevant to elements
}

trait PositionalLikeGroupSequenceChildParseResultMixin
  extends ModelGroupSequenceChildParseResultHelper {

  /**
   * Called directly sometimes.
   * Used by trickier parser (e.g., postfix separator helper) that
   */
  final protected def modelGroupSuccessParseAttemptStatus(
    parser: SequenceChildParser,
    pstate: PState,
    isZL: Boolean,
    mgrd: ModelGroupRuntimeData,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus = {
    Assert.invariant(pstate.isSuccess)
    checkModelGroupZL(pstate, isZL)
    if (isZL)
      ParseAttemptStatus.EmptyRep
    else
      ParseAttemptStatus.NormalRep
  }

}
class PositionalGroupSeparatedSequenceChildParseResultHelper(
  override val mgrd: ModelGroupRuntimeData,
  override val separatedSequenceChildBehavior: SeparatedSequenceChildBehavior,
  override val isModelGroupRepPossiblyZeroLength: Boolean,
  override val isModelGroupRepNonZeroLength: Boolean
) extends GroupSeparatedSequenceChildParseResultHelper
  with PositionalLikeGroupSequenceChildParseResultMixin {}
class PositionalTrailingGroupSeparatedSequenceChildParseResultHelper(
  override val mgrd: ModelGroupRuntimeData,
  override val separatedSequenceChildBehavior: SeparatedSequenceChildBehavior,
  override val isModelGroupRepPossiblyZeroLength: Boolean,
  override val isModelGroupRepNonZeroLength: Boolean
) extends GroupSeparatedSequenceChildParseResultHelper
  with PositionalLikeGroupSequenceChildParseResultMixin {

  final override def computeFailedSeparatorParseAttemptStatus(
    parser: SequenceChildParser,
    prevBitPosBeforeChild: Long,
    pstate: PState,
    isZL: Boolean,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus = {

    // When we're parsing a separated sequence, and we're positional trailing
    // and we don't find a separator, the child never even gets attempted to parse
    // we immediately say the group is absent.
    //
    // This allows it to be skipped, but also ends the sequence.
    //
    if (
      isModelGroupRepPossiblyZeroLength &&
      !isModelGroupRepNonZeroLength
    ) {
      pstate.setSuccess()
      ParseAttemptStatus.AbsentRep
    } else
      ParseAttemptStatus.FailureUnspecified
  }

}

class NonPositionalGroupSeparatedSequenceChildParseResultHelper(
  override val mgrd: ModelGroupRuntimeData,
  override val separatedSequenceChildBehavior: SeparatedSequenceChildBehavior,
  override val isModelGroupRepPossiblyZeroLength: Boolean,
  override val isModelGroupRepNonZeroLength: Boolean
) extends GroupSeparatedSequenceChildParseResultHelper {

  /**
   * Called directly sometimes.
   * Used by trickier parser (e.g., postfix separator helper) that
   */
  final protected def modelGroupSuccessParseAttemptStatus(
    parser: SequenceChildParser,
    pstate: PState,
    isZL: Boolean,
    mgrd: ModelGroupRuntimeData,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus = {
    if (pstate.isSuccess) {
      val maybeElem = pstate.infosetLastChild
      Assert.invariant(maybeElem.isDefined)
      val elem = maybeElem.get
      if (elem.isNilled) {
        ParseAttemptStatus.NilRep
      } else {
        ParseAttemptStatus.NormalRep
      }
    } else {
      if (isZL)
        ParseAttemptStatus.MissingItem // it is an error if ZL in NonPositional
      else
        ParseAttemptStatus.NormalRep
    }
  }

}
