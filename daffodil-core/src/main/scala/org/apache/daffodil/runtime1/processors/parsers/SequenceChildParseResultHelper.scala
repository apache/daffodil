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
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.runtime1.infoset.DIComplex
import org.apache.daffodil.runtime1.infoset.DIElement
import org.apache.daffodil.runtime1.infoset.DISimple
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.ModelGroupRuntimeData

sealed abstract class PotentiallyTrailingStatus
object PotentiallyTrailingStatus {
  case object IsPotentiallyTrailing extends PotentiallyTrailingStatus
  case object NotPotentiallyTrailing extends PotentiallyTrailingStatus
}

/**
 * These helpers convert a parse state into an appropriate
 * ParseAttemptStatus that can be acted upon uniformly by the
 * loops iterating through sequence children parsers.
 *
 * The schema compiler can make many decisions statically, and supply
 * a helper which takes a minimum of runtime overhead to make the actual
 * decisions about whether data for example, should be considered emptyRep
 * or absentRep based on static characteristics of the item (element, group)
 * and the required/optionality of the item.
 */
trait SequenceChildParseResultHelper extends Serializable {

  /**
   * Based on position within the group, is there a specific required/optional status
   * that always holds.
   *
   * This is about position in the group, and not the ability of the element/model-group
   * to have zero-length representation.
   *
   * Defined for non-repeating sequence children (groups, scalar elements) and always
   * Required for them.
   *
   * Undefined for repeating sequence children.
   */
  def maybeStaticRequiredOptionalStatus: Maybe[RequiredOptionalStatus]

  /**
   * Compute the ParseAttemptStatus, given the state of the parse immediately after parsing
   * the item (which could be group or element).
   */
  def computeParseAttemptStatus(
    parser: SequenceChildParser,
    prevBitPosBeforeChild: Long,
    pstate: PState,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus

  /**
   * Computes the parse attempt status after a failure to parse.
   *
   * The concept of zero-length failure actually exists and is sensible, mostly
   * for delimited formats, where a failure to parse can be due to finding adjacent
   * separators. This is recognized specifically, and for Positional children
   * of a sequence, this absence of any representation - but immediately finding
   * another separator is AbsentRep, and it allows us to consume a separator, even
   * though we failed to parse.
   */
  def computeFailedParseAttemptStatus(
    parser: SequenceChildParser,
    prevBitPosBeforeChild: Long,
    pstate: PState,
    isZL: Boolean,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus

}

trait ElementSequenceChildParseResultHelper extends SequenceChildParseResultHelper {

  def erd: ElementRuntimeData

  /**
   * The emptyRep must be zero length.
   *
   * If empty is meaningful, the combination of emptyValueDelimiterPolicy, initiator and
   * terminator is such that the emptyRep is zero length.
   *
   * For empty to be meaningful this requires the element
   * to be defaultable. For simple type elements that means it has XSD default or fixed attributes
   * on the element decl. The lengthKind also must be such that it's possible for
   * the representation to be zero length. (E.g., lengthKind 'explicit' with a constant non-zero length
   * expression can never be zero-length so empty isn't even meaningful.
   *
   * Empty is also not meaningful for nonRepresented elements or model groups (which are model groups with
   * no syntax and all non-represented content)
   *
   * This can be true for model groups if they have no syntax and their content is all optional.
   * defaultable, or nonRepresented. Such model groups can have zero-length representations even though
   * their parsing may succeed and add things to the infoset.
   */
  def isEmptyRepZeroLength: Boolean

  /**
   * The emptyRep must be greater than zero length.
   *
   * If empty is meaningful, the combination of emptyValueDelimiterPolicy, initiator and
   * terminator is such that the emptyRep is greater than zero length.
   *
   * False if empty is not meaningful (see also discussion of isEmptyRepZeroLength).
   *
   * False for model groups, and for complexType elements.
   *
   * The DFDL Spec tells us complex types cannot have non zero emptyRep.
   * Section 9.3.2.2 (Sept 2014 Draft)
   *     "the parser descends into the complex type for the element, and returns
   *      successfully (that is, no unsuppressed processing error occurs).
   *      If the result is zero bits consumed, the representation is then established
   *      by checking, in order, for:
   *      * empty representation.
   *      * absent representation (if none of the prior representations apply).
   *      Otherwise the element has normal representation."
   *
   * This basically says that you can't depend on dfdl:emptyValueDelimiterPolicy and
   * initiator/terminator that define an empty representation that is non-zero length.
   * It really means that feature is only for simple types. Complex types match the emptyRep
   * really only if they are truly zero length, aka "zero bits consumed" above.
   */
  def isEmptyRepNonZeroLength: Boolean

  def emptyElementParsePolicy: EmptyElementParsePolicy

  /**
   * Compute the ParseAttemptStatus, given the state of the parse immediately after parsing
   * the item (which could be group or element).
   */
  final override def computeParseAttemptStatus(
    parser: SequenceChildParser,
    prevBitPosBeforeChild: Long,
    pstate: PState,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus = {

    val currentBitPosAfterChild = pstate.bitPos0b
    val isZL = {
      Assert.invariant(currentBitPosAfterChild >= prevBitPosBeforeChild)
      currentBitPosAfterChild == prevBitPosBeforeChild
    }
    if (pstate.isSuccess) {
      val maybeElem = pstate.infosetLastChild
      Assert.invariant(maybeElem.isDefined)
      val elem = maybeElem.get
      if (elem.isNilled) {
        ParseAttemptStatus.NilRep
      } else {
        // not nilled
        val optPrimType = erd.optPrimType
        if (optPrimType.isDefined) {
          simpleTypeSuccessParseAttemptStatus(
            parser,
            pstate,
            isZL,
            erd,
            elem.asSimple,
            requiredOptional
          )
        } else {
          complexTypeSuccessParseAttemptStatus(
            parser,
            pstate,
            isZL,
            erd,
            elem.asComplex,
            requiredOptional
          )
        }
      }
    } else {
      Assert.invariant(pstate.isFailure)
      computeFailedParseAttemptStatus(
        parser,
        prevBitPosBeforeChild,
        pstate,
        isZL,
        requiredOptional
      )
    } // end if isSuccess/isFailed
  }

  /**
   * Called directly sometimes.
   * Used by trickier parser (e.g., postfix separator helper) that
   */
  final override def computeFailedParseAttemptStatus(
    parser: SequenceChildParser,
    prevBitPosBeforeChild: Long,
    pstate: PState,
    isZL: Boolean,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus = {
    Assert.usage(pstate.isFailure)
    val optPrimType = erd.optPrimType
    if (optPrimType.isDefined) {
      simpleTypeFailedParseAttemptStatus(parser, pstate, isZL, erd, requiredOptional)
    } else {
      complexTypeFailedParseAttemptStatus(parser, pstate, isZL, erd, requiredOptional)
    }
  }

  /**
   * Did the most recent parse succeed consuming the emptyRep.
   */
  final protected def isEmptyRep(
    parser: SequenceChildParser,
    pstate: PState,
    isZL: Boolean,
    maybeElem: Maybe[DIElement]
  ): Boolean = {
    Assert.invariant(pstate.isSuccess)
    val isIt =
      if (isEmptyRepZeroLength) {
        isZL
      } else if (isEmptyRepNonZeroLength) {
        if (isZL)
          false
        else {
          // the empty rep is non zero length, but we got a
          // successful parse that is non-zero length.
          // We need to determine if it was a match for the
          // emptyRep or not
          Assert.invariant(maybeElem.isDefined)
          val elem = maybeElem.get
          val elemERD = elem.erd
          Assert.invariant(elemERD eq erd)
          val optDefaultValue = erd.optDefaultValue
          if (optDefaultValue.isDefined) {
            Assert.invariant(erd.isSimpleType)
            val se = elem.asSimple
            if (se.isDefaulted) {
              // Behave, in this code, as if defaulting was
              // implemented, and the elem would already have its default value.
              true
            } else {
              // did not default the value
              // but we know it is defaultable or we wouldn't be here
              // since isEmptyRepNonZeroLength is true here for this simple type
              // what was consumed must not have matched the emptyRep
              // Behave here as if defaulting was properly implemented, and already
              // would have put the default into the infoset element
              false
            } // end if isDefaulted
          } else {
            Assert.invariant(!optDefaultValue.isDefined)
            // not defaultable element. Must have content.
            Assert.invariant(!isZL)
            false
          } // end isDefaultable
        } // end if isZL
      } else {
        Assert.invariant(!isEmptyRepZeroLength && !isEmptyRepNonZeroLength)
        false
      }
    isIt
  }

  final protected def simpleTypeSuccessParseAttemptStatus(
    parser: SequenceChildParser,
    pstate: PState,
    isZL: Boolean,
    erd: ElementRuntimeData,
    elem: DISimple,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus = {
    Assert.invariant(pstate.isSuccess)
    val isEmpty = isEmptyRep(parser, pstate, isZL, Maybe(elem))
    if (isEmpty) {
      requiredOptional match {
        case _: RequiredOptionalStatus.Required => {
          if (erd.optDefaultValue.isDefined) {
            Assert.invariant(erd.isSimpleType)
            pstate.schemaDefinitionError("Default values not implemented.")
          } else {
            emptyElementParsePolicy match {
              case EmptyElementParsePolicy.TreatAsMissing |
                  EmptyElementParsePolicy.TreatAsAbsent => { // deprecated: TreatAsMissing
                parser.PE(pstate, "Empty element not allowed for required element.")
                ParseAttemptStatus.MissingItem
              }
              case EmptyElementParsePolicy.TreatAsEmpty => {
                elem.dataValue.getAnyRef match {
                  case string: String if string.length == 0 => // ok
                  case byteArray: Array[Byte] if byteArray.length == 0 => // ok
                  case _ => Assert.invariant(!isZL) // must be nonZL empty rep.
                }
                ParseAttemptStatus.EmptyRep // success. EmptyRep. Value is an empty string or hexBinary
              }
            } // end match
          } // end if not defaultable
        } // end case Required
        case _: RequiredOptionalStatus.Optional => {
          ParseAttemptStatus.AbsentRep // callers will backtrack any elements created but retain bit position.
        }
      } // end match requiredOptional
    } else {
      Assert.invariant(!isEmpty)
      // Assert.invariant(!isZL) // does not hold in unseparated cases (NITF ran into this)
      ParseAttemptStatus.NormalRep
    }
  } // end method

  final protected def complexTypeSuccessParseAttemptStatus(
    parser: SequenceChildParser,
    pstate: PState,
    isZL: Boolean,
    erd: ElementRuntimeData,
    elem: DIComplex,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus = {
    requiredOptional match {
      case _: RequiredOptionalStatus.Required if isZL =>
        ParseAttemptStatus.EmptyRep
      case _: RequiredOptionalStatus.Required =>
        ParseAttemptStatus.NormalRep
      case _: RequiredOptionalStatus.Optional if isZL => {
        //
        // This should only happen if the complex type was entirely defaulted.
        // If, for example, the complex type contained a nillable element, and populated
        // that not by defaulting, but by parsing (E.g., if %ES; is a nilValue),
        // then we should NOT treat this as Absent, but Normal.
        if (elem.isDefaulted)
          ParseAttemptStatus.AbsentRep // caller will backtrack this element but retain bit position
        else
          ParseAttemptStatus.NormalRep
      }
      case _: RequiredOptionalStatus.Optional =>
        ParseAttemptStatus.NormalRep
    }
  }

  /**
   * Override in unseparated helpers and Positional helpers
   */
  protected def anyTypeElementFailedParseAttemptStatus(
    pstate: PState,
    isZL: Boolean,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus

  final protected def simpleTypeFailedParseAttemptStatus(
    parser: SequenceChildParser,
    pstate: PState,
    isZL: Boolean,
    erd: ElementRuntimeData,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus =
    anyTypeElementFailedParseAttemptStatus(pstate, isZL, requiredOptional)

  final protected def complexTypeFailedParseAttemptStatus(
    parser: SequenceChildParser,
    pstate: PState,
    isZL: Boolean,
    erd: ElementRuntimeData,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus =
    anyTypeElementFailedParseAttemptStatus(pstate, isZL, requiredOptional)
}

trait ModelGroupSequenceChildParseResultHelper extends SequenceChildParseResultHelper {

  def mgrd: ModelGroupRuntimeData

  /**
   * The model group's representation could be zero length.
   *
   * False if the model group is not represented (contains no syntax, and
   * all content is recursively not represented)
   *
   * True if the model group has no mandatory syntax, and all content within
   * is possibly ZL (recursively), defaultable, optional, or not represented.
   */
  def isModelGroupRepPossiblyZeroLength: Boolean

  /**
   * The model group's representation must be greater than zero length.
   */
  def isModelGroupRepNonZeroLength: Boolean

  /**
   * a group is always required.
   */
  final override def maybeStaticRequiredOptionalStatus: Maybe[RequiredOptionalStatus] =
    Maybe(RequiredOptionalStatus.Required)

  /**
   * Compute the ParseAttemptStatus, given the state of the parse immediately after parsing
   * the item (which could be group or element).
   */
  final override def computeParseAttemptStatus(
    parser: SequenceChildParser,
    prevBitPosBeforeChild: Long,
    pstate: PState,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus = {
    val currentBitPosAfterChild = pstate.bitPos0b
    val isZL = {
      Assert.invariant(currentBitPosAfterChild >= prevBitPosBeforeChild)
      currentBitPosAfterChild == prevBitPosBeforeChild
    }
    if (pstate.isSuccess) {
      checkModelGroupZL(pstate, isZL)
      modelGroupSuccessParseAttemptStatus(parser, pstate, isZL, mgrd, requiredOptional)
    } else {
      Assert.invariant(pstate.isFailure)
      computeFailedParseAttemptStatus(
        parser,
        prevBitPosBeforeChild,
        pstate,
        isZL,
        requiredOptional
      )
    } // end if isSuccess/isFailed
  }

  /**
   * Did the most recent parse succeed consuming zero length, and is that allowed?
   */
  final protected def checkModelGroupZL(pstate: PState, isZL: Boolean): Unit = {
    Assert.invariant(pstate.isSuccess)
    val isIt = isZL
    if (isZL) {
      // This doesn't hold if
      // (a) the format has a terminator which is an expression
      // (b) the expression evaluates to say, %WSP*; or %ES; based on looking at other infoset information.
      // (c) that delimiter matches zero length
      // (d) the lengthKind is NOT delimited. So we're not scanning for this.
      // This comes up in mil-std-2045 and other formats which have an optional
      // final terminator after a string having lengthKind 'pattern'.
      // In that case, the static information would indicate positively that isModelGroupRepNonZeroLength
      // is true, when it isn't.
      //
      // Bug DAFFODIL-2132 is why isModelGroupRepNonZeroLength is incorrect in the above case.
      //
      // Assert.invariant(isModelGroupRepPossiblyZeroLength && !isModelGroupRepNonZeroLength)
    }
  }

  /**
   * Called directly sometimes.
   * Used by trickier parser (e.g., postfix separator helper) that
   */
  protected def modelGroupSuccessParseAttemptStatus(
    parser: SequenceChildParser,
    pstate: PState,
    isZL: Boolean,
    mgrd: ModelGroupRuntimeData,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus

  final def computeFailedParseAttemptStatus(
    parser: SequenceChildParser,
    prevBitPosBeforeChild: Long,
    pstate: PState,
    isZL: Boolean,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus = {
    if (isZL) ParseAttemptStatus.MissingItem
    else ParseAttemptStatus.FailureUnspecified
  }

}

/**
 * Shared logic here between separated and unseparated for Scalar Elements.
 */
trait ScalarElementSequenceChildParseResultHelper
  extends ElementSequenceChildParseResultHelper {

  /**
   * a scalar element is always required.
   */
  override def maybeStaticRequiredOptionalStatus: Maybe[RequiredOptionalStatus] =
    Maybe(RequiredOptionalStatus.Required)

  def erd: ElementRuntimeData
}

trait RepElementSequenceChildParseResultHelper extends ElementSequenceChildParseResultHelper {

  final override def maybeStaticRequiredOptionalStatus: Maybe[RequiredOptionalStatus] =
    Maybe.Nope
}

/**
 * Applies to non-positional separated sequences, and unseparated sequences.
 */
trait NonPositionalLikeElementSequenceChildParseResultMixin
  extends ElementSequenceChildParseResultHelper {

  override protected def anyTypeElementFailedParseAttemptStatus(
    pstate: PState,
    isZL: Boolean,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus = {
    if (isZL)
      ParseAttemptStatus.MissingItem
    else
      ParseAttemptStatus.FailureUnspecified
  }

}
