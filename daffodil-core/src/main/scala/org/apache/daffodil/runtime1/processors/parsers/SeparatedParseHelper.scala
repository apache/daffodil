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
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Failure
import org.apache.daffodil.runtime1.processors.Success

sealed abstract class SeparatorParseStatus
object SeparatorParseStatus {
  type Type = SeparatorParseStatus
  sealed abstract class SeparatorSuccess extends Type
  case object SeparatorFound extends SeparatorSuccess
  case object SeparatorNotNeeded extends SeparatorSuccess
  case object SeparatorFailed extends Type
}

/**
 * Abstracts away separator position. Digests result of parsing
 * item and associated separator into a summary status.
 */
sealed abstract class SeparatorParseHelper(
  protected val sep: Parser,
  protected val childParser: Parser,
  scParserArg: Separated
) extends Serializable {

  protected val scParser = scParserArg.asInstanceOf[SequenceChildParser with Separated]

  /**
   * Checks, without consuming data, whether one of the sequence's in-scope
   * delimiters is present right where this complex type's content would
   * begin. Nope for simple types and for model groups.
   */
  protected final def zeroLengthComplexTypeDelimiterScanner: Maybe[Parser] =
    scParser.parseResultHelper.zeroLengthComplexTypeDelimiterScanner

  def parseOneWithSeparator(
    state: PState,
    requiredOptional: RequiredOptionalStatus,
    maybePOU: Maybe[PState.Mark]
  ): ParseAttemptStatus

  /**
   * Sets PState status to failure when separator is not found.
   */
  final protected def failedSeparator(pstate: PState, kind: String): Unit = {
    val cause = pstate.processorStatus.asInstanceOf[Failure].cause
    scParser.trd match {
      case erd: ElementRuntimeData if (erd.isArray) =>
        sep.PE(
          pstate,
          "Failed to populate %s[%s]. Missing %s separator. Cause: %s",
          erd.prefixedName,
          pstate.mpstate.arrayIterationPos,
          kind,
          cause
        )
      case _ =>
        sep.PE(pstate, "Failed to find %s separator. Cause: %s.", kind, cause)
    }
  }
}

final class PrefixSeparatorHelper(sep: Parser, childParser: Parser, scParserArg: Separated)
  extends SeparatorParseHelper(sep, childParser, scParserArg)
  with InfixPrefixSeparatorHelperMixin {

  override def kind = "prefix"

  override def parseOneWithSeparator(
    pstate: PState,
    requiredOptional: RequiredOptionalStatus,
    maybePOU: Maybe[PState.Mark]
  ): ParseAttemptStatus =
    parseOneWithInfixOrPrefixSeparator(true, pstate, requiredOptional, maybePOU)
}

final class InfixSeparatorHelper(sep: Parser, childParser: Parser, scParserArg: Separated)
  extends SeparatorParseHelper(sep, childParser, scParserArg)
  with InfixPrefixSeparatorHelperMixin {

  override def kind = "infix"

  override def parseOneWithSeparator(
    pstate: PState,
    requiredOptional: RequiredOptionalStatus,
    maybePOU: Maybe[PState.Mark]
  ): ParseAttemptStatus = {

    val infixSepShouldBePresent =
      pstate.mpstate.groupPos > 1

    parseOneWithInfixOrPrefixSeparator(
      infixSepShouldBePresent,
      pstate,
      requiredOptional,
      maybePOU
    )
  }
}

trait InfixPrefixSeparatorHelperMixin { self: SeparatorParseHelper =>

  protected def kind: String

  final protected def parseOneWithInfixOrPrefixSeparator(
    shouldParseTheSep: Boolean,
    pstate: PState,
    requiredOptional: RequiredOptionalStatus,
    maybePOU: Maybe[PState.Mark]
  ): ParseAttemptStatus = {

    val sepStatus =
      if (shouldParseTheSep) {

        // If a separator is expected, but optional, we may generate errors
        // while searching for it which should not be propagated upward
        requiredOptional match {
          case _: RequiredOptionalStatus.Optional => {
            // Optional children always receive the caller's PoU; it is a bug if
            // maybePOU is not defined here.
            Assert.invariant(maybePOU.isDefined)
            val pou = maybePOU.get
            sep.parse1(pstate)

            val rv = if (pstate.processorStatus eq Success) {
              SeparatorParseStatus.SeparatorFound
            } else {
              // reset to the point of uncertainty to discard the errors
              pstate.resetToPointOfUncertainty(pou)
              SeparatorParseStatus.SeparatorFailed
            }
            rv
          }
          case _ => {
            sep.parse1(pstate)
            if (pstate.processorStatus eq Success)
              SeparatorParseStatus.SeparatorFound
            else
              SeparatorParseStatus.SeparatorFailed
          }
        }
      } else
        SeparatorParseStatus.SeparatorNotNeeded

    val prevBitPosBeforeChild = pstate.bitPos0b
    val separatorWasFound = sepStatus eq SeparatorParseStatus.SeparatorFound

    sepStatus match {
      case _: SeparatorParseStatus.SeparatorSuccess => {
        childParser.parse1(pstate)
        val pas =
          scParser.parseResultHelper.computeParseAttemptStatus(
            scParser,
            prevBitPosBeforeChild,
            pstate,
            requiredOptional,
            separatorWasFound
          )
        val anotherSeparatorFollows = pstate.probeNonDestructively(sep)
        pas match {
          case ParseAttemptStatus.AbsentRep
              if pstate.isFailure && 
                separatorWasFound &&
                (scParser.parseResultHelper.separatedSequenceChildBehavior eq
                  SeparatedSequenceChildBehavior.NonPositional) &&
                !anotherSeparatorFollows =>
              // Only for NonPositional (anyEmpty): a failed, zero-length
              // attempt whose separator isn't followed by another one isn't
              // a genuine zero-length occurrence of this item; release it
              // (via the usual MissingItem backtrack) for the next item to
              // claim instead.
              ParseAttemptStatus.MissingItem
          case _ => pas
        }
      }
      case _ => {
        requiredOptional match {
          case _: RequiredOptionalStatus.Required => {
            failedSeparator(pstate, kind)
          }
          case _ => // No action
        }
        scParser.parseResultHelper.computeFailedSeparatorParseAttemptStatus(
          scParser,
          prevBitPosBeforeChild,
          pstate,
          isZL = true,
          requiredOptional
        )
      }
    }
  }

}

final class PostfixSeparatorHelper(
  sep: Parser,
  child: Parser,
  scParserArg: Separated,
  isSimpleDelimited: Boolean
) extends SeparatorParseHelper(sep, child, scParserArg) {

  override def parseOneWithSeparator(
    pstate: PState,
    requiredOptional: RequiredOptionalStatus,
    maybePOU: Maybe[PState.Mark]
  ): ParseAttemptStatus = {
    // Only the trailing model group result helper can mask a missing separator
    // as success, which requires undoing the group first. Such a group never
    // gets a caller PoU, so it uses one scoped to this attempt.
    if (
      scParser.parseResultHelper
        .isInstanceOf[PositionalTrailingGroupSeparatedSequenceChildParseResultHelper]
    ) {
      Assert.invariant(!maybePOU.isDefined)
      pstate.withPointOfUncertainty("PostfixSeparatorHelper", childParser.context) { pou =>
        parseChildAndSeparator(
          pstate,
          requiredOptional,
          callerPou = Maybe.Nope,
          scopedPou = Maybe(pou)
        )
      }
    } else {
      parseChildAndSeparator(
        pstate,
        requiredOptional,
        callerPou = maybePOU,
        scopedPou = Maybe.Nope
      )
    }
  }

  private def parseChildAndSeparator(
    pstate: PState,
    requiredOptional: RequiredOptionalStatus,
    callerPou: Maybe[PState.Mark],
    scopedPou: Maybe[PState.Mark]
  ): ParseAttemptStatus = {

    val prevBitPosBeforeChild = pstate.bitPos0b
    val prh = scParser.parseResultHelper

    childParser.parse1(pstate)
    // The scoped PoU is not one of the schema's points of uncertainty. If a
    // discriminator consumed it, also resolve the enclosing one it was meant
    // for (spec 9.3.3.1).
    if (scopedPou.isDefined && pstate.isPointOfUncertaintyResolved(scopedPou.get)) {
      pstate.resolvePointOfUncertainty()
    }
    val childSuccessful = pstate.processorStatus eq Success
    val bitPosAfterChildAttempt = pstate.bitPos0b
    // This seems odd, but there are cases where after a failure
    // the bitPos has not yet been repositioned back to where it started.
    // This does happen for complex types.
    val hasZLChildAttempt = prevBitPosBeforeChild == bitPosAfterChildAttempt

    if (childSuccessful) {
      val dataOnlyRep =
        prh.computeParseAttemptStatus(
          scParser,
          prevBitPosBeforeChild,
          pstate,
          requiredOptional,
          separatorWasFound = false // separator not parsed yet at this point
        )
      sep.parse1(pstate)
      if (pstate.processorStatus eq Success) {
        // we got the postfix sep after successful parse of the data item
        // so whatever the status of the item was, that's the status overall.
        dataOnlyRep
      } else {
        // child successful, but no postfix sep found. Build the error while the
        // position is still after the child.
        failedSeparator(pstate, "postfix")
        if (scopedPou.isDefined && pstate.isPointOfUncertaintyResolved(scopedPou.get)) {
          // A discriminator inside the child committed to it (spec 9.3.3.1), so
          // the missing separator is a hard failure, not an absent item.
          ParseAttemptStatus.FailureUnspecified
        } else {
          if (scopedPou.isDefined) {
            val sepFailure = pstate.processorStatus.asInstanceOf[Failure].cause
            pstate.resetToPointOfUncertainty(scopedPou.get)
            pstate.setFailed(sepFailure)
          }
          prh.computeFailedSeparatorParseAttemptStatus(
            scParser,
            prevBitPosBeforeChild,
            pstate,
            hasZLChildAttempt,
            requiredOptional
          )
        }
      }
    } else {
      // We might be tempted to just try the postfix sep now,
      // to see if the postfix sep is there immediately.
      // This is, however, problematic.
      //
      // We're depending on this item being an element with lengthKind="delimited"
      // which can't have a zero-length representation, so that if there is zero-data before
      // the postfix separator, we can declare the item to be absentRep.
      //
      // But what if the element is not lengthKind 'delimited'.
      // Or what if it has complex type? But has a non-zero-length structure where this
      // same delimiter is used at a more deeply nested depth?
      //
      // If the type is simple, and lengthKind 'delimited' we can proceed.
      // Otherwise we punt and call it missing.
      //
      // The retry also needs an unresolved PoU to restore to, which a child
      // without one (such as a required scalar) or a discriminator-resolved
      // PoU does not have.
      if (
        isSimpleDelimited && callerPou.isDefined &&
        !pstate.isPointOfUncertaintyResolved(callerPou.get)
      ) {
        val failure = pstate.processorStatus.asInstanceOf[Failure]

        pstate.restoreToPointOfUncertainty(callerPou.get)

        sep.parse1(pstate)
        if (pstate.isSuccess) {
          // child failed, but postfix sep found immediately after reset
          // marking this as ZL content.
          // Use hasZLChildAttempt (captured before sep.parse1) so isZL reflects
          // the child bit position, not the post-separator position.
          pstate.setFailed(failure.cause)
          val pas = prh.computeFailedParseAttemptStatus(
            scParser,
            prevBitPosBeforeChild,
            pstate,
            hasZLChildAttempt,
            requiredOptional,
            separatorWasFound = true
          )
          // For non-positional (anyEmpty) sequences, the child result helper returns
          // MissingItem for optional ZL elements; since the postfix separator WAS
          // found, a non-array element is AbsentRep per DFDL spec 9.2.4.
          val isArrayOccurrence = scParser.trd match {
            case erd: ElementRuntimeData => erd.isArray
            case _ => false
          }
          val isOptional = requiredOptional.isInstanceOf[RequiredOptionalStatus.Optional]
          pas match {
            case ParseAttemptStatus.MissingItem | ParseAttemptStatus.AbsentRep
                if isOptional && isArrayOccurrence =>
              // The array ends here and the separator is left for the enclosing
              // group, which may need it as its own.
              ParseAttemptStatus.MissingSeparator
            case ParseAttemptStatus.MissingItem if isOptional =>
              ParseAttemptStatus.AbsentRep
            case _ => pas
          }
        } else {
          // the separator failed on ZL data
          // so no chance on a ZL representation here.
          val isZL = false
          prh.computeFailedSeparatorParseAttemptStatus(
            scParser,
            prevBitPosBeforeChild,
            pstate,
            isZL,
            requiredOptional
          )
        }
      } else {
        // The bare-separator retry does not apply: not simple delimited, or no
        // unresolved PoU. We have no way of knowing whether trying just the sep
        // makes sense (ex: the child could be failing because it is fixed
        // length, with asserts that check the value that fail.)
        val isZL = false
        prh.computeFailedParseAttemptStatus(
          scParser,
          prevBitPosBeforeChild,
          pstate,
          isZL,
          requiredOptional,
          separatorWasFound = false
        )
      }
    }
  }
}
