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

  def parseOneWithSeparator(
    state: PState,
    requiredOptional: RequiredOptionalStatus
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
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus =
    parseOneWithInfixOrPrefixSeparator(true, pstate, requiredOptional)
}

final class InfixSeparatorHelper(sep: Parser, childParser: Parser, scParserArg: Separated)
  extends SeparatorParseHelper(sep, childParser, scParserArg)
  with InfixPrefixSeparatorHelperMixin {

  override def kind = "infix"

  override def parseOneWithSeparator(
    pstate: PState,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus = {

    val infixSepShouldBePresent =
      pstate.mpstate.groupPos > 1

    parseOneWithInfixOrPrefixSeparator(infixSepShouldBePresent, pstate, requiredOptional)
  }
}

trait InfixPrefixSeparatorHelperMixin { self: SeparatorParseHelper =>

  protected def kind: String

  final protected def parseOneWithInfixOrPrefixSeparator(
    shouldParseTheSep: Boolean,
    pstate: PState,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus = {

    val sepStatus =
      if (shouldParseTheSep) {

        // If a separator is expected, but optional, we may generate errors
        // while searching for it which should not be propagated upward
        requiredOptional match {
          case _: RequiredOptionalStatus.Optional => {
            // Create a point of uncertainty with which to reset if we find errors
            pstate.withPointOfUncertainty(
              "parseOneWithInfixOrPrefixSeparator",
              childParser.context
            ) { pou =>
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

    sepStatus match {
      case _: SeparatorParseStatus.SeparatorSuccess => {
        childParser.parse1(pstate)
        val pas =
          scParser.parseResultHelper.computeParseAttemptStatus(
            scParser,
            prevBitPosBeforeChild,
            pstate,
            requiredOptional
          )
        pas
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
  import ParseAttemptStatus._

  override def parseOneWithSeparator(
    pstate: PState,
    requiredOptional: RequiredOptionalStatus
  ): ParseAttemptStatus = {

    val prevBitPosBeforeChild = pstate.bitPos0b

    pstate.withPointOfUncertainty("PostfixSeparatorHelper", childParser.context) { pou =>
      childParser.parse1(pstate)
      val childSuccessful = pstate.processorStatus eq Success
      val childFailure = !childSuccessful // just makes later logic easier to read
      val bitPosAfterChildAttempt = pstate.bitPos0b
      val prh = scParser.parseResultHelper
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
            requiredOptional
          )
        sep.parse1(pstate)
        if (pstate.processorStatus eq Success) {
          // we got the postfix sep after successful parse of the data item
          // so whatever the status of the item was, that's the status overall.
          dataOnlyRep
        } else {
          // child successful, but
          // no postfix sep found.
          // capture failure reason, but reset position to before the child,
          // and remove any elements. That way this behaves like a prefix
          // or infix separator, where if you don't get the sep, then you
          // usually (except first one in infix case) don't run the child parser
          val failure = pstate.processorStatus.asInstanceOf[Failure]

          Assert.invariant(!pstate.isPointOfUncertaintyResolved(pou))
          pstate.resetToPointOfUncertainty(pou)

          pstate.setFailed(failure.cause)
          failedSeparator(pstate, "postfix")
          prh.computeFailedSeparatorParseAttemptStatus(
            scParser,
            prevBitPosBeforeChild,
            pstate,
            hasZLChildAttempt,
            requiredOptional
          )
        }
      } else {
        Assert.invariant(childFailure)
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
        if (isSimpleDelimited) {
          val failure = pstate.processorStatus.asInstanceOf[Failure]

          Assert.invariant(!pstate.isPointOfUncertaintyResolved(pou))
          pstate.resetToPointOfUncertainty(pou)

          sep.parse1(pstate)
          if (pstate.isSuccess) {
            val posAfterSep = pstate.bitPos0b
            // failed child, but success on postfix sep.
            // behave here just like a prefix separator that was then followed by a child failure
            pstate.setFailed(failure.cause)
            val pas =
              prh.computeParseAttemptStatus(
                scParser,
                prevBitPosBeforeChild,
                pstate,
                requiredOptional
              )
            val res = pas match {
              case AbsentRep => {
                pstate.dataInputStream.setBitPos0b(posAfterSep)
                MissingSeparator
              }
              case _ => pas
            }
            res
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
          // not simple delimited. We really have no way of knowing
          // if trying to parse just the sep now makes any sense at all.
          // (ex: the child could be failing because it is fixed length, with asserts that check the value
          // that fail.)
          val isZL = false
          prh.computeFailedParseAttemptStatus(
            scParser,
            prevBitPosBeforeChild,
            pstate,
            isZL,
            requiredOptional
          )
        }
      }

    }
  }
}
