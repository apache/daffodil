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

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors._
import org.apache.daffodil.schema.annotation.props.SeparatorSuppressionPolicy
import org.apache.daffodil.schema.annotation.props.gen.SeparatorPosition
import ArrayIndexStatus._
import org.apache.daffodil.dpath.NodeInfo

trait Separated { self: SequenceChildParser =>

  def sep: Parser
  def spos: SeparatorPosition
  def ssp: SeparatorSuppressionPolicy

  val childProcessors = Vector(sep, childParser)
}

sealed trait RepeatingSeparatedPoU extends Separated { self: RepeatingChildParser =>

  private lazy val shouldRemoveZLStringHexBinaryValue_ =
    false && // disable this feature as many test in daffodil-core depend on NOT removing these empty strings.
      isPotentiallyTrailing &&
      isLastDeclaredRepresentedInSequence &&
      (
        (ssp eq SeparatorSuppressionPolicy.TrailingEmpty) ||
        (ssp eq SeparatorSuppressionPolicy.TrailingEmptyStrict))

  /**
   * Tells us if we should remove a successfully parsed zero-length string
   * or hexBinary from the infoset, because it is optional, so even though
   * zero length may parse successfully and return an empty string or hexbinary
   * normal value, the optionality of the element wins out over the empty-string value, and
   * we don't put the element into the infoset as an array child.
   */
  final def shouldRemoveZLStringHexBinaryValue(ais: ArrayIndexStatus, erd: ElementRuntimeData): Boolean = {
    shouldRemoveZLStringHexBinaryValue_
  }

  /**
   *  Only needed for separated sequences, but in order to avoid code duplication
   *  we use this "fat interface" approach, and have these here by default.
   *
   *  Override in separated, hasPoU case.
   */
  def isPotentiallyTrailing: Boolean

  /**
   *  Only needed for separated sequences, but in order to avoid code duplication
   *  we use this "fat interface" approach, and have these here by default.
   *
   *  Override in separated, hasPoU case.
   */
  def isLastDeclaredRepresentedInSequence: Boolean

  /**
   * True for cases where we should deal with trailing separator toleration.
   */
  private lazy val shouldSuppressZLDelimitedParseFailures_ = {
    //
    // A complex type that fails to parse can't fail on zero length.
    // It can be successful on zero length, but if it fails, something will
    // have to have been scanned/examined from the input data.
    //
    val result =
      if (erd.isComplexType) false
      else {
        Assert.invariant(erd.isSimpleType)
        val ty = erd.optPrimType.get
        ty match {
          //
          // String and HexBinary, if they FAIL, given ZL, then they must have
          // assertions that cause them to fail because otherwise they would always
          // parse successfully on ZL.
          // So that means these aren't ever ZL failures
          //
          case NodeInfo.String => false
          case NodeInfo.HexBinary => false
          //
          // For non hexbinary or string, if it fails, and the length is zero,
          // that's the case where we should suppress the failure.
          case other => {
            // but only if the element decl is potentially trailing
            {
              isPotentiallyTrailing && // and only if it is declared last in its sequence.
                // isLastDeclaredRepresentedInSequence &&
                (
                  (ssp eq SeparatorSuppressionPolicy.TrailingEmpty) ||
                  (ssp eq SeparatorSuppressionPolicy.TrailingEmptyStrict))
            }
          }

        }
      }
    result
  }

  /**
   * True for cases where we should deal with trailing separator toleration.
   *
   * Combines the runtime information needed with static/schema-compile-time information
   * about the Sequence child.
   *
   * This is applicable only to OPTIONAL elements (as in between min/maxOccurs, i.e.,
   * has variable occurrences, and speculative parsing/PoU.
   */
  final def shouldSuppressZLDelimitedParseFailure(
    pstate: PState,
    hasZLParseAttempt: Boolean): Boolean = {
    val shouldSuppress =
      shouldSuppressZLDelimitedParseFailures_ && {
        Assert.invariant(erd.isSimpleType)
        hasZLParseAttempt
      }
    shouldSuppress
  }
}

final class ScalarOrderedSeparatedSequenceChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  trd: TermRuntimeData,
  val sep: Parser,
  val spos: SeparatorPosition,
  val ssp: SeparatorSuppressionPolicy)
  extends SequenceChildParser(childParser, srd, trd)
  with Separated {

  override def parse(state: PState) = childParser.parse1(state)

}

final class RepOrderedExactlyNSeparatedSequenceChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData,
  val sep: Parser,
  val spos: SeparatorPosition,
  val ssp: SeparatorSuppressionPolicy)
  extends OccursCountExactParser(childParser, srd, erd)
  with Separated

final class RepOrderedExactlyTotalOccursCountSeparatedSequenceChildParser(
  childParser: Parser,
  ocEv: OccursCountEv,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData,
  val sep: Parser,
  val spos: SeparatorPosition,
  val ssp: SeparatorSuppressionPolicy)
  extends OccursCountExpressionParser(childParser, srd, erd, ocEv)
  with Separated

final class RepOrderedWithMinMaxSeparatedSequenceChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData,
  override val sep: Parser,
  override val spos: SeparatorPosition,
  override val ssp: SeparatorSuppressionPolicy,
  override val isPotentiallyTrailing: Boolean,
  override val isLastDeclaredRepresentedInSequence: Boolean)
  extends OccursCountMinMaxParser(childParser, srd, erd)
  with RepeatingSeparatedPoU {

}

final class OrderedSeparatedSequenceParser(
  rd: SequenceRuntimeData,
  ssp: SeparatorSuppressionPolicy,
  spos: SeparatorPosition,
  sep: Parser,
  childrenArg: Vector[SequenceChildParser])
  extends OrderedSequenceParserBase(rd, childrenArg) {

  override lazy val childProcessors = (sep +: childrenArg.asInstanceOf[Seq[Parser]]).toVector

  /**
   * Parses (1) one iteration of an array with fixed/expression occurs count.
   * (2) a model group (3) a scalar element.
   *
   * Returns a status indicating success/failure and the nature of that success/failure.
   *
   * No backtracking supported.
   */
  final protected def parseOneWithoutPoU(
    parserArg: SequenceChildParser,
    trd: TermRuntimeData,
    pstate: PState): ParseAttemptStatus = {

    val parser = parserArg.asInstanceOf[SequenceChildParser with Separated]

    val finalStatus: ParseAttemptStatus = {
      // parse prefix sep if any
      val prefixSepSuccessful =
        if ((spos eq SeparatorPosition.Prefix) && trd.isRepresented) {
          sep.parse1(pstate)
          pstate.processorStatus eq Success
        } else
          true

      if (!prefixSepSuccessful) {
        failedSeparator(pstate, "prefix", trd)
        ParseAttemptStatus.Failed_EntireArray
      } else {
        // except for the first position of the group, parse an infix separator

        val infixSepSuccessful =
          if ((spos eq SeparatorPosition.Infix) && pstate.mpstate.groupPos > 1 && trd.isRepresented) {
            sep.parse1(pstate)
            pstate.processorStatus eq Success
          } else
            true

        if (!infixSepSuccessful) {
          failedSeparator(pstate, "infix", trd)
          ParseAttemptStatus.Failed_EntireArray
        } else {
          //
          // now we parse the child
          //
          if (pstate.dataProc.isDefined) pstate.dataProc.get.beforeRepetition(pstate, this)

          parser.parse1(pstate)

          if (pstate.dataProc.isDefined) pstate.dataProc.get.afterRepetition(pstate, this)

          val childSuccessful = pstate.processorStatus eq Success

          val res: ParseAttemptStatus = {
            if (!childSuccessful) {
              ParseAttemptStatus.Failed_EntireArray
            } else {
              Assert.invariant(childSuccessful)
              // parse postfix sep if any
              val postfixSepSuccessful =
                if ((spos eq SeparatorPosition.Postfix) && trd.isRepresented) {
                  sep.parse1(pstate)
                  pstate.processorStatus eq Success
                } else
                  true

              if (!postfixSepSuccessful) {
                failedSeparator(pstate, "postfix", trd)
                ParseAttemptStatus.Failed_EntireArray
              } else {

                //
                // successful parse of required element. Zero length or not.
                // note that anyEmpty doesn't apply in this case, and
                // that separatorSuppressionPolicy property should be ignored.
                //
                ParseAttemptStatus.Success_LengthUndetermined
                // returning lengthUndeterined indicates that because this was a
                // required element, treat it just like it had content,
                // even if it was zero length

              } // end if postfix success/fail
            } // child if child success/fail
          }
          res
        } // end if infix
      } // end if prefix
    }
    finalStatus
  }

  /**
   * Parses one iteration of an array/optional element, and returns
   * a status indicating success/failure and the nature of that success/failure.
   */
  final protected def parseOneWithPoU(
    parserArg: RepeatingChildParser,
    erd: ElementRuntimeData,
    pstate: PState,
    priorState: PState.Mark,
    ais: GoArrayIndexStatus,
    isBounded: Boolean): ParseAttemptStatus = {

    val parser = parserArg.asInstanceOf[RepeatingChildParser with RepeatingSeparatedPoU]

    val finalStatus: ParseAttemptStatus = {

      val bitPosBeforeSeparator = pstate.bitPos0b

      // parse prefix sep if any
      val prefixSepSuccessful =
        if (spos eq SeparatorPosition.Prefix) {
          sep.parse1(pstate)
          pstate.processorStatus eq Success
        } else
          true

      if (!prefixSepSuccessful) {
        failedSeparator(pstate, "prefix", erd)
        processFailedSeparatorWithPoU(pstate)
      } else {
        // except for the first position of the group, parse an infix separator

        val isInfix = spos eq SeparatorPosition.Infix

        val infixSepShouldBePresent =
          pstate.mpstate.groupPos > 1

        val infixSepSuccessful =
          if (isInfix && infixSepShouldBePresent) {
            sep.parse1(pstate)
            pstate.processorStatus eq Success
          } else
            true

        //
        // captures corner case when there is no separator because it's infix,
        // and this optional/array element was first in the group
        // so we don't put an infix separator in.
        //
        val wasInfixSepSkippedForInitialElement =
          isInfix && !infixSepShouldBePresent

        if (!infixSepSuccessful) {
          failedSeparator(pstate, "infix", erd)
          processFailedSeparatorWithPoU(pstate)
        } else {
          //
          // now we parse the child
          //
          val prevBitPosBeforeChild = pstate.bitPos0b

          pstate.pushDiscriminator

          if (pstate.dataProc.isDefined) pstate.dataProc.get.beforeRepetition(pstate, this)

          parser.parse1(pstate)

          if (pstate.dataProc.isDefined) pstate.dataProc.get.afterRepetition(pstate, this)

          val childSuccessful = pstate.processorStatus eq Success
          val childFailure = !childSuccessful // just makes later logic easier to read

          val bitPosAfterChildAttempt = pstate.bitPos0b

          val hasZLChildAttempt = prevBitPosBeforeChild == bitPosAfterChildAttempt
          val hasZLChildSuccess = childSuccessful && hasZLChildAttempt

          /**
           * This is what the DFDL Spec v1.0 calls the "absent" representation.
           * (Section 9.2.4). This is a kind of missing representation (section 9.2.6)
           */
          val isRepresentationAbsent = childFailure && hasZLChildAttempt

          val postfixSepSuccessful =
            if ((spos eq SeparatorPosition.Postfix) && (
              childSuccessful ||
              isRepresentationAbsent)) {
              // parse postfix sep if any
              sep.parse1(pstate)
              pstate.processorStatus eq Success
            } else
              true

          val bitPosAfterSeparator = pstate.bitPos0b

          if (!postfixSepSuccessful) {
            failedSeparator(pstate, "postfix", erd)
            processFailedSeparatorWithPoU(pstate)
          } else {

            //
            // At this point we know the separator (wherever located) was
            // successfully parsed, and we're past where separators are concerned
            // so the separator was successful.
            //
            val hasMadeForwardProgress = bitPosAfterSeparator > bitPosBeforeSeparator

            Assert.invariant(hasMadeForwardProgress || wasInfixSepSkippedForInitialElement || postfixSepSuccessful)

            val res = {
              if (childFailure) {
                if (pstate.discriminator == true) {
                  ParseAttemptStatus.Failed_WithDiscriminatorSet
                } else {
                  Assert.invariant(pstate.discriminator == false)
                  ais match {
                    case ArrayIndexStatus.Required => {
                      //
                      // failure of required element is handled by caller.
                      //
                      ParseAttemptStatus.Failed_SpeculativeParse
                    }
                    case opt: OptionalArrayIndexStatus => {
                      //
                      // failure of optional element
                      //
                      if (!wasInfixSepSkippedForInitialElement &&
                        isRepresentationAbsent &&
                        parser.shouldSuppressZLDelimitedParseFailure(pstate, hasZLChildAttempt)) {
                        //
                        // optional element failed, but for tolerating excess
                        // trailing separators, we want to keep the separator.
                        // But we must remove the speculated element.
                        //
                        // This will not work right if the separator and terminator are/can-be the same,
                        // as we'll never find the terminator after a failure that happens
                        // to have zero-length. We'll find the "separator", and fail to parse
                        // but we'll suppress this failure in the case where we're supposed
                        // to tolerate trailing additional separators. So we succeed, consume
                        // the "separator", and the separator will never be reconsidered as the terminator.
                        //
                        // However, solving for "separator can be equal to terminator", so as to issue
                        // an SDE is pretty difficult. It has to be done in an Evaluatable, so that
                        // the delimiters can be run-time computed. Since delimiters can have
                        // things like characters class entities in them, deciding this is quite hard.
                        //
                        // In general, the ambiguity this creates is a "user-beware" situation
                        // and the right thing for the runtime to do is make sure a trace/debug
                        // session can help the user isolate what is actually happening.
                        //

                        // backtrack away this speculative element and any variable side-effects
                        pstate.reset(priorState)
                        // but that resets the position to before the separator
                        // so we set position to after the separator
                        pstate.dataInputStream.setBitPos0b(bitPosAfterSeparator)
                        //
                        // use special return status that indicates we've taken care of the
                        // priorState and repositioning after a skipped separator.
                        //
                        ParseAttemptStatus.Success_SkippedSeparator
                      } else {
                        //
                        // optional element failed but separator was found (or it's the first of an infix-separated array)
                        // this is regular speculative parse failure to find the element.
                        //
                        ParseAttemptStatus.Failed_SpeculativeParse
                      }
                    }
                  }
                }
              } else {
                Assert.invariant(childSuccessful)

                val result: ParseAttemptStatus = ais match {
                  case Required => {
                    //
                    // Success on a required element.
                    // We don't actually care if it is zero length or non-zero length.
                    //
                    ParseAttemptStatus.Success_LengthUndetermined
                  }
                  case _: OptionalArrayIndexStatus => {
                    //
                    // Now we have to analyze the cases where ZL matters and needs special handling
                    //
                    if (hasZLChildSuccess) {
                      val shouldRemoveZLElement = parser.shouldRemoveZLStringHexBinaryValue(ais, erd)
                      if (shouldRemoveZLElement) {
                        //
                        // It's an optional element, type is string/hexBinary and length is zero
                        // so we don't want to add it to the infoset
                        //
                        // Note: This does seem to be correct interpretation of the DFDL v1.0 spec.
                        // However, the language is very subtle. Hopefully it gets clarified.
                        //
                        // However, we do want to keep trying to parse more, as they could be trailing separators.
                        // that are to be tolerated.
                        //
                        // So we don't backtrack here. We just remove the accumulated element.
                        //
                        // Note that we do NOT backout variable side-effects that occurred while parsing
                        // and also a discriminator could be set to true.
                        // So while we are suppressing adding the element to the infoset,
                        // we're not suppressing side-effects that occurred during its parsing.
                        //
                        priorState.restoreInfoset(pstate)
                      }
                    } // end if ZL success
                    ParseAttemptStatus.Success_NotZeroLength // NotZeroLength due to separator
                  }
                }
                result
              } // end if child success/fail
            } // end val res
            pstate.popDiscriminator
            res
          } // end if postfix
        } // end if infix
      } // end if prefix
    } // end val finalStatus
    finalStatus
  }

  override protected def zeroLengthSpecialChecks(pstate: PState, wasLastChildZeroLength: Boolean): Unit = {
    if ((pstate.processorStatus eq Success) && wasLastChildZeroLength) {
      if (ssp eq SeparatorSuppressionPolicy.TrailingEmptyStrict)
        PE(pstate, "Empty trailing elements are not allowed when dfdl:separatorSuppressionPolicy='trailingEmptyStrict'")
    }
  }

  /**
   * Sets PState status to failure when separator is not found.
   * Used for BOTH PoU and non-PoU
   */
  private def failedSeparator(pstate: PState, kind: String, trd: TermRuntimeData): Unit = {
    val cause = pstate.processorStatus.asInstanceOf[Failure].cause
    trd match {
      case erd: ElementRuntimeData if (erd.isArray) =>
        PE(pstate, "Failed to populate %s[%s]. Missing %s separator. Cause: %s",
          erd.prefixedName, pstate.mpstate.arrayPos, kind, cause)
      case _ =>
        PE(pstate, "Failed to parse %s separator. Cause: %s.", kind, cause)
    }
  }

  /**
   * Handles backtracking for PoU case of failed separator.
   */
  private def processFailedSeparatorWithPoU(pstate: PState): ParseAttemptStatus = {
    if (pstate.discriminator == true) {
      ParseAttemptStatus.Failed_WithDiscriminatorSet
    } else {
      ParseAttemptStatus.Failed_SpeculativeParse
    }
  }
}
