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

import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.{ ElementRuntimeData, Failure, ModelGroupRuntimeData, SequenceRuntimeData, Success, TermRuntimeData }
import org.apache.daffodil.schema.annotation.props.SeparatorSuppressionPolicy
import org.apache.daffodil.schema.annotation.props.gen.SeparatorPosition
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.schema.annotation.props.gen.OccursCountKind

trait Separated { self: SequenceChildParser =>
  import ArrayIndexStatus._

  def sep: Parser
  def spos: SeparatorPosition
  def ssp: SeparatorSuppressionPolicy

  val childProcessors = Seq(sep, childParser)

  /**
   * Tells us if we should remove a successfully parsed zero-length string
   * or hexBinary from the infoset, because it is optional, so even though
   * zero length may parse successfully and return an empty string or hexbinary,
   * the optionality of the element wins out over the empty-string value, and
   * we don't put the element into the infoset as an array child.
   */
  def shouldRemoveZLStringHexBinaryValue(ais: ArrayIndexStatus, erd: ElementRuntimeData): Boolean = {
    val optional = ais eq Optional
    val res =
      if (!optional) false
      else {
        val optPT = erd.optPrimType
        if (optPT.isEmpty) false
        else {
          val pt: NodeInfo.PrimType = optPT.get
          pt match {
            case NodeInfo.PrimType.String => true
            case NodeInfo.PrimType.HexBinary => true
            case _ => false
          }
        }
      }
    res
  }
}

class ScalarOrderedRequiredSeparatedSequenceChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  trd: TermRuntimeData,
  val sep: Parser,
  val spos: SeparatorPosition,
  val ssp: SeparatorSuppressionPolicy)
  extends SequenceChildParser(childParser, srd, trd)
  with Separated {

  override def parse(state: PState) = childParser.parse1(state)

  override def shouldRemoveZLStringHexBinaryValue(ais: ArrayIndexStatus, erd: ElementRuntimeData): Boolean = false
}

class ExactlyNLoopState(limit: Long) extends ParseLoopState {
  import ArrayIndexStatus._

  def arrayIndexStatus(parser: SequenceChildParser,
    pstate: PState,
    resultOfPriorTry: ParseAttemptStatus) = {
    val res =
      if (pstate.processorStatus ne Success)
        Failed
      else {
        resultOfPriorTry match {
          case _: FailedParseAttemptStatus => ArrayIndexStatus.Failed
          case _ => {
            if (pstate.arrayPos <= limit)
              Required
            else
              MaxExceeded
          }
        }
      }
    res
  }

  def nextArrayIndex(pstate: PState): Long = {
    val res = pstate.arrayPos
    pstate.mpstate.moveOverOneArrayIndexOnly() // advance array position
    res
  }
}

trait OccursCountExactLoopStateMixin { self: SequenceChildParser =>

  def repeatCount: Long
  val minRepeats = 0L
  val maxRepeats = repeatCount

  def loopState(pstate: PState): ParseLoopState = {
    new ExactlyNLoopState(repeatCount)
  }
}

class RepOrderedExactlyNSeparatedSequenceChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  val erd: ElementRuntimeData,
  val repeatCount: Long,
  val sep: Parser,
  val spos: SeparatorPosition,
  val ssp: SeparatorSuppressionPolicy,
  val baseName: String = "ExactlyN")
  extends SequenceChildParser(childParser, srd, erd)
  with OccursCountExactLoopStateMixin
  with Separated
  with RepParser

trait OccursCountExpressionLoopStateMixin { self: SequenceChildParser =>

  def ocParser: Parser

  def loopState(pstate: PState): ParseLoopState = {
    ocParser.parse1(pstate)
    val ocInt = pstate.mpstate.occursBounds.toInt
    new ExactlyNLoopState(ocInt)
  }
}

class RepOrderedExactlyTotalOccursCountSeparatedSequenceChildParser(
  childParser: Parser,
  override val ocParser: Parser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData,
  sep: Parser,
  spos: SeparatorPosition,
  ssp: SeparatorSuppressionPolicy)
  extends RepOrderedExactlyNSeparatedSequenceChildParser(childParser,
    srd, erd,
    { val ignored = 0; ignored },
    sep, spos, ssp,
    "ExactlyTotalOccursCount")
  with OccursCountExpressionLoopStateMixin {

  override val childProcessors = Seq(ocParser, childParser)

  override def loopState(pstate: PState) = super.loopState(pstate)
}

trait OccursCountMinMaxLoopStateMixin { self: SequenceChildParser =>
  def min: Long
  def max: Long
  def erd: ElementRuntimeData

  def loopState(pstate: PState): ParseLoopState =
    new MinMaxLoopState(erd, min, max)
}

/**
 * Pass nothing, or -1 for min/max to mean use erd.minOccurs/maxOccurs
 * Pass -2 for max to force unbounded behavior.
 */
class RepOrderedWithMinMaxSeparatedSequenceChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  val erd: ElementRuntimeData,
  val baseName: String,
  val sep: Parser,
  val spos: SeparatorPosition,
  val ssp: SeparatorSuppressionPolicy,
  val min: Long = -1,
  val max: Long = -1) // pass -2 to force unbounded behavior
  extends SequenceChildParser(childParser, srd, erd)
  with Separated
  with RepParser
  with OccursCountMinMaxLoopStateMixin

class MinMaxLoopState(erd: ElementRuntimeData, min: Long, max: Long) extends ParseLoopState {
  import ArrayIndexStatus._

  Assert.invariant(erd.maybeOccursCountKind.isDefined)
  lazy val minRepeats = if (min == -1) erd.minOccurs else min
  lazy val maxRepeats = max match {
    case -1 if (erd.maxOccurs == -1) => Long.MaxValue
    case -1 => if (erd.maybeOccursCountKind.get eq OccursCountKind.Parsed) Long.MaxValue else erd.maxOccurs
    case -2 => Long.MaxValue
    case _ => max
  }

  def arrayIndexStatus(
    parser: SequenceChildParser,
    pstate: PState,
    resultOfPriorTry: ParseAttemptStatus): ArrayIndexStatus = {
    import ParseAttemptStatus._

    if (pstate.processorStatus ne Success)
      return Failed
    resultOfPriorTry match {
      case Success_EndOfArray => return Done
      case _: SuccessParseAttemptStatus => // ok
      case Uninitialized => // ok
      case FailedEntireArray => return Failed
      case FailedWithDiscriminatorSet => return Failed
      case FailedSpeculativeParse => Assert.invariantFailed("Should already be handled.")
    }
    if (pstate.arrayPos <= minRepeats)
      return Required
    if (pstate.arrayPos <= maxRepeats)
      return Optional
    MaxExceeded
  }

  def nextArrayIndex(pstate: PState): Long = {
    val res = pstate.arrayPos
    pstate.mpstate.moveOverOneArrayIndexOnly() // advance array position
    res
  }
}

class OrderedSeparatedSequenceParser(rd: SequenceRuntimeData,
  ssp: SeparatorSuppressionPolicy,
  spos: SeparatorPosition,
  sep: Parser,
  childrenArg: Seq[SequenceChildParser])
  extends OrderedSequenceParserBase(rd, childrenArg) {

  import SequenceChildParser._
  //
  // TODO: It would be good to get rid of this downcast
  // leaving it for now, as the split into Separated/Unseparated might get
  // recombined if upon implementing the separated interations it seems
  // like they don't need to be independent code.
  //
  private val children = childrenArg.map { _.asInstanceOf[SeparatedChildParser] }

  private def failedSeparator(pstate: PState, priorState: PState.Mark, kind: String): Unit = {
    val cause = pstate.processorStatus.asInstanceOf[Failure].cause
    PE(pstate, "Failed to parse %s separator. Cause: %s.", kind, cause)
  }

  private def failedZeroLengthWithAnyEmpty(pstate: PState, priorState: PState.Mark) = {
    PE(pstate, "Failed to parse sequence child. Cause: zero-length data, but dfdl:separatorSuppressionPolicy is 'anyEmpty'.")
    ParseAttemptStatus.FailedSpeculativeParse
  }

  /**
   * Parses one iteration of an array/optional element, and returns
   * * MaybeBoolean.One(true) - indicates the child parse was zero length
   * * MaybeBoolean.One(false) - indicates the child parse was not zero length or failed
   * * MaybeBoolean.Nope - indicates that the array loop should terminate. If due to discriminator failure
   * the pstate will indicate failure. If due to speculative parsing finding the end of the array
   * then pstate will indicate success.
   */
  protected def parseOne(
    parserArg: SequenceChildParser,
    trd: TermRuntimeData,
    pstate: PState,
    priorState: PState.Mark,
    maybeStartState: Maybe[PState.Mark],
    ais: GoArrayIndexStatus): ParseAttemptStatus = {

    val parser = parserArg.asInstanceOf[SeparatedChildParser]

    val isFixedOccurs = maybeStartState.isEmpty
    val isVariableOccurs = !isFixedOccurs

    val finalStatus: ParseAttemptStatus = {
      // parse prefix sep if any
      val prefixSepSuccessful =
        if ((spos eq SeparatorPosition.Prefix) && trd.isRepresented) {
          sep.parse1(pstate)
          pstate.processorStatus eq Success
        } else
          true

      if (!prefixSepSuccessful) {
        failedSeparator(pstate, priorState, "prefix")
        processFailedChildParseResults(pstate, priorState, maybeStartState)
      } else {
        // except for the first position of the group, parse an infix separator

        val infixSepSuccessful =
          if ((spos eq SeparatorPosition.Infix) && pstate.mpstate.childPos > 1 && trd.isRepresented) {
            sep.parse1(pstate)
            pstate.processorStatus eq Success
          } else
            true

        if (!infixSepSuccessful) {
          failedSeparator(pstate, priorState, "infix")
          processFailedChildParseResults(pstate, priorState, maybeStartState)
        } else {
          //
          // now we parse the child
          //
          val prevBitPos = pstate.bitPos0b

          if (isVariableOccurs) {
            pstate.pushDiscriminator
          }

          if (pstate.dataProc.isDefined) pstate.dataProc.get.beforeRepetition(pstate, this)

          parser.parse1(pstate)

          if (pstate.dataProc.isDefined) pstate.dataProc.get.afterRepetition(pstate, this)

          val childSuccessful = pstate.processorStatus eq Success

          val res: ParseAttemptStatus = {
            if (!childSuccessful) {
              processFailedChildParseResults(pstate, priorState, maybeStartState)
            } else {
              Assert.invariant(childSuccessful)
              val zl = prevBitPos == pstate.bitPos0b
              // parse postfix sep if any
              val postfixSepSuccessful =
                if ((spos eq SeparatorPosition.Postfix) && trd.isRepresented) {
                  sep.parse1(pstate)
                  pstate.processorStatus eq Success
                } else
                  true

              if (!postfixSepSuccessful) {
                failedSeparator(pstate, priorState, "postfix")
                processFailedChildParseResults(pstate, priorState, maybeStartState)
              } else {

                processSuccessfulChildParseResults(trd, parser, zl, pstate, priorState, maybeStartState, ais)

              } // end if postfix success/fail
            } // child if child success/fail
          }
          if (isVariableOccurs) {
            pstate.popDiscriminator
          }
          res
        } // end if infix
      } // end if prefix
    }
    finalStatus
  }

  private def processSuccessfulChildParseResults(
    trd: TermRuntimeData,
    parser: SeparatedChildParser,
    zl: Boolean,
    pstate: PState,
    priorState: PState.Mark,
    maybeStartState: Maybe[PState.Mark],
    ais: GoArrayIndexStatus): ParseAttemptStatus = {

    val isFixedOccurs = maybeStartState.isEmpty
    val isVariableOccurs = !isFixedOccurs

    //
    // Total success with this array child.
    //
    if (isFixedOccurs) {
      //
      // successful parse of required element. Zero length or not.
      // note that anyEmpty doesn't apply in this case, and
      // that property should be ignored.
      //
      ParseAttemptStatus.Success_LengthUndetermined
    } else {
      Assert.invariant(isVariableOccurs)
      val startState = maybeStartState.get
      processSuccessfulVariableOccursChildParseResults(trd, parser, zl, pstate, priorState, startState, ais)
    }
  }

  private def processSuccessfulVariableOccursChildParseResults(trd: TermRuntimeData,
    parser: SeparatedChildParser,
    zl: Boolean,
    pstate: PState,
    priorState: PState.Mark,
    startState: PState.Mark,
    ais: GoArrayIndexStatus): ParseAttemptStatus = {
    import ArrayIndexStatus._

    //
    // Now we have to analyze the cases where ZL matters and needs special handling
    //
    val result: ParseAttemptStatus = ais match {
      case Required => processSuccessfulRequiredVariableOccursChildParseResults(zl, pstate, priorState, startState)
      case Optional => processSuccessfulOptionalVariableOccursChildParseResults(trd, parser, zl, pstate, priorState, startState, ais)
    }
    result

  }

  private def processSuccessfulRequiredVariableOccursChildParseResults(
    zl: Boolean,
    pstate: PState,
    priorState: PState.Mark,
    startState: PState.Mark): ParseAttemptStatus = {
    //
    // It's required. If it is empty, then that has to work as a value
    // for us (so must work as empty string value, or empty hexBinary.
    // we don't need to check for that here, as it is detected elsewhere.
    //
    // But we still have to check if we're allowed to have any empty
    // at all according to separatorSuppressionPolicy
    //
    // FIXME: verify this. It might be that ssp is ignored for required
    // elements
    //
    if (zl) {
      if (ssp eq SeparatorSuppressionPolicy.AnyEmpty) {
        //
        // Not allowed to have any empty at all
        //
        failedZeroLengthWithAnyEmpty(pstate, priorState)
      } else {
        ParseAttemptStatus.Success_ZeroLength // ZL, successful parse of required element
      }
    } else {
      ParseAttemptStatus.Success_NotZeroLength // not ZL, successful parse of required element
    }
  }

  private def processSuccessfulOptionalVariableOccursChildParseResults(trd: TermRuntimeData,
    parser: SeparatedChildParser,
    zl: Boolean,
    pstate: PState,
    priorState: PState.Mark,
    startState: PState.Mark,
    ais: GoArrayIndexStatus): ParseAttemptStatus = {
    if (zl) {
      val shouldRemoveZLElement =
        trd match {
          case erd: ElementRuntimeData =>
            parser.shouldRemoveZLStringHexBinaryValue(ais, erd)
          case mgd: ModelGroupRuntimeData =>
            false
        }
      if (shouldRemoveZLElement) {
        // it's an optional element, type is string/hexBinary and length is zero
        // so we don't want to add it to the infoset
        //
        // However, we do want to keep trying to parse more, as they could be trailing separators.
        // that are to be tolerated.
        //
        // So we don't backtrack here. We just remove the accumulated element.
        //
        pstate.reset(priorState) // no need discard priorState, that is implicitly discarded by resetting the startState
        pstate.discard(startState) // finished with array, so cleanup
        ParseAttemptStatus.Success_EndOfArray // success, but we're at end of array. not zero length (in this case with success at prior element)
      } else {
        //
        // optional, zero length, successful parse, not a string/hexBinary
        //
        // This can happen if you parse a choice group that
        // has all its branches entirely optional
        //
        if (ssp eq SeparatorSuppressionPolicy.AnyEmpty) {
          //
          // Not allowed to have any empty at all
          //
          failedZeroLengthWithAnyEmpty(pstate, priorState)
        } else {
          ParseAttemptStatus.Success_ZeroLength // ZL, successful parse of optional 'thing'
        }
      }
    } else {
      ParseAttemptStatus.Success_NotZeroLength // not ZL, successful optional element parse
    }
  }

  protected def parse(pstate: PState): Unit = {

    var scpIndex = 0
    pstate.mpstate.groupIndexStack.push(1L) // one-based indexing

    val limit = children.length

    var wasLastChildZeroLength = false

    while ((scpIndex < limit) && (pstate.processorStatus eq Success)) {
      val child = children(scpIndex)
      child match {
        case parser: RepSeparatedChildParser => {

          val loopState = parser.loopState(pstate)

          // push new array context for array/optional
          parser.startArray(pstate)

          val maybeStartState =
            if (parser.erd.maxOccurs != parser.erd.minOccurs)
              Maybe(pstate.mark("startState in OrderedSeparatedSequenceParser"))
            else
              Maybe.Nope

          var ais: ArrayIndexStatus = null

          var resultOfTry: ParseAttemptStatus = ParseAttemptStatus.Uninitialized

          while ({
            ais = loopState.arrayIndexStatus(parser, pstate, resultOfTry)
            ais.isInstanceOf[GoArrayIndexStatus]
          }) {

            resultOfTry =
              tryParseDetectMarkLeaks(parser, pstate, maybeStartState, ais.asInstanceOf[GoArrayIndexStatus])

            wasLastChildZeroLength = resultOfTry eq ParseAttemptStatus.Success_ZeroLength

            loopState.nextArrayIndex(pstate)
          } // end while for each repeat

          parser.endArray(pstate)
        } // end match case RepParser

        case scalarParser => {
          val resultOfTry = tryParseDetectMarkLeaks(scalarParser, pstate, Maybe.Nope, ArrayIndexStatus.Required)

          wasLastChildZeroLength = resultOfTry eq ParseAttemptStatus.Success_ZeroLength
        } // end match case scalar parser
      } // end match
      scpIndex += 1
    } // end while for each sequence child parser

    if ((pstate.processorStatus eq Success) && wasLastChildZeroLength) {
      Assert.invariant(ssp eq SeparatorSuppressionPolicy.TrailingEmptyStrict)
      PE(pstate, "Empty trailing elements are not allowed when dfdl:separatorSuppressionPolicy='trailingEmptyStrict'")
    }
    pstate.mpstate.groupIndexStack.pop()
    pstate.mpstate.moveOverOneGroupIndexOnly()
    ()
  }
}
