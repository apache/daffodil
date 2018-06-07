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
import org.apache.daffodil.util.{ Maybe, MaybeBoolean }

trait Separated { self: SequenceChildParser =>
  import ArrayIndexStatus._

  def sep: Parser
  def spos: SeparatorPosition
  def ssp: SeparatorSuppressionPolicy

  val childProcessors = Seq(childParser, sep)

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

//abstract class RepSeparatedParser(
//  childParser: Parser,
//  val minRepeats: Long,
//  val maxRepeats: Long,
//  srd: SequenceRuntimeData,
//  val erd: ElementRuntimeData,
//  val baseName: String,
//  val sep: Parser,
//  val spos: SeparatorPosition,
//  val ssp: SeparatorSuppressionPolicy)
//  extends SequenceChildParser(childParser, srd, erd)
//with Separated
//with RepParser {
//  import ArrayIndexStatus._
//
//
//}

class ExactlyNLoopState(limit: Long) extends ParseLoopState {
  import ArrayIndexStatus._

  def arrayIndexStatus(parser: SequenceChildParser, pstate: PState) = {
    val res =
      if (pstate.processorStatus ne Success)
        Failed
      else if (pstate.arrayPos <= limit)
        Required
      else
        MaxExceeded
    res
  }

  def nextArrayIndex(pstate: PState): Long = {
    val res = pstate.arrayPos
    pstate.mpstate.moveOverOneArrayIndexOnly() // advance array position
    res
  }
}

class RepOrderedExactlyNSeparatedSequenceChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  val erd: ElementRuntimeData,
  repeatCount: Long,
  val sep: Parser,
  val spos: SeparatorPosition,
  val ssp: SeparatorSuppressionPolicy,
  val baseName: String = "ExactlyN")
  extends SequenceChildParser(childParser, srd, erd)
  with Separated
  with RepParser {
  val minRepeats = 0L
  val maxRepeats = repeatCount

  def loopState(pstate: PState): ParseLoopState = {
    new ExactlyNLoopState(repeatCount)
  }

}

trait OccursCountLoopStateMixin { self: SequenceChildParser =>

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
  with OccursCountLoopStateMixin {

  override def loopState(pstate: PState) = super.loopState(pstate)
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
  min: Long = -1,
  max: Long = -1) // pass -2 to force unbounded behavior
  extends SequenceChildParser(childParser, srd, erd)
  with Separated
  with RepParser {
  val minRepeats = if (min == -1) erd.minOccurs else min
  val maxRepeats = max match {
    case -1 if (erd.maxOccurs == -1) => Long.MaxValue
    case -1 => erd.maxOccurs
    case -2 => Long.MaxValue
    case _ => max
  }

  override def loopState(pstate: PState): ParseLoopState = ???
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

  private def failedSeparator(pstate: PState, priorState: PState.Mark, kind: String) = {
    val cause = pstate.processorStatus.asInstanceOf[Failure].cause
    pstate.reset(priorState)
    PE(pstate, "Failed to parse %s separator. Cause: %s.", kind, cause)
    MaybeBoolean(false)
  }

  private def failedChild(pstate: PState, priorState: PState.Mark) = {
    val cause = pstate.processorStatus.asInstanceOf[Failure].cause
    pstate.reset(priorState)
    PE(pstate, "Failed to parse sequence child. Cause: %s.", cause)
    MaybeBoolean(false)
  }

  private def failedZeroLengthWithAnyEmpty(pstate: PState, priorState: PState.Mark) = {
    pstate.reset(priorState)
    PE(pstate, "Failed to parse sequence child. Cause: zero-length data, but dfdl:separatorSuppressionPolicy is 'anyEmpty'.")
    MaybeBoolean(false)
  }

  /**
   * Parses one iteration of an array/optional element, and returns
   * * MaybeBoolean.One(true) - indicates the child parse was zero length
   * * MaybeBoolean.One(false) - indicates the child parse was not zero length or failed
   * * MaybeBoolean.Nope - indicates that the array loop should terminate due to discriminator failure. in which case the pstate will indicate failure.
   */
  protected def parseOne(
    parserArg: SequenceChildParser,
    trd: TermRuntimeData,
    pstate: PState,
    priorState: PState.Mark,
    maybeStartState: Maybe[PState.Mark],
    ais: GoArrayIndexStatus): MaybeBoolean = {

    val parser = parserArg.asInstanceOf[SeparatedChildParser]

    val isFixedOccurs = maybeStartState.isEmpty
    val isVariableOccurs = !isFixedOccurs

    val wasZeroLength: MaybeBoolean = {
      // parse prefix sep if any
      val prefixSepSuccessful =
        if ((spos eq SeparatorPosition.Prefix) && trd.isRepresented) {
          sep.parse1(pstate)
          pstate.processorStatus eq Success
        } else
          true

      if (!prefixSepSuccessful) {
        failedSeparator(pstate, priorState, "prefix")
      } else {
        // except for the first position of the group, parse an infix separator

        val infixSepSuccessful =
          if ((spos eq SeparatorPosition.Infix) && pstate.mpstate.childPos != 0 && trd.isRepresented) {
            pstate.processorStatus eq Success
          } else
            true

        if (!infixSepSuccessful) {
          failedSeparator(pstate, priorState, "infix")
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

          val res: MaybeBoolean = {
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
    wasZeroLength
  }

  private def processSuccessfulChildParseResults(
    trd: TermRuntimeData,
    parser: SeparatedChildParser,
    zl: Boolean,
    pstate: PState,
    priorState: PState.Mark,
    maybeStartState: Maybe[PState.Mark],
    ais: GoArrayIndexStatus): MaybeBoolean = {

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
      MaybeBoolean(zl)
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
    ais: GoArrayIndexStatus): MaybeBoolean = {
    import ArrayIndexStatus._

    //
    // Now we have to analyze the cases where ZL matters and needs special handling
    //
    val result: MaybeBoolean = ais match {
      case Required => processSuccessfulRequiredVariableOccursChildParseResults(zl, pstate, priorState, startState)
      case Optional => processSuccessfulOptionalVariableOccursChildParseResults(trd, parser, zl, pstate, priorState, startState, ais)
    }
    result

  }

  private def processSuccessfulRequiredVariableOccursChildParseResults(
    zl: Boolean,
    pstate: PState,
    priorState: PState.Mark,
    startState: PState.Mark): MaybeBoolean = {
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
        MaybeBoolean(true) // ZL, successful parse of required element
      }
    } else {
      MaybeBoolean(false) // not ZL, successful parse of required element
    }
  }

  private def processSuccessfulOptionalVariableOccursChildParseResults(trd: TermRuntimeData,
    parser: SeparatedChildParser,
    zl: Boolean,
    pstate: PState,
    priorState: PState.Mark,
    startState: PState.Mark,
    ais: GoArrayIndexStatus): MaybeBoolean = {
    if (zl) {
      val shouldRemoveZLElement =
        trd match {
          case erd: ElementRuntimeData =>
            parser.shouldRemoveZLStringHexBinaryValue(ais, erd)
          case mgd: ModelGroupRuntimeData =>
            false
        }
      if (shouldRemoveZLElement) {
        // we want to proclaim failure at this element and behave
        // as if we had success at the prior element and this one failed.
        pstate.reset(priorState) // no need discard priorState, that is implicitly discarded by resetting the startState
        pstate.discard(startState) // finished with array, so cleanup
        MaybeBoolean(false) // not zero length (in this case with success at prior element)
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
          MaybeBoolean(true) // ZL, successful parse of optional 'thing'
        }
      }
    } else {
      MaybeBoolean(false) // not ZL, successful optional element parse
    }
  }

  private def processFailedChildParseResults(pstate: PState, priorState: PState.Mark, maybeStartState: Maybe[PState.Mark]): MaybeBoolean = {
    val isFixedOccurs = maybeStartState.isEmpty
    val isVariableOccurs = !isFixedOccurs
    //
    // we failed to parse a child
    //
    failedChild(pstate, priorState)
    if (isFixedOccurs) {
      //
      // in fixed occurs, there are no points of uncertainty for the
      // individual elements, so a failure is a failure of the whole loop.
      // And any discriminator that has been set is the discriminator
      // of some surrounding scope.
      //
      pstate.discard(priorState) // deallocate
      MaybeBoolean.Nope // exit array
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
        MaybeBoolean.Nope
      } else {
        Assert.invariant(pstate.discriminator == false)
        //
        // discriminator false, variable occurs case, we failed the element, but that
        // just means we back out to prior element.
        //
        pstate.reset(priorState) // no need discard priorState, that is implicitly discarded by resetting the startState
        pstate.discard(startState) // finished with array, so cleanup
        MaybeBoolean(false) // not zero length (in this case with success at prior element)
      }
    } // end if fixed/variable
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

          while ({
            ais = loopState.arrayIndexStatus(parser, pstate)
            ais.isInstanceOf[GoArrayIndexStatus]
          }) {

            val resultOfTry: MaybeBoolean =
              tryParseDetectMarkLeaks(parser, pstate, maybeStartState, ais.asInstanceOf[GoArrayIndexStatus])

            wasLastChildZeroLength = if (resultOfTry.isDefined) resultOfTry.get else false

            loopState.nextArrayIndex(pstate)
          } // end while for each repeat

          parser.endArray(pstate)
        } // end match case RepParser

        case scalarParser => {
          val resultOfTry: MaybeBoolean = tryParseDetectMarkLeaks(scalarParser, pstate, Maybe.Nope, ArrayIndexStatus.Required)

          wasLastChildZeroLength = if (resultOfTry.isDefined) resultOfTry.get else false
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
