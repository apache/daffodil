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

import org.apache.daffodil.processors.{ SequenceRuntimeData, Success, TermRuntimeData }
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.util.Maybe

trait Unseparated { self: SequenceChildParser =>

  val childProcessors = Seq(childParser)
}

class ScalarOrderedRequiredUnseparatedSequenceChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  trd: TermRuntimeData)
  extends SequenceChildParser(childParser, srd, trd) with Unseparated {

  override protected def parse(state: PState) = childParser.parse1(state)
}

abstract class RepUnseparatedParser(
  childParser: Parser,
  val min: Long,
  val max: Long,
  srd: SequenceRuntimeData,
  val erd: ElementRuntimeData,
  val baseName: String)
  extends SequenceChildParser(childParser, srd, erd) with Unseparated with RepParser {
}

class RepOrderedExactlyNUnseparatedSequenceChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData,
  val repeatCount: Long,
  baseName: String = "ExactlyN")
  extends RepUnseparatedParser(childParser, 0, repeatCount, srd, erd, baseName)
  with RepParser
  with OccursCountExactLoopStateMixin

class RepOrderedExactlyTotalOccursCountUnseparatedSequenceChildParser(
  childParser: Parser,
  override val ocParser: Parser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData)
  extends RepOrderedExactlyNUnseparatedSequenceChildParser(childParser,
    srd, erd,
    { val ignored = 0; ignored },
    "ExactlyTotalOccursCount")
  with OccursCountExpressionLoopStateMixin {

  override val childProcessors = Seq(ocParser, childParser)

  override def loopState(pstate: PState) = super.loopState(pstate)
}

class RepOrderedWithMinMaxUnseparatedSequenceChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData,
  baseName: String,
  min: Long = -1,
  max: Long = -1) // pass -2 to force unbounded behavior
  extends RepUnseparatedParser(
    childParser,
    min, max,
    srd, erd, baseName)
  with OccursCountMinMaxLoopStateMixin

class OrderedUnseparatedSequenceParser(rd: SequenceRuntimeData, childParsersArg: Seq[SequenceChildParser])
  extends OrderedSequenceParserBase(rd, childParsersArg) {

  import SequenceChildParser._

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
    ais: GoArrayIndexStatus): ParseAttemptStatus = {

    val parser = parserArg.asInstanceOf[UnseparatedChildParser]

    val isFixedOccurs = maybeStartState.isEmpty
    val isVariableOccurs = !isFixedOccurs

    if (isVariableOccurs) {
      pstate.pushDiscriminator
    }

    if (pstate.dataProc.isDefined) pstate.dataProc.get.beforeRepetition(pstate, this)

    parser.parse1(pstate)

    if (pstate.dataProc.isDefined) pstate.dataProc.get.afterRepetition(pstate, this)

    val childSuccessful = pstate.processorStatus eq Success

    val res: ParseAttemptStatus = {
      if (!childSuccessful)
        processFailedChildParseResults(pstate, priorState, maybeStartState)
      else
        ParseAttemptStatus.Success_LengthUndetermined

    }
    if (isVariableOccurs) {
      pstate.popDiscriminator
    }
    res
  }

  //
  // TODO: It would be good to get rid of this downcast
  // leaving it for now, as the split into Separated/Unseparated might get
  // recombined if upon implementing the separated interations it seems
  // like they don't need to be independent code.
  //
  private val children = childParsersArg.map { _.asInstanceOf[UnseparatedChildParser] }

  protected def parse(pstate: PState): Unit = {

    var scpIndex = 0
    pstate.mpstate.groupIndexStack.push(1L) // one-based indexing

    val limit = children.length

    while ((scpIndex < limit) && (pstate.processorStatus eq Success)) {
      val child = children(scpIndex)
      child match {
        case parser: RepUnseparatedChildParser => {

          val loopState = parser.loopState(pstate)

          // push new array context for array/optional
          parser.startArray(pstate)

          var ais: ArrayIndexStatus = null

          var resultOfTry: ParseAttemptStatus = ParseAttemptStatus.Uninitialized

          val maybeStartState =
            if (parser.erd.maxOccurs != parser.erd.minOccurs)
              Maybe(pstate.mark("startState in OrderedUnseparatedSequenceParser"))
            else
              Maybe.Nope

          while ({
            ais = loopState.arrayIndexStatus(parser, pstate, resultOfTry)
            ais.isInstanceOf[GoArrayIndexStatus]
          }) {

            resultOfTry = tryParseDetectMarkLeaks(parser, pstate, maybeStartState, ais.asInstanceOf[GoArrayIndexStatus])

            loopState.nextArrayIndex(pstate)
          } // end while for each repeat

          parser.endArray(pstate)
        } // end match case RepParser

        case scalarParser => {
          tryParseDetectMarkLeaks(scalarParser, pstate, Maybe.Nope, ArrayIndexStatus.Required)
        } // end match case scalar parser
      } // end match
      scpIndex += 1
    } // end while for each sequence child parser

    pstate.mpstate.groupIndexStack.pop()
    pstate.mpstate.moveOverOneGroupIndexOnly()
    ()
  }
}
