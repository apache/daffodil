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
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.Failure
import org.apache.daffodil.processors.OccursCountEv

trait Unseparated { self: SequenceChildParser =>

  val childProcessors = Vector(childParser)
}

class ScalarOrderedUnseparatedSequenceChildParser(
  override val childParser: Parser,
  override val srd: SequenceRuntimeData,
  override val trd: TermRuntimeData)
  extends SequenceChildParser(childParser, srd, trd) with Unseparated {

  override protected def parse(state: PState) = childParser.parse1(state)
}

class RepOrderedExactlyNUnseparatedSequenceChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData,
  val repeatCount: Long)
  extends OccursCountExactParser(childParser, srd, erd)
  with Unseparated

class RepOrderedExactlyTotalOccursCountUnseparatedSequenceChildParser(
  childParser: Parser,
  ocEv: OccursCountEv,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData)
  extends OccursCountExpressionParser(childParser, srd, erd, ocEv)
  with Unseparated

class RepOrderedWithMinMaxUnseparatedSequenceChildParser(
  childParser: Parser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData) // pass -2 to force unbounded behavior
  extends OccursCountMinMaxParser(childParser, srd, erd)
  with Unseparated

class OrderedUnseparatedSequenceParser(rd: SequenceRuntimeData, childParsersArg: Vector[SequenceChildParser])
  extends OrderedSequenceParserBase(rd, childParsersArg) {

  /**
   * Parses (1) one iteration of an array with fixed/expression occurs count.
   * (2) a model group (3) a scalar element.
   *
   * Returns a status indicating success/failure and the nature of that success/failure.
   *
   * No backtracking supported.
   */
  override protected def parseOneWithoutPoU(
    parser: SequenceChildParser,
    trd: TermRuntimeData,
    pstate: PState): ParseAttemptStatus = {

    if (pstate.dataProc.isDefined) pstate.dataProc.get.beforeRepetition(pstate, this)

    parser.parse1(pstate)

    if (pstate.dataProc.isDefined) pstate.dataProc.get.afterRepetition(pstate, this)

    val childSuccessful = pstate.processorStatus eq Success

    val res: ParseAttemptStatus = pstate.processorStatus match {
      case Success => {

        Assert.invariant(childSuccessful)
        //
        // successful parse of required element. Zero length or not.
        //
        ParseAttemptStatus.Success_LengthUndetermined
      }
      case _ => {
        //
        // There are no PoU for this case, so a failure fails everything.
        //
        ParseAttemptStatus.Failed_EntireArray
      }
    }
    res
  }

  /**
   * Parses one iteration of an array/optional element, and returns
   * a status indicating success/failure and the nature of that success/failure.
   *
   * Does not modify priorState.
   */
  override protected def parseOneWithPoU(
    parser: RepeatingChildParser,
    erd: ElementRuntimeData,
    pstate: PState,
    priorStateArg: PState.Mark,
    aisArg: GoArrayIndexStatus,
    isBounded: Boolean): ParseAttemptStatus = {

    Assert.invariant(erd.isArray || erd.isOptional) // not a PoU otherwise.

    pstate.pushDiscriminator

    if (pstate.dataProc.isDefined) pstate.dataProc.get.beforeRepetition(pstate, this)

    val posBefore = pstate.bitPos0b

    parser.parse1(pstate)

    if (pstate.dataProc.isDefined) pstate.dataProc.get.afterRepetition(pstate, this)

    val res: ParseAttemptStatus =
      pstate.processorStatus match {
        case Success => {
          if (posBefore == pstate.bitPos0b) {
            if (isBounded) {
              // ok to have no forward progress, so long as iterations are bounded
              ParseAttemptStatus.Success_ZeroLength
            } else {
              //
              // success but no forward progress in unbounded case
              // This is the parser equivalent of an infinite loop.
              //
              PE(
                pstate,
                "Repeating or Optional Element - No forward progress at byte %s. Attempt to parse %s " +
                  "succeeded but consumed no data.\nPlease re-examine your schema to correct this infinite loop.",
                pstate.bytePos, erd.diagnosticDebugName)
              ParseAttemptStatus.Failed_NoForwardProgress
            }
          } else {
            // ordinary success
            ParseAttemptStatus.Success_NotZeroLength
          }
        }
        case _: Failure => {
          if (pstate.discriminator == true) {
            ParseAttemptStatus.Failed_WithDiscriminatorSet
          } else {
            ParseAttemptStatus.Failed_SpeculativeParse
          }
        }
      }
    pstate.popDiscriminator
    res
  }

  override protected def zeroLengthSpecialChecks(pstate: PState, wasLastChildZeroLength: Boolean): Unit = {
    // Nothing for unseparated sequences
  }
}
