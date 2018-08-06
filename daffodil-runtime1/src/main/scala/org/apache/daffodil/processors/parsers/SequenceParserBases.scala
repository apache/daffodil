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

import org.apache.daffodil.processors.Evaluatable
import java.io.PrintWriter
import org.apache.daffodil.exceptions.UnsuppressableException
import java.io.StringWriter
import org.apache.daffodil.dsom.SchemaDefinitionDiagnosticBase
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.Success
import org.apache.daffodil.processors.SequenceRuntimeData
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.processors.Failure

abstract class OrderedSequenceParserBase(
  srd: SequenceRuntimeData,
  protected val childParsers: Seq[SequenceChildParser])
  extends CombinatorParser(srd) {
  override def nom = "Sequence"

  override lazy val runtimeDependencies: Seq[Evaluatable[AnyRef]] = Nil

  override lazy val childProcessors: Seq[Parser] = childParsers

  /**
   * Parses (1) one iteration of an array with fixed/expression occurs count.
   * (2) a model group (3) a scalar element.
   *
   * Returns a status indicating success/failure and the nature of that success/failure.
   *
   * No backtracking supported.
   */
  protected def parseOneWithoutPoU(
    parserArg: SequenceChildParser,
    trd: TermRuntimeData,
    pstate: PState): ParseAttemptStatus

  /**
   * Parses one iteration of an array/optional element, and returns
   * a status indicating success/failure and the nature of that success/failure.
   *
   * Supports speculative parsing via backtracking.
   */
  protected def parseOneWithPoU(
    parser: RepeatingChildParser,
    erd: ElementRuntimeData,
    pstate: PState,
    priorState: PState.Mark,
    ais: GoArrayIndexStatus,
    isBounded: Boolean): ParseAttemptStatus

  protected def zeroLengthSpecialChecks(pstate: PState, wasLastChildZeroLength: Boolean): Unit

  final protected def checkN(pstate: PState): Boolean = {
    if (pstate.arrayPos > pstate.tunable.maxOccursBounds) {
      PE(pstate, "Occurs count %s exceeds implementation maximum of %s.", pstate.arrayPos, pstate.tunable.maxOccursBounds)
      false
    } else true
  }

  /**
   * This parse method is used for both separated and unseparated sequences.
   */
  override protected def parse(pstate: PState): Unit = {
    val children = childParsers

    var scpIndex = 0
    pstate.mpstate.groupIndexStack.push(1L) // one-based indexing

    val limit = children.length

    var wasLastChildZeroLength = false

    //
    // This loop iterates over the children terms of the sequence
    //
    while ((scpIndex < limit) && (pstate.processorStatus eq Success)) {
      val child = children(scpIndex)
      child match {
        case parser: RepeatingChildParser => {
          //
          // The sequence child is an array/repeating element (or ooptional
          // element as the runtime doesn't distinguish them.)
          //
          //
          val min = parser.minRepeats(pstate)
          val max = parser.maxRepeats(pstate)
          val isBounded = parser.isBoundedMax(max)
          val erd = parser.trd.asInstanceOf[ElementRuntimeData]

          parser.startArray(pstate)

          //
          // There are two kinds of loops. Arrays which have points of uncertainty (PoU)
          // where speculative parsing is used to determine how many occurrences,
          // and specified-number of occurrences, where a number is known or is computed.
          //
          parser.hasPoU match {
            case true => {
              //
              // This case for array/optionals where the number of occurences is
              // determined by speculative parsing. OCK=implicit with min/maxOccurs
              // different, or OCK=parsed.
              //

              //
              // The beforeArrayState will be assigned the priorState before the whole array
              // This is the same object as the priorState before the first
              // occurrence.
              //
              var beforeArrayState: PState.Mark = null

              var resultOfTry: ParseAttemptStatus = ParseAttemptStatus.Uninitialized

              var ais: ArrayIndexStatus = null
              var goAIS: GoArrayIndexStatus = null

              var isFirstIteration = true

              while ({
                ais = parser.arrayIndexStatus(min, max, pstate, resultOfTry)
                ais match {
                  case go: GoArrayIndexStatus => { goAIS = go; true }
                  case _ => false
                }
              }) {

                //
                // Saved state before an individual occurrence.
                //
                // On the first iteration this is the same as the beforeArrayState
                // and by "same" we mean same object in the 'eq' sense.
                //
                // If we are in a second or subsequent iteration then an invariant is
                // that this is NOT the same as the beforeArrayState.
                //
                // These should not leak as we iterate the occurrences.
                // The lifetime of these is the parse attempt for a single occurrence
                // only.
                //
                val priorState =
                  if (isFirstIteration) {
                    beforeArrayState = pstate.mark("before all occurrences")
                    beforeArrayState
                  } else {
                    pstate.mark("before second/subsequent occurrence")
                  }

                checkN(pstate) // check if arrayIndex exceeds tunable limit.

                var markLeakCausedByException = false
                var wasThrow = true
                try {
                  resultOfTry =
                    parseOneWithPoU(parser, erd, pstate, priorState, goAIS, isBounded)
                  wasThrow = false
                  //
                  // Now we handle the result of the parse attempt.
                  //

                  // check for consistency - failure comes with a PE in the PState.
                  Assert.invariant((pstate.processorStatus eq Success) ||
                    resultOfTry.isInstanceOf[FailedParseAttemptStatus])

                  resultOfTry match {
                    //
                    // These statuses for whole array/optional are not used
                    // for PoU occurrences.
                    //
                    case ParseAttemptStatus.Failed_EntireArray |
                      ParseAttemptStatus.Success_EndOfArray =>
                      Assert.invariantFailed("not valid return status for a PoU array/optional.")

                    case ParseAttemptStatus.Success_SkippedSeparator => {
                      //
                      // In the case of separated sequences when we skip just the separator,
                      // the parseWithOnePoU method has handled the reset to priorState
                      // and advance past the separator. It has to, as it knows
                      // how long the separator was.
                      //
                      wasLastChildZeroLength = true
                    }
                    case _: SuccessParseAttemptStatus => {
                      pstate.mpstate.moveOverOneGroupIndexOnly()
                      //
                      // Here we leave beforeArrayState alone. May still
                      // need it later.
                      //
                      wasLastChildZeroLength = resultOfTry eq ParseAttemptStatus.Success_ZeroLength
                    }

                    case ParseAttemptStatus.Failed_WithDiscriminatorSet => {
                      // Just allow the failure to propagate.
                    }

                    case ParseAttemptStatus.Failed_SpeculativeParse => {
                      // We failed.
                      goAIS match {
                        case ArrayIndexStatus.Required => {
                          //
                          // Did we reach minOccurs? I.e., did we fail on a required element?
                          // if so, then we failed the whole array
                          //
                          // Grab the cause, restore the state prior to the whole array,
                          // then re-assert the failure cause.
                          //
                          val Failure(cause) = pstate.processorStatus

                          //
                          // Discarding has to be done before resetting of an outer/earlier mark
                          // because the InputDataStream marks are unwound in a stack manner. I.e., resetting
                          // to an earlier one implicitly discards any nested within that.
                          //
                          // This was causing a problem when we didn't discard priorState until later, since it
                          // would then try to discard the InputDataStream's mark, and find it had already been
                          // discarded.
                          //
                          if (!isFirstIteration) pstate.discard(priorState)
                          pstate.reset(beforeArrayState)
                          pstate.setFailed(cause)
                          resultOfTry = ParseAttemptStatus.Failed_EntireArray
                        }
                        case _: OptionalArrayIndexStatus => {
                          //
                          // We failed on an optional element.
                          // Means we back out and succeed on prior
                          // (which might be none - could be no occurrences at all)
                          //
                          pstate.reset(priorState)
                          //
                          // The above doesn't reset the array index back to the prior, because that is
                          // advanced at the bottom of the loop before the priorState is captured for this
                          // iteration, so the array index needs to back up by 1.
                          //
                          // We're done with the array, but the arrayPos still has to be correct for validation
                          // purposes - it is used to measure how many occurrences.
                          //
                          pstate.mpstate.moveBackOneArrayIndexOnly()
                          Assert.invariant(pstate.processorStatus eq Success)
                          resultOfTry = ParseAttemptStatus.Success_EndOfArray
                        }
                      }
                    }

                    case ParseAttemptStatus.Failed_NoForwardProgress => {
                      // Just allow the failure to propagate.
                    }

                    case other => Assert.invariantFailed("Unexpected parse attempt status: " + other)
                  }

                } catch {
                  // Similar try/catch/finally logic for returning marks is also used in
                  // the Choice parser. The logic isn't
                  // easily factored out so it is duplicated. Changes made here should also
                  // be made there. Only these parsers deal with taking marks, so this logic
                  // should not be needed elsewhere.
                  case t: Throwable => {
                    if (pstate.isInUse(priorState)) {
                      markLeakCausedByException = true
                      if (!t.isInstanceOf[SchemaDefinitionDiagnosticBase] && !t.isInstanceOf[UnsuppressableException]) {
                        val stackTrace = new StringWriter()
                        t.printStackTrace(new PrintWriter(stackTrace))
                        Assert.invariantFailed("Exception thrown with mark not returned: " + t + "\nStackTrace:\n" + stackTrace)
                      }
                    }
                    throw t
                  }
                }
                // if it wasn't reset, we discard priorState here.
                if (!isFirstIteration && pstate.isInUse(priorState)) {
                  pstate.discard(priorState)
                }

                //
                // advance array position.
                // Done unconditionally, as some failures get converted into successes
                //
                // If ultimately this is a real failure, then mothing cares about this, it is
                // about to get poppped/cleared anyway.
                //
                pstate.mpstate.moveOverOneArrayIndexOnly()
                isFirstIteration = false

              } // end while for each repeat

              // if it wasn't reset or discarded, discard here.
              if (pstate.isInUse(beforeArrayState)) {
                pstate.discard(beforeArrayState)
              }

            } // end match case hasPoU = true

            case false => {
              //
              // This case for array/optionals where the number of occurences is
              // specified or fixed. dfdl:occursCountKind='expression' or 'fixed'
              // or 'implicit' when minOccurs = maxOccurs.
              //

              var resultOfTry: ParseAttemptStatus = ParseAttemptStatus.Uninitialized
              var ais: ArrayIndexStatus = null
              var goAIS: GoArrayIndexStatus = null

              while ({
                ais = parser.arrayIndexStatus(min, max, pstate, resultOfTry)
                ais match {
                  case go: GoArrayIndexStatus => { goAIS = go; true }
                  case _ => false
                }
              }) {

                checkN(pstate)
                resultOfTry = parseOneWithoutPoU(parser, parser.trd, pstate)
                resultOfTry match {
                  case ParseAttemptStatus.Success_EndOfArray => // ok, success will move on to next sequence child.
                  case _: SuccessParseAttemptStatus => {
                    pstate.mpstate.moveOverOneGroupIndexOnly() // advance group position
                    pstate.mpstate.moveOverOneArrayIndexOnly() // advance array position
                  }
                  case _: FailedParseAttemptStatus => // ok, failure will propagate from pstate.
                  case ParseAttemptStatus.Uninitialized =>
                    Assert.invariantFailed("Cannot be uninitialized")
                }
                wasLastChildZeroLength = resultOfTry eq ParseAttemptStatus.Success_ZeroLength
              } // end while for each occurrence
            } // end match case hasPoU == false
          } // end match hasPoU
          parser.endArray(pstate)
        } // end match case RepeatingChildParser

        //
        // This case for scalar parsers. This includes both scalar elements, and
        // model group terms (choices, or sequences that are children of a sequence).
        // A model group term is considered scalar
        // in that they cannot be repeating at all in DFDL v1.0.
        //
        case scalarParser => {
          val resultOfTry = parseOneWithoutPoU(scalarParser, scalarParser.trd, pstate)
          if (resultOfTry.isSuccess) {
            // only move over in group if the scalar "thing" is an element
            // that is represented.
            if (scalarParser.trd.isRepresented) {
              // allows for non-represented elements, and if we add it to DFDL
              // a concept of non-represented model groups - model groups containing
              // only statement annotations for example, or only non-represented elements.
              pstate.mpstate.moveOverOneGroupIndexOnly()
            }
          }
          // in failure case, pstate indicates failure of this scalar, and
          // we will exit the loop for the sequence's children.
          wasLastChildZeroLength = resultOfTry eq ParseAttemptStatus.Success_ZeroLength
        } // end match case scalar parser
      } // end match
      scpIndex += 1
    } // end while for each sequence child parser

    zeroLengthSpecialChecks(pstate, wasLastChildZeroLength)

    pstate.mpstate.groupIndexStack.pop()
    ()
  }
}

