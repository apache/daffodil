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
import org.apache.daffodil.exceptions.UnsuppressableException
import java.io.PrintWriter
import java.io.StringWriter
import org.apache.daffodil.dsom.SchemaDefinitionDiagnosticBase
import org.apache.daffodil.dsom.TunableLimitExceededError
import org.apache.daffodil.infoset.DIComplex
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.Success
import org.apache.daffodil.processors.SequenceRuntimeData
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.Failure

/**
 * Base class for all sequence parsers, which are the combinators that coordinate
 * all the parsing of the sequence child parsers.
 */
abstract class SequenceParserBase(
  srd: SequenceRuntimeData,
  childParsers: Vector[Parser],
  isOrdered: Boolean = true)
  extends CombinatorParser(srd) {
  override def nom = "Sequence"

  override lazy val runtimeDependencies: Vector[Evaluatable[AnyRef]] = Vector()
  override lazy val childProcessors = childParsers

  import ParseAttemptStatus._
  import ArrayIndexStatus._

  final protected def checkN(pstate: PState, childParser: SequenceChildParser): Unit = {
    if (pstate.arrayPos > pstate.tunable.maxOccursBounds) {
      throw new TunableLimitExceededError(
        childParser.trd.schemaFileLocation,
        "Array occurrences excceeds the maxOccursBounds tunable limit of %s",
        pstate.tunable.maxOccursBounds)
    }
  }

  final protected def checkForwardProgress(
    pstate: PState,
    currentPos: Long,
    priorPos: Long,
    ais: ArrayIndexStatus): ArrayIndexStatus = {
    Assert.invariant(currentPos >= priorPos)
    if (currentPos == priorPos && pstate.groupPos > 1) {
      PE(pstate, "No forward progress.")
      Done
    } else {
      ais
    }
  }

  override protected def parse(pstate: PState): Unit = {
    pstate.mpstate.groupIndexStack.push(1L)

    val children = childParsers

    var scpIndex = 0

    val limit = children.length

    var resultOfTry: ParseAttemptStatus = ParseAttemptStatus.Uninitialized

    val infosetIndexStart = pstate.infoset.asInstanceOf[DIComplex].childNodes.size

    /**
     * On exit from the sequence loop, if the last thing was Missing, we
     * want to look back one prior to see if that followed a EmptyRep or AbsentRep,
     * so that we can implement the check for trailingEmptyStrict
     */
    var priorResultOfTry: ParseAttemptStatus = ParseAttemptStatus.Uninitialized

    var child: SequenceChildParser = null

    var isDone = false

    //
    // This loop iterates over the children terms of the sequence
    //
    while (!isDone && (scpIndex < limit) && (pstate.processorStatus eq Success)) {
      child = children(scpIndex).asInstanceOf[SequenceChildParser]
      child match {
        case parser: RepeatingChildParser => {
          //
          // The sequence child is an array/repeating element (or ooptional
          // element as the runtime doesn't distinguish them.)
          //
          //
          val min = parser.minRepeats(pstate)
          val max = parser.maxRepeats(pstate)
          val isBounded = parser.isBoundedMax
          val erd = parser.trd.asInstanceOf[ElementRuntimeData]

          parser.startArray(pstate)

          //
          // This case for array/optionals where the number of occurences is
          // determined by speculative parsing. OCK=implicit with min/maxOccurs
          // different, or OCK=parsed.
          //

          priorResultOfTry = resultOfTry
          resultOfTry = ParseAttemptStatus.Uninitialized

          var ais: ArrayIndexStatus = ArrayIndexStatus.Uninitialized
          while ((ais ne Done) && { // check ais for Done in case it was assigned
            ais = parser.arrayIndexStatus(min, max, pstate)
            (pstate.isSuccess) && (ais ne Done) // check ais for done from min/max computation
          }) {
            val roStatus = ais.asInstanceOf[RequiredOptionalStatus]

            val priorPos = pstate.bitPos0b

            {
              //
              // Note: Performance - counting on Scala compiler to optimize away
              // this 2-tuple to avoid allocation in the inner loop here.
              //
              val (nextAIS, nextResultOfTry) = parseOneInstance(parser, pstate, roStatus, resultOfTry)
              ais = nextAIS
              priorResultOfTry = resultOfTry
              resultOfTry = nextResultOfTry
            }
            val currentPos = pstate.bitPos0b
            if (pstate.isSuccess && !isBounded && (
              resultOfTry match {
                case ParseAttemptStatus.AbsentRep => true
                case _: ParseAttemptStatus.SuccessParseAttemptStatus => true
                case _ => false
              })) {
              //
              // result of try could be missing if we just ended an array
              // by speculation.
              //
              // result of try could also be absent if we just ended a group
              // by not finding a separator
              //
              ais = checkForwardProgress(pstate, currentPos, priorPos, ais)
            }
            //
            // advance array position.
            // Done unconditionally, as some failures get converted into successes
            //
            // If ultimately this is a real failure, then mothing cares about this, it is
            // about to get poppped/cleared anyway.
            //
            if (ais ne Done) {
              pstate.mpstate.moveOverOneArrayIndexOnly()
            }
            if (currentPos > priorPos ||
              ((resultOfTry eq AbsentRep) && pstate.isSuccess
                && parser.isPositional) ||
                resultOfTry.isInstanceOf[SuccessParseAttemptStatus]) {
              // we moved past something, so we're definitely not first
              // in the group any more.
              //
              // Or if AbsentRep, and we're positional. Then also we
              // move on in the group.
              //
              // But if not, if we're still at position zero, then
              // whatever is next could still be first in the group
              // and not get an infix separator. So we have to conditionally
              // not move the group index unless we really did parse something.
              //
              pstate.mpstate.moveOverOneGroupIndexOnly()
            }
          } // end while for each repeat
          parser.endArray(pstate)
        } // end match case RepeatingChildParser

        //
        // This case for scalar parsers. This includes both scalar elements, and
        // model group terms (choices, or sequences that are children of a sequence).
        // A model group term is considered scalar
        // in that they cannot be repeating at all in DFDL v1.0.
        //
        case nonRepresentedParser: NonRepresentedSequenceChildParser => {
          nonRepresentedParser.parseOne(pstate, null)
          // don't need to digest result from this. All
          // information about success/failure is in the pstate.
          //
          // We do NOT move over the group index state for non-represented things.
        }
        case scalarParser => {
          val roStatus = scalarParser.maybeStaticRequiredOptionalStatus.get
          val (_, nextResultOfTry) = parseOneInstance(scalarParser, pstate, roStatus, resultOfTry)
          priorResultOfTry = resultOfTry
          resultOfTry = nextResultOfTry
          resultOfTry match {
            case AbsentRep => {
              // a scalar element, or a model group is absent. That means no separator
              // was found for it.
              //
              // That means were at the end of the representation of this sequence,
              // This is only returned as resultOfTry if it is
              // OK for us to act on it. I.e., we know that the situation is
              // Positional trailing, with a group that can have zero-length representation.
              // and no separator was found for it.
              //
              // So we mask the failure, and exit the sequence successfully
              pstate.setSuccess()
              isDone = true
            }

            // We successfully parsed a discriminator, but failed to parse the discriminated content.
            // Do not continue trying to parse other memebers of the unordered sequence.
            case UnorderedSeqDiscriminatedFailure => isDone = true

            case (MissingItem | MissingSeparator | FailureUnspecified) if (!isOrdered) => {
              // We have hit the end of an unordered sequence, mask the failure and exit
              // the sequence succesfully
              isDone = true
              pstate.setSuccess()
            }

            case _ => // ok.
          }
          pstate.mpstate.moveOverOneGroupIndexOnly()
        } // end case scalarParser
      } // end match case parser
      if (isOrdered)
        scpIndex += 1
      else if (isDone) {
        val infoset = pstate.infoset.asInstanceOf[DIComplex]
        infoset.flattenAndValidateChildNodes(pstate, infosetIndexStart)
      }
    } // end while for each sequence child parser

    if (child ne null) child.finalChecks(pstate, resultOfTry, priorResultOfTry)
    pstate.mpstate.groupIndexStack.pop()
    ()
  }

  private def parseOneInstance(
    parser: SequenceChildParser,
    pstate: PState,
    roStatus: RequiredOptionalStatus,
    resultOfTryArg: ParseAttemptStatus): (ArrayIndexStatus, ParseAttemptStatus) = {
    var resultOfTry = resultOfTryArg
    var ais: ArrayIndexStatus = ArrayIndexStatus.Uninitialized

    val hasPoU = parser.pouStatus eq PoUStatus.HasPoU

    //
    // Saved state before an individual occurrence.
    // These should not leak as we iterate the occurrences.
    // The lifetime of these is the parse attempt for a single occurrence
    // only.
    //
    val priorState = if (hasPoU) pstate.mark("before occurrence") else null

    var wasThrow = true
    try {
      checkN(pstate, parser) // check if occursIndex exceeds tunable limit.

      if (hasPoU) pstate.pushPointOfUncertainty
      val priorPos = pstate.bitPos0b

      resultOfTry = parser.parseOne(pstate, roStatus)

      wasThrow = false

      val currentPos = pstate.bitPos0b

      val wasDiscriminatorSet = pstate.discriminator
      //
      // Now we handle the result of the parse attempt.
      //
      // check for consistency - failure comes with a PE in the PState.
      Assert.invariant((pstate.processorStatus eq Success) ||
        resultOfTry.isInstanceOf[FailedParseAttemptStatus])

      resultOfTry match {
        case _: SuccessParseAttemptStatus => // ok
        case AbsentRep => {
          if (hasPoU) pstate.reset(priorState) // back out any side effects of the attempt to parse
          pstate.dataInputStream.setBitPos0b(currentPos) // skip syntax such as a separator
        }
        case MissingSeparator if (pstate.isSuccess) => {
          // missing separator with parse success indicates that we should end the sequence now
          if (hasPoU) pstate.discard(priorState)
          ais = Done
        }
        case _: FailedParseAttemptStatus => { // MissingSeparator with failure will match here
          Assert.invariant(pstate.isFailure)
          if (hasPoU && !wasDiscriminatorSet &&
            (roStatus.isInstanceOf[RequiredOptionalStatus.Optional])) {
            // we back up and finish the array at the prior element if any.
            pstate.reset(priorState)
            Assert.invariant(pstate.isSuccess)
          } else if (hasPoU && wasDiscriminatorSet) {
            resultOfTry = UnorderedSeqDiscriminatedFailure
          } else {
            val cause = pstate.processorStatus.asInstanceOf[Failure].cause
            parser.trd match {
              case erd: ElementRuntimeData if (erd.isArray) => {
                parser.PE(pstate, "Failed to populate %s[%s]. Cause: %s",
                  erd.prefixedName, pstate.mpstate.arrayPos, cause)
              }
              case _ => // ok
            }
          }
          ais = Done // exits the while loop for the array
        }
        case other => Assert.invariantFailed("Unexpected parse attempt status: " + other)
      }

      if (hasPoU) pstate.popPointOfUncertainty

    } catch {
      // Similar try/catch/finally logic for returning marks is also used in
      // the Choice parser. The logic isn't
      // easily factored out so it is duplicated. Changes made here should also
      // be made there. Only these parsers deal with taking marks, so this logic
      // should not be needed elsewhere.
      case t: Throwable => {
        if (hasPoU) {
          if (pstate.isInUse(priorState)) {
            pstate.discard(priorState)
            if (!t.isInstanceOf[SchemaDefinitionDiagnosticBase] && !t.isInstanceOf[UnsuppressableException] && !t.isInstanceOf[java.lang.Error]) {
              val stackTrace = new StringWriter()
              t.printStackTrace(new PrintWriter(stackTrace))
              Assert.invariantFailed("Exception thrown with mark not returned: " + t + "\nStackTrace:\n" + stackTrace)
            }
          }
        }
        throw t
      }
    }
    // if it wasn't reset, we discard priorState here.
    if (hasPoU && pstate.isInUse(priorState)) {
      pstate.discard(priorState)
    }
    (ais, resultOfTry)
  }
}
