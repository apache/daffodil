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


import org.apache.daffodil.dsom.TunableLimitExceededError
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.infoset.DIComplex
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.processors.Failure
import org.apache.daffodil.processors.SequenceRuntimeData
import org.apache.daffodil.processors.Success
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe.Nope
import org.apache.daffodil.util.Maybe.One

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

    if (!isOrdered) {
      // If this is an unordered sequence, upon completion of parsing all the
      // elements we will reorder the elements into schema definition order.
      // This means that we cannot let the infoset walker walk any of the
      // elements while we are parsing because their event order might change.
      // To ensure we don't walk, add a block on the parent of the infoset
      // elements. The infoset walker will inspect this to see if it should
      // walk any children. We'll remove this block once the unordered sequence
      // is complete.
      pstate.infoset.infosetWalkerBlockCount += 1
    }

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

      // keep track of the current last child node. If the last child changes
      // while parsing, we know a new child was added in this loop

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
              val (nextAIS, nextResultOfTry) = parseOneInstance(parser, pstate, roStatus)
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
              ((resultOfTry eq AbsentRep) && pstate.isSuccess) ||
                resultOfTry.isInstanceOf[SuccessParseAttemptStatus]) {
              // If we consumed some bits, then we moved past something, and so
              // we're definitely not first in the group any more.
              //
              // Or if AbsentRep, that means we sucessfully parsed a
              // zero-length separated element. Even though this element may
              // not end up in the infoset due to separator suppression, we
              // must still increment the group index since that is used to
              // determine when to consume infix separators
              //
              // Otherwise, the parse failed or nothing was consumed and we do
              // should not increment the group index.
              pstate.mpstate.moveOverOneGroupIndexOnly()
            }

            val newLastChildNode = pstate.infoset.maybeLastChild
            if (newLastChildNode.isDefined) {
              // We have potentially added a child to to this complex during
              // this array loop.
              //
              // If the new child is a DIArray, we know this DIArray has at
              // least one element, but we don't know if we actually added a
              // new one in this loop or not. So just get the last array
              // element and set it as final anyways.
              //
              // If it's not a DIArray, that means it's just an optional
              // simple/complex and that will get set final below where all
              // other non-array elements get set as final.
              val lastChild = newLastChildNode.get
              if (lastChild.isArray) {
                // not simple or complex, must be an array
                val lastArrayElem = lastChild.maybeLastChild
                if (lastArrayElem.isDefined) {
                  lastArrayElem.get.isFinal = true
                  pstate.walker.walk()
                }
              }
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
          val (_, nextResultOfTry) = parseOneInstance(scalarParser, pstate, roStatus)
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

      // now that we have finished parsing a single instance of this sequence,
      // we need to potentially set things as final, get the last child to
      // determine if it changed from the saved last child, which lets us know
      // if a new child was actually added.
      val newLastChildNode = pstate.infoset.maybeLastChild

      if (!isOrdered) {
        // In the special case of unordered sequences with arrays, the
        // childParser is not a RepeatingChildParser, so array elements aren't
        // set final above like normal arrays are. Instead we parse one
        // instance at a time in this loop.
        //
        // So if this the new last child node is a DIArray, we must set new
        // array elements as final here. We can't know if we actually added a
        // new DIArray element or not, so just set the last one as final
        // regardless.
        //
        // Note that we do not need to do a null check because in an unordered
        // sequence we are blocking, so we can't possible walk/free any of
        // these newly added elements.
        //
        // Also note that the DIArray itself is set as final right below this.
        // Again, because unordred sequences get blocked, that array won't be
        // walked even though it's final.
        if (newLastChildNode.isDefined && newLastChildNode.get.isArray) {
          // we have a new last child, and it's not simple or complex, so must
          // be an array. Set its last child final
          newLastChildNode.get.maybeLastChild.get.isFinal = true
        }
      }

      // We finished parsing one part of a sequence, which could either be an
      // array, simple or complex. If the last child is different from when we
      // started that means we must have added a new element and we can set it
      // final and walk.
      if (newLastChildNode.isDefined) {
        newLastChildNode.get.isFinal = true
        pstate.walker.walk()
      }

      if (isOrdered) {
        // only increment scpIndex for ordered sequences. For unordered
        // sequences, we just parse the single child parser repeatedly until we
        // get a failure
        scpIndex += 1
      }

    } // end while for each sequence child parser

    if (!isOrdered) {
      // we are unordered, so we need to reorder the new children into schema
      // definition order, flatten arrays, and validate
      val infoset = pstate.infoset.asInstanceOf[DIComplex]
      infoset.flattenAndValidateChildNodes(pstate, infosetIndexStart)

      // now that we have flattened children, we can decrement the block count
      // that we incremented above. This will allow the infoset walker to walk
      // into the new children that are now in the correct order.
      pstate.infoset.infosetWalkerBlockCount -= 1

      // we've unblocked the unordered sequence, try walking to output
      // everything we've created
      pstate.walker.walk()
    }

    if (child ne null) child.finalChecks(pstate, resultOfTry, priorResultOfTry)
    pstate.mpstate.groupIndexStack.pop()
    ()
  }

  private def parseOneInstance(
    parser: SequenceChildParser,
    pstate: PState,
    roStatus: RequiredOptionalStatus): (ArrayIndexStatus, ParseAttemptStatus) = {

    // Determine if we need a PoU. Note that we only have a point of
    // uncertainty if the sequence child parser has points of uncertainty (e.g.
    // array with min/max) and the require/optional status is not required.
    // Additionally, we only have this for ordered sequence. Unordered
    // sequences PoU's are handled by the choice parser
    val needsPoU =
      isOrdered &&
      (parser.pouStatus eq PoUStatus.HasPoU) &&
      !roStatus.isInstanceOf[RequiredOptionalStatus.Required]

    if (needsPoU) {
      val ans = pstate.withPointOfUncertainty("SequenceParserBase", parser.context) { pou =>
        parseOneInstanceWithMaybePoU(parser, pstate, roStatus, One(pou))
      }
      ans
    } else {
      parseOneInstanceWithMaybePoU(parser, pstate, roStatus, Nope)
    }
  }

  private def parseOneInstanceWithMaybePoU(
    parser: SequenceChildParser,
    pstate: PState,
    roStatus: RequiredOptionalStatus,
    maybePoU: Maybe[PState.Mark]): (ArrayIndexStatus, ParseAttemptStatus) = {

    var ais: ArrayIndexStatus = ArrayIndexStatus.Uninitialized

    checkN(pstate, parser) // check if occursIndex exceeds tunable limit.
    val priorPos = pstate.bitPos0b

    var resultOfTry = parser.parseOne(pstate, roStatus)

    val currentPos = pstate.bitPos0b

    val isPoUResolved =
      if (maybePoU.isDefined) pstate.isPointOfUncertaintyResolved(maybePoU.get)
      else true

    //
    // Now we handle the result of the parse attempt.
    //
    // check for consistency - failure comes with a PE in the PState.
    Assert.invariant((pstate.processorStatus eq Success) ||
      resultOfTry.isInstanceOf[FailedParseAttemptStatus])

    resultOfTry match {
      case _: SuccessParseAttemptStatus => { // ok
        if (maybePoU.isDefined && !isPoUResolved) pstate.discardPointOfUncertainty(maybePoU.get)
      }
      case AbsentRep => {
        if (maybePoU.isDefined) {
          Assert.invariant(!isPoUResolved) // impossible for an absent rep to resolve the PoU
          pstate.resetToPointOfUncertainty(maybePoU.get) // back out any side effects of the attempt to parse
        }
        pstate.dataInputStream.setBitPos0b(currentPos) // skip syntax such as a separator
      }
      case MissingSeparator if (pstate.isSuccess) => {
        // missing separator with parse success indicates that we should end the sequence now
        ais = Done
      }
      case _: FailedParseAttemptStatus => { // MissingSeparator with failure will match here
        Assert.invariant(pstate.isFailure)
        if (maybePoU.isDefined && !isPoUResolved &&
          (roStatus.isInstanceOf[RequiredOptionalStatus.Optional])) {
          // we back up and finish the array at the prior element if any.
          pstate.resetToPointOfUncertainty(maybePoU.get)
          Assert.invariant(pstate.isSuccess)
        } else if (maybePoU.isDefined && isPoUResolved) {
          resultOfTry = UnorderedSeqDiscriminatedFailure
        } else {
          parser.trd match {
            case erd: ElementRuntimeData if (erd.isArray) => {
              val cause = pstate.processorStatus.asInstanceOf[Failure].cause
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

    (ais, resultOfTry)

  }
}
