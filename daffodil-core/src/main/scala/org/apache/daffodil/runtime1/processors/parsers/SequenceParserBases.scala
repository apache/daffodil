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
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.runtime1.dsom.TunableLimitExceededError
import org.apache.daffodil.runtime1.infoset.DIComplex
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Failure
import org.apache.daffodil.runtime1.processors.SequenceRuntimeData
import org.apache.daffodil.runtime1.processors.Success

/**
 * Base class for all sequence parsers, which are the combinators that coordinate
 * all the parsing of the sequence child parsers.
 */
abstract class SequenceParserBase(
  srd: SequenceRuntimeData,
  isOrdered: Boolean
) extends CombinatorParser(srd) {
  override def nom = "Sequence"

  val childParsers: Array[SequenceChildParser]

  import ArrayIndexStatus._
  import ParseAttemptStatus._

  final protected def checkN(pstate: PState, childParser: SequenceChildParser): Unit = {
    if (pstate.arrayIterationPos > pstate.tunable.maxOccursBounds) {
      throw new TunableLimitExceededError(
        childParser.trd.schemaFileLocation,
        "Array occurrences excceeds the maxOccursBounds tunable limit of %s",
        pstate.tunable.maxOccursBounds
      )
    }
  }

  final protected def checkForwardProgress(
    pstate: PState,
    parser: RepeatingChildParser,
    currentPos: Long,
    priorPos: Long,
    ais: ArrayIndexStatus
  ): ArrayIndexStatus = {
    Assert.invariant(currentPos >= priorPos)
    if (currentPos == priorPos && pstate.groupPos > 1) {
      parser.PE(
        pstate,
        "Array element parsed succesfully, but consumed no data and is stuck in an infinite loop as it is unbounded."
      )
      Done
    } else {
      ais
    }
  }

  override protected def parse(pstate: PState): Unit = {
    pstate.mpstate.groupIndexStack.push(1L)

    try {

      val children = childParsers

      var scpIndex = 0

      val limit = children.length

      var resultOfTry: ParseAttemptStatus = ParseAttemptStatus.Uninitialized

      val infosetIndexStart = pstate.infoset.asInstanceOf[DIComplex].numChildren

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
          case parser: RepeatingChildParser if isOrdered => {
            //
            // The sequence child is an array/repeating element (or optional
            // element as the runtime doesn't distinguish them) of an ordered
            // sequence. Unordred sequences are treated as scalars below.
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
            while (
              (ais ne Done) && { // check ais for Done in case it was assigned
                ais = parser.arrayIndexStatus(min, max, pstate)
                (pstate.isSuccess) && (ais ne Done) // check ais for done from min/max computation
              }
            ) {
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
              if (
                pstate.isSuccess && !isBounded && (resultOfTry match {
                  case ParseAttemptStatus.AbsentRep => true
                  case _: ParseAttemptStatus.SuccessParseAttemptStatus => true
                  case _ => false
                })
              ) {
                //
                // result of try could be missing if we just ended an array
                // by speculation.
                //
                // result of try could also be absent if we just ended a group
                // by not finding a separator
                //
                ais = checkForwardProgress(pstate, parser, currentPos, priorPos, ais)
              }
              //
              // advance array position.
              // Done unconditionally, as some failures get converted into successes
              //
              // If ultimately this is a real failure, then nothing cares about this, it is
              // about to get popped/cleared anyway.
              //
              if (ais ne Done) {
                pstate.mpstate.moveOverOneArrayIterationIndexOnly()

                // If the emptyElementParsePolicy is set to treatAsAbsent, don't
                // increment the occursIndex if the element is absent
                if (resultOfTry != AbsentRep) {
                  pstate.mpstate.moveOverOneOccursIndexOnly()
                }
              }

              if (
                currentPos > priorPos ||
                ((resultOfTry eq AbsentRep) && pstate.isSuccess) ||
                resultOfTry.isInstanceOf[SuccessParseAttemptStatus]
              ) {
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
            parser.arrayCompleteChecks(pstate, resultOfTry, priorResultOfTry)
          } // end match case RepeatingChildParser

          case nonRepresentedParser: NonRepresentedSequenceChildParser => {
            // should never have non-represented children in unordered sequences
            Assert.invariant(isOrdered)

            nonRepresentedParser.parseOne(pstate, null)
            // don't need to digest result from this. All
            // information about success/failure is in the pstate.
            //
            // We do NOT move over the group index state for non-represented things.
          }

          // This case for scalar parsers. This includes both scalar elements,
          // and model group terms (choices, or sequences that are children of a
          // sequence). A model group term is considered scalar in that they
          // cannot be repeating at all in DFDL v1.0.
          //
          // This case is also used for all children of unordered sequences. In
          // that case, we repeatedly attempt to parse all the children (starting
          // over on success), until all children fail or if discriminated
          // content causes us to bail early.
          case scalarParser => {
            val diagnosticsBeforeAttempt = pstate.diagnostics
            val roStatus =
              if (isOrdered)
                scalarParser.maybeStaticRequiredOptionalStatus.get
              else {
                // Treat unordered sequence children as if they are required.
                // This way if they fail, we will simply backtrack and try the
                // next child
                RequiredOptionalStatus.Required
              }
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
                // If we're masking the failure, we don't want the error dianostics
                // to flow up. Restore the diagnostics from before the parse
                // attempt
                pstate.diagnostics = diagnosticsBeforeAttempt
              }

              // This child alternative of an unordered sequence failed, and that
              // failure occurred after a discriminator within that child
              // evaluated to true. This means that this unordered sequence has
              // failed. We set isDone to true so we do not attempt anymore
              // children of this unordered sequence. Additionally, that failure
              // is still in the PState, which will cause us to backtrack to the
              // nearest unresolved point of uncertainty.
              case UnorderedSeqDiscriminatedFailure => isDone = true

              // We failed to parse a single instance of an unordered sequence
              // element, and we did not hit a discriminator. parseOneInstance
              // will have backtracked to before this instance was attempted, so
              // we can just try to parse the next child. We do not need to do
              // anything special, the end of this loop will increment to the
              // next child.
              case _: FailedParseAttemptStatus if (!isOrdered) => // no-op
              case _ => {
                if (isOrdered) {
                  // Successfully parsed a scalar ordered sequence element,
                  // nothing to do. We'll increment scpIndex before looping
                  // back around and try parsing the next sequence child
                } else {
                  // Successfully parsed an unordered sequence child. We want to
                  // try parsing the unordered sequence children again from the
                  // beginning, so we set the index to -1 so it is incremented
                  // back to zero at the end of the while loop
                  scpIndex = -1
                }
                // We successfully parsed something, so we must increment the group
                // index
                pstate.mpstate.moveOverOneGroupIndexOnly()
              }
            }
          } // end case scalarParser
        } // end match case parser

        // now that we have finished parsing a single instance of this sequence,
        // we need to potentially set things as final, get the last child to
        // determine if it changed from the saved last child, which lets us know
        // if a new child was actually added.
        val newLastChildNode = pstate.infoset.maybeLastChild

        if (!isOrdered) {
          // In the special case of unordered sequences with arrays, we do not
          // use the RepatingChildParser. Instead we parse on instance at a time
          // in this loop. So array elements aren't set final above like normal
          // arrays are.
          //
          // So if the last child node is a DIArray, we must set new array
          // elements as final here. We can't know if we actually added a new
          // DIArray element or not, so just set the last one as final
          // regardless.
          //
          // Note that we do not need to do a null check because in an unordered
          // sequence we are blocking, so we can't possibly walk/free any of
          // these newly added elements.
          if (newLastChildNode.isDefined && newLastChildNode.get.isArray) {
            // we have a new last child, and it's not simple or complex, so must
            // be an array. Set its last child final
            newLastChildNode.get.maybeLastChild.get.isFinal = true
          }
        }

        // We finished parsing one part of a sequence, which could either be an
        // array, simple, or complex. We aren't sure if we actually added a new
        // element or not, but in case we did, mark the last node as final.
        //
        // Additionally, if this is an ordered sequence, try to walk the infoset
        // to output events for this potentially new element. If this is an
        // unordered sequence, walking is unnecessary. This is because we may
        // need to reorder the infoset once this unordered sequence is complete
        // (via flattenAndValidateChildNodes below) and cannot walk until that
        // happens. To ensure we don't walk even if a child parser tries to call
        // walk() we incremented infosetWalkerBlockCount at the beginning of this
        // function, so the walker is effectively blocked from making any
        // progress. So we don't even bother calling walk() in this case.
        if (newLastChildNode.isDefined) {
          newLastChildNode.get.isFinal = true
          if (isOrdered) pstate.walker.walk()
        }

        scpIndex += 1

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

      if (child ne null) child.sequenceCompleteChecks(pstate, resultOfTry, priorResultOfTry)
      ()
    } finally {
      pstate.mpstate.groupIndexStack.pop()
    }
  }

  private def parseOneInstance(
    parser: SequenceChildParser,
    pstate: PState,
    roStatus: RequiredOptionalStatus
  ): (ArrayIndexStatus, ParseAttemptStatus) = {

    // Determine if we need a PoU. Note that we only have a point of
    // uncertainty if the sequence child parser has points of uncertainty (e.g.
    // array with min/max) and the require/optional status is not required.
    //
    // Additionally, we also have a PoU for unordered sequences. The result of
    // this PoU lets us know if a discriminator tells us to stop trying more
    // unordered sequence children
    val needsPoU =
      !isOrdered ||
        (
          (parser.pouStatus eq PoUStatus.HasPoU) &&
            !roStatus.isInstanceOf[RequiredOptionalStatus.Required]
        )

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
    maybePoU: Maybe[PState.Mark]
  ): (ArrayIndexStatus, ParseAttemptStatus) = {

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
    Assert.invariant(
      (pstate.processorStatus eq Success) ||
        resultOfTry.isInstanceOf[FailedParseAttemptStatus]
    )

    resultOfTry match {
      case _: SuccessParseAttemptStatus => { // ok
        if (maybePoU.isDefined && !isPoUResolved) pstate.discardPointOfUncertainty(maybePoU.get)
      }
      case AbsentRep => {
        if (maybePoU.isDefined) {
          Assert.invariant(!isPoUResolved) // impossible for an absent rep to resolve the PoU
          pstate.resetToPointOfUncertainty(
            maybePoU.get
          ) // back out any side effects of the attempt to parse
        }
        pstate.dataInputStream.setBitPos0b(currentPos) // skip syntax such as a separator
      }
      case MissingSeparator if (pstate.isSuccess) => {
        // missing separator with parse success indicates that we should end the sequence now
        ais = Done
      }
      case _: FailedParseAttemptStatus => { // MissingSeparator with failure will match here
        Assert.invariant(pstate.isFailure)
        if (!isOrdered) {
          if (isPoUResolved) {
            // failed this unordered sequence branch, and the PoU was resolved
            // so the unordered sequence failed. Change the resultOfTry to a
            // special state indicated this so this failure will propogate up
            resultOfTry = UnorderedSeqDiscriminatedFailure
          } else {
            // failed this unordered sequence branch, but nothing resolved the
            // PoU. We need to just try the next branch from the PoU. So just
            // reset to the PoU. The resultOfTry will be returned and will be
            // acted on appropriately
            pstate.resetToPointOfUncertainty(maybePoU.get)
          }
        } else if (
          maybePoU.isDefined && !isPoUResolved &&
          (roStatus.isInstanceOf[RequiredOptionalStatus.Optional])
        ) {
          // we back up and finish the array at the prior element if any.
          pstate.resetToPointOfUncertainty(maybePoU.get)
          Assert.invariant(pstate.isSuccess)
        } else {
          parser.trd match {
            case erd: ElementRuntimeData if (erd.isArray) => {
              val cause = pstate.processorStatus.asInstanceOf[Failure].cause
              parser.PE(
                pstate,
                "Failed to populate %s[%s]. Cause: %s",
                erd.prefixedName,
                pstate.mpstate.arrayIterationPos,
                cause
              )
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
