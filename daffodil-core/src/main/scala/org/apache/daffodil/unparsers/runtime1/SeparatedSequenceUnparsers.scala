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
package org.apache.daffodil.unparsers.runtime1

import scala.collection.mutable.Buffer

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.SeparatorSuppressionPolicy
import org.apache.daffodil.lib.schema.annotation.props.SeparatorSuppressionPolicy._
import org.apache.daffodil.lib.schema.annotation.props.gen.OccursCountKind
import org.apache.daffodil.lib.schema.annotation.props.gen.SeparatorPosition
import org.apache.daffodil.lib.schema.annotation.props.gen.SeparatorPosition._
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.MaybeInt
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.ModelGroupRuntimeData
import org.apache.daffodil.runtime1.processors.SequenceRuntimeData
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.unparsers._

trait Separated { self: SequenceChildUnparser =>

  def sep: Unparser
  def spos: SeparatorPosition
  def ssp: SeparatorSuppressionPolicy
  def zeroLengthDetector: ZeroLengthDetector
  def isPotentiallyTrailing: Boolean

  def isKnownStaticallyNotToSuppressSeparator: Boolean

  def isDeclaredLast: Boolean

  def isPositional: Boolean

  def childProcessors = Vector(childUnparser, sep)
}

sealed abstract class ScalarOrderedSeparatedSequenceChildUnparserBase(
  childUnparser: Unparser,
  srd: SequenceRuntimeData,
  trd: TermRuntimeData,
  override val sep: Unparser,
  override val spos: SeparatorPosition,
  override val ssp: SeparatorSuppressionPolicy,
  override val zeroLengthDetector: ZeroLengthDetector,
  override val isPotentiallyTrailing: Boolean,
  override val isKnownStaticallyNotToSuppressSeparator: Boolean,
  override val isPositional: Boolean,
  override val isDeclaredLast: Boolean
) extends SequenceChildUnparser(childUnparser, srd, trd)
  with Separated {

  override def unparse(state: UState) = childUnparser.unparse1(state)
}

class ScalarOrderedSeparatedSequenceChildUnparser(
  childUnparser: Unparser,
  srd: SequenceRuntimeData,
  trd: TermRuntimeData,
  sep: Unparser,
  spos: SeparatorPosition,
  ssp: SeparatorSuppressionPolicy,
  zlDetector: ZeroLengthDetector,
  isPotentiallyTrailing: Boolean,
  isKnownStaticallyNotToSuppressSeparator: Boolean,
  isPositional: Boolean,
  isDeclaredLast: Boolean
) extends ScalarOrderedSeparatedSequenceChildUnparserBase(
    childUnparser,
    srd,
    trd,
    sep,
    spos,
    ssp,
    zlDetector,
    isPotentiallyTrailing,
    isKnownStaticallyNotToSuppressSeparator,
    isPositional,
    isDeclaredLast
  )

class RepOrderedSeparatedSequenceChildUnparser(
  childUnparser: Unparser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData,
  override val sep: Unparser,
  override val spos: SeparatorPosition,
  override val ssp: SeparatorSuppressionPolicy, // need for diagnostics perhaps
  override val zeroLengthDetector: ZeroLengthDetector,
  override val isPotentiallyTrailing: Boolean,
  override val isKnownStaticallyNotToSuppressSeparator: Boolean,
  override val isPositional: Boolean,
  override val isDeclaredLast: Boolean
) extends RepeatingChildUnparser(childUnparser, srd, erd)
  with Separated {

  override def checkArrayPosAgainstMaxOccurs(state: UState) =
    state.arrayIterationPos <= maxRepeats(state)
}

class OrderedSeparatedSequenceUnparser(
  rd: SequenceRuntimeData,
  ssp: SeparatorSuppressionPolicy,
  spos: SeparatorPosition,
  sepMtaAlignmentMaybe: MaybeInt,
  sepMtaUnparserMaybe: Maybe[Unparser],
  sep: Unparser,
  childUnparsers: Array[SequenceChildUnparser with Separated]
) extends OrderedSequenceUnparserBase(rd) {
  // Sequences of nothing (no initiator, no terminator, nothing at all) should
  // have been optimized away
  Assert.invariant(childUnparsers.length > 0)

  override def runtimeDependencies = Vector()

  override def childProcessors = childUnparsers.toVector

  /**
   * Unparses one occurrence with associated separator (non-suppressable).
   */
  protected def unparseOne(
    unparser: SequenceChildUnparser,
    trd: TermRuntimeData,
    state: UState
  ): Unit = {

    if (trd.isRepresented) {
      spos match {
        case Prefix => {
          unparseJustSeparator(state)
          unparser.unparse1(state)
        }
        case Infix => {
          if (state.groupPos > 1) {
            unparseJustSeparator(state)
          }
          unparser.unparse1(state)
        }
        case Postfix => {
          unparser.unparse1(state)
          unparseJustSeparator(state)
        }
      }
    } else {
      Assert.invariant(!trd.isRepresented)
      unparser.unparse1(state)
    }
  }

  /**
   * Unparses just the separator, as well as any mandatory text alignment if necessary
   *
   * Does not deals with infix boundary condition.
   */
  private def unparseJustSeparator(state: UState): Unit = {
    if (sepMtaUnparserMaybe.isDefined) {
      // we know we are unparsing a separator here, so we must also unparse
      // mandatory text alignment for that separator. If we didn't staticaly
      // determine MTA isn't necessary, we must unparse the MTA. This might
      // lead to a suspension, which is okay in this case because this logic is
      // not in a suspension, so nested suspensions are avoided.
      sepMtaUnparserMaybe.get.unparse1(state)
    }
    sep.unparse1(state)
  }

  /**
   * Unparses the separator only, which might be optional. The suspension
   * handles determine if the separator should be unparsed as well as if
   * alignment is needed, and avoids issues with nested suspensions.
   *
   * Does not have to deal with infix and first child.
   *
   * FIXME: this function is not used anywhere and appears to be dead code.
   * This is commented out for now so as to not affect code coverage. See
   * DAFFODIL-2405 and potentially related DAFFODIL-2219 to determine the
   * future of this code.
   */
//private def unparseJustSeparatorWithTrailingSuppression(
//  trd: TermRuntimeData,
//  state: UState,
//  trailingSuspendedOps: Buffer[SuppressableSeparatorUnparserSuspendableOperation]): Unit = {
//
//  // We don't know if the unparse will result in zero length or not. We have
//  // to use a suspendable unparser here for the separator which suspends
//  // until it is known whether the unparse of the contents were ZL or not. If
//  // the suspension determines that the field is non-zero length then the
//  // suspenion must also unparser mandatory text alignment for the separator.
//  // This cannot be done with a standard MTA alignment unparser since that is
//  // a suspension and suspensions cannot create suspensions. This this
//  // suspension is also responsible for unparsing alignment if the separator
//  // should be unparsed.
//
//  val suspendableOp = new SuppressableSeparatorUnparserSuspendableOperation(sepMtaAlignmentMaybe, sep, trd)
//  // TODO: merge these two objects. We can allocate just one thing here.
//  val suppressableSep = SuppressableSeparatorUnparser(sep, trd, suspendableOp)
//
//  suppressableSep.unparse1(state)
//  trailingSuspendedOps += suspendableOp
//}

  /**
   * Unparses an entire sequence, including both scalar and array/optional children.
   */
  protected def unparse(state: UState): Unit = {
    ssp match {
      case Never =>
        unparseWithNoSuppression(state)
      case _ =>
        unparseWithSuppression(state)
    }
  }

  private def unparseOneWithSuppression(
    unparser: SequenceChildUnparser,
    trd: TermRuntimeData,
    state: UState,
    trailingSuspendedOps: Buffer[SuppressableSeparatorUnparserSuspendableOperation],
    onlySeparatorFlag: Boolean
  ): Unit = {
    val doUnparseChild = !onlySeparatorFlag
    // We don't know if the unparse will result in zero length or not. We have
    // to use a suspendable unparser here for the separator which suspends
    // until it is known whether the unparse of the contents were ZL or not. If
    // the suspension determines that the field is non-zero length then the
    // suspension must also unparse mandatory text alignment for the separator.
    // This cannot be done with a standard MTA alignment unparser since that is
    // a suspension and suspensions cannot create suspensions. This suspension
    // is also responsible for unparsing alignment if the separator should be
    // unparsed.
    //
    // infix, prefix, postfix matters here, because the separator comes after
    // for postfix.

    if ((spos eq Infix) && state.groupPos == 1) {
      // no separator possible; hence, no suppression
      if (doUnparseChild) unparser.unparse1(state)
    } else {
      val suspendableOp =
        new SuppressableSeparatorUnparserSuspendableOperation(sepMtaAlignmentMaybe, sep, trd)
      // TODO: merge these two objects. We can allocate just one thing here.
      val suppressableSep = SuppressableSeparatorUnparser(sep, trd, suspendableOp)

      spos match {
        case Prefix | Infix => {
          suppressableSep.unparse1(state)
          if (doUnparseChild) unparser.unparse1(state)
          ssp match {
            case AnyEmpty => {
              suspendableOp.captureStateAtEndOfPotentiallyZeroLengthRegionFollowingTheSeparator(
                state
              )
            }
            case TrailingEmpty | TrailingEmptyStrict => {
              trailingSuspendedOps += suspendableOp
            }
            case Never => Assert.invariantFailed("Should not be ssp Never")
          }
        }
        case Postfix => {
          ssp match {
            case AnyEmpty => {
              suspendableOp.captureDOSForStartOfSeparatedRegionBeforePostfixSeparator(state)
              if (doUnparseChild) unparser.unparse1(state)
              suspendableOp.captureDOSForEndOfSeparatedRegionBeforePostfixSeparator(state)
              suppressableSep.unparse1(state)
              suspendableOp.captureStateAtEndOfPotentiallyZeroLengthRegionFollowingTheSeparator(
                state
              )
            }
            case TrailingEmpty | TrailingEmptyStrict => {
              suspendableOp.captureDOSForStartOfSeparatedRegionBeforePostfixSeparator(state)
              if (doUnparseChild) unparser.unparse1(state)
              suspendableOp.captureDOSForEndOfSeparatedRegionBeforePostfixSeparator(state)
              suppressableSep.unparse1(state)
              trailingSuspendedOps += suspendableOp
            }
            case Never => Assert.invariantFailed("Should not be ssp Never")
          }
        }
      }
    }
  }

  private def unparseWithSuppression(state: UState): Unit = {

    state.groupIndexStack.push(1L) // one-based indexing

    var index = 0
    var doUnparser = false
    val limit = childUnparsers.length

    lazy val trailingSuspendedOps = Buffer[SuppressableSeparatorUnparserSuspendableOperation]()

    while (index < limit) {
      val childUnparser = childUnparsers(index)
      val trd = childUnparser.trd
      state.pushTRD(
        trd
      ) // because we inspect before we call the unparse1 for the child unparser.
      val zlDetector = childUnparser.zeroLengthDetector
      childUnparser match {
        case unparser: RepOrderedSeparatedSequenceChildUnparser => {
          state.arrayIterationIndexStack.push(1L)
          state.occursIndexStack.push(1L)
          val erd = unparser.erd
          var numOccurrences = 0
          val maxReps = unparser.maxRepeats(state)
          //
          // The number of occurrances we unparse is always exactly driven
          // by the number of infoset events for the repeating/optional element.
          //
          // For RepUnparser - array/optional case - in all cases we should get a
          // startArray event. That is, defaulting of required array elements
          // (up to minOccurs) happens elsewhere, and we get events for all of those
          // here.
          //
          // If we don't get any array element events, then the element must be
          // entirely optional, so we get no events for it at all.
          //
          if (state.inspect) {
            val ev = state.inspectAccessor
            val isArr = erd.isArray
            if (ev.isStart && (isArr || erd.isOptional)) {
              if (ev.erd eq erd) {

                //
                // Note: leaving in some of these println, since debugger for unparsing is so inadequate currently.
                // This is the only way to figure out what is going on.
                //
                // System.err.println("Starting unparse of array/opt %s. Array Index Stack is: %s".format(
                //   erd.namedQName, state.arrayIndexStack))
                //

                // StartArray for this unparser's array element
                //
                unparser.startArrayOrOptional(state)
                while ({
                  doUnparser = unparser.shouldDoUnparser(unparser, state)
                  doUnparser
                }) {
                  //
                  // These are so we can check invariants on these stacks being
                  // pushed and popped reliably, and incremented only once
                  //
                  val arrayIterationIndexBefore = state.arrayIterationPos
                  val arrayIterationIndexStackDepthBefore =
                    state.arrayIterationIndexStack.length
                  val occursIndexBefore = state.occursPos
                  val occursIndexStackDepthBefore = state.occursIndexStack.length
                  val groupIndexBefore = state.groupPos
                  val groupIndexStackDepthBefore = state.groupIndexStack.length

                  Assert.invariant(
                    erd.isRepresented
                  ) // since this is an array, can't have inputValueCalc

                  if (isArr)
                    if (state.dataProc.isDefined)
                      state.dataProc.get.beforeRepetition(state, this)

                  if (
                    unparser.isKnownStaticallyNotToSuppressSeparator || {
                      val isKnownNonZeroLength =
                        zlDetector.isKnownNonZeroLength(state.inspectAccessor.info.element)
                      isKnownNonZeroLength
                    }
                  ) {
                    unparseOne(unparser, erd, state)
                  } else {
                    unparseOneWithSuppression(
                      unparser,
                      erd,
                      state,
                      trailingSuspendedOps,
                      onlySeparatorFlag = false
                    )
                  }
                  numOccurrences += 1
                  Assert.invariant(
                    state.arrayIterationIndexStack.length == arrayIterationIndexStackDepthBefore
                  )
                  state.moveOverOneArrayIterationIndexOnly()
                  Assert.invariant(state.arrayIterationPos == arrayIterationIndexBefore + 1)

                  Assert.invariant(state.occursIndexStack.length == occursIndexStackDepthBefore)
                  state.moveOverOneOccursIndexOnly()
                  Assert.invariant(state.occursPos == occursIndexBefore + 1)

                  Assert.invariant(state.groupIndexStack.length == groupIndexStackDepthBefore)
                  state.moveOverOneGroupIndexOnly() // array elements are always represented.
                  Assert.invariant(state.groupPos == groupIndexBefore + 1)

                  if (isArr)
                    if (state.dataProc.isDefined)
                      state.dataProc.get.afterRepetition(state, this)

                }
                numOccurrences = unparsePositionallyRequiredSeps(
                  unparser,
                  erd,
                  state,
                  numOccurrences,
                  trailingSuspendedOps
                )
                unparser.checkFinalOccursCountBetweenMinAndMaxOccurs(
                  state,
                  unparser,
                  numOccurrences,
                  maxReps,
                  state.arrayIterationPos - 1
                )
                unparser.endArrayOrOptional(erd, state)
              } else {
                //
                // start array for some other array. Not this one.
                //
                Assert.invariant(erd.minOccurs == 0L)
                numOccurrences = unparsePositionallyRequiredSeps(
                  unparser,
                  erd,
                  state,
                  numOccurrences,
                  trailingSuspendedOps
                )
              }

            } else if (ev.isStart) {
              Assert.invariant(!ev.erd.isArray && !erd.isOptional)
              val eventNQN = ev.erd.namedQName
              Assert.invariant(eventNQN != erd.namedQName)
              //
              // start of scalar.
              // That has to be for a different element later in the sequence
              // since this one has a RepUnparser (i.e., is NOT scalar)
              //
              numOccurrences = unparsePositionallyRequiredSeps(
                unparser,
                erd,
                state,
                numOccurrences,
                trailingSuspendedOps
              )
            } else {
              Assert.invariant(ev.isEnd && ev.erd.isComplexType)
              unparser.checkFinalOccursCountBetweenMinAndMaxOccurs(
                state,
                unparser,
                numOccurrences,
                maxReps,
                0
              )
              numOccurrences = unparsePositionallyRequiredSeps(
                unparser,
                erd,
                state,
                numOccurrences,
                trailingSuspendedOps
              )
            }
          } else {
            // no event (state.inspect returned false)
            Assert.invariantFailed("No event for unparsing.")
          }
          state.arrayIterationIndexStack.pop()
          state.occursIndexStack.pop()
        }
        case scalarUnparser =>
          trd match {
            case erd: ElementRuntimeData => {
              // scalar element case. These always get associated separator (if represented)
              unparseOne(scalarUnparser, trd, state) // handles case of non-represented.
              if (erd.isRepresented)
                state.moveOverOneGroupIndexOnly()
            }
            case mgrd: ModelGroupRuntimeData => {
              //
              // There are cases where we suppress the separator associated with a model group child
              //
              // The model group must have no required syntax (initiator/terminator nor alignment)
              // and all optional children (none of which can be present if there are unsuppressed separators)
              //
              // The model group must be zero-length
              // The SSP must be AnyEmpty or
              // The SSP must be TrailingEmpty | TrailingEmptyStrict, AND the model group must be
              // potentially trailing AND actually trailing.
              //
              // Most of the above is static information. Only whether the length is nonZero or zero, and
              // whether it is actually trailing are run-time concepts.
              //
              if (scalarUnparser.isKnownStaticallyNotToSuppressSeparator) {
                unparseOne(scalarUnparser, trd, state)
              } else {
                unparseOneWithSuppression(
                  scalarUnparser,
                  trd,
                  state,
                  trailingSuspendedOps,
                  onlySeparatorFlag = false
                )
              }
              state.moveOverOneGroupIndexOnly()
            }
          }
      }
      state.popTRD(trd)
      index += 1
    }
    ssp match {
      case TrailingEmpty | TrailingEmptyStrict => {
        //
        // For trailing empty suppression, things have to be actually trailing in the sequence.
        // By setting the after-state to the complete end of the sequence, we insure that we suppress
        // separators only if there is nothing at all after them.
        //
        // Note: Quadratic behavior here.
        // When determining if trailing, each suspended separator will examine a list of length N, where
        // N is number of children in the list. It will examine them only to determine if the entries are
        // zero-length or not. But these chains could in principle have shared structure so that there
        // would, in principle, be only one chain length N, not Sum(for i from 1 to N)of(N - i), whichis O(n^2).
        //
        // In practice, these chains are short (separator suppression seldom applies to long arrays, usually to
        // optional fields near the end of a record. So the above may simply not matter.
        //
        for (suspendedOp <- trailingSuspendedOps.toSeq) {
          suspendedOp.captureStateAtEndOfPotentiallyZeroLengthRegionFollowingTheSeparator(state)
        }
      }
      case _ => // do nothing
    }
    state.groupIndexStack.pop()
  }

  private def unparsePositionallyRequiredSeps(
    unparserArg: SequenceChildUnparser,
    erd: ElementRuntimeData,
    state: UState,
    numOccurs: Int,
    trailingSuspendedOps: Buffer[SuppressableSeparatorUnparserSuspendableOperation]
  ): Int = {
    var numOccurrences = numOccurs
    unparserArg match {
      case unparser: RepOrderedSeparatedSequenceChildUnparser => {
        // note that if we are unparsing an array/optional, we only add positionally
        // required separators for occursCountKind="implicit". All other
        // occursCountKind's use the number of elements in the infoset to determine
        // the number of separators, which would have already been unparsed prior to
        // this function being called.
        if (
          (unparser.ock eq OccursCountKind.Implicit) &&
          unparser.isPositional && unparser.isBoundedMax &&
          (!unparser.isDeclaredLast || !unparser.isPotentiallyTrailing)
        ) {
          val maxReps = unparser.maxRepeats(state)
          while (numOccurrences < maxReps) {
            unparseOneWithSuppression(
              unparser,
              erd,
              state,
              trailingSuspendedOps,
              onlySeparatorFlag = true
            )
            state.moveOverOneArrayIterationIndexOnly()
            state.moveOverOneOccursIndexOnly()
            state.moveOverOneGroupIndexOnly()
            numOccurrences += 1
          }
        }
      }
      case _ => Assert.invariantFailed("Not a repeating element")
    }
    numOccurrences
  }

  private def unparseWithNoSuppression(state: UState): Unit = {

    state.groupIndexStack.push(1L) // one-based indexing

    var index = 0
    var doUnparser = false
    val limit = childUnparsers.length

    while (index < limit) {
      val childUnparser = childUnparsers(index)
      val trd = childUnparser.trd
      state.pushTRD(trd) // because we inspect before we invoke child unparser
      //
      // Unparsing an ordered sequence depends on the incoming
      // stream of infoset events matching up with the order that
      // they are expected as the unparser recurses through the
      // child term unparsers.
      //
      childUnparser match {
        case unparser: RepOrderedSeparatedSequenceChildUnparser => {
          state.arrayIterationIndexStack.push(1L)
          state.occursIndexStack.push(1L)
          val erd = unparser.erd
          Assert.invariant(erd.isArray || erd.isOptional)
          Assert.invariant(erd.isRepresented) // arrays/optionals cannot have inputValueCalc

          var numOccurrences = 0
          val maxReps = unparser.maxRepeats(state)

          Assert.invariant(state.inspect)
          val ev = state.inspectAccessor
          val isArr = erd.isArray

          // If the event is for this Rep unparser, we need to consume the StartArray event
          if (ev.erd eq erd) {
            unparser.startArrayOrOptional(state)
          }

          // Unparse each occurrence of this array in the infoset. Note that there could be zero
          // occurrences
          while ({
            doUnparser = unparser.shouldDoUnparser(unparser, state)
            doUnparser
          }) {
            //
            // These are so we can check invariants on these stacks being
            // pushed and popped reliably, and incremented only once
            //
            val arrayIterationIndexBefore = state.arrayIterationPos
            val arrayIterationIndexStackDepthBefore =
              state.arrayIterationIndexStack.length
            val occursIndexBefore = state.occursPos
            val occursIndexStackDepthBefore = state.occursIndexStack.length
            val groupIndexBefore = state.groupPos
            val groupIndexStackDepthBefore = state.groupIndexStack.length

            if (isArr && state.dataProc.isDefined)
              state.dataProc.get.beforeRepetition(state, this)

            unparseOne(unparser, erd, state)
            numOccurrences += 1
            Assert.invariant(
              state.arrayIterationIndexStack.length == arrayIterationIndexStackDepthBefore
            )
            state.moveOverOneArrayIterationIndexOnly()
            Assert.invariant(state.arrayIterationPos == arrayIterationIndexBefore + 1)

            Assert.invariant(state.occursIndexStack.length == occursIndexStackDepthBefore)
            state.moveOverOneOccursIndexOnly()
            Assert.invariant(state.occursPos == occursIndexBefore + 1)

            Assert.invariant(state.groupIndexStack.length == groupIndexStackDepthBefore)
            state.moveOverOneGroupIndexOnly() // array elements are always represented.
            Assert.invariant(state.groupPos == groupIndexBefore + 1)

            if (isArr && state.dataProc.isDefined)
              state.dataProc.get.afterRepetition(state, this)
          }

          // If not enough occurrences are in the infoset, we output extra separators because
          // we are unparsing with no suppression
          if (maxReps > numOccurrences) {
            var numExtraSeps = {
              val sepsNeeded = erd.maxOccurs - numOccurrences
              if ((spos eq Infix) && state.groupPos == 1) {
                // If separatorPosition is infix and we haven't output anything for this sequence
                // yet, then we need one less extra separator, since the separator is skipped
                // for the first instance of infix separators.
                sepsNeeded - 1
              } else {
                sepsNeeded
              }
            }
            while (numExtraSeps > 0) {
              unparseJustSeparator(state)
              numExtraSeps -= 1
            }
          }

          unparser.checkFinalOccursCountBetweenMinAndMaxOccurs(
            state,
            unparser,
            numOccurrences,
            maxReps,
            state.arrayIterationPos - 1
          )

          // If the event is for this Rep unparser, we need to consume the EndArray event
          if (ev.erd eq erd) {
            unparser.endArrayOrOptional(erd, state)
          }

          state.arrayIterationIndexStack.pop()
          state.occursIndexStack.pop()
        }
        case scalarUnparser => {
          unparseOne(scalarUnparser, trd, state)
          // only move over in group if the scalar "thing" is an element
          // that is represented.
          trd match {
            case erd: ElementRuntimeData if (!erd.isRepresented) => // ok, skip group advance
            case _ => state.moveOverOneGroupIndexOnly()
          }
        }
      }
      state.popTRD(trd)
      index += 1
    }
    state.groupIndexStack.pop()
  }
}
