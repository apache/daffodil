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
package org.apache.daffodil.processors.unparsers

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.{ ElementRuntimeData, SequenceRuntimeData, TermRuntimeData }
import org.apache.daffodil.schema.annotation.props.SeparatorSuppressionPolicy
import org.apache.daffodil.schema.annotation.props.gen.{ SeparatorPosition }
import org.apache.daffodil.processors.ModelGroupRuntimeData
import scala.collection.mutable.Buffer
import SeparatorSuppressionPolicy._
import SeparatorPosition._

trait Separated { self: SequenceChildUnparser =>

  def sep: Unparser
  def spos: SeparatorPosition
  def ssp: SeparatorSuppressionPolicy
  def zeroLengthDetector: ZeroLengthDetector
  def isPotentiallyTrailing: Boolean

  def isKnownStaticallyNotToSuppressSeparator: Boolean

  def isDeclaredLast: Boolean

  def isPositional: Boolean

  val childProcessors = Vector(childUnparser, sep)
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
  override val isDeclaredLast: Boolean)
  extends SequenceChildUnparser(childUnparser, srd, trd)
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
  isDeclaredLast: Boolean)
  extends ScalarOrderedSeparatedSequenceChildUnparserBase(childUnparser, srd, trd, sep, spos, ssp,
    zlDetector, isPotentiallyTrailing, isKnownStaticallyNotToSuppressSeparator, isPositional,
    isDeclaredLast)

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
  override val isDeclaredLast: Boolean)
  extends RepeatingChildUnparser(childUnparser, srd, erd)
  with Separated {

  override def checkArrayPosAgainstMaxOccurs(state: UState) =
    state.arrayPos <= maxRepeats(state)
}

class OrderedSeparatedSequenceUnparser(
  rd: SequenceRuntimeData,
  ssp: SeparatorSuppressionPolicy,
  spos: SeparatorPosition,
  sep: Unparser,
  childUnparsersArg: Vector[SequenceChildUnparser])
  extends OrderedSequenceUnparserBase(rd, childUnparsersArg :+ sep) {

  private val childUnparsers =
    childUnparsersArg.asInstanceOf[Seq[SequenceChildUnparser with Separated]]

  /**
   * Unparses one occurrence with associated separator (non-suppressable).
   */
  protected def unparseOne(
    unparser: SequenceChildUnparser,
    trd: TermRuntimeData,
    state: UState): Unit = {

    if (trd.isRepresented) {
      spos match {
        case Prefix => {
          sep.unparse1(state)
          unparser.unparse1(state)
        }
        case Infix => {
          if (state.groupPos > 1)
            sep.unparse1(state)
          unparser.unparse1(state)
        }
        case Postfix => {
          unparser.unparse1(state)
          sep.unparse1(state)
        }
      }
    } else {
      Assert.invariant(!trd.isRepresented)
      unparser.unparse1(state)
    }
  }

  /**
   * Unparses the separator only.
   *
   * Does not deals with infix boundary condition.
   */
  private def unparseJustSeparator(state: UState): Unit = {
    sep.unparse1(state)
  }

  /**
   * Unparses the separator only.
   *
   * Does not have to deal with infix and first child.
   */
  private def unparseJustSeparatorWithTrailingSuppression(
    trd: TermRuntimeData,
    state: UState,
    trailingSuspendedOps: Buffer[SuppressableSeparatorUnparserSuspendableOperation]): Unit = {

    val suspendableOp = new SuppressableSeparatorUnparserSuspendableOperation(sep, trd)
    // TODO: merge these two objects. We can allocate just one thing here.
    val suppressableSep = SuppressableSeparatorUnparser(sep, trd, suspendableOp)

    suppressableSep.unparse1(state)
    trailingSuspendedOps += suspendableOp
  }

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
    onlySeparatorFlag: Boolean): Unit = {
    val doUnparseChild = !onlySeparatorFlag
    // We don't know if the unparse will result in zero length or not.
    // We have to use a suspendable unparser here for the separator
    // which suspends until it is known whether the unparse of the contents
    // were ZL or not.
    //
    // infix, prefix, postfix matters here, because the separator comes after
    // for postfix.

    if ((spos eq Infix) && state.groupPos == 1) {
      // no separator possible; hence, no suppression
      if (doUnparseChild) unparser.unparse1(state)
    } else {

      val suspendableOp = new SuppressableSeparatorUnparserSuspendableOperation(sep, trd)
      // TODO: merge these two objects. We can allocate just one thing here.
      val suppressableSep = SuppressableSeparatorUnparser(sep, trd, suspendableOp)

      spos match {
        case Prefix | Infix => {
          suppressableSep.unparse1(state)
          if (doUnparseChild) unparser.unparse1(state)
          ssp match {
            case AnyEmpty => {
              suspendableOp.captureStateAtEndOfPotentiallyZeroLengthRegionFollowingTheSeparator(state)
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
              suspendableOp.captureStateAtEndOfPotentiallyZeroLengthRegionFollowingTheSeparator(state)
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
      state.pushTRD(trd) // because we inspect before we call the unparse1 for the child unparser.
      val zlDetector = childUnparser.zeroLengthDetector
      childUnparser match {
        case unparser: RepOrderedSeparatedSequenceChildUnparser => {
          state.arrayIndexStack.push(1L)
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
                  val arrayIndexBefore = state.arrayPos
                  val arrayIndexStackDepthBefore = state.arrayIndexStack.length
                  val groupIndexBefore = state.groupPos
                  val groupIndexStackDepthBefore = state.groupIndexStack.length

                  Assert.invariant(erd.isRepresented) // since this is an array, can't have inputValueCalc

                  if (isArr) if (state.dataProc.isDefined) state.dataProc.get.beforeRepetition(state, this)

                  if (unparser.isKnownStaticallyNotToSuppressSeparator ||
                    {
                      val isKnownNonZeroLength =
                        zlDetector.isKnownNonZeroLength(state.inspectAccessor.info.element)
                      isKnownNonZeroLength
                    }) {
                    unparseOne(unparser, erd, state)
                  } else {
                    unparseOneWithSuppression(unparser, erd, state, trailingSuspendedOps,
                      onlySeparatorFlag = false)
                  }
                  numOccurrences += 1
                  Assert.invariant(state.arrayIndexStack.length == arrayIndexStackDepthBefore)
                  state.moveOverOneArrayIndexOnly()
                  Assert.invariant(state.arrayPos == arrayIndexBefore + 1)

                  Assert.invariant(state.groupIndexStack.length == groupIndexStackDepthBefore)
                  state.moveOverOneGroupIndexOnly() // array elements are always represented.
                  Assert.invariant(state.groupPos == groupIndexBefore + 1)

                  if (isArr) if (state.dataProc.isDefined) state.dataProc.get.afterRepetition(state, this)

                }
                numOccurrences = unparsePositionallyRequiredSeps(unparser, erd, state, numOccurrences, trailingSuspendedOps)
                unparser.checkFinalOccursCountBetweenMinAndMaxOccurs(state, unparser, numOccurrences, maxReps, state.arrayPos - 1)
                unparser.endArrayOrOptional(erd, state)
              } else {
                //
                // start array for some other array. Not this one.
                //
                Assert.invariant(erd.minOccurs == 0L)
                numOccurrences = unparsePositionallyRequiredSeps(unparser, erd, state, numOccurrences, trailingSuspendedOps)
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
              numOccurrences = unparsePositionallyRequiredSeps(unparser, erd, state, numOccurrences, trailingSuspendedOps)
            } else {
              Assert.invariant(ev.isEnd && ev.erd.isComplexType)
              unparser.checkFinalOccursCountBetweenMinAndMaxOccurs(state, unparser, numOccurrences, maxReps, 0)
              numOccurrences = unparsePositionallyRequiredSeps(unparser, erd, state, numOccurrences, trailingSuspendedOps)
            }
          } else {
            // no event (state.inspect returned false)
            Assert.invariantFailed("No event for unparsing.")
          }
          state.arrayIndexStack.pop()
        }
        case scalarUnparser => trd match {
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
              unparseOneWithSuppression(scalarUnparser, trd, state, trailingSuspendedOps,
                onlySeparatorFlag = false)
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
    erd: ElementRuntimeData, state: UState, numOccurs: Int, trailingSuspendedOps: Buffer[SuppressableSeparatorUnparserSuspendableOperation]): Int = {
    var numOccurrences = numOccurs
    unparserArg match {
      case unparser: RepOrderedSeparatedSequenceChildUnparser => {
        val isBounded = unparser.isBoundedMax
        if (unparser.isPositional && isBounded &&
          (!unparser.isDeclaredLast || !unparser.isPotentiallyTrailing)) {
          val maxReps = unparser.maxRepeats(state)
          while (numOccurrences < maxReps) {
            unparseOneWithSuppression(unparser, erd, state, trailingSuspendedOps,
              onlySeparatorFlag = true)
            state.moveOverOneArrayIndexOnly()
            state.moveOverOneGroupIndexOnly()
            numOccurrences += 1
          }
          //
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
          state.arrayIndexStack.push(1L)
          val erd = unparser.erd
          var numOccurrences = 0
          val maxReps = unparser.maxRepeats(state)
          //val isBounded = unparser.isBoundedMax // not needed for the no-suppression case

          //
          // The number of occurrances we unparse is always exactly driven
          // by the number of infoset events for the repeating/optional element.
          //
          // For RepUnparser - array/optional case - in all cases we should get a
          // startArray event. If we don't then
          // the element must be entirely optional, so we get no events for it
          // at all.
          //

          if (state.inspect) {
            val ev = state.inspectAccessor
            val isArr = erd.isArray
            if (ev.isStart && (isArr || erd.isOptional)) {
              if (ev.erd eq erd) {
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
                  val arrayIndexBefore = state.arrayPos
                  val arrayIndexStackDepthBefore = state.arrayIndexStack.length
                  val groupIndexBefore = state.groupPos
                  val groupIndexStackDepthBefore = state.groupIndexStack.length

                  Assert.invariant(erd.isRepresented) // since this is an array, can't have inputValueCalc

                  if (isArr) if (state.dataProc.isDefined) state.dataProc.get.beforeRepetition(state, this)

                  unparseOne(unparser, erd, state)
                  numOccurrences += 1
                  Assert.invariant(state.arrayIndexStack.length == arrayIndexStackDepthBefore)
                  state.moveOverOneArrayIndexOnly()
                  Assert.invariant(state.arrayPos == arrayIndexBefore + 1)

                  Assert.invariant(state.groupIndexStack.length == groupIndexStackDepthBefore)
                  state.moveOverOneGroupIndexOnly() // array elements are always represented.
                  Assert.invariant(state.groupPos == groupIndexBefore + 1)

                  if (isArr) if (state.dataProc.isDefined) state.dataProc.get.afterRepetition(state, this)
                }
                //
                // If not enough occurences in array, we output extra separators
                //
                if (maxReps > numOccurrences) {
                  var numExtraSeps = erd.maxOccurs - numOccurrences
                  while (numExtraSeps > 0) {
                    unparseJustSeparator(state)
                    numExtraSeps -= 1
                  }
                }
                unparser.checkFinalOccursCountBetweenMinAndMaxOccurs(state, unparser, numOccurrences, maxReps, state.arrayPos - 1)
                unparser.endArrayOrOptional(erd, state)
              } else {
                //
                // start array for some other array. Not this one. So we
                // don't unparse anything here, and we'll go on to the next
                // sequence child, which hopefully will be a matching array.
                //
                Assert.invariant(erd.minOccurs == 0L)
              }
            } else if (ev.isStart) {
              Assert.invariant(!ev.erd.isArray && !erd.isOptional)
              //
              // start of scalar.
              // That has to be for a different element later in the sequence
              // since this one has a RepUnparser (i.e., is NOT scalar)
              //
              val eventNQN = ev.erd.namedQName
              Assert.invariant(eventNQN != erd.namedQName)
            } else {
              Assert.invariant(ev.isEnd && ev.erd.isComplexType)
              unparser.checkFinalOccursCountBetweenMinAndMaxOccurs(state, unparser, numOccurrences, maxReps, 0)
            }
          } else {
            // no event (state.inspect returned false)
            Assert.invariantFailed("No event for unparsing.")
          }
          state.arrayIndexStack.pop()
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
