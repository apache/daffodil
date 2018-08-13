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

import org.apache.daffodil.equality.TypeEqual
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.{ ElementRuntimeData, SequenceRuntimeData, TermRuntimeData }
import org.apache.daffodil.schema.annotation.props.SeparatorSuppressionPolicy
import org.apache.daffodil.schema.annotation.props.gen.{ SeparatorPosition }
import org.apache.daffodil.infoset.DIElement

/**
 * DFDL Spec. section 14.2.3 specifies only a few different behaviors
 * for separator suppression. Each has an algorithm.
 */
sealed trait SeparatorSuppressionMode extends Serializable {

  /**
   * Determines if we should suppress a zero length element and
   * separator.
   *
   * Checks if the length is zero if necessary to decide.
   *
   * Returns Suppress or IfTrailing when the element is, in fact, zero length,
   * and the algorithm wants those to be suppressed.
   * Returns DoNotSuppress otherwise.
   */
  def shouldSuppressIfZeroLength(state: UState, infosetElement: DIElement): SeparatorSuppressionAction

  def shouldDoExtraSeparators = false

  def checkArrayPosAgainstMaxOccurs(state: UState, maxRepeats: Long): Boolean = true

}

object SeparatorSuppressionMode {
  type Type = SeparatorSuppressionMode
  import SeparatorSuppressionAction._

  object None extends Type {
    def shouldSuppressIfZeroLength(state: UState, infosetElement: DIElement) = Assert.usageError("not to be called.")
  }
  object FixedOrExpression extends Type {
    def shouldSuppressIfZeroLength(state: UState, infosetElement: DIElement) = DoNotSuppress
  }

  class SuppressAnyEmpty(zeroLengthDetector: ZeroLengthDetector) extends Type {
    def shouldSuppressIfZeroLength(state: UState, infosetElement: DIElement) = {
      val isZL = zeroLengthDetector.isZeroLength(infosetElement)
      if (isZL) Suppress
      else DoNotSuppress
    }
  }

  trait CheckArrayPosMixin { self: SeparatorSuppressionMode =>
    override def checkArrayPosAgainstMaxOccurs(state: UState, maxRepeats: Long): Boolean = {
      state.arrayPos <= maxRepeats
    }
  }

  class ImplicitSuppressAnyEmpty(zeroLengthDetector: ZeroLengthDetector)
    extends SuppressAnyEmpty(zeroLengthDetector)
    with CheckArrayPosMixin

  object ImplicitNeverOrNotPotentiallyTrailing extends Type
    with CheckArrayPosMixin {
    def shouldSuppressIfZeroLength(state: UState, infosetElement: DIElement) = DoNotSuppress

    override def shouldDoExtraSeparators = true

  }

  class ImplicitPotentiallyTrailing(zeroLengthDetector: ZeroLengthDetector) extends Type
    with CheckArrayPosMixin {
    def shouldSuppressIfZeroLength(state: UState, infosetElement: DIElement) = {
      val isZL = zeroLengthDetector.isZeroLength(infosetElement)
      if (isZL) IfTrailing
      else DoNotSuppress
    }
  }
}

sealed trait SeparatorSuppressionAction extends Serializable
object SeparatorSuppressionAction {
  type Type = SeparatorSuppressionAction
  case object Suppress extends Type
  case object DoNotSuppress extends Type
  case object IfTrailing extends Type
}

trait Separated { self: SequenceChildUnparser =>

  def sep: Unparser
  def spos: SeparatorPosition
  def ssp: SeparatorSuppressionPolicy
  def ssAlgorithm: SeparatorSuppressionMode

  val childProcessors = Vector(childUnparser, sep)
}

class ScalarOrderedSeparatedSequenceChildUnparser(
  childUnparser: Unparser,
  srd: SequenceRuntimeData,
  trd: TermRuntimeData,
  val sep: Unparser,
  val spos: SeparatorPosition,
  val ssp: SeparatorSuppressionPolicy,
  val ssAlgorithm: SeparatorSuppressionMode)
  extends SequenceChildUnparser(childUnparser, srd, trd)
  with Separated {

  override def unparse(state: UState) = childUnparser.unparse1(state)
}

class RepOrderedSeparatedSequenceChildUnparser(
  childUnparser: Unparser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData,
  val sep: Unparser,
  val spos: SeparatorPosition,
  val ssp: SeparatorSuppressionPolicy, // need for diagnostics perhaps
  val ssAlgorithm: SeparatorSuppressionMode)
  extends RepeatingChildUnparser(childUnparser, srd, erd)
  with Separated {

  override def checkArrayPosAgainstMaxOccurs(state: UState) = ssAlgorithm.checkArrayPosAgainstMaxOccurs(state, maxRepeats(state))
}

class OrderedSeparatedSequenceUnparser(
  rd: SequenceRuntimeData,
  ssp: SeparatorSuppressionPolicy,
  spos: SeparatorPosition,
  sep: Unparser,
  childUnparsersArg: Vector[SequenceChildUnparser])
  extends OrderedSequenceUnparserBase(rd, childUnparsersArg) {

  private val childUnparsers = childUnparsersArg.asInstanceOf[Seq[SequenceChildUnparser with Separated]]

  /**
   * True if requires special treatment in the unparse processing loop, as occurrences
   * of later sequence children can influence whether possible trailing separators
   * from earlier sequence children are actually trailing or not.
   */
  private type IPT = SeparatorSuppressionMode.ImplicitPotentiallyTrailing

  private val hasTrailingSeparatorSuppression = {
    childUnparsers.last.ssAlgorithm.isInstanceOf[IPT]
  }

  /**
   * Unparses one occurrence.
   */
  protected def unparseOne(
    unparser: SequenceChildUnparser,
    trd: TermRuntimeData,
    state: UState): Unit = {

    if (trd.isRepresented) {
      if ((spos eq SeparatorPosition.Prefix)) {
        sep.unparse1(state)
      } else if ((spos eq SeparatorPosition.Infix) && state.groupPos > 1) {
        sep.unparse1(state)
      }
    }

    unparser.unparse1(state)

    if ((spos eq SeparatorPosition.Postfix) && trd.isRepresented) {
      sep.unparse1(state)
    }
  }

  /**
   * Unparses a zero-length occurrence, without the separator. This is so that
   * any statements or other side-effects (discriminators, setVariable, etc.)
   * will occur.
   */
  private def unparseZeroLengthWithoutSeparatorForSideEffect(
    unparser: SequenceChildUnparser,
    trd: TermRuntimeData,
    state: UState): Unit = {
    //
    // Unfortunately there's no way to confirm that this produced zero length
    // because of the possible buffering going on in the unparser.
    // We'd have to depend on intricate details of the unparser behavior to do this,
    // and that's unwise from separation-of-concerns perspective.
    //
    unparser.unparse1(state)
  }

  /**
   * Unparses the separator only.
   *
   * Does not deals with infix boundary condition, because
   * the counting of the potential trailing separators takes
   * this into account.
   */
  private def unparseJustSeparator(state: UState): Unit = {
    sep.unparse1(state)
  }

  /**
   * Returns 1, or 0 if infix separator, and this is the first thing
   * in the sequence meaning there is no separator for it.
   *
   * However, if we're not doing trailing separator suppression, always
   * returns 0.
   */
  private def suppressedTrailingSeparatorIncrement(unparser: SequenceChildUnparser with Separated, state: UState): Int = {
    Assert.usage(unparser.trd.isRepresented)
    val notIPT = !unparser.ssAlgorithm.isInstanceOf[IPT]
    val result =
      if (notIPT)
        0
      else {
        val infixAndFirst = (spos eq SeparatorPosition.Infix) && state.groupPos == 1
        if (infixAndFirst)
          0
        else
          1
      }
    result
  }

  /**
   * Unparses an entire sequence, including both scalar and array/optional children.
   */
  protected def unparse(state: UState): Unit = {

    state.groupIndexStack.push(1L) // one-based indexing

    var index = 0
    var doUnparser = false
    val limit = childUnparsers.length

    var potentialTrailingSeparatorCount: Int = 0

    // This state var just lets us check some important
    // invariants about potentially trailing, e.g., once
    // you hit it, it sticks until the end of the group.
    //
    var haveSeenPotentiallyTrailingSeparators = false

    while (index < limit) {
      val childUnparser = childUnparsers(index)
      val trd = childUnparser.trd

      //
      // Unparsing an ordered sequence depends on the incoming
      // stream of infoset events matching up with the order that
      // they are expected as the unparser recurses through the
      // child term unparsers.
      //
      childUnparser match {
        case unparser: RepOrderedSeparatedSequenceChildUnparser => {
          val erd = unparser.erd
          var numOccurrences = 0
          val maxReps = unparser.maxRepeats(state)
          //
          // The number of occurrances we unparse is always exactly driven
          // by the number of infoset events for the repeating/optional element.
          //
          // For RepUnparser - array/optional case - in all cases we should get a
          // startArray event. If we don't then
          // the element must be entirely optional, so we get no events for it
          // at all.
          //
          val ssAlgorithm = unparser.ssAlgorithm

          if (state.inspect) {
            val ev = state.inspectAccessor
            val isArr = erd.isArray
            if (ev.isStart && (isArr || erd.isOptional)) {
              val eventNQN = ev.node.namedQName
              if (eventNQN =:= erd.namedQName) {

                //
                // Note: leaving in some ofthese println, since debugger for unparsing is so inadequate currently.
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
                  val suppressionAction =
                    ssAlgorithm.shouldSuppressIfZeroLength(
                      state,
                      state.inspectAccessor.asElement)
                  import SeparatorSuppressionAction._
                  suppressionAction match {
                    case DoNotSuppress => {
                      //
                      // If there are pending potentially trailing separators,
                      // then we've just proven that they are NOT actually trailing
                      // So we output them all.
                      //
                      while (potentialTrailingSeparatorCount > 0) {
                        Assert.invariant(haveSeenPotentiallyTrailingSeparators)
                        Assert.invariant(ssAlgorithm.isInstanceOf[IPT]) // sticks once we hit one
                        unparseJustSeparator(state)
                        potentialTrailingSeparatorCount -= 1
                      }

                      if (isArr) if (state.dataProc.isDefined) state.dataProc.get.beforeRepetition(state, this)
                      // System.err.println("Starting unparse of occurrence of %s. Array Index Stack is: %s".format(
                      //   erd.namedQName, state.arrayIndexStack))

                      unparseOne(unparser, erd, state)
                      numOccurrences += 1

                      state.moveOverOneArrayIndexOnly()
                      // System.err.println("Finished unparse of occurrence of %s. Array Index Stack is: %s".format(
                      //   erd.namedQName, state.arrayIndexStack))

                      state.moveOverOneGroupIndexOnly() // array elements are always represented.

                      if (isArr) if (state.dataProc.isDefined) state.dataProc.get.afterRepetition(state, this)
                    }
                    case Suppress => {
                      unparseZeroLengthWithoutSeparatorForSideEffect(unparser, trd, state)
                      state.moveOverOneArrayIndexOnly()
                      state.moveOverOneGroupIndexOnly() // array elements are always represented.
                    }
                    case IfTrailing => {
                      Assert.invariant(hasTrailingSeparatorSuppression)
                      haveSeenPotentiallyTrailingSeparators = true // sticks once we've hit one.
                      unparseZeroLengthWithoutSeparatorForSideEffect(unparser, trd, state)
                      state.moveOverOneArrayIndexOnly()
                      state.moveOverOneGroupIndexOnly() // array elements are always represented
                      potentialTrailingSeparatorCount += suppressedTrailingSeparatorIncrement(unparser, state)
                    }
                  }
                }
                //
                // For maxOccurs bounded, and never or not potentially trailing
                //
                if (ssAlgorithm.shouldDoExtraSeparators && maxReps > numOccurrences) {
                  var numExtraSeps = erd.maxOccurs - numOccurrences
                  while (numExtraSeps > 0) {
                    unparseJustSeparator(state)
                    numExtraSeps -= 1
                  }
                }
                // System.err.println("Finished unparse of array/opt %s. Array Index Stack is: %s, maxReps %s, numOccurrences %s".format(
                //   erd.namedQName, state.arrayIndexStack, maxReps, numOccurrences))
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
              val eventNQN = ev.node.namedQName
              Assert.invariant(eventNQN != erd.namedQName)
            } else {
              Assert.invariant(ev.isEnd && ev.isComplex)
              unparser.checkFinalOccursCountBetweenMinAndMaxOccurs(state, unparser, numOccurrences, maxReps, 0)
            }
          } else {
            // no event (state.inspect returned false)
            Assert.invariantFailed("No event for unparsing.")
          }
        }
        //
        case scalarUnparser => {
          //
          // Note that once we've encountered a trailing separator situation,
          // all subsequent sequence children must also be that, otherwise
          // the first one wouldn't have been in trailing position to begin with.
          //
          // So that means we can never find ourself encountering
          // a need to output potentially trailing separators here.
          //
          Assert.invariant(!haveSeenPotentiallyTrailingSeparators)
          Assert.invariant(potentialTrailingSeparatorCount == 0)
          unparseOne(scalarUnparser, trd, state)
          // only move over in group if the scalar "thing" is an element
          // that is represented.
          trd match {
            case erd: ElementRuntimeData if (!erd.isRepresented) => // ok, skip group advance
            case _ => state.moveOverOneGroupIndexOnly()
          }
        }
      }
      index += 1
    }
    state.groupIndexStack.pop()
  }

}

