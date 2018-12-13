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
import org.apache.daffodil.processors.ModelGroupRuntimeData

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

sealed abstract class ScalarOrderedSeparatedSequenceChildUnparserBase(childUnparser: Unparser,
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

class ScalarOrderedSeparatedSequenceChildUnparser(childUnparser: Unparser,
  srd: SequenceRuntimeData,
  trd: TermRuntimeData,
  sep: Unparser,
  spos: SeparatorPosition,
  ssp: SeparatorSuppressionPolicy,
  ssAlgorithm: SeparatorSuppressionMode)
  extends ScalarOrderedSeparatedSequenceChildUnparserBase(childUnparser, srd, trd, sep, spos, ssp, ssAlgorithm)

class PotentiallyTrailingGroupSeparatedSequenceChildUnparser(childUnparser: Unparser,
  srd: SequenceRuntimeData,
  val mrd: ModelGroupRuntimeData,
  sep: Unparser,
  spos: SeparatorPosition,
  ssp: SeparatorSuppressionPolicy,
  ssAlgorithm: SeparatorSuppressionMode)
  extends ScalarOrderedSeparatedSequenceChildUnparserBase(childUnparser, srd, mrd, sep, spos, ssp, ssAlgorithm)
  with PotentiallyTrailingGroupSequenceChildUnparser

class RepOrderedSeparatedSequenceChildUnparser(childUnparser: Unparser,
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

class OrderedSeparatedSequenceUnparser(rd: SequenceRuntimeData,
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
  protected def unparseOne(unparser: SequenceChildUnparser,
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
  private def unparseZeroLengthWithoutSeparatorForSideEffect(unparser: SequenceChildUnparser,
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
                    ssAlgorithm.shouldSuppressIfZeroLength(state,
                      state.inspectAccessor.asElement)
                  import SeparatorSuppressionAction._
                  suppressionAction match {
                    case DoNotSuppress => {
                      //
                      // If there are pending potentially trailing separators,
                      // then we've just proven that they are NOT actually trailing
                      // So we output them all.
                      //
                      // This works for array elements of a simpleType element array, since
                      // we can examine each element and determine if it will be zero-length
                      // when unparsed without having to unparse it.
                      //
                      // This is a heuristic though. It isn't necessarily correct.
                      // For example, if when we unparse the simple element, there could be
                      // statements (set var, asserts, etc.) those could cause the execution to suspend
                      // which is transparent to us, but for example, if the element has
                      // dfdl:lengthKind="pattern" (say), but dfdl:terminator="{ some expression }"
                      // and the expression returns zero-length (e.g., '%ES;' - which is allowed when
                      // the lengthKind is not 'delimited') then the value
                      // could be zero length even though there is a terminator.
                      // Hence, the zero-length predictor really has to take lengthKind and the complete unparsing
                      // into account. Furthermore, that dfdl:terminator expression could refer to
                      // an element with dfdl:outputValueCalc that does not yet have a value.
                      // If so then we can't know ZL for sure.
                      //
                      // In general, we can't know ZL without doing the unparsing and measuring it,
                      // and that's what all the unparser complexity of suspensions and split buffering is
                      // enabling. So really this quick check of a zeroLengthPredictor is just going to
                      // give an approximation that says "not ZL for sure" because it sees the value has
                      // characters/bytes in it, so can analyze that, or if there aren't characters/bytes it
                      // has to be conservative and go back to actually doing the unparsing, block until the
                      // length is resolved, and measure if it is zero length.
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

        case null => { // potentiallyTrailingGroupUnparser: PotentiallyTrailingGroupSequenceChildUnparser => {
          //
          // We have to unparse the group, for side-effects potentially.
          // however, we don't know whether to output the separator.
          //
          // The problem is that whether we put down this separator depends on whether
          // the children of this group are in fact all absent and so there's nothing to output here, AND
          // there's no subsequent siblings (i.e, this model group is truly trailing empty.)
          //
          // This really is a case where we can't stream.
          //
          // Simpler DFDL implementations could implement this behavior by some sort of backtracking on the output stream.
          // They could use similar techniques for dealing with the storing of prefix-lengths. They don't implement
          // don't have to do dfdl:outputValueCalc with arbitrary forward-reference though, so
          // our technique for implementing this might have to be different.
          //

          //
          // Possible mechanism 1:
          //
          // A zeroLengthPredictor for a model group will pull elements until either (a) it finds one that
          // allows it to know non-zero length, or it finds one beyond end of group. If it detects end of group
          // but has all ZL-predicted elements (or no elements) pulled, then it can successfully predict that the
          // whole group will be zero length.
          //
          // ZL predictors are potentially problematic in case of non-delimited lengthKind, where a delimiter expression can
          // return a zero-length. That would make a ZL predictor conservative - it would assume non-zero length if there
          // are any delimiter expressions. The same is true if elements have outputValueCalc. In that case those would
          // have to be evaluated so as to get the values in order to determine if they will in fact be zero length.
          //
          // So a ZL predictor could, in general, have to suspend.
          //
          // And the zeroLengthPredictor for a model group potentially has to recurse. It's a schema-aware structure.
          //
          // Look for events corresponding to "beyond the group". if next event is "beyond the group's elements",
          // then we know the group won't have any element contents, and so knowing that it is
          // potentially trailing, if it has ZL predicted, then we can queue up (add to the counter) another separator for it
          // which will ultimately get discarded if this is truly trailing. Then we can unparse the child recursively
          // for side-effects (like variable setting), knowing that it will use zero-length in doing so.
          //
          // If we do find events for simple elements, if the are predictably zero-length (using zeroLengthPredictor)
          // then again we know not to output the separator.
          //
          // If we find events for other things within the group.... punt and output the separator.
          //

          //
          // The Architecturally "Right Thing" to always get this right....
          //
          // Right thing is to suspend a conditional separator unparser here that is blocked until
          // it is known whether (a) the group contents unparsed to non-zero length or (b)
          // subsequent siblings unparsed to non-zero length.
          //
          // We would need a way to suspend on this complex predicate. It can't be expressed in terms of DPath
          // language, but like alignment suspensions, we can have specific code for this kind of a suspension.
          //
          // Since many many unparsers have to work by way of suspensions and such, this *shouldn't be* that
          // hard to do, and should be preferable to some ad-hoc zero-length-predictor.
          //
          // Important to keep in mind that as we recursively invoke unparsers, the unparser is doing its thing
          // suspending things, splitting dataOutputStreams into new buffering streams, etc. All that is
          // transparent to us.
          //
          // We just want to play that same game here to reduce complexity, and avoid a bunch of ad-hoc control
          // flow that ultimately won't be correct in all cases.
          //
        }
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
