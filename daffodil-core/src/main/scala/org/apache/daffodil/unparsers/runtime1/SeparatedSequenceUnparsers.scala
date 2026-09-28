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
import org.apache.daffodil.lib.schema.annotation.props.SeparatorSuppressionPolicy.*
import org.apache.daffodil.lib.schema.annotation.props.gen.OccursCountKind
import org.apache.daffodil.lib.schema.annotation.props.gen.SeparatorPosition
import org.apache.daffodil.lib.schema.annotation.props.gen.SeparatorPosition.*
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.MaybeInt
import org.apache.daffodil.runtime1.infoset.DIArray
import org.apache.daffodil.runtime1.infoset.DIComplex
import org.apache.daffodil.runtime1.infoset.DINode
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.ModelGroupRuntimeData
import org.apache.daffodil.runtime1.processors.SequenceRuntimeData
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.unparsers.*

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
) extends OrderedSequenceUnparserBase(rd)
  with WriteUnparser {
  // Sequences of nothing (no initiator, no terminator, nothing at all) should
  // have been optimized away
  Assert.invariant(childUnparsers.length > 0)

  override val runtimeDependencies = Array()

  override def childProcessors = childUnparsers.toVector

  /**
   * In-flight separator-suppression state for one writeContent call:
   * whether any term has already written something (`wroteAny`), whichever
   * separator is currently deferred pending its term's content
   * (`pendingPostfixSeparatorAtTerm` for ssp Never,
   * `pendingSuppressibleOp` for AnyEmpty/TrailingEmpty(Strict)), and the
   * end-of-sequence TrailingEmpty(Strict) queue (`trailingSuspendedOps`).
   */
  private class SeparatorSuppressionState(state: UState) {

    private var wroteAny = false

    // for spos == Postfix, the separator for a represented term must come
    // AFTER its content is written, not before. Set by beforeSeparator,
    // cleared by afterSeparator once that term's content has been written.
    private var pendingPostfixSeparatorAtTerm = false

    // For ssp AnyEmpty/TrailingEmpty(Strict): the in-flight suspension
    // beforeSeparator started for the current term, completed by
    // afterSeparator. Mirrors pendingPostfixSeparatorAtTerm's role for ssp
    // Never, but as a suspension object rather than a flag.
    private var pendingSuppressibleOp: SuppressableSeparatorUnparserSuspendableOperation = null

    // For ssp TrailingEmpty/TrailingEmptyStrict: separators deferred until
    // the sequence's own end (DFDL requires trailing through the whole
    // sequence, not just the local group), matching
    // unparseWithSuppression's end-of-loop resolution.
    private val trailingSuspendedOps =
      scala.collection.mutable.Buffer[SuppressableSeparatorUnparserSuspendableOperation]()

    /**
     * ssp=never never omits separators based on content, so a term with
     * fewer than maxOccurs actual occurrences still gets one separator per
     * missing occurrence. Other policies decide via content length
     * instead; irrelevant here.
     */
    def writeExtraSeparatorsIfNeverSuppressed(
      rep: RepeatingChildUnparser,
      numOccurrences: Long
    ): Unit = {
      if (ssp != Never) return
      // Uses erd.maxOccurs, not rep.maxRepeats(state): for
      // occursCountKind="expression"/"parsed", maxRepeats(state) is
      // Long.MaxValue, which would loop until OOM. An unbounded array's
      // maxOccurs is -1, so sepsNeeded goes negative and this loop no-ops.
      val sepsNeeded = rep.erd.maxOccurs - numOccurrences
      if (sepsNeeded <= 0) return
      val numExtraSeps = if ((spos eq Infix) && !wroteAny) {
        sepsNeeded - 1
      } else {
        sepsNeeded
      }
      var n = numExtraSeps
      while (n > 0) {
        unparseJustSeparator(state)
        n -= 1
      }
      // Deliberately NOT wroteAny = true: this never advances
      // state.groupPos, so a following Infix term must still see this as
      // unwritten, or it would wrongly get its own separator too.
    }

    /**
     * occursCountKind="implicit" is positional, so a non-trailing bounded
     * array/optional still needs speculative missing-occurrence separators,
     * or a later term shifts position. stacksAlreadyPushed: true only right
     * after an actual occurrence loop already positioned the stacks.
     */
    def writePositionallyRequiredSepsIfSuppressed(
      rep: RepeatingChildUnparser with Separated,
      numOccurrences: Long,
      stacksAlreadyPushed: Boolean
    ): Unit = {
      if (ssp == Never) return
      if (
        (rep.ock ne OccursCountKind.Implicit) ||
        !rep.isPositional || !rep.isBoundedMax ||
        (rep.isDeclaredLast && rep.isPotentiallyTrailing)
      ) return
      // safe: isBoundedMax being true guarantees maxRepeats(state) is the
      // actual, finite erd.maxOccurs, never Long.MaxValue.
      val maxReps = rep.maxRepeats(state)
      if (numOccurrences >= maxReps) return
      if (!stacksAlreadyPushed) {
        state.pushOccurrenceIndices()
      }
      var n = numOccurrences
      while (n < maxReps) {
        beforeSeparator(rep.erd, rep.isKnownStaticallyNotToSuppressSeparator)
        afterSeparator()
        n += 1
        state.moveOverOneArrayIterationIndexOnly()
        state.moveOverOneOccursIndexOnly()
      }
      if (!stacksAlreadyPushed) {
        state.popOccurrenceIndices()
      }
    }

    /**
     * Called before a term/occurrence's content is written, once known
     * to actually be written. For ssp Never (or staticallyNotSuppressible),
     * Prefix/Infix write immediately and Postfix defers; otherwise
     * Prefix/Infix speculatively unparse a suppressible separator.
     */
    def beforeSeparator(
      trd: TermRuntimeData,
      staticallyNotSuppressible: Boolean
    ): Unit = {
      if (staticallyNotSuppressible || (ssp eq Never)) {
        spos match {
          case Prefix => unparseJustSeparator(state)
          case Infix => if (wroteAny) unparseJustSeparator(state)
          case Postfix => pendingPostfixSeparatorAtTerm = true
        }
        wroteAny = true
        return
      }
      ssp match {
        case Never =>
          Assert.invariantFailed("handled above")
        case AnyEmpty | TrailingEmpty | TrailingEmptyStrict =>
          spos match {
            case Prefix | Infix =>
              if ((spos eq Infix) && !wroteAny) {
                // no separator possible; hence, no suppression
              } else {
                val suspendableOp =
                  new SuppressableSeparatorUnparserSuspendableOperation(
                    sepMtaAlignmentMaybe,
                    sep,
                    trd
                  )
                val suppressableSep = SuppressableSeparatorUnparser(sep, trd, suspendableOp)
                suppressableSep.unparse1(state)
                pendingSuppressibleOp = suspendableOp
              }
            case Postfix =>
              val suspendableOp =
                new SuppressableSeparatorUnparserSuspendableOperation(
                  sepMtaAlignmentMaybe,
                  sep,
                  trd
                )
              suspendableOp.captureDOSForStartOfSeparatedRegionBeforePostfixSeparator(state)
              pendingSuppressibleOp = suspendableOp
          }
      }
      wroteAny = true
    }

    /**
     * Completes whatever beforeSeparator started for a term. Handles ssp
     * Never (pendingPostfixSeparatorAtTerm, immediate write) and
     * AnyEmpty/TrailingEmpty(Strict) (pendingSuppressibleOp); exactly one
     * is ever set, matching beforeSeparator's own ssp branch.
     */
    def afterSeparator(): Unit = {
      if (pendingPostfixSeparatorAtTerm) {
        pendingPostfixSeparatorAtTerm = false
        unparseJustSeparator(state)
      }
      if (pendingSuppressibleOp ne null) {
        val op = pendingSuppressibleOp
        pendingSuppressibleOp = null
        ssp match {
          case AnyEmpty =>
            spos match {
              case Prefix | Infix =>
                op.captureStateAtEndOfPotentiallyZeroLengthRegionFollowingTheSeparator(state)
              case Postfix =>
                op.captureDOSForEndOfSeparatedRegionBeforePostfixSeparator(state)
                SuppressableSeparatorUnparser(sep, op.rd, op).unparse1(state)
                op.captureStateAtEndOfPotentiallyZeroLengthRegionFollowingTheSeparator(state)
            }
          case TrailingEmpty | TrailingEmptyStrict =>
            spos match {
              case Prefix | Infix =>
                trailingSuspendedOps += op
              case Postfix =>
                op.captureDOSForEndOfSeparatedRegionBeforePostfixSeparator(state)
                SuppressableSeparatorUnparser(sep, op.rd, op).unparse1(state)
                trailingSuspendedOps += op
            }
          case Never =>
            Assert.invariantFailed("pendingSuppressibleOp should never be set for ssp Never")
        }
      }
    }

    // The term produced zero occurrences (a different term's child took
    // this position, or build finished with nothing more coming); no
    // tree node was added, so position doesn't move, but it may still owe
    // separators, like an actual occurrence loop's own end-of-term handling.
    def zeroOccurrences(rep: RepeatingChildUnparser with Separated): Unit = {
      if (ssp eq Never) {
        writeExtraSeparatorsIfNeverSuppressed(rep, 0)
      } else {
        writePositionallyRequiredSepsIfSuppressed(rep, 0, stacksAlreadyPushed = false)
      }
    }

    // ssp TrailingEmpty(Strict): now that nothing at all remains in this
    // sequence, every deferred separator's "after" boundary is this exact
    // point. Resolve them all.
    def resolveTrailingSuspended(): Unit = {
      if ((ssp eq TrailingEmpty) || (ssp eq TrailingEmptyStrict)) {
        trailingSuspendedOps.foreach {
          _.captureStateAtEndOfPotentiallyZeroLengthRegionFollowingTheSeparator(state)
        }
        trailingSuspendedOps.clear()
      }
    }
  }

  /**
   * Walks childUnparsers positionally against an already-built
   * containerNode, writing each term's content/separator per spos; blocks
   * via childExistsOrFinal/awaitChild wherever a needed child isn't ready.
   * childIndexStack.top tracks position (an array is ONE slot).
   */
  override def writeContent(containerNode: DINode, state: UState): Unit = {
    val sharedCtx = state.sharedContext.get
    val complex = containerNode.asComplex
    val sepState = new SeparatorSuppressionState(state)

    var index = 0
    while (index < childUnparsers.length) {
      childUnparsers(index) match {
        case rep: RepeatingChildUnparser =>
          writeRepeatingTerm(rep, complex, sharedCtx, state, sepState)
        case cu =>
          writeRequiredTerm(cu, complex, sharedCtx, state, sepState)
      }
      index += 1
    }

    sepState.resolveTrailingSuspended()
  }

  /**
   * Writes an array/optional term: either its full occurrence loop (an
   * actual `DIArray`, or a scalar optional's single occurrence) or, if the
   * term produced no occurrences at all, its zero-occurrences separator
   * bookkeeping.
   */
  private def writeRepeatingTerm(
    rep: RepeatingChildUnparser,
    complex: DIComplex,
    sharedCtx: UnparseSharedContext,
    state: UState,
    sepState: SeparatorSuppressionState
  ): Unit = {
    val idx = state.childIndexStack.top.toInt
    if (sharedCtx.childExistsOrFinal(complex, idx)) {
      val next = complex.child(idx)
      if (next.erd eq rep.erd) {
        next match {
          case arrayNode: DIArray =>
            val repSep = rep.asInstanceOf[RepeatingChildUnparser with Separated]
            // A dfdl:occursIndex() expression in the occurrence's content
            // reads state.occursIndexStack.top, which must track it or
            // every occurrence would evaluate as if it were the first.
            state.pushOccurrenceIndices()
            try {
              var arrayOcc = 0
              while (sharedCtx.childExistsOrFinal(arrayNode, arrayOcc)) {
                val occNode = sharedCtx.awaitChild(arrayNode, arrayOcc)
                // Postfix's separator comes after this occurrence's
                // content; afterSeparator runs once it's written.
                sepState.beforeSeparator(
                  repSep.erd,
                  repSep.isKnownStaticallyNotToSuppressSeparator
                )
                repSep.childUnparser
                  .asInstanceOf[ElementUnparserBase]
                  .writeContent(occNode, state)
                sepState.afterSeparator()
                arrayNode.freeChildIfNoLongerNeeded(arrayOcc, state.releaseUnneededInfoset)
                arrayOcc += 1
                state.moveOverOneArrayIterationIndexOnly()
                state.moveOverOneOccursIndexOnly()
              }
              // this whole array term is done; it occupies exactly one
              // slot among containerNode's own children, regardless of
              // how many occurrences it held
              complex.freeChildIfNoLongerNeeded(idx, state.releaseUnneededInfoset)
              state.moveOverOneElementChildOnly()
              if (ssp eq Never) {
                sepState.writeExtraSeparatorsIfNeverSuppressed(repSep, arrayOcc)
              } else {
                sepState.writePositionallyRequiredSepsIfSuppressed(
                  repSep,
                  arrayOcc,
                  stacksAlreadyPushed = true
                )
              }
            } finally {
              state.popOccurrenceIndices()
            }
          case scalarOptional =>
            val repSep = rep.asInstanceOf[RepeatingChildUnparser with Separated]
            val readyChild = sharedCtx.awaitChild(complex, idx)
            // Same push as a true array's entry (see above); a
            // scalar optional is still a RepeatingChildUnparser (just
            // one that can never hold more than one occurrence).
            state.pushOccurrenceIndices()
            try {
              // Postfix's separator comes after this element's
              // content; afterSeparator runs once it's written.
              sepState.beforeSeparator(
                repSep.erd,
                repSep.isKnownStaticallyNotToSuppressSeparator
              )
              repSep.childUnparser
                .asInstanceOf[ElementUnparserBase]
                .writeContent(readyChild, state)
              sepState.afterSeparator()
              state.moveOverOneElementChildOnly()
              complex.freeChildIfNoLongerNeeded(idx, state.releaseUnneededInfoset)
              // Matches the DIArray arm's own per-occurrence advancement:
              // occursIndexStack.top must reflect the next (missing)
              // occurrence's index before writePositionallyRequiredSepsIfSuppressed's
              // loop runs, or its first separator would see index 1, not 2.
              state.moveOverOneArrayIterationIndexOnly()
              state.moveOverOneOccursIndexOnly()
              if (ssp eq Never) {
                sepState.writeExtraSeparatorsIfNeverSuppressed(repSep, 1)
              } else {
                sepState.writePositionallyRequiredSepsIfSuppressed(
                  repSep,
                  1,
                  stacksAlreadyPushed = true
                )
              }
            } finally {
              state.popOccurrenceIndices()
            }
        }
      } else {
        // this term produced zero occurrences: a different term's
        // child appeared in this tree position instead
        sepState.zeroOccurrences(rep.asInstanceOf[RepeatingChildUnparser with Separated])
      }
    } else {
      // no more children will ever come; this optional/array term is absent
      sepState.zeroOccurrences(rep.asInstanceOf[RepeatingChildUnparser with Separated])
    }
  }

  /**
   * Writes a required term (exactly one occurrence): a simple/complex
   * element, a nested bare group, or a statement-only term with no tree
   * child of its own. Includes its own separator bookkeeping if represented.
   */
  private def writeRequiredTerm(
    cu: SequenceChildUnparser with Separated,
    complex: DIComplex,
    sharedCtx: UnparseSharedContext,
    state: UState,
    sepState: SeparatorSuppressionState
  ): Unit = {
    cu.childUnparser match {
      case nvi: NewVariableInstanceStartUnparser =>
        nvi.unparse1(state)
      case sv: SetVariableUnparser =>
        sv.unparse1(state)
      case nvi: NewVariableInstanceEndUnparser =>
        nvi.unparse1(state)
      case align: AlignmentPrimUnparser =>
        // Padding has no non-idempotent side effect (unlike assert/discriminator
        // below), so re-running it here is required, not forbidden: build's
        // own recursion only ran it against build's no-op-sink DOS. Always
        // represented, and never suppressible since its content is deterministic.
        if (cu.trd.isRepresented) {
          sepState.beforeSeparator(cu.trd, staticallyNotSuppressible = true)
        }
        align.unparse1(state)
        if (cu.trd.isRepresented) {
          sepState.afterSeparator()
        }
      case statementOnly if !statementOnly.isInstanceOf[WriteUnparser] =>
        // No tree child, so no separator, and nothing to wait on.
        ()
      case _ =>
        val idx = state.childIndexStack.top.toInt
        // A nested bare group (e.g. a choice) may resolve to a branch with no
        // infoset footprint at all; once build is done and no child showed up,
        // let the group's own dispatch decide. A plain element term always
        // needs an actual child, so it always waits unconditionally.
        val isGroupTerm = !cu.childUnparser.isInstanceOf[ElementUnparserBase] &&
          cu.childUnparser.isInstanceOf[WriteUnparser]
        if (isGroupTerm) {
          if (sharedCtx.childExistsOrFinal(complex, idx)) {
            sharedCtx.awaitChild(complex, idx)
          }
        } else {
          sharedCtx.awaitChild(complex, idx)
        }
        // A non-represented term gets no separator, so wroteAny must not
        // flip true. Suppression only applies to terms whose presence is
        // uncertain (array/optional, or a bare group not statically known
        // to need it), never to a plain element's own content length.
        val useSuppression = isGroupTerm && !cu.isKnownStaticallyNotToSuppressSeparator
        if (cu.trd.isRepresented) {
          sepState.beforeSeparator(cu.trd, staticallyNotSuppressible = !useSuppression)
        }
        cu.childUnparser match {
          case elemUnp: ElementUnparserBase =>
            elemUnp.writeContent(complex.child(idx), state)
            sepState.afterSeparator()
            state.moveOverOneElementChildOnly()
            complex.freeChildIfNoLongerNeeded(idx, state.releaseUnneededInfoset)
          case wu: WriteUnparser =>
            wu.writeContent(complex, state)
            sepState.afterSeparator()
          case other =>
            Assert.usageError(s"unhandled sequence term unparser type: $other")
        }
    }
  }

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
          state.pushOccurrenceIndices()
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
          state.popOccurrenceIndices()
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
          state.pushOccurrenceIndices()
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

          state.popOccurrenceIndices()
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
