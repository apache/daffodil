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

package org.apache.daffodil.runtime1.processors.unparsers

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.*
import org.apache.daffodil.runtime1.infoset.DINode
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.SuspensionTracker

/**
 * What build and write genuinely need to share by reference: the
 * `SuspensionTracker` (one queue; build opportunistically resolves
 * write-created suspensions early against the tree it has already
 * built, and write drains whatever remains at the end).
 *
 * Also owns the lead counter: how far build is ahead of write,
 * incremented once per node build constructs and decremented once per
 * node write finishes. Once `leadExceedsPrefetchLimit` or the
 * pending-suspension backlog exceeds `pendingSuspensionTripLimit`
 * (below), build must resume write before continuing its own
 * recursion, bounding how far ahead it may run.
 */
final class UnparseSharedContext(
  val suspensionTracker: SuspensionTracker,
  val dataProc: DataProcessor,
  val tunable: DaffodilTunables,
  val prefetchLimit: Long
) {
  private var buildLead: Long = 0

  def incrementLead(): Unit = buildLead += 1

  def decrementLead(): Unit = {
    buildLead -= 1
    Assert.invariant(buildLead >= 0)
  }

  def currentLead: Long = buildLead

  def leadExceedsPrefetchLimit: Boolean = buildLead > prefetchLimit

  /**
   * A second, independent limit on how far build may run ahead of write,
   * alongside prefetchLimit: a suspension can be created without moving
   * the lead counter, so the lead alone doesn't bound how many pile up
   * pending.
   */
  def pendingSuspensionTripLimit: Long = tunable.unparsePendingSuspensionTripLimit

  private var buildCoroutine_ : BuildCoroutine = null
  private var writeCoroutine_ : WriteCoroutine = null

  def setCoroutines(bc: BuildCoroutine, wc: WriteCoroutine): Unit = {
    buildCoroutine_ = bc
    writeCoroutine_ = wc
  }
  def buildCoroutine: BuildCoroutine = buildCoroutine_
  def writeCoroutine: WriteCoroutine = writeCoroutine_

  private var recordedWriteDone: Maybe[WriteDone] = Nope

  /**
   * Resumes writeCoroutine with `signal`, unless write already finished
   * (a second resume would park forever), in which case the cached
   * WriteDone is returned instead. Always go through this, never resume
   * writeCoroutine directly.
   */
  def resumeWrite(signal: BuildSignal): WriteSignal = {
    if (recordedWriteDone.isDefined) {
      recordedWriteDone.get
    } else {
      val result = buildCoroutine.resume(writeCoroutine, signal)
      result match {
        case wd: WriteDone => recordedWriteDone = One(wd)
        case _ =>
      }
      result
    }
  }

  /**
   * Records write's own result without ever resuming writeCoroutine,
   * for when build never needed write started at all and it ran
   * directly on build's own thread instead. Still must be recorded
   * here so a later abortWrite/resumeWrite sees write as finished.
   */
  def recordWriteDone(wd: WriteDone): Unit = {
    recordedWriteDone = One(wd)
  }

  /**
   * Wakes write's thread (spawning it first if never started) after
   * build's own thread failed before reaching the normal resumeWrite
   * handoff, so it can clean up instead of hanging forever. Blocks until
   * that cleanup actually finishes, not just until it starts, so the
   * caller never reports an error result while write's cleanup is still
   * running on another thread. A no-op if write already finished, or if
   * setCoroutines was never reached.
   */
  def abortWrite(): Unit = {
    if (recordedWriteDone.isEmpty && writeCoroutine_ != null) {
      buildCoroutine.resume(writeCoroutine, BuildAborted)
    }
  }

  /**
   * True if `child` may safely be written now: complex/array existing is
   * enough; simple needs a value, except hidden/nilled elements (never
   * given one) and OVC (deferred via its own Suspension; must not block,
   * or an OVC depending on a later sibling's write-time property would deadlock).
   */
  private def isChildReady(child: DINode): Boolean = {
    if (child.isSimple) {
      val s = child.asSimple
      child.isHidden || s.isNilled || s.hasValue || s.erd.dpathElementCompileInfo.isOutputValueCalc
    } else {
      // complex/array, hidden or not; existing is enough
      true
    }
  }

  /**
   * True once build's recursion has fully returned. Sticky: once true,
   * awaitChild must never again resume buildCoroutine; build's thread has
   * moved on to waiting for write's ultimate WriteDone, not another
   * WriteNeedsMore cycle, so this can't be a per-call local flag.
   */
  private var buildFinished: Boolean = false

  /**
   * Records a signal write's coroutine received, without itself resuming
   * anyone. Needed for the signal that STARTS write's thread, which can
   * legitimately already be BuildFinished; must be recorded before any
   * awaitChild call, or tryUnblockWrite would wrongly resume build again.
   */
  def observeBuildSignal(signal: BuildSignal): Unit = {
    if (signal == BuildFinished) buildFinished = true
    else if (signal == BuildAborted) throw new BuildAbortedException
  }

  /**
   * One attempt at unblocking write: resumes build if not finished yet,
   * else retries suspensions directly. False means no progress was made;
   * callers must throw AwaitChildStalledException rather than loop again,
   * deferring to evalSuspensions(isFinal = true) for the real diagnosis.
   */
  private def tryUnblockWrite(): Boolean = {
    if (!buildFinished) {
      observeBuildSignal(writeCoroutine.resume(buildCoroutine, WriteNeedsMore))
      true
    } else {
      val before = suspensionTracker.suspensions.length
      suspensionTracker.evalSuspensionsUnthrottled()
      suspensionTracker.suspensions.length < before
    }
  }

  /**
   * Blocks (parks write's thread) until `parent.child(index)` both exists
   * and is ready (isChildReady above), throwing AwaitChildStalledException
   * once tryUnblockWrite reports no further progress is possible.
   */
  def awaitChild(parent: DINode, index: Int): DINode = {
    while (index >= parent.numChildren || !isChildReady(parent.child(index))) {
      if (!tryUnblockWrite()) throw new AwaitChildStalledException
    }
    parent.child(index)
  }

  /**
   * Blocks until `parent.child(index)` exists, or `parent.isFinal` with no
   * child there (none ever will be); for callers asking "is there more
   * data at all" rather than awaiting a child known to be coming. A true
   * result may still need awaitChild afterward for value readiness.
   */
  def childExistsOrFinal(parent: DINode, index: Int): Boolean = {
    while (index >= parent.numChildren) {
      if (parent.isFinal) return false
      if (!tryUnblockWrite()) throw new AwaitChildStalledException
    }
    true
  }
}

/**
 * Thrown when write can make no further progress after build has
 * finished and an unthrottled suspension retry made no headway. Caught
 * only by the top-level coroutine driver, which still runs its normal
 * finalization (the genuine, diagnostic-producing final suspension
 * drain) rather than treating this as the final outcome itself.
 */
final class AwaitChildStalledException extends Exception

/**
 * Thrown the moment write's thread observes that build's own thread
 * failed with an exception before reaching its normal completion, either
 * as the very first signal write's thread ever receives, or as the
 * result of a resume from deep inside a block on a pending child. Caught
 * only by the top-level coroutine driver, which skips its own normal
 * finalization entirely (those invariants and the final suspension drain
 * assume a consistently, fully-built tree that a genuine abort may not
 * have produced) and just cleans up write's own resources; build's own
 * exception, not this one, is what actually gets reported.
 */
final class BuildAbortedException extends Exception
