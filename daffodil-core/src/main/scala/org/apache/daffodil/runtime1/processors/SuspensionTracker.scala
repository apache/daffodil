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

package org.apache.daffodil.runtime1.processors

import scala.collection.mutable
import scala.collection.mutable.Queue

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.runtime1.dsom.RuntimeSchemaDefinitionError

/**
 * A suspension lives in exactly one of three places at a time:
 *
 *   - suspensionsYoung / suspensionsOld: the normal rotation. Old is swept
 *     less often than young; a suspension still not done after a young
 *     sweep moves into old via foldYoungIntoOld, never dropped.
 *   - suspensionsParked: has a targeted wake-up registered (isParked), so
 *     it's pulled out of the rotation entirely instead of being retried
 *     on every sweep. Only a real notification (moveParkedToYoung) or one
 *     of the two force-retry paths below (evalParkedSuspensions,
 *     requireFinal) ever moves it back out.
 *
 * A suspension only ever becomes isParked as a side effect of blocking and
 * registering a wake-up inside doTask, either its first attempt (run,
 * before it's even tracked here) or a later retry (runSuspension). Because
 * of the first case, trackSuspension can hand this tracker a suspension
 * that's already isParked before it's ever been dequeued once.
 * evalSuspensionQueue's periodic sweep calls (skipWaiters=true) divert a
 * suspension to suspensionsParked the moment it's isParked, whether
 * already true at dequeue or just became so from running it, so it's
 * never left sitting in young/old waiting for a later dequeue to notice.
 *
 * The two force-retry paths (evalParkedSuspensions, requireFinal, both
 * passing evalSuspensionQueue skipWaiters=false) deliberately do NOT
 * divert this way: they exist specifically to let one suspension's
 * progress unblock another's within the same pass, which only works if a
 * freshly re-parked suspension stays in the queue and keeps getting
 * retried rather than being pulled out mid-pass. Their own code sorts by
 * isParked only once the whole pass is done instead.
 */
class SuspensionTracker(suspensionWaitYoung: Int, suspensionWaitOld: Int) {

  private val suspensionsYoung = new Queue[Suspension]
  private val suspensionsOld = new Queue[Suspension]
  // Iteration order (retry order, and SuspensionDeadlockException's
  // reported order) must stay insertion-ordered and reproducible across
  // runs, not depend on Suspension's identity hashcodes.
  private val suspensionsParked = new mutable.LinkedHashSet[Suspension]

  /**
   * Every still-tracked, not-yet-done suspension, for debugging use
   * only: combining three buckets into one Seq allocates and copies, so
   * nothing in the unparse hot path should call this. Must include
   * suspensionsParked, or a parked suspension looks like resolved progress.
   */
  def suspensions: Seq[Suspension] =
    suspensionsYoung.toSeq ++ suspensionsOld.toSeq ++ suspensionsParked.toSeq

  private var count: Int = 0

  private var suspensionStatTracked: Int = 0
  private var suspensionStatRuns: Int = 0

  def trackSuspension(s: Suspension): Unit = {
    suspensionsYoung.enqueue(s)
    suspensionStatTracked += 1
  }

  /** Attempts to evaluate suspensions on a throttled schedule; see the class comment above. */
  def evalSuspensions(): Unit = {
    if (count % suspensionWaitOld == 0) {
      evalSuspensionQueue(suspensionsOld, skipWaiters = true)
      evalParkedSuspensions()
    }
    if (count % suspensionWaitYoung == 0) {
      evalSuspensionQueue(suspensionsYoung, skipWaiters = true)
      foldYoungIntoOld()
    }

    if (count == suspensionWaitOld) {
      count = 0
    } else {
      count += 1
    }
  }

  // Some suspensions only resolve via a real, unconditional retry, not
  // their own wake-up firing; bounds how long one waits to requireFinal.
  // Doesn't drain suspensionsParked up front (a suspension staying
  // parked unchanged costs no mutation), so guarded against reentry.
  private var evaluatingParked: Boolean = false

  private def evalParkedSuspensions(): Unit = {
    if (suspensionsParked.isEmpty || evaluatingParked) {
      return
    }
    evaluatingParked = true
    try {
      val toRetry = new Queue[Suspension]
      toRetry ++= suspensionsParked
      // Suspensions that resolve during this pass, collected via
      // evalSuspensionQueue's isDone check below: empty in the common
      // case, unlike a before/after Set snapshot and diff would cost.
      var resolvedThisPass: List[Suspension] = Nil
      reentrantlyNotified = new mutable.HashSet[Suspension]
      try {
        // No deregister here: block()'s maybeRegisterWaiterFor
        // reconciles registration per retry. Safe since skipWaiters=false
        // below always attempts every member regardless of isParked,
        // unlike the periodic sweep, so there's no silent re-parking risk.
        toRetry.foreach(_.markMidForcedRetry())
        evalSuspensionQueue(toRetry, skipWaiters = false, onDone = s => resolvedThisPass ::= s)
      } finally {
        val notified = reentrantlyNotified
        reentrantlyNotified = null
        // Unconditional regardless of how far the marking/retry got:
        // unmarking an entry that was never actually marked is a
        // harmless no-op (e.g. an exception partway through above).
        resolvedThisPass.foreach { s =>
          s.clearMidForcedRetry()
          suspensionsParked.remove(s)
        }
        // Reentrant notify takes priority; still-isParked stays parked
        // (already sitting there untouched); everything else goes back
        // to the normal rotation for real retries, not parked forever.
        toRetry.foreach { s =>
          s.clearMidForcedRetry()
          if (notified.contains(s)) {
            suspensionsParked.remove(s)
            suspensionsYoung.enqueue(s)
          } else if (s.isParked) {
            // no-op
          } else {
            suspensionsParked.remove(s)
            suspensionsOld.enqueue(s)
          }
        }
      }
    } finally {
      evaluatingParked = false
    }
  }

  // Non-null only while evalParkedSuspensions' own retry loop is in
  // flight; see the reentrant-notify comment there.
  private var reentrantlyNotified: mutable.HashSet[Suspension] = null

  private def foldParkedIntoOld(): Unit = {
    // requireFinal's retry doesn't go through block() first, so
    // deregister here instead.
    suspensionsParked.foreach { s =>
      s.clearAllRegistrations()
      suspensionsOld.enqueue(s)
    }
    suspensionsParked.clear()
  }

  private def foldYoungIntoOld(): Unit = {
    while (suspensionsYoung.nonEmpty) {
      suspensionsOld.enqueue(suspensionsYoung.dequeue())
    }
  }

  /**
   * Called once a suspension's targeted wake-up actually fires. A
   * suspension mid-retry inside evalParkedSuspensions' own pass is
   * recognized via isMidForcedRetry, since suspensionsParked membership
   * alone doesn't distinguish it; anything else moves if found parked.
   */
  def moveParkedToYoung(s: Suspension): Unit = {
    if (s.isMidForcedRetry) {
      if (reentrantlyNotified ne null) {
        reentrantlyNotified.add(s)
      }
    } else if (suspensionsParked.remove(s)) {
      suspensionsYoung.enqueue(s)
    }
  }

  /**
   * Evaluates all suspensions until deadlocked, folding parked and young
   * into old for one final unconditional attempt each; other unrelated
   * preconditions may have resolved by now even if this suspension's own
   * registered wake-up never fired. Still-stuck ones are reported deadlocked.
   */
  def requireFinal(): Unit = {
    foldParkedIntoOld()
    foldYoungIntoOld()

    evalSuspensionQueue(suspensionsOld, skipWaiters = false)

    Assert.invariant(
      suspensionsOld.length != 1,
      "Single suspended expression making no forward progress. " + suspensionsOld(0)
    )

    if (suspensionsOld.nonEmpty) {
      throw new SuspensionDeadlockException(suspensionsOld.toSeq)
    }

    Logger.log.debug(
      f"Suspension runs/tracked: ${suspensionStatRuns}%d/${suspensionStatTracked}%d (${(suspensionStatRuns.toFloat / suspensionStatTracked) * 100}%.2f%%)"
    )
  }

  /**
   * Repeatedly attempts suspensions on queue until no progress is made;
   * still-blocked ones go back on the queue. skipWaiters distinguishes
   * the periodic sweep's isParked-diversion behavior from the force-retry
   * paths'; see the class comment above. onDone is invoked exactly once
   * for a suspension that resolves during this call, letting a caller
   * collect exactly those cheaply.
   */
  private def evalSuspensionQueue(
    queue: Queue[Suspension],
    skipWaiters: Boolean,
    onDone: Suspension => Unit = SuspensionTracker.NoOpOnDone
  ): Unit = {
    // A plain local, not captured by parkIfNeeded below: the caller resets
    // it on a park, keeping this var unboxed (a var mutated from inside a
    // nested closure forces the compiler to heap-allocate it as an IntRef).
    var countOfNotMakingProgress = 0
    // Diverts s to suspensionsParked if skipWaiters applies and s is
    // isParked; returns whether it did so.
    def parkIfNeeded(s: Suspension): Boolean = {
      val park = skipWaiters && s.isParked
      if (park) {
        suspensionsParked.add(s)
      }
      park
    }
    while (!queue.isEmpty && countOfNotMakingProgress < queue.length) {
      val s = queue.dequeue()
      // Nothing outside this function's own runSuspension call below ever
      // sets isDone, so a suspension just dequeued from a queue this
      // function itself maintains can never already be done.
      Assert.invariant(!s.isDone)
      if (parkIfNeeded(s)) {
        countOfNotMakingProgress = 0
      } else {
        suspensionStatRuns += 1
        s.runSuspension()
        if (s.isDone) {
          onDone(s)
          countOfNotMakingProgress = 0
        } else if (parkIfNeeded(s)) {
          countOfNotMakingProgress = 0
        } else {
          queue.enqueue(s)
          if (s.isMakingProgress) {
            countOfNotMakingProgress = 0
          } else {
            countOfNotMakingProgress += 1
          }
        }
      }
    }
  }

}

object SuspensionTracker {
  // A stable value rather than a literal `_ => ()` at each unmodified
  // evalSuspensionQueue call site, so those callers don't allocate a
  // fresh closure per call just to pass the default.
  private val NoOpOnDone: Suspension => Unit = _ => ()
}

class SuspensionDeadlockException(suspExprs: Seq[Suspension])
  extends RuntimeSchemaDefinitionError(
    suspExprs(0).rd.schemaFileLocation,
    "Expressions/Unparsers are circularly deadlocked (mutually defined):\n%s",
    suspExprs
      .groupBy {
        _.rd
      }
      .view
      .mapValues {
        _(0)
      }
      .values
      .mkString(" - ", "\n - ", "")
  )
