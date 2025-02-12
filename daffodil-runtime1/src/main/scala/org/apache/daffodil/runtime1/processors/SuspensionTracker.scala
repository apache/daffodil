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

import scala.collection.mutable.Queue

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.runtime1.dsom.RuntimeSchemaDefinitionError

class SuspensionTracker(suspensionWaitYoung: Int, suspensionWaitOld: Int) {

  private val suspensionsYoung = new Queue[Suspension]
  private val suspensionsOld = new Queue[Suspension]

  def suspensions: Seq[Suspension] = suspensionsYoung.toSeq ++ suspensionsOld.toSeq

  private var count: Int = 0

  private var suspensionStatTracked: Int = 0
  private var suspensionStatRuns: Int = 0

  def trackSuspension(s: Suspension): Unit = {
    suspensionsYoung.enqueue(s)
    suspensionStatTracked += 1
  }

  /**
   * Attempts to evaluate suspensions. Old suspensions are evaluated less
   * frequently than young suspensions. Any young suspensions that fail to
   * evaluate are moved to the old suspensions list. If we evaluate old
   * suspensions, we attempt to evaluate them first, with the hope that their
   * resolution might make the young suspensions more likely to evaluate.
   */
  def evalSuspensions(): Unit = {
    if (count % suspensionWaitOld == 0) {
      evalSuspensionQueue(suspensionsOld)
    }
    if (count % suspensionWaitYoung == 0) {
      evalSuspensionQueue(suspensionsYoung)
      while (suspensionsYoung.nonEmpty) {
        suspensionsOld.enqueue(suspensionsYoung.dequeue())
      }
    }

    if (count == suspensionWaitOld) {
      count = 0
    } else {
      count += 1
    }
  }

  /**
   * Evaluates all suspensions until either they are all evaluated or a
   * deadlock is detected. This moves all young suspensions to the old queue,
   * and evaluates all old suspensions. If the old queue is non-empty, that
   * means some suspensions are blocked, likely due to a circular deadlock, and
   * we output diagnostics.
   */
  def requireFinal(): Unit = {
    while (suspensionsYoung.nonEmpty) {
      suspensionsOld.enqueue(suspensionsYoung.dequeue())
    }

    evalSuspensionQueue(suspensionsOld)

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
   * Attempt to evaluate suspensions on the provie queue. Keep repeating the
   * evaluates as long as some progress is being made. Suspensions that
   * evaluate sucessfully are removed from the queue. Once suspensions make no
   * further progress and are all blocked, we return. Blocked suspensions put
   * back on the same queue.
   */
  private def evalSuspensionQueue(queue: Queue[Suspension]): Unit = {
    var countOfNotMakingProgress = 0
    while (!queue.isEmpty && countOfNotMakingProgress < queue.length) {
      val s = queue.dequeue()
      suspensionStatRuns += 1
      s.runSuspension()
      if (!s.isDone) queue.enqueue(s)
      if (s.isDone || s.isMakingProgress) {
        countOfNotMakingProgress = 0
      } else {
        countOfNotMakingProgress += 1
      }
    }
  }

}

class SuspensionDeadlockException(suspExprs: Seq[Suspension])
  extends RuntimeSchemaDefinitionError(
    suspExprs(0).rd.schemaFileLocation,
    "Expressions/Unparsers are circularly deadlocked (mutually defined):\n%s",
    suspExprs
      .groupBy {
        _.rd
      }
      .values
      .map {
        _(0)
      }
      .mkString(" - ", "\n - ", "")
  )
