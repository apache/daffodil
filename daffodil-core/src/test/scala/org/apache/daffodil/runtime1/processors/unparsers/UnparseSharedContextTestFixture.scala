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

import org.apache.daffodil.runtime1.infoset.DIDocument
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.SuspensionTracker
import org.apache.daffodil.unparsers.runtime1.ElementUnparserBase

/**
 * Shared BuildState construction for tests, mirroring production's `* 2`
 * doubling of tunable-derived suspension-wait thresholds (default only).
 */
object UnparseSharedContextTestFixture {
  def build(dp: DataProcessor, prefetchLimit: Long)(
    suspensionWaitYoung: Int = dp.tunables.unparseSuspensionWaitYoung * 2,
    suspensionWaitOld: Int = dp.tunables.unparseSuspensionWaitOld * 2
  ): UnparseSharedContext = {
    new UnparseSharedContext(
      new SuspensionTracker(suspensionWaitYoung, suspensionWaitOld),
      dp,
      dp.tunables,
      prefetchLimit
    )
  }

  /** Wires a BuildCoroutine/WriteCoroutine pair onto sharedCtx, mirroring
    * production's WriteCoroutine, so mid-recursion resumeWrite calls do
    * something; caller must still perform the final handoff afterward.
    */
  def wireCoroutines(
    sharedCtx: UnparseSharedContext,
    documentElement: DIDocument,
    rootUnparser: ElementUnparserBase,
    writeState: UState
  ): Unit = {
    val buildCoroutine = new BuildCoroutine()
    val writeCoroutine = new WriteCoroutine({ (wc, firstSignal) =>
      try {
        sharedCtx.observeBuildSignal(firstSignal)
        try {
          val rootNode = sharedCtx.awaitChild(documentElement, 0)
          rootUnparser.writeContent(rootNode, writeState)
        } catch {
          case _: AwaitChildStalledException =>
        }
        wc.resumeFinal(buildCoroutine, WriteDone(None))
      } catch {
        case t: Throwable =>
          wc.resumeFinal(buildCoroutine, WriteDone(Some(t)))
      }
    })
    sharedCtx.setCoroutines(buildCoroutine, writeCoroutine)
  }
}
