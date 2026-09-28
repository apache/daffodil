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

import org.apache.daffodil.runtime1.infoset.DINode
import org.apache.daffodil.runtime1.processors.unparsers.*

/**
 * Write-side dispatch for the build/write-prefetch unparse path, by
 * ordinary recursive calls, using `UnparseSharedContext.awaitChild` to
 * block (park write's coroutine thread, see `BuildWriteCoroutines.scala`)
 * wherever a needed child doesn't yet exist or isn't ready.
 */
trait WriteUnparser {

  // Writes containerNode's content via direct recursive calls on write's
  // own coroutine thread - "where we are" is just the JVM call stack, not
  // a return-value state machine. May block (via awaitChild) until a
  // needed child exists and is ready, then resumes where it left off.
  def writeContent(containerNode: DINode, state: UState): Unit

  // Shared push-once/pop-once skeleton: setup runs before recursing into
  // bodyUnparser (dispatched to writeContent if it's a WriteUnparser, else
  // plain unparse1), teardown runs once that call returns with setup's
  // result (e.g. threading a detached element from setup to teardown).
  protected def writeWithPushPop[A](
    containerNode: DINode,
    bodyUnparser: Unparser,
    state: UState,
    setup: UState => A,
    teardown: (UState, A) => Unit
  ): Unit = {
    val setupResult = setup(state)
    // Unlike single-pass unparse(), a stall here is caught higher up and
    // followed by finishWriteSide's invariant checks against this same
    // state, so teardown must still run, or those checks fail for an
    // unrelated reason.
    try {
      bodyUnparser match {
        case wu: WriteUnparser => wu.writeContent(containerNode, state)
        case _ => bodyUnparser.unparse1(state)
      }
    } finally {
      teardown(state, setupResult)
    }
  }

  /**
   * `writeWithPushPop` for a combinator with nothing to push or pop; just
   * the dispatch-to-`writeContent`-or-`unparse1` part.
   */
  protected def writeWithPushPop(
    containerNode: DINode,
    bodyUnparser: Unparser,
    state: UState
  ): Unit =
    writeWithPushPop(
      containerNode,
      bodyUnparser,
      state,
      (_: UState) => (),
      (_: UState, _: Unit) => ()
    )
}
