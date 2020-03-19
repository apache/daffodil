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

package org.apache.daffodil.processors

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.util.Misc
import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.api.ThinDiagnostic
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.infoset.RetryableException

/**
 * SuspendableOperation is used for suspending and retrying things that aren't
 * expressions. Example is an alignmentFill unparser. Until we know the absolute
 * start bit positon, we can't lay down alignment fill bits.
 *
 * This has to be suspended and retried later, but it's not an expression
 * being evaluated that has forward references.
 */
trait SuspendableOperation
  extends Suspension {

  override def rd: TermRuntimeData

  override def toString = "%s for %s".format(Misc.getNameFromClass(this), rd.diagnosticDebugName)

  /**
   * Returns true if continuation can be run.
   *
   * If false, the operation will be suspended, and resumed
   * later. Once test is true, then the continuation will be run.
   */
  protected def test(ustate: UState): Boolean

  /**
   * The operation we want to do only if the test is true.
   */
  protected def continuation(ustate: UState): Unit

  override protected final def doTask(ustate: UState) {
    if (isBlocked) {
      setUnblocked()
      log(LogLevel.Debug, "retrying %s", this)
    }
    while (!isDone && !isBlocked) {
      try {
        val tst = test(ustate)
        if (tst) {
          log(LogLevel.Debug, "test() of %s %s passed", this, tst)
          setDone
        } else {
          log(LogLevel.Debug, "test() of %s %s failed", this, tst)
          val nodeOpt = if (ustate.currentInfosetNodeMaybe.isDefined) ustate.currentInfosetNodeMaybe.get else "No Node"
          block(nodeOpt, ustate.dataOutputStream, 0, this)
        }
      } catch {
        case e: RetryableException => {
          log(LogLevel.Debug, "test() of %s threw %s", this, e)
          val nodeOpt = if (ustate.currentInfosetNodeMaybe.isDefined) ustate.currentInfosetNodeMaybe.get else "No Node"
          block(nodeOpt, ustate.dataOutputStream, 0, e)
        }
      }
      if (!isDone) {
        Assert.invariant(isBlocked)
      }
    }
    if (isDone) {
      log(LogLevel.Debug, "continuation() of %s", this)
      continuation(ustate)
      log(LogLevel.Debug, "continuation() of %s done!", this)

    }
  }
}

class SuspendableOperationException(m: String)
  extends ThinDiagnostic(Nope, Nope, Nope, Maybe(m)) {
  override def isError = true
  override def modeName = "Unparse"
}
