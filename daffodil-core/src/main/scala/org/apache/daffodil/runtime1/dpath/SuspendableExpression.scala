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

package org.apache.daffodil.runtime1.dpath

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.runtime1.dsom.CompiledExpression
import org.apache.daffodil.runtime1.infoset.DataValue
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitiveNullable
import org.apache.daffodil.runtime1.processors.RuntimeData
import org.apache.daffodil.runtime1.processors.Suspension
import org.apache.daffodil.runtime1.processors.unparsers.UState

/**
 * Base for unparse-time expression evaluation that can have forward reference.
 * There are only two such cases, which is dfdl:outputValueCalc, and
 * dfdl:setVariable expressions (which variables are in-turn used by
 * dfdl:outputValueCalc.
 */
trait SuspendableExpression extends Suspension {

  override val isReadOnly = true

  protected def expr: CompiledExpression[AnyRef]

  override def toString =
    "SuspendableExpression(" + rd.diagnosticDebugName + ", expr=" + expr.prettyExpr + ")"

  protected def processExpressionResult(ustate: UState, v: DataValuePrimitive): Unit

  override protected final def doTask(ustate: UState): Unit = {
    var v: DataValuePrimitiveNullable = DataValue.NoValue
    if (!isBlocked) {
      Logger.log.debug(
        s"Starting suspendable expression for ${rd.diagnosticDebugName}, expr=${expr.prettyExpr}"
      )
    } else {
      this.setUnblocked()
      Logger.log.debug(
        s"Retrying suspendable expression for ${rd.diagnosticDebugName}, expr=${expr.prettyExpr}"
      )
    }
    while (v.isEmpty && !this.isBlocked) {
      v = DataValue.unsafeFromMaybeAnyRef(expr.evaluateForwardReferencing(ustate, this))
      if (v.isEmpty) {
        Assert.invariant(this.isBlocked)
        Logger.log.debug(
          s"UnparserBlocking suspendable expression for ${rd.diagnosticDebugName}, expr=${expr.prettyExpr}"
        )
      } else {
        Assert.invariant(this.isDone)
        Assert.invariant(ustate.currentInfosetNodeMaybe.isDefined)
        Logger.log.debug(
          s"Completed suspendable expression for ${rd.diagnosticDebugName}, expr=$expr.prettyExpr{}"
        )
        processExpressionResult(ustate, v.getNonNullable)
      }
    }
  }

}

/**
 * Implemented by a shared unparser that creates suspendable expressions whose
 * per-call state is created by newSuspensionState, and passed to the
 * processing of the expression result.
 */
trait StatefulDelegatedSuspendableExpression[S] {
  def rd: RuntimeData

  def expr: CompiledExpression[AnyRef]

  def newSuspensionState(): S

  def processExpressionResult(ustate: UState, v: DataValuePrimitive, state: S): Unit
}

/**
 * Implemented by a shared unparser that creates suspendable expressions that
 * have no per-call state.
 */
trait DelegatedSuspendableExpression extends StatefulDelegatedSuspendableExpression[Unit] {

  def processExpressionResult(ustate: UState, v: DataValuePrimitive): Unit

  final override def newSuspensionState(): Unit = ()

  final override def processExpressionResult(
    ustate: UState,
    v: DataValuePrimitive,
    state: Unit
  ): Unit = processExpressionResult(ustate, v)
}

/**
 * An expression that does not write to the data output stream, so it has a
 * known length of zero.
 */
class StatefulForwardingSuspendableExpression[S](
  host: StatefulDelegatedSuspendableExpression[S]
) extends SuspendableExpression {

  override val rd = host.rd

  override protected val expr = host.expr

  val state: S = host.newSuspensionState()

  override protected def processExpressionResult(ustate: UState, v: DataValuePrimitive): Unit =
    host.processExpressionResult(ustate, v, state)

  override protected def maybeKnownLengthInBits(ustate: UState): MaybeULong = MaybeULong(0)
}

class ForwardingSuspendableExpression(host: DelegatedSuspendableExpression)
  extends StatefulForwardingSuspendableExpression[Unit](host)
