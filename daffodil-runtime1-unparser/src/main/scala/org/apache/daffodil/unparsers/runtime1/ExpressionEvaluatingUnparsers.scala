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

import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.runtime1.dpath.SuspendableExpression
import org.apache.daffodil.runtime1.dsom.CompiledExpression
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.processors.NonTermRuntimeData
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.VariableInProcess
import org.apache.daffodil.runtime1.processors.VariableInstance
import org.apache.daffodil.runtime1.processors.VariableRuntimeData
import org.apache.daffodil.runtime1.processors.unparsers._

final class SetVariableSuspendableExpression(
  override val expr: CompiledExpression[AnyRef],
  override val rd: VariableRuntimeData,
  referencingContext: NonTermRuntimeData
) extends SuspendableExpression {

  override protected def processExpressionResult(
    ustate: UState,
    v: DataValuePrimitive
  ): Unit = {
    ustate.setVariable(rd, v, referencingContext)
  }

  override protected def maybeKnownLengthInBits(ustate: UState) = MaybeULong(0)
}

/**
 * Used when unparsing to evaluate dfdl:setVariable statements.
 *
 * TODO: Possible bug. This will allow expressions to forward reference, even
 * when the variables are being referenced from expressions that are NOT
 * allowed to forward reference - e.g., property value expressions such
 * as delimiters and byte order.
 *
 * This forward suspension is only supposed to be allowed for dfdl:outputValueCalc.
 */
final class SetVariableUnparser(
  val expr: CompiledExpression[AnyRef],
  override val context: VariableRuntimeData,
  referencingContext: NonTermRuntimeData
) extends PrimUnparserNoData {

  override def runtimeDependencies = Vector()

  override def childProcessors = Vector()

  def suspendableExpression =
    new SetVariableSuspendableExpression(expr, context, referencingContext)

  override def unparse(state: UState): Unit = {
    suspendableExpression.run(state)
  }

}

final class NewVariableInstanceDefaultValueSuspendableExpression(
  override val expr: CompiledExpression[AnyRef],
  override val rd: VariableRuntimeData,
  nvi: VariableInstance
) extends SuspendableExpression {

  override protected def processExpressionResult(
    ustate: UState,
    v: DataValuePrimitive
  ): Unit = {
    nvi.setDefaultValue(v) // This also sets variable state to VariableDefined
  }

  override protected def maybeKnownLengthInBits(ustate: UState) = MaybeULong(0)
}

// When implemented this almost certainly wants to be a combinator
// Not two separate unparsers.
class NewVariableInstanceStartUnparser(vrd: VariableRuntimeData, trd: TermRuntimeData)
  extends PrimUnparserNoData {

  override def context = trd
  override def runtimeDependencies = Vector()

  override def childProcessors = Vector()

  override def unparse(state: UState) = {
    val nvi = state.newVariableInstance(vrd)

    if (vrd.maybeDefaultValueExpr.isDefined) {
      val dve = vrd.maybeDefaultValueExpr.get
      nvi.setState(VariableInProcess)
      val suspendableExpression =
        new NewVariableInstanceDefaultValueSuspendableExpression(dve, vrd, nvi)
      suspendableExpression.run(state)
    } else if (nvi.firstInstanceInitialValue.isDefined) {
      // The NVI will inherit the default value of the original variable instance
      // This will also inherit any externally provided bindings.
      nvi.setDefaultValue(nvi.firstInstanceInitialValue)
    }
  }
}

class NewVariableInstanceEndUnparser(vrd: VariableRuntimeData, trd: TermRuntimeData)
  extends PrimUnparserNoData {

  override def context = trd
  override def runtimeDependencies = Vector()

  override def childProcessors = Vector()

  override def unparse(state: UState) = state.removeVariableInstance(vrd)
}
