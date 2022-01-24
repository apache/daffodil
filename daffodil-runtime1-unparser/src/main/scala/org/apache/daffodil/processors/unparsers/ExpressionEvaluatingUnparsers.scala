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

import org.apache.daffodil.dpath.SuspendableExpression
import org.apache.daffodil.dsom.CompiledExpression
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.infoset.DISimple
import org.apache.daffodil.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.infoset.DataValue.DataValuePrimitiveNullable
import org.apache.daffodil.infoset.Infoset
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.processors.NonTermRuntimeData
import org.apache.daffodil.processors.Success
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.processors.TypeCalculator
import org.apache.daffodil.processors.VariableRuntimeData
import org.apache.daffodil.processors.VariableInProcess
import org.apache.daffodil.processors.VariableInstance
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.MaybeULong

final class SetVariableSuspendableExpression(
  override val expr: CompiledExpression[AnyRef],
  override val rd: VariableRuntimeData,
  referencingContext: NonTermRuntimeData)
  extends SuspendableExpression {

  override protected def processExpressionResult(ustate: UState, v: DataValuePrimitive): Unit = {
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
  referencingContext: NonTermRuntimeData)
  extends PrimUnparserNoData {

  override lazy val runtimeDependencies = Vector()

  override lazy val childProcessors = Vector()

  def suspendableExpression =
    new SetVariableSuspendableExpression(
      expr, context, referencingContext)

  override def unparse(state: UState): Unit = {
    suspendableExpression.run(state)
  }

}

final class NewVariableInstanceDefaultValueSuspendableExpression(
  override val expr: CompiledExpression[AnyRef],
  override val rd: VariableRuntimeData,
  nvi: VariableInstance)
  extends SuspendableExpression {

  override protected def processExpressionResult(ustate: UState, v: DataValuePrimitive): Unit = {
    nvi.setDefaultValue(v) // This also sets variable state to VariableDefined
  }

  override protected def maybeKnownLengthInBits(ustate: UState) = MaybeULong(0)
}

// When implemented this almost certainly wants to be a combinator
// Not two separate unparsers.
class NewVariableInstanceStartUnparser(vrd: VariableRuntimeData, trd: TermRuntimeData)
  extends PrimUnparserNoData {

  override def context = trd
  override lazy val runtimeDependencies = Vector()

  override lazy val childProcessors = Vector()

  override def unparse(state: UState) = {
    val nvi = state.newVariableInstance(vrd)

    if (vrd.maybeDefaultValueExpr.isDefined) {
      val dve = vrd.maybeDefaultValueExpr.get
      nvi.setState(VariableInProcess)
      val suspendableExpression = new NewVariableInstanceDefaultValueSuspendableExpression(dve, vrd, nvi)
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
  override lazy val runtimeDependencies = Vector()

  override lazy val childProcessors = Vector()

  override def unparse(state: UState) = state.removeVariableInstance(vrd)
}

class TypeValueCalcUnparser(typeCalculator: TypeCalculator, repTypeUnparser: Unparser, e: ElementRuntimeData, repTypeRuntimeData: ElementRuntimeData)
  extends CombinatorUnparser(e) {

  override def childProcessors: Vector[Unparser] = Vector(repTypeUnparser)

  def runtimeDependencies: Vector[Evaluatable[AnyRef]] = Vector()

  protected def unparse(ustate: UState): Unit = {
    Assert.invariant(ustate.currentInfosetNodeMaybe.isDefined)
    Assert.invariant(ustate.currentInfosetNode.isSimple)

    val currentSimple = ustate.currentInfosetNode.asSimple

    val logicalValueNullable = currentSimple.dataValue
    Assert.invariant(logicalValueNullable.isDefined)
    val logicalValue=logicalValueNullable.getNonNullable
    val logicalValueType = currentSimple.erd.optPrimType.get
    val repTypeValue: DataValuePrimitiveNullable = {
      val ans = typeCalculator.outputTypeCalcUnparse(ustate, e, logicalValue.getNonNullable, logicalValueType)
      ans
    }

    val origInfosetElement = ustate.currentInfosetNode
    val tmpInfosetElement = Infoset.newElement(repTypeRuntimeData).asInstanceOf[DISimple]
    if (ustate.withinHiddenNest)
      tmpInfosetElement.setHidden()

    // Although quasi elements aren't really part of the infoset, we still
    // require that certain invariants hold. One of which is that all elements
    // have a parent. This is necessary for things like running the
    // InfosetWalker in the interactive debugger. To ensure this invariant
    // holds, we set the parent of this quasi element to the same as that of
    // the current infoset node.
    tmpInfosetElement.setParent(currentSimple.parent)

    if (ustate.processorStatus == Success) {

      Assert.invariant(repTypeValue.isDefined)
      tmpInfosetElement.setDataValue(repTypeValue)
      ustate.currentInfosetNodeStack.push(Maybe(tmpInfosetElement))
      repTypeUnparser.unparse1(ustate)
      ustate.currentInfosetNodeStack.pop
    }
  }
}
