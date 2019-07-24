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

package org.apache.daffodil.dpath

import org.apache.daffodil.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.processors.TypeCalculator

case class DFDLXInputTypeCalc(typedRecipes: List[(CompiledDPath, NodeInfo.Kind)], calc: TypeCalculator[AnyRef, AnyRef])
  extends FNTwoArgs(typedRecipes.map(_._1)) {

  val srcType = typedRecipes(1)._2

  override def computeValue(arg1: DataValuePrimitive, arg2: DataValuePrimitive, dstate: DState): DataValuePrimitive = {
    calc.inputTypeCalcRun(dstate, arg2.getAnyRef, srcType)
    dstate.currentValue.getNonNullable
  }

}

case class DFDLXOutputTypeCalc(typedRecipes: List[(CompiledDPath, NodeInfo.Kind)], calc: TypeCalculator[AnyRef, AnyRef])
  extends FNTwoArgs(typedRecipes.map(_._1)) {

  val srcType = typedRecipes(1)._2

  def computeValue(arg1: DataValuePrimitive, arg2: DataValuePrimitive, dstate: DState): DataValuePrimitive = {
    calc.outputTypeCalcRun(dstate, arg2.getAnyRef, srcType)
    dstate.currentValue.getNonNullable
  }
}

case class DFDLXOutputTypeCalcNextSibling(a: CompiledDPath, b: NodeInfo.Kind) extends RecipeOp {

  def run(dstate: DState): Unit = {
    if (dstate.isCompile) {
      //CompileDPath.runExpressionForConstant (in DPathRuntime.scala) determines
      //that an expression is not constant by seeing if evalutating it throws an exception
      throw new IllegalStateException()
    }

    val nextSibling = dstate.nextSibling.asSimple
    val typeCalculator = nextSibling.erd.optSimpleTypeRuntimeData.get.typeCalculator.get
    /*
     * The compiler knows about all the potential typeCalculators we can see here
     * so any validation of typeCalculator should go in Expression.scala as part of compilation
     */

    val primType = nextSibling.erd.optPrimType.get

    val x = nextSibling.dataValue
    typeCalculator.outputTypeCalcRun(dstate, x.getAnyRef, primType)
  }
}

case class DFDLXRepTypeValue(a: CompiledDPath, b: NodeInfo.Kind)
  extends RecipeOp {

  override def run(dstate: DState): Unit = {
    if (dstate.isCompile){
      throw new IllegalStateException()
    }
    
    if (!dstate.repValue.isDefined) {
      /*
     * In theory, we should be able to detect this error at compile time. In practice
     * the compiler does not provide sufficient details to the expression compiler for it
     * to notice.
     */
      dstate.SDE("dfdlx:repTypeValue() may only be called from within dfdlx:inputTypeCalc")
    }
    dstate.setCurrentValue(dstate.repValue)
  }
}

case class DFDLXLogicalTypeValue(a: CompiledDPath, b: NodeInfo.Kind)
  extends RecipeOp {

  override def run(dstate: DState): Unit = {
    
    if (dstate.isCompile){
      throw new IllegalStateException()
    }
    
    if (!dstate.logicalValue.isDefined) {
      /*
     * In theory, we should be able to detect this error at compile time. In practice
     * the compiler does not provide sufficient details to the expression compiler for it
     * to notice.
     */
      dstate.SDE("dfdlx:logicalTypeValue() may only be called from within dfdlx:outputTypeCalc")
    }
    dstate.setCurrentValue(dstate.logicalValue)
  }
}
