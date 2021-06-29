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

case class DFDLXInputTypeCalc(typedRecipes: List[(CompiledDPath, NodeInfo.Kind)], calc: TypeCalculator)
  extends FNTwoArgs(typedRecipes.map(_._1)) {

  val srcType = typedRecipes(1)._2

  override def computeValue(arg1: DataValuePrimitive, arg2: DataValuePrimitive, dstate: DState): DataValuePrimitive = {
    calc.inputTypeCalcRun(dstate, arg2, srcType)
    dstate.currentValue.getNonNullable
  }

}

case class DFDLXOutputTypeCalc(typedRecipes: List[(CompiledDPath, NodeInfo.Kind)], calc: TypeCalculator)
  extends FNTwoArgs(typedRecipes.map(_._1)) {

  val srcType = typedRecipes(1)._2

  def computeValue(arg1: DataValuePrimitive, arg2: DataValuePrimitive, dstate: DState): DataValuePrimitive = {
    calc.outputTypeCalcRun(dstate, arg2, srcType)
    dstate.currentValue.getNonNullable
  }
}

