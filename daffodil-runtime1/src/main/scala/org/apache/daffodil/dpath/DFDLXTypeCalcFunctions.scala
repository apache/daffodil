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

import org.apache.daffodil.xml.QName
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.processors.TypeCalculator
import org.apache.daffodil.processors.SimpleTypeRuntimeData
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.dpath.NodeInfo.PrimType.IntegerKind
import org.apache.daffodil.dpath.NodeInfo.PrimType.String
import org.apache.daffodil.dpath.NodeInfo.PrimType.Integer

trait TypeCalculatorNamedDispatch { self: FNTwoArgs =>
  val srcType: NodeInfo.Kind
  val dstType: NodeInfo.Kind

  /*
   * Note the the src/dst type of the calculator are not nessasarily expected to match the src/dst type of the expression.
   * In particular, there are 2 things to be aware of:
   *
   * 1) The src/dst type of the calculator refer to the type of the inputTypeCalc function
   *       they will be swapped for the outputTypeCalc function
   *
   *    For clarity, "inputType" and "outputType" refer to the domain and codomain of calculator function that the caller plans on using.
   *
   * 2) It is possible for the srcType of the expression to be a subtype of inputType
   *    it is possible for the dstType of the expression to be a subtype of outputType
   */
  def getCalculator(arg1: AnyRef, dstate: DState, expectedInputType: NodeInfo.Kind, expectedOutputType: NodeInfo.Kind, requireInputCalc: Boolean = false, requireOutputCalc: Boolean = false): TypeCalculator[AnyRef, AnyRef] = {
    val qn = arg1.asInstanceOf[String]
    val runtimeData = dstate.runtimeData.get
    val refType = QName.resolveRef(qn, runtimeData.namespaces, runtimeData.tunable).get.toGlobalQName
    val maybeCalc = dstate.maybeSsrd.get.typeCalculators.get(refType)
    if (!maybeCalc.isDefined) {
      throw FNErrorFunctionException(Maybe(runtimeData.schemaFileLocation), dstate.contextLocation, s"Simple type ${refType} does not exist or does not have a repType")
    }
    val calc = maybeCalc.get
    if (!expectedInputType.isSubtypeOf(calc.srcType)) {
      dstate.SDE(s"The type calculator defined by ${qn} has a source type of ${calc.srcType}, but its usage requires ${expectedInputType}")
    }
    if (!calc.dstType.isSubtypeOf(expectedOutputType)) {
      dstate.SDE(s"The type calculator defined by ${qn} has a destination type of ${calc.dstType}, but its usage requires ${expectedOutputType}")
    }

    Assert.invariant(requireInputCalc || requireOutputCalc)
    
    if (requireInputCalc && !calc.supportsParse) {
      dstate.SDE(s"${qn} does not define an inputValueCalc")
    }
    if (requireOutputCalc && !calc.supportsUnparse) {
      dstate.SDE(s"${qn} does not define an outputValueCalc")
    }
    calc
  }
}

case class DFDLXInputTypeCalcInt(typedRecipes: List[(CompiledDPath, NodeInfo.Kind)])
  extends FNTwoArgs(typedRecipes.map(_._1))
  with TypeCalculatorNamedDispatch {
  
  override val dstType = NodeInfo.Int
  override val srcType = typedRecipes(1)._2

  override def computeValue(arg1: AnyRef, arg2: AnyRef, dstate: DState): AnyRef = {
    val calc = getCalculator(arg1, dstate, srcType, dstType, requireInputCalc=true)
    calc.inputTypeCalcRun(dstate, arg2, srcType)
    dstate.currentValue
  }

}

case class DFDLXInputTypeCalcString(typedRecipes: List[(CompiledDPath, NodeInfo.Kind)])
  extends FNTwoArgs(typedRecipes.map(_._1))
  with TypeCalculatorNamedDispatch {
  
  override val dstType = NodeInfo.String
  override val srcType = typedRecipes(1)._2
  
  def computeValue(arg1: AnyRef, arg2: AnyRef, dstate: DState): AnyRef = {
    getCalculator(arg1, dstate, srcType, dstType, requireInputCalc=true).inputTypeCalcRun(dstate, arg2, srcType)
    dstate.currentValue
  }
}

case class DFDLXOutputTypeCalcInt(typedRecipes: List[(CompiledDPath, NodeInfo.Kind)])
  extends FNTwoArgs(typedRecipes.map(_._1))
  with TypeCalculatorNamedDispatch {
  
  override val dstType = NodeInfo.Int
  override val srcType = typedRecipes(1)._2
  
  def computeValue(arg1: AnyRef, arg2: AnyRef, dstate: DState): AnyRef = {
    getCalculator(arg1, dstate, dstType, srcType, requireOutputCalc=true).outputTypeCalcRun(dstate, arg2, srcType)
    dstate.currentValue
  }
}

case class DFDLXOutputTypeCalcString(typedRecipes: List[(CompiledDPath, NodeInfo.Kind)])
  extends FNTwoArgs(typedRecipes.map(_._1))
  with TypeCalculatorNamedDispatch {
  override val dstType = NodeInfo.String
  override val srcType = typedRecipes(1)._2
  
  def computeValue(arg1: AnyRef, arg2: AnyRef, dstate: DState): AnyRef = {
    getCalculator(arg1, dstate, dstType, srcType, requireOutputCalc=true).outputTypeCalcRun(dstate, arg2, srcType)
    dstate.currentValue
  }
}

trait DFDLXOutputTypeCalcNextSibling {
  def dstType: NodeInfo.Kind

  def run(dstate: DState): Unit = {
    if (dstate.isCompile) {
      //CompileDPath.runExpressionForConstant (in DPathRuntime.scala) determines
      //that an expression is not constant by seeing if evalutating it throws an exception
      throw new IllegalStateException()
    }

    val nextSibling = dstate.nextSibling.asSimple
    val typeCalculator = nextSibling.erd.optSimpleTypeRuntimeData.get.typeCalculator.get
    if(!typeCalculator.supportsUnparse){
      dstate.SDE(s"The type calculator defined by ${nextSibling.erd.diagnosticDebugName} does not define an outputCalc")
    }
    if (!typeCalculator.srcType.isSubtypeOf(dstType)) {
      dstate.SDE(s"The type calculator defined by ${nextSibling.erd.diagnosticDebugName} has a source type of ${typeCalculator.srcType}, but its usage requires ${dstType}")
    }
    val primType = nextSibling.erd.optPrimType.get
    if (!primType.isSubtypeOf(typeCalculator.dstType)) {
      dstate.SDE(s"The type calculator defined by ${nextSibling.erd.diagnosticDebugName} has a destination type of ${typeCalculator.dstType}, but its usage requires ${primType}")
    }
    val x = nextSibling.dataValue
    typeCalculator.outputTypeCalcRun(dstate, x, primType)
  }
}

case class DFDLXOutputTypeCalcNextSiblingInt(a: CompiledDPath, b: NodeInfo.Kind) extends RecipeOp
  with DFDLXOutputTypeCalcNextSibling {
  override def dstType = NodeInfo.Int
}

case class DFDLXOutputTypeCalcNextSiblingString(a: CompiledDPath, b: NodeInfo.Kind) extends RecipeOp
  with DFDLXOutputTypeCalcNextSibling {
  override def dstType = NodeInfo.String
}

trait DFDLXWithExtractTypedValue { self: RecipeOp =>
  def returnType: NodeInfo.Kind

  //Below strings are to provide better diagnostic messages
  def functionName: String
  def legalContext: String
  def repOrLogicalType: String

  def extractVal(typedVal: Maybe[(AnyRef, NodeInfo.Kind)], dstate: DState): AnyRef = {
    if (dstate.isCompile) {
      //CompileDPath.runExpressionForConstant (in DPathRuntime.scala) determines
      //that an expression is not constant by seeing if evalutating it throws an exception
      throw new IllegalStateException()
    }
    if (typedVal.isEmpty) {
      dstate.SDE(s"${functionName} may only be called from within a ${legalContext} annotation")
    }
    val (x, xKind) = typedVal.get
    if (!xKind.isSubtypeOf(returnType)) {
      xKind.isSubtypeOf(returnType)
      dstate.SDE(s"${repOrLogicalType} is a(n) ${xKind} type, where ${returnType} is expected")
    }
    x
  }
}

case class DFDLXRepTypeValueInt(a: CompiledDPath, b: NodeInfo.Kind)
  extends RecipeOp
  with DFDLXWithExtractTypedValue {
  override val returnType = Integer
  override val functionName = "dfdl:repTypeValueInt()"
  override val legalContext = "dfdl:inputTypeCalc"
  override val repOrLogicalType = "repType"

  override def run(dstate: DState): Unit = {
    val repValue = extractVal(dstate.repValue, dstate)
    dstate.setCurrentValue(repValue)
  }
}

case class DFDLXRepTypeValueString(a: CompiledDPath, b: NodeInfo.Kind)
  extends RecipeOp
  with DFDLXWithExtractTypedValue {

  override val returnType = String
  override val functionName = "dfdl:repTypeValueString()"
  override val legalContext = "dfdl:inputTypeCalc"
  override val repOrLogicalType = "repType"

  override def run(dstate: DState): Unit = {
    val repValue = extractVal(dstate.repValue, dstate)
    dstate.setCurrentValue(repValue)
  }
}

case class DFDLXLogicalTypeValueInt(a: CompiledDPath, b: NodeInfo.Kind)
  extends RecipeOp
  with DFDLXWithExtractTypedValue {

  override val returnType = Integer
  override val functionName = "dfdl:logicalTypeValueInt()"
  override val legalContext = "dfdl:outputTypeCalc"
  override val repOrLogicalType = "logical type"

  override def run(dstate: DState): Unit = {
    val repValue = extractVal(dstate.logicalValue, dstate)
    dstate.setCurrentValue(repValue)
  }
}
case class DFDLXLogicalTypeValueString(a: CompiledDPath, b: NodeInfo.Kind)
  extends RecipeOp
  with DFDLXWithExtractTypedValue {
  override val returnType = String
  override val functionName = "dfdl:logicalTypeValueString()"
  override val legalContext = "dfdl:outputTypeCalc"
  override val repOrLogicalType = "logical type"

  override def run(dstate: DState): Unit = {
    val repValue = extractVal(dstate.logicalValue, dstate)
    dstate.setCurrentValue(repValue)
  }
}
