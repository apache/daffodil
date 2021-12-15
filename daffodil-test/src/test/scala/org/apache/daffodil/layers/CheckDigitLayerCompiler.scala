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

package org.apache.daffodil.layers

import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.processors.VariableRuntimeData
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthKind
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthUnits

final class CheckDigitLayerCompiler
extends LayerCompiler("checkDigit") {

  val variablesNamespace = "urn:org.apache.daffodil.layers.checkDigit"
  val variablesPreferredNamespacePrefix = "cd"
  val localNamesAndTypesOfVariablesToRead = Seq(("checkDigitParams", PrimType.String))
  val localNameOfVariableToWrite = name
  val outputVariableType = PrimType.Int

  override def compileLayer(layerCompileInfo: LayerCompileInfo) = {
    val outputVar =
      layerCompileInfo.getVariableRuntimeData(
        variablesPreferredNamespacePrefix,
        variablesNamespace,
        localNameOfVariableToWrite,
        outputVariableType)

    val inputVRDs = localNamesAndTypesOfVariablesToRead.map {
      case (local, primType) =>
        layerCompileInfo.getVariableRuntimeData(variablesPreferredNamespacePrefix, variablesNamespace, local, primType)
    }
    layerCompileInfo.optLayerLengthKind match {
      case Some(LayerLengthKind.Explicit) => // ok
      case None => layerCompileInfo.SDE("The property dfdlx:layerLengthKind must be defined and have value 'explicit'.")
      case Some(other) => layerCompileInfo.SDE("The property dfdlx:layerLengthKind must be 'explicit' but was '$other'.")
    }
    layerCompileInfo.SDEUnless(
      layerCompileInfo.optLayerLengthOptConstantValue.isDefined,
      "The property dfdlx:layerLength must be defined.")
    layerCompileInfo.optLayerLengthUnits match {
      case Some(LayerLengthUnits.Bytes) => // ok
      case None => // ok
      case Some(other) => layerCompileInfo.SDE("The property dfdlx:layerLengthKind must be 'bytes' but was '$other'.")
    }
    layerCompileInfo.SDEUnless(
      layerCompileInfo.optLayerJavaCharsetOptConstantValue.isDefined,
      "The property dfdlx:layerEncoding must be defined.")

    new CheckDigitLayerTransformerFactory(name, inputVRDs, outputVar)
  }
}

class CheckDigitLayerTransformerFactory(name: String, inputVars: Seq[VariableRuntimeData], outputVar: VariableRuntimeData)
  extends LayerTransformerFactory("checkDigit") {

  override def newInstance(layerRuntimeInfo: LayerRuntimeInfo)= {
    val xformer = new CheckDigitExplicit(name, layerRuntimeInfo, outputVar, inputVars)
    xformer
  }
}


