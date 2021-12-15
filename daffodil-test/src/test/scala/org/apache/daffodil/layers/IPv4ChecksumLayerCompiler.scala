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

final class IPv4ChecksumLayerCompiler
  extends LayerCompiler("IPv4Checksum") {


  val variablesNamespace = "urn:org.apache.daffodil.layers.IPv4Checksum"
  val variablesPreferredNamespacePrefix = "chksum"
  val localNameOfVariableToWrite = name
  val outputVariableType = PrimType.UnsignedShort

  override def compileLayer(layerCompileInfo: LayerCompileInfo) = {

    val outputVar = layerCompileInfo.getVariableRuntimeData(
      variablesPreferredNamespacePrefix,
      variablesNamespace,
      localNameOfVariableToWrite,
      outputVariableType)

    layerCompileInfo.optLayerLengthKind match {
      case Some(LayerLengthKind.Implicit) => // ok
      case None => // ok
      case Some(other) => layerCompileInfo.SDE(s"The property dfdlx:layerLengthKind must be 'implicit' (or omitted) but was '$other'.")
    }
    layerCompileInfo.optLayerLengthOptConstantValue match {
      case Some(_) =>  layerCompileInfo.SDE("The dfdlx:layerLength property should not be supplied, as this layer is fixed length (20 bytes).")
      case None => // ok
    }
    layerCompileInfo.optLayerLengthUnits match {
      case Some(LayerLengthUnits.Bytes) => // ok
      case None => // ok
      case Some(other) => layerCompileInfo.SDE("The property dfdlx:layerLengthKind must be 'bytes' but was '$other'.")
    }

    new IPv4ChecksumLayerTransformerFactory(name, outputVar)
  }
}

class IPv4ChecksumLayerTransformerFactory(name: String, outputVar: VariableRuntimeData)
    extends LayerTransformerFactory(name) {

  override def newInstance(layerRuntimeInfo: LayerRuntimeInfo)= {
    val xformer = new IPv4Checksum(name, layerRuntimeInfo, outputVar)
    xformer
  }
}
