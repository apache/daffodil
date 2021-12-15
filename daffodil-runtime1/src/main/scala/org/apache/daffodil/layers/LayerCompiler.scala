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

import org.apache.daffodil.api.WarnID
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.dsom.ImplementsThrowsOrSavesSDE
import org.apache.daffodil.processors.SequenceRuntimeData
import org.apache.daffodil.processors.VariableRuntimeData
import org.apache.daffodil.processors.charset.BitsCharsetJava
import org.apache.daffodil.processors.charset.BitsCharsetNonByteSize
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthKind
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthUnits
import org.apache.daffodil.xml.NS
import org.apache.daffodil.xml.RefQName

/**
 * Must be implemented by all layers.
 *
 * These are the classes which must be dynamically loaded in order to add a layer implementation
 * to Daffodil.
 *
 * These instances are NOT serialized as part of a saved processor. The compileLayer method
 * is called and the resulting LayerTransformerFactory is the serialized object.
 */
abstract class LayerCompiler(nom: String) {

  def name() = nom

  /**
   * Performs all schema-compile-time checking for the layer parameters, and constructs
   * a LayerTransformerFactory which is the serialized runtime object that becomes part of the
   * processor.
   * @param layerCompileInfo Schema-compilation time information about the layer properties.
   * @return
   */
  def compileLayer(layerCompileInfo: LayerCompileInfo): LayerTransformerFactory

}

/**
 * Provides access to DFDL schema compile-time information about the layer properties.
 *
 * Allows reporting of schema definition errors and warnings at schema compile time.
 */
final class LayerCompileInfo(sequence: ImplementsThrowsOrSavesSDE,
  val layerRuntimeInfo: LayerRuntimeInfo) {

  private def lri = layerRuntimeInfo
  private def srd: SequenceRuntimeData = lri.runtimeData

  def getVariableRuntimeData(prefix: String, namespace: String, localName: String, primType: PrimType) : VariableRuntimeData = {
    val varNamespace = NS(namespace)
    val qName = RefQName(Some(prefix), localName, varNamespace).toGlobalQName
    val vrd = srd.variableMap.getVariableRuntimeData(qName).getOrElse {
      srd.SDE("Variable '%s' is not defined.", qName.toExtendedSyntax)
    }
    srd.schemaDefinitionUnless(vrd.primType == primType,
      "Variable '%s' is not of type '%s'.", qName.toExtendedSyntax, primType)
    vrd
  }

  /**
   * If defined, and the value is a compile-time constant then this will be Some(Some(Charset))
   * If defined, and the value is non-constant or not a regular JVM charset, then this will be Some(None)
   * If undefined, the value is None
   */
  def optLayerJavaCharsetOptConstantValue: Option[Option[java.nio.charset.Charset]] = {
    if (lri.maybeLayerCharsetEv.isEmpty) None
    else
      lri.maybeLayerCharsetEv.get.optConstant.map {
        case java: BitsCharsetJava => Some(java.javaCharset)
        case _: BitsCharsetNonByteSize => None
      }
  }

  def optLayerLengthKind: Option[LayerLengthKind] = {
    lri.maybeLayerLengthKind.toScalaOption
  }

  def optLayerLengthOptConstantValue: Option[Option[Long]] = {
    if (lri.maybeLayerLengthEv.isEmpty) None
    else Some(lri.maybeLayerLengthEv.get.optConstant.map {
      _.toLong
    })
  }

  def optLayerLengthUnits: Option[LayerLengthUnits] = {
    lri.maybeLayerLengthUnits.toScalaOption
  }

  def optLayerBoundaryMarkOptConstantValue: Option[Option[String]] = {
    if (lri.maybeLayerBoundaryMarkEv.isEmpty) None
    else Some(lri.maybeLayerBoundaryMarkEv.get.optConstant)
  }

  def schemaDefinitionError(message: String, args: Any*): Nothing = {
    sequence.schemaDefinitionError(message, args: _*)
  }

  def schemaDefinitionWarning(message: String, args: Any*): Unit = {
    sequence.SDW(WarnID.LayerCompileWarning, message, args: _*)
  }

  def SDEUnless(test: Boolean, message: String, args: Any*): Unit = if (!test) SDE(message, args: _*)

  def SDE(message: String, args: Any*): Nothing = schemaDefinitionError(message, args: _*)
}


