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

package org.apache.daffodil.runtime1.processors

import java.net.URI

import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.runtime1.layers.LayerRuntimeCompiler
import org.apache.daffodil.runtime1.layers.LayerRuntimeData
import org.apache.daffodil.runtime1.layers.LayerVarsRuntime
import org.apache.daffodil.runtime1.processors.parsers.Parser
import org.apache.daffodil.runtime1.processors.unparsers.Unparser

final class SchemaSetRuntimeData(
  val parser: Parser,
  val unparser: Unparser,
  val elementRuntimeData: ElementRuntimeData,
  /*
   * The original variables determined by the schema compiler.
   */
  variables: VariableMap,
  allLayers: Seq[LayerRuntimeData],
  @transient layerRuntimeCompilerArg: LayerRuntimeCompiler,
  /**
   * URI to the root schema, used only for compiling a validator for full validation--this
   * should not be used for any other purposes. This is marked as transient so that if the
   * DataProcessor is serialized this URI will not be serialized, which ensures the absolute URI
   * does not cause issues with save processor reproducibility. Note that saved parsers cannot
   * be used with full validation so this does not break anything.
   */
  @transient val mainSchemaUriForFullValidation: URI
) extends Serializable
  with ThrowsSDE {

  def unqualifiedPathStepPolicy = elementRuntimeData.unqualifiedPathStepPolicy
  def encodingInfo = elementRuntimeData.encodingInfo
  override def schemaFileLocation = elementRuntimeData.schemaFileLocation
  override def SDE(str: String, args: Any*) = elementRuntimeData.SDE(str, args: _*)

  private def writeObject(oos: java.io.ObjectOutputStream): Unit = {
    oos.defaultWriteObject()
    elementRuntimeData.dpathElementCompileInfo.serializeParents(oos)
  }

  private def readObject(ois: java.io.ObjectInputStream): Unit = {
    ois.defaultReadObject()
    elementRuntimeData.dpathElementCompileInfo.deserializeParents(ois)
  }

  /**
   * Always return a copy when original variables is requested, thus preserving
   * the state of the actual original variables
   */
  def originalVariables = variables.copy

  /**
   * This deals with the situation where schema compilation
   * immediately is followed by execution so there is no serialize/deserialize.
   * We still need the layers to have been compiled before the
   * execution begins, so we use the LayerRuntimeCompiler from
   * the schema compiler in that case. Otherwise if the compiled
   * schema is loaded then this is recreated and all the compilations
   * done after deserialization completes.
   */
  @transient private lazy val layerRuntimeCompiler =
    if (layerRuntimeCompilerArg ne null) layerRuntimeCompilerArg
    else new LayerRuntimeCompiler

  def getLayerVarsRuntime(lrd: LayerRuntimeData): LayerVarsRuntime = {
    layerRuntimeCompiler.getLayerVarsRuntime(lrd)
  }

  /**
   * Intended to be called after a schema is re-loaded.
   * Ensures that all the layers have had their layerRuntimeData compiled.
   */
  def compileLayers(): Unit = layerRuntimeCompiler.compileAll(allLayers)
}
