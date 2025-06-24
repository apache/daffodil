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

package org.apache.daffodil.core.runtime1

import org.apache.daffodil.core.dsom.SchemaSet
import org.apache.daffodil.core.dsom.SequenceTermBase
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.runtime1.iapi.DFDL
import org.apache.daffodil.runtime1.layers.LayerRuntimeCompiler
import org.apache.daffodil.runtime1.layers.LayerRuntimeData
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.Processor
import org.apache.daffodil.runtime1.processors.SchemaSetRuntimeData
import org.apache.daffodil.runtime1.processors.VariableMap
import org.apache.daffodil.runtime1.processors.parsers.NotParsableParser
import org.apache.daffodil.runtime1.processors.unparsers.NotUnparsableUnparser

trait SchemaSetRuntime1Mixin {
  self: SchemaSet =>

  requiredEvaluationsAlways(parser)
  requiredEvaluationsAlways(unparser)
  requiredEvaluationsAlways(allLayers)
  requiredEvaluationsAlways(root.elementRuntimeData.initialize)

  override lazy val variableMap: VariableMap = LV(Symbol("variableMap")) {
    val vrds = allDefinedVariables.map { _.variableRuntimeData }
    val vmap = VariableMap(vrds)
    vmap
  }.value

  lazy val parser = LV(Symbol("parser")) {
    val par =
      if (generateParser) root.document.parser
      else new NotParsableParser(root.erd)
    Processor.initialize(par)
    par
  }.value

  lazy val unparser = LV(Symbol("unparser")) {
    val unp =
      if (generateUnparser) root.document.unparser
      else new NotUnparsableUnparser(root.erd)
    Processor.initialize(unp)
    unp
  }.value

  private lazy val layerRuntimeCompiler = new LayerRuntimeCompiler

  private lazy val allLayers: Seq[LayerRuntimeData] = LV(Symbol("allLayers")) {
    val lrds: Seq[LayerRuntimeData] = self.allSchemaComponents
      .collect {
        case stb: SequenceTermBase if (stb.isLayered) => stb
      }
      .map { _.optionLayerRuntimeData.get }
    layerRuntimeCompiler.compileAll(lrds) // for checking only. We're not saving this.
    lrds
  }.value

  def onPath(xpath: String): DFDL.DataProcessor = {
    checkNotError()
    if (xpath != "/")
      root.notYetImplemented("""Path must be "/". Other path support is not yet implemented.""")
    val rootERD = root.elementRuntimeData
    root.schemaDefinitionUnless(
      !rootERD.dpathElementCompileInfo.isOutputValueCalc,
      "The root element cannot have the dfdl:outputValueCalc property."
    )
    val p = if (!root.isError) parser else null
    val u = if (!root.isError) unparser else null
    val ssrd =
      new SchemaSetRuntimeData(p, u, rootERD, variableMap, allLayers, layerRuntimeCompiler)
    if (root.numComponents > root.numUniqueComponents)
      Logger.log.debug(
        s"Compiler: component counts: unique ${root.numUniqueComponents}, actual ${root.numComponents}."
      )
    val dataProc =
      new DataProcessor(ssrd, tunable, variableMap.copy(), diagnostics = this.diagnostics)
    dataProc
  }
}
