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

package org.apache.daffodil.runtime1

import org.apache.daffodil.api.DFDL
import org.apache.daffodil.dsom.SchemaSet
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.grammar.VariableMapFactory
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.processors.Processor
import org.apache.daffodil.processors.SchemaSetRuntimeData
import org.apache.daffodil.processors.VariableMap
import org.apache.daffodil.processors.parsers.NotParsableParser
import org.apache.daffodil.processors.unparsers.NotUnparsableUnparser
import org.apache.daffodil.util.Logger

trait SchemaSetRuntime1Mixin {
  self: SchemaSet =>

  requiredEvaluationsAlways(parser)
  requiredEvaluationsAlways(unparser)
  requiredEvaluationsAlways(root.elementRuntimeData.initialize)

  /**
   * This initialization is required for simpleTypeDefs used only
   * for type calculations where those simpleTypeDefs do not have
   * a corresponding element of that type.
   */
  requiredEvaluationsAlways(typeCalcMap.foreach {
    case (_, typeCalculator) =>
      typeCalculator.initialize()
  })

  override lazy val variableMap: VariableMap = LV('variableMap) {
    val dvs = allSchemaDocuments.flatMap {
      _.defineVariables
    }
    val alldvs = dvs.union(predefinedVars)
    val vmap = VariableMapFactory.create(alldvs)
    vmap
  }.value

  lazy val parser = LV('parser) {
    val par = if (generateParser) root.document.parser else new NotParsableParser(root.erd)
    Processor.initialize(par)
    par
  }.value

  lazy val unparser = LV('unparser) {
    val unp = if (generateUnparser) root.document.unparser else new NotUnparsableUnparser(root.erd)
    Processor.initialize(unp)
    unp
  }.value

  def onPath(xpath: String): DFDL.DataProcessor = {
    Assert.usage(!isError)
    if (xpath != "/") root.notYetImplemented("""Path must be "/". Other path support is not yet implemented.""")
    val rootERD = root.elementRuntimeData
    root.schemaDefinitionUnless(
      !rootERD.dpathElementCompileInfo.isOutputValueCalc,
      "The root element cannot have the dfdl:outputValueCalc property.")
    val p = if (!root.isError) parser else null
    val u = if (!root.isError) unparser else null
    val ssrd = new SchemaSetRuntimeData(
      p,
      u,
      this.diagnostics,
      rootERD,
      variableMap,
      typeCalcMap)
    if (root.numComponents > root.numUniqueComponents)
      Logger.log.debug(s"Compiler: component counts: unique ${root.numUniqueComponents}, actual ${root.numComponents}.")
    val dataProc = new DataProcessor(ssrd, tunable, variableMap.copy())
    if (dataProc.isError) {
    } else {
      Logger.log.debug(s"Parser = ${ssrd.parser.toString}.")
      Logger.log.debug(s"Unparser = ${ssrd.unparser.toString}.")
      Logger.log.debug(s"Compilation (DataProcesor) completed with no errors.")
    }
    dataProc
  }
}
