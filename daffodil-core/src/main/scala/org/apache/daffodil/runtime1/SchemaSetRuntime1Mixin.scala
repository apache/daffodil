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

import org.apache.daffodil.ExecutionMode
import org.apache.daffodil.api.DFDL
import org.apache.daffodil.api.ValidationMode
import org.apache.daffodil.dsom.SchemaSet
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.externalvars.ExternalVariablesLoader
import org.apache.daffodil.grammar.VariableMapFactory
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.processors.Processor
import org.apache.daffodil.processors.SchemaSetRuntimeData
import org.apache.daffodil.processors.VariableMap
import org.apache.daffodil.processors.parsers.NotParsableParser
import org.apache.daffodil.processors.unparsers.NotUnparsableUnparser
import org.apache.daffodil.util.LogLevel

trait SchemaSetRuntime1Mixin { self : SchemaSet =>

  // requiredEvaluationsAlways(variableMap)
  requiredEvaluationsAlways(parser)
  requiredEvaluationsAlways(unparser)
  // requiredEvaluationsAlways(root.runtimeData)

  override def variableMap: VariableMap = LV('variableMap) {
    val dvs = allSchemaDocuments.flatMap { _.defineVariables }
    val alldvs = dvs.union(predefinedVars)
    val vmap = VariableMapFactory.create(alldvs)
    //
    // Here we verify that any external variable bindings supplied at schema compilation time
    // are properly structured - the variables they reference exist, are external, and the
    // supplied value string is convertible into the declared variable's type.
    //
    // But... then we throw it away. We don't save the external variable bindings as
    // part of the saved data structure representing the compiled schema.
    // Because external variables really have to be bound at runtime, and the schema has
    // to work with and without any such bindings.
    //
    // This same checking must be done at runtime, so we share that same code here at schema compile time.
    // This is yet another example of where runtime1-specific code is being used in a general
    // schema-compilation role. So even if building a runtime 2, you would still want to keep much of
    // the runtime 1 code around.
    //
    ExternalVariablesLoader.loadVariables(compilerExternalVarSettings, this, vmap)
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
    ExecutionMode.usingCompilerMode {
      Assert.usage(!isError)
      if (xpath != "/") root.notYetImplemented("""Path must be "/". Other path support is not yet implemented.""")
      val rootERD = root.elementRuntimeData
      root.schemaDefinitionUnless(
        rootERD.outputValueCalcExpr.isEmpty,
        "The root element cannot have the dfdl:outputValueCalc property.")
      val validationMode = ValidationMode.Off
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
        log(LogLevel.Info, "Compiler: component counts: unique %s, actual %s.",
          root.numUniqueComponents, root.numComponents)
      val dataProc = new DataProcessor(ssrd, tunable, self.compilerExternalVarSettings)
      if (dataProc.isError) {
        // NO longer printing anything here. Callers must do this.
        //        val diags = dataProc.getDiagnostics
        //        log(LogLevel.Error,"Compilation (DataProcessor) reports %s compile errors/warnings.", diags.length)
        //        diags.foreach { diag => log(LogLevel.Error, diag.toString()) }
      } else {
        log(LogLevel.Compile, "Parser = %s.", ssrd.parser.toString)
        //log(LogLevel.Error, "Unparser = %s.", ssrd.unparser.toString)
        log(LogLevel.Compile, "Compilation (DataProcesor) completed with no errors.")
      }
      dataProc
    }
  }

}
