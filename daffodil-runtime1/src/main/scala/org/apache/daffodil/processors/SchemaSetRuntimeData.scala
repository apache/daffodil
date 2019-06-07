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

package org.apache.daffodil.processors

import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.api.ValidationMode
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.processors.unparsers.Unparser
import org.apache.daffodil.processors.parsers.Parser
import org.apache.daffodil.xml.GlobalQName
import org.apache.daffodil.processors.TypeCalculatorCompiler.TypeCalcMap

final class SchemaSetRuntimeData(
  val parser: Parser,
  val unparser: Unparser,
  val diagnostics: Seq[Diagnostic],
  val elementRuntimeData: ElementRuntimeData,
  var variables: VariableMap,
  var validationMode: ValidationMode.Type,
  val typeCalculators: TypeCalcMap)
  extends Serializable with ThrowsSDE {

  def tunable = elementRuntimeData.tunable
  def encodingInfo = elementRuntimeData.encodingInfo
  override def schemaFileLocation = elementRuntimeData.schemaFileLocation
  override def SDE(str: String, args: Any*) = elementRuntimeData.SDE(str, args)

}
