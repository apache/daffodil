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

import org.apache.daffodil.lib.exceptions.ThrowsSDE
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

}
