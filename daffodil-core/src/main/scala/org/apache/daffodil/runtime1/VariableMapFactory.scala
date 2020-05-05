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

package org.apache.daffodil.grammar

import org.apache.daffodil.dsom._
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.processors.VariableMap

object VariableMapFactory {

  def create(dvs: Seq[DFDLDefineVariable]): VariableMap = {
    val vrds = dvs.map { _.variableRuntimeData }
    val vmap = new VariableMap(vrds)
    vmap
  }

  def setExternalVariables(currentVMap: VariableMap, bindings: Seq[Binding], referringContext: ThrowsSDE) = {
    bindings.foreach(b => currentVMap.setExtVariable(b.varQName, b.varValue, referringContext))
  }
}
