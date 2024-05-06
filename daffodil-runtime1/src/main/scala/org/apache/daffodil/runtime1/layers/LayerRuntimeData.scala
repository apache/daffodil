/*
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.apache.daffodil.runtime1.layers

import org.apache.daffodil.lib.exceptions.SchemaFileLocation
import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.xml.NS
import org.apache.daffodil.lib.xml.RefQName
import org.apache.daffodil.runtime1.processors.VariableRuntimeData

/**
 * Like ElementRuntimeData, SequenceRuntimeData, etc. this class
 * contains the static information created by the schema compilation
 * process that is _used_ by the runtime system.
 *
 */
class LayerRuntimeData(
  val layerQName: RefQName,
  val schemaFileLocation: SchemaFileLocation,
  val qNameToVRD: Map[String, VariableRuntimeData],
  val context: ThrowsSDE
) extends Serializable {

  final def localName: String = layerQName.local
  final def namespace: NS = layerQName.namespace

  final def toss(th: Throwable): Nothing =
    throw th // good place for a breakpoint

  lazy val spiName: String = LayerUtils.spiName(localName, namespace)
}
