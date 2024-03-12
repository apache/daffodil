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

package org.apache.daffodil.runtime1.layers

/**
 * Factory for a layer
 *
 * This is the serialized object which is saved as part of a processor.
 * It constructs the layer at runtime when newInstance() is called.
 *
 * This allows the layer instance itself to be stateful and not serializable.
 */
class LayerFactory(val layerRuntimeData: LayerRuntimeData) extends Serializable {

  /**
   * Call at runtime to create a layer object. This layer object can be stateful
   * and non-thread safe.
   *
   * Called by the LayeredSequenceParser or LayeredSequenceUnparser to allocate the
   * layer, and the result is used to carry out the layering mechanism.
   *
   * @param lri the layer runtime info which includes both static and runtime
   *           state-based information for the parse or unparse
   * @return the Layer properly initialized/constructed for this layer
   */
  def newInstance(lri: LayerRuntime): LayerDriver = {
    val layerVarsRuntime = LayerRuntimeCompiler.compile(lri.layerRuntimeData)
    val newInstance = layerVarsRuntime.constructInstance()

    LayerDriver(lri, newInstance, layerVarsRuntime)
  }
}
