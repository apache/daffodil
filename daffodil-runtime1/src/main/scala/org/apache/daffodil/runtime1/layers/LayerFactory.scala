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

import org.apache.daffodil.runtime1.layers.api.Layer
import org.apache.daffodil.runtime1.layers.api.LayerCompileInfo
import org.apache.daffodil.runtime1.layers.api.LayerException

/**
 * Factory for a layer transformer.
 *
 * This is the serialized object which is saved as part of a processor.
 * It constructs the layer at runtime when newInstance() is called.
 *
 * This allows the layer instance itself to be stateful and not serializable.
 */
class LayerFactory(val layerCompileInfo: LayerCompileInfo) extends Serializable {

  private def layerTransformName = layerCompileInfo.layerName

  /**
   * Call at runtime to create a layer transformer object. This can be stateful
   * and non-thread safe.
   *
   * Called by the LayeredSequenceParser or LayeredSequenceUnparser to allocate the
   * layer transformer, and the result is used to carry out the layering mechanism.
   *
   * @param lr the layer runtime info which includes both static and runtime
   *           state-based information for the parse or unparse
   * @return the LayerTransformer properly initialized/constructed for this layer
   */
  def newInstance(lr: LayerRuntimeImpl): LayerDriver = {
    val optLayerInstance = LayerRegistry.findLayer(layerTransformName)
    val spiLayerInstance: Layer = optLayerInstance.getOrElse {
      lr.runtimeSchemaDefinitionError(
        new LayerException(
          s"Unable to load class for layerTransform '$layerTransformName'.",
        ),
      )
    }
    // The LayerRegistry is not going to reload the class every time we ask for it.
    // Rather, it is going to load it once. So we have to make new instances
    // ourselves. But Layers are required to have a zero-arg constructor, so
    // we can just call it.
    val newInstance = spiLayerInstance.getClass.getConstructor().newInstance()

    new LayerDriver(layerCompileInfo, layer = newInstance)
  }

}
