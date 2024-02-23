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
package org.apache.daffodil.core.layers

import org.apache.daffodil.core.dsom.SequenceGroupTermBase
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.layers.LayerRegistry
import org.apache.daffodil.runtime1.layers.LayerTransformerFactory
import org.apache.daffodil.runtime1.layers.api.Layer
import org.apache.daffodil.runtime1.layers.api.LayerCompileInfo

object LayerCompiler {

  /**
   * Compiles the layer doing all compile time checks, into a serializable runtime object
   * which is a LayerTransformerFactory
   *
   * Digests the properties and definition of the layer and does compile-time checking
   * for consistency and completeness of the definition.
   *
   * Chooses the right length-limiter class, and instantiates it.
   *
   * Constructs a LayerTransformerFactory which is the serializable runtime data structure
   * used by the LayerParser, and LayerUnparser at runtime.
   */
  def compileLayer(sq: SequenceGroupTermBase): LayerTransformerFactory = {
    val lc = new LayerCompiler(sq)
    val res = lc.compile()
    res
  }
}

/**
 *
 * @param sq
 */
private class LayerCompiler private (sq: SequenceGroupTermBase) {
  Assert.usage(sq.isLayered)

  private def srd = sq.sequenceRuntimeData

  private val layeredSequenceAllowedProps = Seq(
    "ref",
    "layerTransform",
  )

  private lazy val layerTransformName: String = sq.optionLayerTransform.get

  private lazy val layer: Layer = {
    // need to check that only layering properties are specified
    val localProps = sq.formatAnnotation.justThisOneProperties
    val localKeys = localProps.keySet
    val disallowedKeys = localKeys.filterNot(k => layeredSequenceAllowedProps.contains(k))
    if (disallowedKeys.size > 0)
      sq.SDE(
        "Sequence has dfdlx:layerTransform specified, so cannot have non-layering properties: %s",
        disallowedKeys.mkString(", "),
      )
    LayerRegistry.find(layerTransformName, sq)
  }

  /**
   * Gathers all the DFDL schema provided information about  the layer
   * from the layer properties.
   */
  lazy val layerCompileInfo = srd.asInstanceOf[LayerCompileInfo]

  def compile(): LayerTransformerFactory = {

    layer.check(layerCompileInfo) // layer's own checks. E.g., some require layerEncoding.

    new LayerTransformerFactory(layerCompileInfo)
  }

}
