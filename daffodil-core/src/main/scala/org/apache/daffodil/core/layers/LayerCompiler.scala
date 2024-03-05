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
import org.apache.daffodil.lib.xml.RefQName
import org.apache.daffodil.runtime1.layers.LayerFactory
import org.apache.daffodil.runtime1.layers.LayerRegistry
import org.apache.daffodil.runtime1.layers.LayerRuntimeData
import org.apache.daffodil.runtime1.layers.api.Layer

object LayerCompiler {

  /**
   * Compiles the layer.
   *
   * This is mostly checking for errant use of DFDL properties that
   * can't be used on layers.
   *
   * The real compilation - creating runtime data structures based
   * on constructor signatures and DFDL variables that are in the
   * layer's namespace, that is called here, but it is part of
   * the daffodil runtime because that step has to be carried out
   * *also* at runtime to verify that the dynamically loaded layer
   * classes are compatible with the variables and their definitions.
   *
   * Constructs a LayerFactory which is the serializable runtime data structure
   * used by the LayerParser, and LayerUnparser at runtime.
   */
  def compileLayer(sq: SequenceGroupTermBase): LayerFactory = {
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
  private def lrd = sq.optionLayerRuntimeData.get

  private def layerQName: RefQName = lrd.layerQName
  private def layerName = layerQName.local
  private def layerNamespace = layerQName.namespace

  private val layeredSequenceAllowedProps = Seq(
    "layer", // no other properties are allowed.
  )
  private def checkOnlyAllowedProperties(): Unit = {
    // need to check that only layering properties are specified
    val localProps = sq.formatAnnotation.justThisOneProperties
    val localKeys = localProps.keySet
    val disallowedKeys = localKeys.filterNot(k => layeredSequenceAllowedProps.contains(k))
    if (disallowedKeys.nonEmpty)
      sq.SDE(
        "Sequence has dfdlx:layer specified, so cannot have non-layering properties: %s",
        disallowedKeys.mkString(", "),
      )
  }

  private lazy val layer: Layer = {
    checkOnlyAllowedProperties()
    LayerRegistry.find(lrd.spiName, sq)
  }

  /**
   * Gathers all the DFDL schema provided information about  the layer
   * from the layer properties
   */
  private def layerRuntimeData: LayerRuntimeData = srd.layerRuntimeData

  def compile(): LayerFactory = {

    LayerFactory.computeLayerVarsRuntime(layerRuntimeData, layer)
    new LayerFactory(layerRuntimeData)
  }

}
