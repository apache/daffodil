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

import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.util.SimpleNamedServiceLoader
import org.apache.daffodil.runtime1.layers.api.Layer

/**
 * Uses Java SPI to dynamically load Layer classes from the class path.
 * (The classes must be listed in an M.services file with the name
 * corresponding the the Layer class full name)
 */
object LayerRegistry {

  private lazy val layerMap: Map[String, Layer] =
    SimpleNamedServiceLoader.loadClass[Layer](classOf[Layer])

  /**
   * This is used to find the Layer corresponding to the use of a Layer
   * at schema compilation time. SDE otherwise.
   */
  def find(spiName: String, context: ThrowsSDE): Layer = {
    val optLayer: Option[Layer] = layerMap.get(spiName)
    if (optLayer.isEmpty) {
      val choices = layerMap.keySet.mkString(", ")
      context.SDE(
        "The dfdlx:layerTransform '%s' was not found. Available choices are: %s",
        spiName,
        choices,
      )
    } else {
      optLayer.get
    }
  }

  /**
   * This is used to find a Layer when creating an instance at runtime.
   *
   * It is the caller's responsibility to issue the proper error if this is
   * not found.
   *
   * @param spiName the name of the schema according to the SPI loader. The Layer.name() method
   *                returns this string. This string includes both name and namespace information.
   * @return
   */
  def findLayer(spiName: String): Option[Layer] = layerMap.get(spiName)
}
