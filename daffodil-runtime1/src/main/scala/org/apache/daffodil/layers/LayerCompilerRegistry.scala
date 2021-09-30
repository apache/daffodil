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
package org.apache.daffodil.layers

import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.util.SimpleNamedServiceLoader


/**
 * Transformers have factories. This lets you find the transformer factory
 * by the name obtained from dfdlx:layerTransform.
 */
object LayerCompilerRegistry {

  private lazy val layerCompilerMap: Map[String, LayerCompiler] =
    SimpleNamedServiceLoader.loadClass[LayerCompiler](classOf[LayerCompiler])

  /**
   * Given name, finds the factory for the transformer. SDE otherwise.
   *
   * The state is passed in order to provide diagnostic context if not found.
   */
  def find(name: String, context: ThrowsSDE): LayerCompiler = {
    val maybeCompiler: Option[LayerCompiler] = layerCompilerMap.get(name)
    if (maybeCompiler.isEmpty) {
      val choices = layerCompilerMap.keySet.mkString(", ")
      context.SDE("The dfdlx:layerTransform '%s' was not found. Available choices are: %s", name, choices)
    } else {
      maybeCompiler.get
    }
  }
}