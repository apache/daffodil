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

object LayerSchemaCompiler {

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
   * This compilation produces no output object, as the LayerRuntimeData
   * already contains everything needed. 
   */
  def compile(sq: SequenceGroupTermBase): Unit = {
    val lc = new LayerSchemaCompiler(sq)
    lc.compile
  }
}

private class LayerSchemaCompiler private (sq: SequenceGroupTermBase) {

  Assert.usage(sq.isLayered)

  private def lrd = sq.optionLayerRuntimeData.get

  private val layeredSequenceAllowedProps = Seq("layer") // no other properties are allowed.

  private def checkOnlyAllowedProperties(): Unit = {
    // need to check that only layering properties are specified
    val localProps = sq.formatAnnotation.justThisOneProperties
    val localKeys = localProps.keySet
    val disallowedKeys = localKeys.filterNot(k => layeredSequenceAllowedProps.contains(k))
    if (disallowedKeys.nonEmpty)
      sq.SDE(
        "Sequence has dfdlx:layer specified, so cannot have non-layering properties: %s",
        disallowedKeys.mkString(", ")
      )
  }

  lazy val compile: Unit = {
    checkOnlyAllowedProperties()
    //
    // Note: does not check layer vars against layer code setters/getters.
    // That checking is done elsewhere.
  }

}
