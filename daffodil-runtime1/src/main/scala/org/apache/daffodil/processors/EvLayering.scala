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

package org.apache.daffodil.processors

import org.apache.daffodil.dsom._

import java.lang.{Long => JLong}

final class LayerEncodingEv(override val expr: CompiledExpression[String], tci: DPathCompileInfo)
  extends EncodingEvBase(expr, tci)

final class LayerCharsetEv(layerEncodingEv: LayerEncodingEv, override val ci: DPathCompileInfo)
  extends CharsetEvBase(layerEncodingEv, ci)

final class LayerLengthEv(override val expr: CompiledExpression[JLong], override val ci: DPathCompileInfo)
  extends EvaluatableExpression[JLong](
    expr,
    ci)
  with NoCacheEvaluatable[JLong] {
  override lazy val runtimeDependencies = Vector()

  override def compute(state: State): JLong = {
    val v: JLong = super.compute(state)
    if (v < 0) {
      state.SDE("dfdl:length expression result must be non-negative, but was: %d", v)
    }
    v
  }
}

final class LayerBoundaryMarkEv(override val expr: CompiledExpression[String], override val ci: DPathCompileInfo)
  extends EvaluatableExpression[String](
    expr,
    ci)
  with NoCacheEvaluatable[String] {
  override lazy val runtimeDependencies = Vector()
}
