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

import org.apache.daffodil.cookers.UpperCaseTokenCooker
import org.apache.daffodil.dsom._
import java.lang.{ Long => JLong }
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthUnits
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthKind
import org.apache.daffodil.layers.LayerTransformer
import org.apache.daffodil.layers.LayerTransformerFactory

/*
 * Layering-related Evaluatables
 */
final class LayerTransformEv(override val expr: CompiledExpression[String], trd: DPathCompileInfo)
  extends EvaluatableConvertedExpression[String, String](
    expr,
    UpperCaseTokenCooker, // cooker insures upper-case and trimmed of whitespace.
    trd)
  with NoCacheEvaluatable[String]

final class LayerEncodingEv(override val expr: CompiledExpression[String], trd: DPathCompileInfo)
  extends EncodingEvBase(expr, trd)

final class LayerCharsetEv(layerEncodingEv: LayerEncodingEv, override val ci: DPathCompileInfo)
  extends CharsetEvBase(layerEncodingEv, ci)

final class LayerLengthInBytesEv(override val expr: CompiledExpression[JLong], override val ci: DPathCompileInfo)
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

final class LayerTransformerEv(
  layerTransformEv: LayerTransformEv,
  maybeLayerCharsetEv: Maybe[LayerCharsetEv],
  maybeLayerLengthKind: Maybe[LayerLengthKind],
  maybeLayerLengthInBytesEv: Maybe[LayerLengthInBytesEv],
  maybeLayerLengthUnits: Maybe[LayerLengthUnits],
  maybeLayerBoundaryMarkEv: Maybe[LayerBoundaryMarkEv],
  ci: DPathCompileInfo)
  extends Evaluatable[LayerTransformer](ci)
  with NoCacheEvaluatable[LayerTransformer] {

  override lazy val runtimeDependencies = layerTransformEv +:
    (maybeLayerCharsetEv.toList ++
      maybeLayerLengthInBytesEv.toList ++
      maybeLayerBoundaryMarkEv.toList)

  /**
   * Finds the proper layer transformer and constructs it with its parameters
   * as needed from the various layer properties.
   */
  override def compute(state: State): LayerTransformer = {
    val layerTransform = layerTransformEv.evaluate(state)
    val factory = LayerTransformerFactory.find(layerTransform, state)
    val xformer = factory.newInstance(
      maybeLayerCharsetEv,
      maybeLayerLengthKind,
      maybeLayerLengthInBytesEv,
      maybeLayerLengthUnits,
      maybeLayerBoundaryMarkEv,
      ci)
    xformer
  }
}
