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

package org.apache.daffodil.grammar.primitives

import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.dsom._
import org.apache.daffodil.layers.LayerCompileInfo
import org.apache.daffodil.layers.LayerRuntimeInfo
import org.apache.daffodil.processors.parsers.{Parser => DaffodilParser}
import org.apache.daffodil.processors.unparsers.{Unparser => DaffodilUnparser}
import org.apache.daffodil.util.Misc
import org.apache.daffodil.processors.parsers.LayeredSequenceParser
import org.apache.daffodil.processors.unparsers.LayeredSequenceUnparser
import org.apache.daffodil.util.Maybe

case class LayeredSequence(sq: SequenceGroupTermBase, bodyTerm: SequenceChild)
  extends Terminal(sq, true) {

  private val srd = sq.sequenceRuntimeData
  private val trd = bodyTerm.termRuntimeData

  val layerCompileInfo = new LayerCompileInfo(
    sq,
    new LayerRuntimeInfo(
      sq.sequenceRuntimeData,
      sq.maybeLayerCharsetEv,
      Maybe.toMaybe(sq.optionLayerLengthKind),
      sq.maybeLayerLengthEv,
      Maybe.toMaybe(sq.optionLayerLengthUnits),
      sq.maybeLayerBoundaryMarkEv
    ))

  private val layerTransformerFactory = {
    val layerCompiler = sq.layerCompiler
    //
    // this layer compilation step allows each layer to check
    // the layering properties at schema-compilation time.
    //
    layerCompiler.compileLayer(layerCompileInfo)
  }

  override def toString() =
    "<" + Misc.getNameFromClass(this) + ">" +
      bodyTerm.toString() +
      "</" + Misc.getNameFromClass(this) + ">"

  lazy val bodyParser = bodyTerm.parser
  lazy val bodyUnparser = bodyTerm.unparser

  lazy val layerRuntimeInfo = layerCompileInfo.layerRuntimeInfo

  override lazy val parser: DaffodilParser =
    new LayeredSequenceParser(srd, layerTransformerFactory,  layerRuntimeInfo, bodyParser)

  override lazy val unparser: DaffodilUnparser = {
    new LayeredSequenceUnparser(srd, layerTransformerFactory, layerRuntimeInfo, bodyUnparser)
  }
}
