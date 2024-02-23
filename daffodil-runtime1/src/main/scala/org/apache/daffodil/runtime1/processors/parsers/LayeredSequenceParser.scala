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

package org.apache.daffodil.runtime1.processors.parsers

import org.apache.daffodil.runtime1.layers.LayerRuntimeImpl
import org.apache.daffodil.runtime1.layers.LayerTransformerFactory
import org.apache.daffodil.runtime1.layers.api.LayerUnexpectedException
import org.apache.daffodil.runtime1.processors.SequenceRuntimeData

class LayeredSequenceParser(
  rd: SequenceRuntimeData,
  layerTransformerFactory: LayerTransformerFactory,
  bodyParser: SequenceChildParser,
) extends OrderedUnseparatedSequenceParser(rd, Vector(bodyParser)) {
  override def nom = "LayeredSequence"

  override def parse(state: PState): Unit = {

    // TODO: Separate the creation of layer transformers into layerParseTransformers and layer
    //   unparse transformers. Right now they're blended onto one object.
    //   It should be possible to define only a layer parser if a schema (and its requried layers)
    //   are intended to be used only to parse data.

    val layerRuntimeImpl = new LayerRuntimeImpl(state, rd)
    val layerTransformer = layerTransformerFactory.newInstance(layerRuntimeImpl)
    val savedDIS = state.dataInputStream

    val isAligned = savedDIS.align(layerTransformer.mandatoryLayerAlignmentInBits, state)
    if (!isAligned)
      PE(
        state,
        "Unable to align to the mandatory layer alignment of %s(bits)",
        layerTransformer.mandatoryLayerAlignmentInBits,
      )

    try {
      val newDIS = layerTransformer.addLayer(savedDIS, layerRuntimeImpl)

      state.dataInputStream = newDIS
      super.parse(state)
      layerTransformer.removeLayer(newDIS)
    } catch {
      case pe: ParseError =>
        state.setFailed(pe)
      case e: Exception =>
        throw new LayerUnexpectedException(rd.layerName, e)
    } finally {
      state.dataInputStream = savedDIS
    }
  }
}
