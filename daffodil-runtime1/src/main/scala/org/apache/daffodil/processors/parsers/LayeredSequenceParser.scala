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

package org.apache.daffodil.processors.parsers

import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.processors.LayerTransformerEv
import org.apache.daffodil.io.ByteBufferDataInputStream

class LayeredSequenceParser(rd: TermRuntimeData,
  layerTransformerEv: LayerTransformerEv,
  bodyParser: Parser)
  extends SequenceCombinatorParser(rd, bodyParser) {
  override def nom = "LayeredSequence"

  override lazy val runtimeDependencies = Seq(layerTransformerEv)

  override def parse(state: PState): Unit = {

    val layerTransformer = layerTransformerEv.evaluate(state)
    val savedDIS = state.dataInputStream

    val isAligned = savedDIS.align(layerTransformer.mandatoryLayerAlignmentInBits, state)
    if (!isAligned)
      PE(state, "Unable to align to the mandatory layer alignment of %s(bits)",
        layerTransformer.mandatoryLayerAlignmentInBits)

    val newDIS = layerTransformer.addLayer(savedDIS, state)

    //
    // FIXME: Cast should not be needed
    //
    state.dataInputStream = newDIS.asInstanceOf[ByteBufferDataInputStream]

    super.parse(state)

    layerTransformer.removeLayer(newDIS)

    state.dataInputStream = savedDIS
  }
}
