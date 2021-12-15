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

import org.apache.daffodil.layers.LayerNotEnoughDataException
import org.apache.daffodil.layers.LayerRuntimeInfo
import org.apache.daffodil.layers.LayerTransformerFactory
import org.apache.daffodil.processors.SequenceRuntimeData
import org.apache.daffodil.util.MaybeULong

class LayeredSequenceParser(
  rd: SequenceRuntimeData,
  layerTransformerFactory: LayerTransformerFactory,
  layerRuntimeInfo: LayerRuntimeInfo,
  bodyParser: SequenceChildParser)
  extends OrderedUnseparatedSequenceParser(rd, Vector(bodyParser)) {
  override def nom = "LayeredSequence"

  override lazy val runtimeDependencies =
    layerRuntimeInfo.evaluatables.toVector

  override def parse(state: PState): Unit = {

    val layerTransformer = layerTransformerFactory.newInstance(layerRuntimeInfo)
    val savedDIS = state.dataInputStream

    val isAligned = savedDIS.align(layerTransformer.mandatoryLayerAlignmentInBits, state)
    if (!isAligned)
      PE(state, "Unable to align to the mandatory layer alignment of %s(bits)",
        layerTransformer.mandatoryLayerAlignmentInBits)

    try {
      val newDIS = layerTransformer.addLayer(savedDIS, state)

      state.dataInputStream = newDIS
      layerTransformer.startLayerForParse(state)
      super.parse(state)
      layerTransformer.removeLayer(newDIS)
    } catch {
      case le: LayerNotEnoughDataException =>
        PENotEnoughBits(state, le.schemaFileLocation, le.dataLocation, le.nBytesRequired * 8, MaybeULong.Nope)
    } finally {
      state.dataInputStream = savedDIS
    }
  }
}
