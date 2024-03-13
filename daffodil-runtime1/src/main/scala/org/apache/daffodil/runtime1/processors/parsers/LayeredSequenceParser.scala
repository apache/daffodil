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

import org.apache.daffodil.runtime1.dsom.RuntimeSchemaDefinitionError
import org.apache.daffodil.runtime1.layers.LayerDriver
import org.apache.daffodil.runtime1.layers.LayerException
import org.apache.daffodil.runtime1.layers.LayerUnexpectedException
import org.apache.daffodil.runtime1.processors.SequenceRuntimeData

class LayeredSequenceParser(
  rd: SequenceRuntimeData,
  bodyParser: SequenceChildParser,
) extends OrderedUnseparatedSequenceParser(rd, Vector(bodyParser)) {
  override def nom = "LayeredSequence"

  override def parse(state: PState): Unit = {

    val savedDIS = state.dataInputStream
    try {
      val layerDriver = LayerDriver(state, rd.layerRuntimeData)
      val isAligned = savedDIS.align(layerDriver.mandatoryLayerAlignmentInBits, state)
      if (!isAligned)
        PE(
          state,
          "Unable to align to the mandatory layer alignment of %s(bits)",
          layerDriver.mandatoryLayerAlignmentInBits,
        )

      val newDIS = layerDriver.addInputLayer(savedDIS)

      state.dataInputStream = newDIS
      super.parse(state)
      layerDriver.removeInputLayer(newDIS)
    } catch {
      case pe: ParseError =>
        state.setFailed(pe)
      case sde: RuntimeSchemaDefinitionError =>
        throw sde
      case le: LayerException =>
        state.setFailed(state.toProcessingError(le))
      case e: Exception =>
        state.setFailed(state.toProcessingError(new LayerUnexpectedException(e)))
    } finally {
      // Restore the data stream to the original
      state.dataInputStream = savedDIS
    }
  }
}
