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

package org.apache.daffodil.processors.unparsers

import org.apache.daffodil.processors.LayerTransformerEv
import org.apache.daffodil.io.DirectOrBufferedDataOutputStream
import org.apache.daffodil.processors.SequenceRuntimeData

class LayeredSequenceUnparser(ctxt: SequenceRuntimeData,
  layerTransformerEv: LayerTransformerEv,
  childUnparser: SequenceChildUnparser)
  extends OrderedUnseparatedSequenceUnparser(ctxt, Seq(childUnparser)) {

  override lazy val runtimeDependencies = Vector(layerTransformerEv)

  override def nom = "LayeredSequence"

  override def unparse(state: UState): Unit = {
    val layerTransformer = layerTransformerEv.evaluate(state)

    val originalDOS = state.dataOutputStream // layer will output to the original, then finish it upon closing.

    val newDOS = originalDOS.addBuffered // newDOS is where unparsers after this one returns will unparse into.

    //
    // FIXME: Cast should not be necessary
    //
    // New layerDOS is where the layer will unparse into. Ultimately anything written
    // to layerDOS ends up, post transform, in originalDOS.
    //
    val layerDOS = layerTransformer.addLayer(originalDOS, state).asInstanceOf[DirectOrBufferedDataOutputStream]

    // unparse the layer body into layerDOS
    state.dataOutputStream = layerDOS
    super.unparse(state)

    // now we're done with the layer, so finalize the layer
    layerDOS.lastInChain.setFinished(state)

    // clean up resources - note however, that due to suspensions, the whole
    // layer stack is potentially still needed, so not clear what can be
    // cleaned up at this point.
    //
    layerTransformer.removeLayer(layerDOS, state)
    state.dataOutputStream = newDOS
  }

}

