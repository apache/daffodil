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

import org.apache.daffodil.layers.LayerRuntimeInfo
import org.apache.daffodil.layers.LayerTransformerFactory
import org.apache.daffodil.processors.SequenceRuntimeData

class LayeredSequenceUnparser(ctxt: SequenceRuntimeData,
  layerTransformerFactory: LayerTransformerFactory,
  layerRuntimeInfo: LayerRuntimeInfo,
  childUnparser: SequenceChildUnparser)
  extends OrderedUnseparatedSequenceUnparser(ctxt, Seq(childUnparser)) {

  override lazy val runtimeDependencies =
    layerRuntimeInfo.evaluatables.toVector

  override def nom = "LayeredSequence"

  override def unparse(state: UState): Unit = {
    val layerTransformer = layerTransformerFactory.newInstance(layerRuntimeInfo)

    val originalDOS = state.dataOutputStream

    // create a new buffered DOS that this layer will flush to when the layer
    // completes unparsing. This ensures that any fragment bits or bitOrder
    // state in the original DOS does not affect how the layer flushes bytes to
    // the underlying DOS. Only when this new DOS is delivered to the
    // originalDOS will the bitOrder checks be done.
    val layerUnderlyingDOS = originalDOS.addBuffered()

    // clone the UState to be used when the layer flushes its buffered content
    // to layerUnderlyingDOS. This layer isn't a suspension, but this gives us
    // an immutable clone that the layer can safely use. This is important
    // since the flushing of the layer might be delayed due to suspensions. By
    // getting an immutable state, we ensure that the flushing of the layer
    // occurs with the state at this point.
    val formatInfoPre = state.asInstanceOf[UStateMain].cloneForSuspension(layerUnderlyingDOS)

    // mark the original DOS as finished--no more data will be unparsed to it.
    // If known, this will carry bit position forward to the layerUnerlyingDOS,
    // or could even make that DOS direct. Note that it is important to use the
    // cloned state from above. This is because the setFinished function stores
    // the formatInfo (as finishedFormatInfo) to be used when delivering
    // content to its previous direct DOS. The deliver content call could be
    // delayed due to suspensions/unfinished DOSs, so it must store an
    // immutable state that won't change as unparsers are evaluated.
    originalDOS.setFinished(formatInfoPre)

    // create a new DOS where unparsers following this layer will unparse
    val layerFollowingDOS = layerUnderlyingDOS.addBuffered()

    // New layerDOS is where the layer will unparse into. Ultimately anything written
    // to layerDOS ends up, post transform, in layerUnderlyingDOS
    val layerDOS = layerTransformer.addLayer(layerUnderlyingDOS, state, formatInfoPre)

    // unparse the layer body into layerDOS
    state.dataOutputStream = layerDOS
    super.unparse(state)
    layerTransformer.endLayerForUnparse(state)

    // now we're done unparsing the layer, so finalize the last DOS in the
    // chain. Note that there might be suspensions so some parts of the
    // layerDOS chain may not be finished. When those suspensions are all
    // finished, the layerDOS content will be written to the
    // layerUnderlyingDOS, which will subsequently be finished. Like above, it
    // is important to pass an immutable UState into the setFinished function
    // because that state is stored in the DOS (as finishedFormatInfo) and its
    // use may be delayed to after the actual UState has been changed. The
    // UState has almost certainly changed since the last cloneForSuspension
    // call, so we need a new clone to finish this DOS.
    val layerDOSLast = layerDOS.lastInChain
    val formatInfoPost = state.asInstanceOf[UStateMain].cloneForSuspension(layerDOSLast)
    layerDOSLast.setFinished(formatInfoPost)

    // clean up resources - note however, that due to suspensions, the whole
    // layer stack is potentially still needed, so not clear what can be
    // cleaned up at this point.
    layerTransformer.removeLayer(layerDOS, state)

    // reset the state so subsequent unparsers write to the following DOS
    state.dataOutputStream = layerFollowingDOS
  }

}

