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

package org.apache.daffodil.unparsers.runtime1

import org.apache.daffodil.runtime1.layers.LayerDriver
import org.apache.daffodil.runtime1.processors.SequenceRuntimeData
import org.apache.daffodil.runtime1.processors.unparsers._

class LayeredSequenceUnparser(
  ctxt: SequenceRuntimeData,
  childUnparser: SequenceChildUnparser
) extends OrderedUnseparatedSequenceUnparser(ctxt, Array(childUnparser)) {

  override def nom = "LayeredSequence"

  override def unparse(state: UState): Unit = {

    val originalDOS = state.getDataOutputStream

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
    //
    // TODO: we're not unparsing here, just writing bytes, so perhaps we do not
    // need this cloned state? Everything in layers is byte-centric, so there is
    // no issue of fragment bytes.
    val formatInfoPre = state.asInstanceOf[UStateMain].cloneForSuspension(layerUnderlyingDOS)

    // mark the original DOS as finished--no more data will be unparsed to it.
    // If known, this will carry bit position forward to the layerUnderlyingDOS,
    // or could even make that DOS direct. Note that it is important to use the
    // cloned state from above. This is because the setFinished function stores
    // the formatInfo (as finishedFormatInfo) to be used when delivering
    // content to its previous direct DOS. The deliver content call could be
    // delayed due to suspensions/unfinished DOSs, so it must store an
    // immutable state that won't change as unparsers are evaluated.
    originalDOS.setFinished(formatInfoPre)

    // create a new DOS where unparsers following this layer will unparse
    val layerFollowingDOS = layerUnderlyingDOS.addBuffered()
    var layerDriver: LayerDriver = null
    try {
      layerDriver = LayerDriver(formatInfoPre, ctxt.layerRuntimeData)
      layerDriver.init() // could fail if layer setter code causes errors

      // New layerDOS is where the layer will unparse into. Ultimately anything written
      // to layerDOS ends up, post transform, in layerUnderlyingDOS
      val layerDOS = layerDriver.addOutputLayer(layerUnderlyingDOS)

      // unparse the layer body into layerDOS
      state.setDataOutputStream(layerDOS)
      super.unparse(state)
      // now we're done unparsing the layer recursively.
      // While doing that unparsing, the data output stream may have been split, so the
      // DOS in the state may no longer be the layerDOS.
      //
      // However, it is when whatever DOS is in the state at this point, that, when that
      // DOS is consolidated and written out, that is when the layer is finished
      // and the wrap-up of the layer (such as writing output variables) can occur.
      //
      val endOfLayerUnparseDOS = state.getDataOutputStream
      val formatInfoPost =
        state.asInstanceOf[UStateMain].cloneForSuspension(endOfLayerUnparseDOS)

      // setFinished on this end-of-layer-unparse data-output-stream  ensures
      // that the layerDOS gets close() called on it.
      endOfLayerUnparseDOS.setFinished(formatInfoPost)

      // clean up resources - note however, that due to suspensions, the whole
      // layer stack is potentially still needed, so
      // nothing can be cleaned up at this point.
    } catch {
      case t: Throwable if (layerDriver ne null) => layerDriver.handleThrowable(t)
      case t: Throwable => LayerDriver.handleThrowableWithoutLayer(t)
      // otherwise we have no layer driver, so we were unable to load the layer.
      // just let that propagate.
    } finally {
      // reset the state so subsequent unparsers write to the following DOS
      state.setDataOutputStream(layerFollowingDOS)
    }
  }

}
