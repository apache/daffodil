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

package org.apache.daffodil.layers

import org.apache.daffodil.schema.annotation.props.gen.LayerLengthKind
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthUnits
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.processors.LayerLengthInBytesEv
import org.apache.daffodil.processors.LayerBoundaryMarkEv
import org.apache.daffodil.processors.LayerCharsetEv
import org.apache.daffodil.processors.parsers.PState
import org.apache.daffodil.io.ExplicitLengthLimitingStream
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.dsom.DPathCompileInfo

class GZIPTransformer(layerLengthInBytesEv: LayerLengthInBytesEv)
  extends LayerTransformer() {

  override def wrapLayerDecoder(jis: java.io.InputStream) = {
    val s = new java.util.zip.GZIPInputStream(jis)
    s
  }

  override def wrapLimitingStream(jis: java.io.InputStream, state: PState) = {
    val layerLengthInBytes: Int = layerLengthInBytesEv.evaluate(state).toInt
    val s = new ExplicitLengthLimitingStream(jis, layerLengthInBytes)
    s
  }

  override protected def wrapLayerEncoder(jos: java.io.OutputStream): java.io.OutputStream = {
    val s = new java.util.zip.GZIPOutputStream(jos)
    s
  }

  override protected def wrapLimitingStream(jis: java.io.OutputStream, state: UState): java.io.OutputStream = {
    jis // just return jis. The way the length will be used/stored is by way of
    // taking the content length of the enclosing element. That will measure the
    // length relative to the "ultimate" data output stream.
  }
}

object GZIPTransformerFactory
  extends LayerTransformerFactory("gzip") {

  override def newInstance(
    maybeLayerCharsetEv: Maybe[LayerCharsetEv],
    maybeLayerLengthKind: Maybe[LayerLengthKind],
    maybeLayerLengthInBytesEv: Maybe[LayerLengthInBytesEv],
    maybeLayerLengthUnits: Maybe[LayerLengthUnits],
    maybeLayerBoundaryMarkEv: Maybe[LayerBoundaryMarkEv],
    tci: DPathCompileInfo): LayerTransformer = {

    val xformer = new GZIPTransformer(maybeLayerLengthInBytesEv.get)
    xformer
  }
}
