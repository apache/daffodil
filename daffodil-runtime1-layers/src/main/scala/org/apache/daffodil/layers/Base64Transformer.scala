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

import org.apache.daffodil.io.BoundaryMarkLimitingStream
import org.apache.daffodil.io.LayerBoundaryMarkInsertingJavaOutputStream
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthKind

final class Base64MIMELayerCompiler
  extends LayerCompiler("base64_MIME") {

  override def compileLayer(layerCompileInfo: LayerCompileInfo): Base64MIMETransformerFactory = {

    layerCompileInfo.SDEUnless(
      scala.util.Properties.isJavaAtLeast("1.8"),
      "Base64 layer support requires Java 8 (aka Java 1.8).")

    layerCompileInfo.SDEUnless(
      layerCompileInfo.optLayerBoundaryMarkOptConstantValue.isDefined,
      "Property dfdlx:layerBoundaryMark was not defined.")
    layerCompileInfo.SDEUnless(
      layerCompileInfo.optLayerLengthKind.isEmpty ||
        (layerCompileInfo.optLayerLengthKind.get eq LayerLengthKind.BoundaryMark),
      "Only dfdlx:layerLengthKind 'boundaryMark' is supported, but '%s' was specified",
      layerCompileInfo.optLayerLengthKind.get.toString)

    layerCompileInfo.SDEUnless(
      layerCompileInfo.optLayerJavaCharsetOptConstantValue match {
        case Some(Some(_)) => true
        case _ => false
      }, "Property dfdlx:layerEncoding must be defined.")

    val xformer = new Base64MIMETransformerFactory(name)
    xformer
  }
}

final class Base64MIMETransformerFactory(name: String)
  extends LayerTransformerFactory(name) {

  override def newInstance(layerRuntimeInfo: LayerRuntimeInfo)= {
    val xformer = new Base64MIMETransformer(name, layerRuntimeInfo)
    xformer
  }
}

final class Base64MIMETransformer(name: String, layerRuntimeInfo: LayerRuntimeInfo)
  extends LayerTransformer(name, layerRuntimeInfo) {

  override def wrapLayerDecoder(jis: java.io.InputStream): java.io.InputStream = {
    val b64 = java.util.Base64.getMimeDecoder().wrap(jis)
    b64
  }

  override def wrapLimitingStream(state: ParseOrUnparseState, jis: java.io.InputStream) = {
    val javaCharset = layerRuntimeInfo.optLayerCharset(state).get
    val layerBoundaryMark = layerRuntimeInfo.optLayerBoundaryMark(state).get
    val s = BoundaryMarkLimitingStream(jis, layerBoundaryMark, javaCharset)
    s
  }

  override protected def wrapLayerEncoder(jos: java.io.OutputStream): java.io.OutputStream = {
    val b64 = java.util.Base64.getMimeEncoder().wrap(jos)
    b64
  }

  override protected def wrapLimitingStream(state: ParseOrUnparseState, jos: java.io.OutputStream) = {
    val javaCharset = layerRuntimeInfo.optLayerCharset(state).get
    val layerBoundaryMark = layerRuntimeInfo.optLayerBoundaryMark(state).get
    val newJOS = new LayerBoundaryMarkInsertingJavaOutputStream(jos, layerBoundaryMark, javaCharset)
    newJOS
  }
}

