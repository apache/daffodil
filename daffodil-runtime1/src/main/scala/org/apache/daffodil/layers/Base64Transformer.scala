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
import org.apache.daffodil.io.BoundaryMarkLimitingStream
import org.apache.daffodil.processors.parsers.PState
import org.apache.daffodil.processors.charset.BitsCharsetJava
import java.nio.charset.Charset
import org.apache.daffodil.processors.charset.BitsCharset
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.io.LayerBoundaryMarkInsertingJavaOutputStream
import org.apache.daffodil.dsom.DPathCompileInfo

class Base64MIMETransformer(layerCharsetEv: LayerCharsetEv, layerBoundaryMarkEv: LayerBoundaryMarkEv)
  extends LayerTransformer() {

  override def wrapLayerDecoder(jis: java.io.InputStream): java.io.InputStream = {
    val b64 = java.util.Base64.getMimeDecoder().wrap(jis)
    b64
  }

  override def wrapLimitingStream(jis: java.io.InputStream, state: PState) = {
    val layerCharset: BitsCharset = layerCharsetEv.evaluate(state)
    val layerBoundaryMark = layerBoundaryMarkEv.evaluate(state)
    val javaCharset: Charset = layerCharset match {
      case jbcs: BitsCharsetJava => jbcs.javaCharset
      case _ => Assert.invariantFailed("Not a java-compatible charset: " + layerCharset)
    }
    val s = BoundaryMarkLimitingStream(jis, layerBoundaryMark, javaCharset)
    s
  }

  override protected def wrapLayerEncoder(jos: java.io.OutputStream): java.io.OutputStream = {
    val b64 = java.util.Base64.getMimeEncoder().wrap(jos)
    b64
  }

  override protected def wrapLimitingStream(jos: java.io.OutputStream, state: UState): java.io.OutputStream = {
    val layerCharset: BitsCharset = layerCharsetEv.evaluate(state)
    val layerBoundaryMark = layerBoundaryMarkEv.evaluate(state)
    val javaCharset: Charset = layerCharset match {
      case jbcs: BitsCharsetJava => jbcs.javaCharset
      case _ => Assert.invariantFailed("Not a java-compatible charset: " + layerCharset)
    }
    val newJOS = new LayerBoundaryMarkInsertingJavaOutputStream(jos, layerBoundaryMark, javaCharset)
    newJOS
  }
}

object Base64MIMETransformerFactory
  extends LayerTransformerFactory("base64_MIME") {

  override def newInstance(
    maybeLayerCharsetEv: Maybe[LayerCharsetEv],
    maybeLayerLengthKind: Maybe[LayerLengthKind],
    maybeLayerLengthInBytesEv: Maybe[LayerLengthInBytesEv],
    maybeLayerLengthUnits: Maybe[LayerLengthUnits],
    maybeLayerBoundaryMarkEv: Maybe[LayerBoundaryMarkEv],
    tci: DPathCompileInfo): LayerTransformer = {

    tci.schemaDefinitionUnless(
      scala.util.Properties.isJavaAtLeast("1.8"),
      "Base64 layer support requires Java 8 (aka Java 1.8).")

    tci.schemaDefinitionUnless(
      maybeLayerBoundaryMarkEv.isDefined,
      "Property dfdlx:layerBoundaryMark was not defined.")
    tci.schemaDefinitionUnless(
      maybeLayerLengthKind.isEmpty ||
        (maybeLayerLengthKind.get eq LayerLengthKind.BoundaryMark),
      "Only dfdlx:layerLengthKind 'boundaryMark' is supported, but '%s' was specified",
      maybeLayerLengthKind.get.toString)

    tci.schemaDefinitionUnless(
      maybeLayerCharsetEv.isDefined,
      "Property dfdlx:layerEncoding must be defined.")

    val xformer = new Base64MIMETransformer(maybeLayerCharsetEv.get, maybeLayerBoundaryMarkEv.get)
    xformer
  }
}
