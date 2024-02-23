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
package org.apache.daffodil.layers.runtime1

import java.io.InputStream
import java.io.OutputStream

import org.apache.daffodil.io.BoundaryMarkInsertingJavaOutputStream
import org.apache.daffodil.io.BoundaryMarkLimitingInputStream
import org.apache.daffodil.runtime1.api.DFDLPrimType
import org.apache.daffodil.runtime1.layers.api.Layer
import org.apache.daffodil.runtime1.layers.api.LayerCompileInfo
import org.apache.daffodil.runtime1.layers.api.LayerRuntime
import org.apache.daffodil.runtime1.layers.api.LayerVariable

final class BoundaryMarkLayer extends Layer("boundaryMark") {
  var boundaryMarkVar: LayerVariable = _
  var layerEncodingVar: LayerVariable = _

  override def check(lpi: LayerCompileInfo): Unit = {
    if (boundaryMarkVar == null) {
      boundaryMarkVar =
        lpi.layerVariable("urn:org.apache.daffodil.layers.boundaryMark", "boundaryMark")
      if (boundaryMarkVar.dfdlPrimType != DFDLPrimType.String)
        lpi.schemaDefinitionError("boundaryMark variable does not have 'xs:string' type.")
      layerEncodingVar =
        lpi.layerVariable("urn:org.apache.daffodil.layers.boundaryMark", "layerEncoding")
      if (layerEncodingVar.dfdlPrimType != DFDLPrimType.String)
        lpi.schemaDefinitionError("layerEncoding variable does not have 'xs:string' type.")
    }
  }

  override def wrapLayerDecoder(jis: InputStream, lr: LayerRuntime): InputStream = {
    check(lr)
    val boundaryMark = lr.getString(boundaryMarkVar)
    val layerEncoding = lr.getString(layerEncodingVar)
    val charset = lr.getCharset(layerEncoding)
    val s = new BoundaryMarkLimitingInputStream(jis, boundaryMark, charset)
    s
  }

  override def wrapLayerEncoder(jos: OutputStream, lr: LayerRuntime): OutputStream = {
    check(lr)
    val boundaryMark = lr.getString(boundaryMarkVar)
    val layerEncoding = lr.getString(layerEncodingVar)
    val charset = lr.getCharset(layerEncoding)
    val s = new BoundaryMarkInsertingJavaOutputStream(jos, boundaryMark, charset)
    s
  }
}
