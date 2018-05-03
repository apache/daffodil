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
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.processors.LayerLengthInBytesEv
import org.apache.daffodil.processors.LayerBoundaryMarkEv
import org.apache.daffodil.processors.LayerCharsetEv
import org.apache.daffodil.processors.parsers.PState
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.processors.charset.AIS_PAYLOAD_ARMORING
import java.nio._
import java.nio.charset._
import java.io._
import org.apache.commons.io.IOUtils
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.io.BoundaryMarkLimitingStream
import org.apache.daffodil.io.LayerBoundaryMarkInsertingJavaOutputStream

object AISPayloadArmoringTransformer {
  val iso8859 = StandardCharsets.ISO_8859_1
}

class AISPayloadArmoringTransformer()
  extends LayerTransformer() {
  import AISPayloadArmoringTransformer._

  /**
   * Decoding AIS payload armoring is encoding the ASCII text into the
   * underlying binary data.
   */
  override def wrapLayerDecoder(jis: java.io.InputStream) = {
    new AISPayloadArmoringInputStream(jis)
  }

  override def wrapLimitingStream(jis: java.io.InputStream, state: PState) = {
    val layerBoundaryMark = ","
    val s = BoundaryMarkLimitingStream(jis, layerBoundaryMark, iso8859)
    s
  }

  override protected def wrapLayerEncoder(jos: java.io.OutputStream): java.io.OutputStream = {
    new AISPayloadArmoringOutputStream(jos)
  }

  override protected def wrapLimitingStream(jos: java.io.OutputStream, state: UState): java.io.OutputStream = {
    val layerBoundaryMark = ","
    val newJOS = new LayerBoundaryMarkInsertingJavaOutputStream(jos, layerBoundaryMark, iso8859)
    newJOS
  }
}

class AISPayloadArmoringInputStream(jis: InputStream)
  extends InputStream {
  import AISPayloadArmoringTransformer._

  private lazy val enc = AIS_PAYLOAD_ARMORING.newEncoder()

  private lazy val bais = {
    val armoredText = IOUtils.toString(jis, iso8859)
    val cb = CharBuffer.wrap(armoredText)

    val numBytes = (6 * armoredText.length) / 8
    val ba = new Array[Byte](numBytes + 1)
    val bb = ByteBuffer.wrap(ba)
    val cr = enc.encode(cb, bb, true)
    //
    // We made bb have one extra byte in it.
    // So this should end on UNDERFLOW meaning it could take more characters.
    //
    Assert.invariant(cr.isUnderflow())

    val bais = new ByteArrayInputStream(ba, 0, numBytes)
    bais
  }

  override def read(): Int = {
    bais.read()
  }
}

class AISPayloadArmoringOutputStream(jos: java.io.OutputStream)
  extends OutputStream {
  import AISPayloadArmoringTransformer._

  private lazy val dec = AIS_PAYLOAD_ARMORING.newDecoder()

  private val baos = new ByteArrayOutputStream()

  private var closed = false

  override def close(): Unit = {
    if (!closed) {
      val ba = baos.toByteArray()
      val bb = ByteBuffer.wrap(ba)
      // must call our own decode. This is not a regular java CharsetDecoder, its Daffodil's BitsCharsetDecoder
      val cb = dec.decode(bb)
      IOUtils.write(cb.toString, jos, iso8859)
      jos.close()
      closed = true
    }
  }

  override def write(bInt: Int): Unit = {
    baos.write(bInt)
  }

}

object AISPayloadArmoringTransformerFactory
  extends LayerTransformerFactory("aisPayloadArmor") {

  override def newInstance(maybeLayerCharsetEv: Maybe[LayerCharsetEv],
    maybeLayerLengthKind: Maybe[LayerLengthKind],
    maybeLayerLengthInBytesEv: Maybe[LayerLengthInBytesEv],
    maybeLayerLengthUnits: Maybe[LayerLengthUnits],
    maybeLayerBoundaryMarkEv: Maybe[LayerBoundaryMarkEv],
    trd: TermRuntimeData): LayerTransformer = {

    val xformer = new AISPayloadArmoringTransformer()
    xformer
  }
}
