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
import org.apache.daffodil.io.ExplicitLengthLimitingStream
import org.apache.daffodil.processors.ParseOrUnparseState

final class GZIPLayerCompiler
  extends LayerCompiler("gzip") {

  override def compileLayer(layerCompileInfo: LayerCompileInfo): GZIPTransformerFactory = {

    layerCompileInfo.SDEUnless(
      layerCompileInfo.optLayerLengthKind.isEmpty ||
        (layerCompileInfo.optLayerLengthKind.get eq LayerLengthKind.Explicit),
      "Only dfdlx:layerLengthKind 'explicit' is supported, but '%s' was specified",
      layerCompileInfo.optLayerLengthKind.get.toString)

    val xformer = new GZIPTransformerFactory(name)
    xformer
  }
}

final class GZIPTransformerFactory(name: String)
  extends LayerTransformerFactory(name) {

  override def newInstance(layerRuntimeInfo: LayerRuntimeInfo)= {
    val xformer = new GZIPTransformer(name, layerRuntimeInfo)
    xformer
  }
}

class GZIPTransformer(name: String, layerRuntimeInfo: LayerRuntimeInfo)
  extends LayerTransformer(name, layerRuntimeInfo) {

  override def wrapLayerDecoder(jis: java.io.InputStream) = {
    val s = new java.util.zip.GZIPInputStream(jis)
    s
  }

  override def wrapLimitingStream(state: ParseOrUnparseState, jis: java.io.InputStream) = {
    val layerLengthInBytes = layerRuntimeInfo.optLayerLength(state).get
    val s = new ExplicitLengthLimitingStream(jis, layerLengthInBytes)
    s
  }

  override protected def wrapLayerEncoder(jos: java.io.OutputStream): java.io.OutputStream = {
    val s = GZIPFixedOutputStream(jos)
    s
  }

  override protected def wrapLimitingStream(state: ParseOrUnparseState, jis: java.io.OutputStream) = {
    jis // just return jis. The way the length will be used/stored is by way of
    // taking the content length of the enclosing element. That will measure the
    // length relative to the "ultimate" data output stream.
  }
}

object GZIPFixedOutputStream {

  private val fixIsNeeded = !scala.util.Properties.isJavaAtLeast("16")

  /**
   * Create a GZIPOutputStream that, if necessary, proxies writes through an
   * OutputStream that fixes inconsistencies between Java versions
   */
  def apply(os: java.io.OutputStream) = {
    val fixedOS = if (fixIsNeeded) new GZIPFixedOutputStream(os) else os
    new java.util.zip.GZIPOutputStream(fixedOS)
  }
}

/**
 * Prior to Java 16, the java.util.zip.GZIPOutputStream wrote a value of zero for
 * the OS field in the header (byte index 9). In Java 16, this was changed to a
 * value of 255 to better abide by the GZIP specification. Unfortunately, this
 * means unparsed data using a GZIP layer might have a single byte difference,
 * depending on the Java version used. This can lead to inconsistent behavior of
 * test failures that expect a certain byte value.
 *
 * To resolve this issue, we create this GZIPFixedOutputStream. This should wrap
 * the underlying OutputStream and be passed as the OutputStream to the
 * GZIPOutputStream. When the GZIPOutputStream writes the 9th byte to this
 * GZIPFixedOutputStream, this will always write a value of 255, making all Java
 * versions prior to 16 consistent with Java 16+ behavior.
 */
class GZIPFixedOutputStream private (os: java.io.OutputStream) extends java.io.OutputStream {

  /**
   * The next byte position that byte will be written to. If this is negative,
   * that means we have already fixed the output and everything should just
   * pass straight through.
   */
  private var bytePosition = 0

  override def close(): Unit = os.close()
  override def flush(): Unit = os.flush()

  override def write(b: Array[Byte], off: Int, len: Int): Unit = {
    if (bytePosition < 0) {
      // The bad byte has been fixed, pass all writes directly through to the
      // underlying OutputStream. This may be more efficient than the default
      // OutputStream write() function, which writes the bytes from this array
      // one at a time
      os.write(b, off, len)
    } else {
      // The bad byte has not been fixed yet. Unless a newer version of Java
      // has made changes, the GZIPOutputStreamm will have passed in a 10 byte
      // array to this function that includes the bad byte. Let's just write
      // that array using the default write(array) method that writes these
      // bytes one at a time and will call the write(int) method that will fix
      // that byte. Calling write() one at a time is maybe inefficient but for
      // such a small array it should not have a noticeable effect.
      super.write(b, off, len)
    }
  }

  override def write(b: Int): Unit = {
    if (bytePosition < 0) {
      // The bad byte has already been fixed, simply pass this byte through to
      // the underlying OutputStream
      os.write(b)
    } else if (bytePosition < 9) {
      // The bad byte has not been fixed, and we haven't reached it yet, simply
      // pass this byte through and increment our byte position
      os.write(b)
      bytePosition += 1
    } else if (bytePosition == 9) {
      // This is the bad byte, it is a 0 on some Java versions. Write 255
      // instead of to match Java 16+ behavior. Also, set bytePosition to -1 to
      // signify that we have fixed the bad byte and that all other writes
      // should just pass directly to the underlying OutputStream
      os.write(255)
      bytePosition = -1
    }
  }
}
