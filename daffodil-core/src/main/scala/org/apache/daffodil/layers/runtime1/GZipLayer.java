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

package org.apache.daffodil.layers.runtime1;

import org.apache.daffodil.runtime1.layers.api.Layer;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Objects;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

public final class GZipLayer extends Layer {

  private static Boolean fixNeeded = null;

  public static boolean fixIsNeeded() {
    if (Objects.isNull(fixNeeded)) {
      // prior to java 16
      String versionString = System.getProperty("java.version");

      // Extract the major version using string manipulation
      int majorVersion;
      if (versionString.startsWith("1.8")) {
        majorVersion = 8;
      } else {
        String[] parts = versionString.split("\\.");
        assert(parts.length > 0);
        majorVersion = Integer.parseInt(parts[0]);
      }
      fixNeeded = (majorVersion < 16);
    }
    return fixNeeded;
  }

  public GZipLayer() {
    super("gzip", "urn:org.apache.daffodil.layers.gzip");
    // convert IOExceptions to Processing Errors
    setProcessingErrorException(IOException.class);
  }

  @Override
  public InputStream wrapLayerInput(InputStream jis) throws Exception {
    return new GZIPInputStream(jis);
  }

  @Override
  public OutputStream wrapLayerOutput(OutputStream jos) throws Exception {
    OutputStream fixedOS = fixIsNeeded() ? new GZIPFixedOutputStream(jos) : jos;
    return new GZIPOutputStream(fixedOS);
  }

}

/**
 * Prior to Java 16, the java.util.zip.GZIPOutputStream wrote a value of zero for
 * the OS field in the header (byte index 9).
 * In Java 16, this was changed to a
 * value of 255 to better abide by the GZIP specification. Unfortunately, this
 * means unparsed data using a GZIP layer might have a single byte difference,
 * depending on the Java version used. This can lead to inconsistent behavior of
 * test failures that expect a certain byte value.
 * <p>
 * To resolve this issue, we create this GZIPFixedOutputStream. This should wrap
 * the underlying OutputStream and be passed as the OutputStream to the
 * GZIPOutputStream. When the GZIPOutputStream writes the 9th byte to this
 * GZIPFixedOutputStream, this will always write a value of 255, making all Java
 * versions prior to 16 consistent with Java 16+ behavior.
 */
class GZIPFixedOutputStream extends OutputStream {

  private final OutputStream os;

  public GZIPFixedOutputStream(OutputStream os) {
    this.os = os;
  }

  /**
   * The next byte position that byte will be written to. If this is negative,
   * that means we have already fixed the output and everything should just
   * pass straight through.
   */
  private int bytePosition = 0;

  @Override
  public void close() throws IOException { os.close(); }

  @Override
  public void flush() throws IOException { os.flush(); }

  @Override
  public void write(byte[] b, int off, int len) throws IOException {
    if (bytePosition < 0) {
      // The bad byte has been fixed, pass all writes directly through to the
      // underlying OutputStream. This may be more efficient than the default
      // OutputStream write() function, which writes the bytes from this array
      // one at a time
      os.write(b, off, len);
    } else {
      // The bad byte has not been fixed yet. Unless a newer version of Java
      // has made changes, the GZIPOutputStreamm will have passed in a 10 byte
      // array to this function that includes the bad byte. Let's just write
      // that array using the default write(array) method that writes these
      // bytes one at a time and will call the write(int) method that will fix
      // that byte. Calling write() one at a time is maybe inefficient but for
      // such a small array it should not have a noticeable effect.
      super.write(b, off, len);
    }
  }

  @Override
  public void write(int b) throws IOException {
    if (bytePosition < 0) {
      // The bad byte has already been fixed, simply pass this byte through to
      // the underlying OutputStream
      os.write(b);
    } else if (bytePosition < 9) {
      // The bad byte has not been fixed, and we haven't reached it yet, simply
      // pass this byte through and increment our byte position
      os.write(b);
      bytePosition += 1;
    } else if (bytePosition == 9) {
      // This is the bad byte, it is a 0 on some Java versions. Write 255
      // instead of to match Java 16+ behavior. Also, set bytePosition to -1 to
      // signify that we have fixed the bad byte and that all other writes
      // should just pass directly to the underlying OutputStream
      os.write(255);
      bytePosition = -1;
    }
  }
}
