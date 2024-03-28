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

import java.io.ByteArrayOutputStream
import java.util.zip.GZIPOutputStream

import org.apache.commons.io.IOUtils
import org.junit.Assert._
import org.junit.Test

class TestGzipLayer {

  val gl = new GZipLayer // default constructor has to work.

  @Test def testConstruction() = {
    assertNotNull(gl)
  }

  /**
   * Tests that our gzip fixer fixes up byte 9 of the gzip header
   * so that it has 255 in it for java versions prior to Java 16
   */
  @Test def testByte9() = {
    val baos = new ByteArrayOutputStream()
    val baosRaw = new ByteArrayOutputStream()
    val gzos = gl.wrapLayerOutput(baos)
    val gzosRaw = new GZIPOutputStream(baosRaw)
    val data = "testing 1, 2, 3"
    IOUtils.write(data, gzos, "ascii")
    IOUtils.write(data, gzosRaw, "ascii")
    gzos.close()
    gzosRaw.close()
    val byte9 = baos.toByteArray.apply(9)
    val byte9Raw = baosRaw.toByteArray.apply(9)
    if (GZipLayer.fixIsNeeded()) {
      assertEquals(0, byte9Raw.toInt & 0xff)
      assertEquals(255, byte9.toInt & 0xff)
    } else {
      assertEquals(255, byte9Raw.toInt & 0xff)
      assertEquals(255, byte9.toInt & 0xff)
    }
  }
}
