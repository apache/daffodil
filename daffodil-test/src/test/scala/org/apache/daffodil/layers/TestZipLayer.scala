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

import org.apache.commons.compress.archivers.zip.ZipArchiveEntry
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream
import org.apache.commons.io.IOUtils
import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.apache.daffodil.util.Misc
import org.junit.AfterClass
import org.junit.Assert.assertEquals

import java.io.BufferedInputStream
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

object TestZipLayer {
  lazy val testDir = "/org/apache/daffodil/layers/"
  lazy val runner = Runner(testDir, "ziplayer.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}

class TestZipLayer {

  import TestZipLayer._

  @Test def test_zip1p(): Unit = { runner.runOneTest("zipLayer1p") }

  @Test def test_zip1u(): Unit = { runner.runOneTest("zipLayer1u") }

  @Test def zip1() : Unit = {
    val hex = """50 4b 03 04 0a 03 00 00 00 00 46 71 36 52 b5 37
                |3c 98 10 00 00 00 10 00 00 00 07 00 00 00 66 6f
                |6f 2e 74 78 74 30 31 32 33 34 35 36 37 38 39 41
                |42 43 44 45 46 50 4b 01 02 3f 03 0a 03 00 00 00
                |00 46 71 36 52 b5 37 3c 98 10 00 00 00 10 00 00
                |00 07 00 24 00 00 00 00 00 00 00 20 80 b4 81 00
                |00 00 00 66 6f 6f 2e 74 78 74 0a 00 20 00 00 00
                |00 00 01 00 18 00 00 c2 3d 35 f2 f0 d6 01 80 39
                |cc 3b f2 f0 d6 01 00 c2 3d 35 f2 f0 d6 01 50 4b
                |05 06 00 00 00 00 01 00 01 00 59 00 00 00 35 00
                |00 00 00 00""".stripMargin.replaceAll("\\s", "")
    val bytes = Misc.hex2Bytes(hex)
    // println("data length = " + bytes.size)
    val bais = new BufferedInputStream(new ByteArrayInputStream(bytes))
    val zis = new ZipArchiveInputStream(bais)
    var e: ZipArchiveEntry = zis.getNextZipEntry()
    while (e ne null) {
      assertEquals("foo.txt", e.getName())
      val data = IOUtils.toString(zis, StandardCharsets.UTF_8)
      assertEquals("0123456789ABCDEF", data)
      e = zis.getNextZipEntry()
    }
  }
}
