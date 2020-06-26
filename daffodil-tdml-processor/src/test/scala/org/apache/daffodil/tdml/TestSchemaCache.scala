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

package org.apache.daffodil.tdml

import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Test
import java.io.FileOutputStream
import org.apache.daffodil.api.URISchemaSource
import org.junit.Before
import scala.Right

class TestSchemaCache {

  object SCache extends SchemaCache[Null, Null]

  var compileCount = 0
  var originalUSS: URISchemaSource = null
  var newUSS: URISchemaSource = null
  var tempFile: File = null

  @Before def setup: Unit = {
    compileCount = 0
    SCache.resetCache
    tempFile = java.io.File.createTempFile("tdml", "tdml")
    tempFile.deleteOnExit()
    touchFile()
    val originalURI = tempFile.toURI
    originalUSS = URISchemaSource(originalURI)
    val newURI = tempFile.toURI
    newUSS = URISchemaSource(newURI)
  }

  /**
   * Touches the file, insures it happens far enough in the future
   * that the file modification times are different.
   */
  def touchFile(): Unit = {
    val startingModTime = tempFile.lastModified()
    var iters = 0
    while (tempFile.lastModified() <= startingModTime) {
      iters += 1
      Thread.sleep(100)
      val os = new FileOutputStream(tempFile)
      os.write(0)
      os.flush()
      os.close()
    }
    // println("iters = " + iters)
  }

  def compileTheSchema(uss: URISchemaSource): Unit = {
    SCache.compileAndCache(uss, false, false, null, null) {
      compileCount += 1
      uss.newInputSource().getByteStream().close()
      Right(null)
    }
  }

  @Test def testReset: Unit = {
    compileTheSchema(originalUSS)
    SCache.resetCache
  }

  @Test def testSameFileCompiledOnce: Unit = {
    compileTheSchema(originalUSS)
    assertEquals(1, compileCount)
    compileTheSchema(newUSS) // file has not been touched, so this should hit the cache.
    assertEquals(1, compileCount)
  }

  @Test def testSameFileCompiledTwice: Unit = {
    compileTheSchema(originalUSS)
    assertEquals(1, compileCount)

    touchFile()

    compileTheSchema(newUSS) // file has changed
    assertEquals(2, compileCount)

  }

}
