/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package org.apache.daffodil.tdml

import java.io.File
import junit.framework.Assert.assertEquals
import org.junit.Test
import org.apache.daffodil.Implicits._; object INoWarnTDML2 { ImplicitsSuppressUnusedImportWarning() }
import java.io.FileOutputStream
import org.apache.daffodil.api.URISchemaSource
import org.junit.Before

class TestSchemaCache {

  object SCache extends SchemaCache[Null, Null]

  var compileCount = 0
  var originalUSS: URISchemaSource = null
  var newUSS: URISchemaSource = null
  var tempFile: File = null

  @Before def setup {
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
  def touchFile() {
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

  def compileTheSchema(uss: URISchemaSource) {
    SCache.compileAndCache(uss, false) {
      compileCount += 1
      uss.newInputSource().getByteStream().close()
      Right(null)
    }
  }

  @Test def testReset {
    compileTheSchema(originalUSS)
    SCache.resetCache
  }

  @Test def testSameFileCompiledOnce {
    compileTheSchema(originalUSS)
    assertEquals(1, compileCount)
    compileTheSchema(newUSS) // file has not been touched, so this should hit the cache.
    assertEquals(1, compileCount)
  }

  @Test def testSameFileCompiledTwice {
    compileTheSchema(originalUSS)
    assertEquals(1, compileCount)

    touchFile()

    compileTheSchema(newUSS) // file has changed
    assertEquals(2, compileCount)

  }

}
