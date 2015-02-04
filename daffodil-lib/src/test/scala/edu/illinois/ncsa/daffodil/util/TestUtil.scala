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

package edu.illinois.ncsa.daffodil.util

import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.exceptions._
import org.junit.Test
import edu.illinois.ncsa.daffodil.Implicits._

class TestUtil {

  @Test def testGetRequiredResourceSucceeds() {
    val res = Misc.getRequiredResource("/xsd/XMLSchema.xsd")
    assertNotNull(res)
  }

  @Test def testGetRequiredResourceFails() {
    val e = intercept[Exception] {
      val res = Misc.getRequiredResource("/xsd/NotAResourceName.foo")
    }
    assertTrue(e.getMessage().contains("NotAResourceName"))
  }

  @Test
  def testStripQuotes() {
    assertEquals("foo", Misc.stripQuotes("\"foo\""))
  }

  @Test
  def testAssert() {
    intercept[Abort] {
      Assert.abort("yadda")
    }
  }

  @Test
  def testCopyrightNotices() {
    val fake = """
  Test fake Copyright (C) 3023 by Space Invaders of the Earth. All rights reserved.
  Of course these can be long verbose license statements and such.
  """
    CopyrightNotices.add(fake)
    val notices = CopyrightNotices.getNotices()
    val hasFake = notices.contains("Test fake Copyright")
    assertTrue(hasFake)
  }

  @Test
  def testBitsConverters1() {
    val bytes = Misc.bits2Bytes("11")
    val theByte = bytes(0)
    assertEquals(3, theByte.toInt)
  }

  @Test
  def testBitsConverters2() {
    val bytes = Misc.bits2Bytes("110110110110")
    val byte0 = bytes(0)
    val byte1 = bytes(1)
    assertEquals(-37, byte0.toInt)
    assertEquals(6, byte1.toInt)
  }

}
