package edu.illinois.ncsa.daffodil.util

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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


import junit.framework.Assert.assertEquals
import edu.illinois.ncsa.daffodil.util._
import org.junit.Test
import org.junit.Test

class TestListUtils {

  @Test def testTailAfter1 = {
    val actual = ListUtils.tailAfter(List(1, 2, 3, 4, 5), 3)
    val expected = List(4, 5)
    assertEquals(expected, actual)
  }

  @Test def testTailAfter2 = {
    val actual = ListUtils.tailAfter(Nil, 3)
    val expected = Nil
    assertEquals(expected, actual)
  }

  @Test def testTailAfter3 = {
    val actual = ListUtils.tailAfter(List(1, 2, 3, 4, 5), 5)
    val expected = Nil
    assertEquals(expected, actual)
  }

  @Test def testTailAfter4 = {
    val actual = ListUtils.tailAfter(List(1, 2, 3, 4, 5), 0)
    val expected = Nil // The answer if not found at all is Nil
    assertEquals(expected, actual)
  }

  @Test def testPreceding1 = {
    val actual = ListUtils.preceding(List(1, 2, 3, 4, 5), 3)
    val expected = List(1, 2)
    assertEquals(expected, actual)
  }

  @Test def testPreceding2 = {
    val actual = ListUtils.preceding(Nil, 3)
    val expected = Nil
    assertEquals(expected, actual)
  }

  @Test def testPreceding3 = {
    val actual = ListUtils.preceding(List(1, 2, 3, 4, 5), 1)
    val expected = Nil
    assertEquals(expected, actual)
  }

  @Test def testPreceding4 = {
    val actual = ListUtils.preceding(List(1, 2, 3, 4, 5), 0)
    val expected = Nil // The answer if not found at all is Nil
    assertEquals(expected, actual)
  }

  @Test def testPreceding5 = {
    val actual = ListUtils.preceding(List(1, 2, 3, 4, 5), 5)
    val expected = List(1, 2, 3, 4)
    assertEquals(expected, actual)
  }

}