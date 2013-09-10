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
import scala.collection.immutable.ListMap
import scala.util.Random

class TestListMap {

  /*
   * The purpose of this test is to ensure that ListMap maintains insertion
   * order. A ListMap is used in the IIMap structure, but it is importatnt that
   * it maintains insertion order. From the limited documentation, there does
   * not appear to be a contract that ListMap maintain order. However, in
   * practice, it is implemented as a linked list that does maintain order.
   * This test ensures that this behavior does not change.
   */
  @Test def test_listMap = {
    val orig = Random.shuffle(0 until 1000)
    val emptyListMap = ListMap[Int,String]()
    val listMap = orig.foldLeft(emptyListMap) {
      (lm, n) => lm + (n -> n.toString)
    }

    // test that removals still maintain order
    val removeKey = Random.nextInt(1000)
    val smallerOrig = orig.filter(_ != removeKey)
    val smallerMap = listMap - removeKey

    val zipped = smallerOrig.zip(smallerMap)
    zipped.foreach { case (o, (k, v)) => assertEquals(o, k) }
  }
}
