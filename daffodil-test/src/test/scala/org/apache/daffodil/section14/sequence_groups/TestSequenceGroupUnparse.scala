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

package org.apache.daffodil.section14.sequence_groups

import org.junit.Test
import org.apache.daffodil.util.Misc
import org.apache.daffodil.tdml.DFDLTestSuite
import org.junit.AfterClass

object TestSequenceGroupUnparse {
  val testDir = "org/apache/daffodil/section14/sequence_groups/"
  val aa = testDir + "SequenceGroupUnparse.tdml"
  val res = Misc.getRequiredResource(aa)
  var runner = new DFDLTestSuite(res)
  @AfterClass def shutDown {
    runner = null
  }
}
class TestSequenceGroupUnparse {
  import TestSequenceGroupUnparse._

  @Test def test_seqWithOptionals1() { runner.runOneTest("seqWithOptionals1") }
  @Test def test_seqWithOptionals2() { runner.runOneTest("seqWithOptionals2") }
  @Test def test_seqWithOptionals3() { runner.runOneTest("seqWithOptionals3") }
  @Test def test_seqWithOptionals4() { runner.runOneTest("seqWithOptionals4") }
  @Test def test_seqWithOptionals5() { runner.runOneTest("seqWithOptionals5") }

  @Test def test_seqWithHiddenGroupContainingComplex() { runner.runOneTest("seqWithHiddenGroupContainingComplex") }

}
