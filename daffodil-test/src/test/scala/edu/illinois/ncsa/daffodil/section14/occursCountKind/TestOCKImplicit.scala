/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.section14.occursCountKind

/* This section00 is for testing general features of DFDL that are
 * not related to any specific requirement
 */

import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.util.Misc

import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestOCKImplicit {
  val testDir = "/edu/illinois/ncsa/daffodil/section14/occursCountKind/"
  val runner = Runner(testDir, "ockImplicit.tdml")

  @AfterClass def shutDown {
    runner.reset
  }

}

class TestOCKImplicit {
  import TestOCKImplicit._

  @Test def test_ockImplicit1() { runner.runOneTest("ockImplicit1") }
  @Test def test_ockImplicit2() { runner.runOneTest("ockImplicit2") }
  @Test def test_ockImplicit3() { runner.runOneTest("ockImplicit3") }
  @Test def test_ockImplicit4() { runner.runOneTest("ockImplicit4") }
  @Test def test_ockImplicit5() { runner.runOneTest("ockImplicit5") }
  @Test def test_ockImplicit6() { runner.runOneTest("ockImplicit6") }
  @Test def test_ockImplicit7() { runner.runOneTest("ockImplicit7") }
  @Test def test_ockImplicit8() { runner.runOneTest("ockImplicit8") }
  @Test def test_ockImplicit9() { runner.runOneTest("ockImplicit9") }
  @Test def test_ockImplicit10() { runner.runOneTest("ockImplicit10") }
  @Test def test_ockImplicit11() { runner.runOneTest("ockImplicit11") }
  @Test def test_ockImplicit12() { runner.runOneTest("ockImplicit12") }
  @Test def test_ockImplicit13() { runner.runOneTest("ockImplicit13") }
  @Test def test_ockImplicit14() { runner.runOneTest("ockImplicit14") }
  @Test def test_ockImplicit15() { runner.runOneTest("ockImplicit15") }

  @Test def test_ockImplicit16() { runner.runOneTest("ockImplicit16") }
  @Test def test_ockImplicit17() { runner.runOneTest("ockImplicit17") }
  @Test def test_ockImplicit18() { runner.runOneTest("ockImplicit18") }
  @Test def test_ockImplicit19() { runner.runOneTest("ockImplicit19") }

  @Test def test_ockImplicit20() { runner.runOneTest("ockImplicit20") }
  @Test def test_ockImplicit21() { runner.runOneTest("ockImplicit21") }
  @Test def test_ockImplicit22() { runner.runOneTest("ockImplicit22") }
  @Test def test_ockImplicit23() { runner.runOneTest("ockImplicit23") }
}
