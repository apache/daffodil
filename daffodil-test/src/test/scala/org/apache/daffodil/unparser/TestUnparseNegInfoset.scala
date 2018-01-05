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

package org.apache.daffodil.unparser

import org.junit.Test
import org.junit.AfterClass
import org.apache.daffodil.util._
import org.apache.daffodil.tdml.DFDLTestSuite

object TestUnparseNegInfoset {
  val testDir = "/org/apache/daffodil/unparser/"
  val aa = testDir + "unparseNegInfosetTest.tdml"
  var runner = new DFDLTestSuite(Misc.getRequiredResource(aa), validateTDMLFile = false)

  @AfterClass def tearDown() {
    runner = null
  }
}

class TestUnparseNegInfoset {
  import TestUnparseNegInfoset._

  @Test def test_schemaElementRoot1Good() { runner.runOneTest("schemaElementRoot1Good") }
  @Test def test_schemaElementRoot2Good() { runner.runOneTest("schemaElementRoot2Good") }

  @Test def test_unexpectedNextNone() { runner.runOneTest("unexpectedNextNone") }
  @Test def test_unexpectedNextSingle() { runner.runOneTest("unexpectedNextSingle") }
  @Test def test_unexpectedNextMultiple() { runner.runOneTest("unexpectedNextMultiple") }

  @Test def test_uenxpectedChildNone() { runner.runOneTest("unexpectedChildNone") }
  @Test def test_unexpectedChildSingle() { runner.runOneTest("unexpectedChildSingle") }
  @Test def test_unexpectedChildMultiple() { runner.runOneTest("unexpectedChildMultiple") }
  @Test def test_unexpectedChildSameAsSibling() { runner.runOneTest("unexpectedChildSameAsSibling") }

  @Test def test_unexpectedRootSingle() { runner.runOneTest("unexpectedRootSingle") }

  @Test def test_nilledTrueNonNillable() { runner.runOneTest("nilledTrueNonNillable") }
  @Test def test_nilledFalseNonNillable() { runner.runOneTest("nilledFalseNonNillable") }
  @Test def test_nilledSimpleWithContent() { runner.runOneTest("nilledSimpleWithContent") }
  @Test def test_nilledComplexWithContent() { runner.runOneTest("nilledComplexWithContent") }
  @Test def test_nilledBadValue() { runner.runOneTest("nilledBadValue") }
}
