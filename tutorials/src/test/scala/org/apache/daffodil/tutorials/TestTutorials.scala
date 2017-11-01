package org.apache.daffodil.tutorials

/* Copyright (c) 2012-2016 Tresys Technology, LLC. All rights reserved.
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

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestTutorials {
  val runner1 = Runner("/", "bitorder.tutorial.tdml.xml")
  val runner2 = Runner("/", "test1.tutorial.tdml.xml")
  val runner3 = Runner("/", "testTDMLTutorialFile2.tutorial.tdml.xml")
  val runner4 = Runner("/", "tdmlTutorial.tdml.xml")
  val runner5 = Runner("/", "bugReportTemplate.tdml")

  @AfterClass def shutDown {
    runner1.reset
    runner2.reset
    runner3.reset
    runner4.reset
    runner5.reset
  }

}
class TestTutorials {
  import TestTutorials._

  @Test def testTutorialElementsParse() { runner2.runOneTest("testTutorialElementsParse") }
  @Test def testTutorialElementsUnparse() { runner2.runOneTest("testTutorialElementsUnparse") }

  // removed for now. This will probably go back into this tutorial
  // @Test def test_MIL2045_47001D_1() { runner1.runOneTest("TestMIL2045_47001D_1") }
  @Test def test_leastSignificantBitFirst() { runner1.runOneTest("leastSignificantBitFirst") }
  @Test def test_leastSignificantBitFirstRTL() { runner1.runOneTest("leastSignificantBitFirstRTL") }
  @Test def test_mostSignificantBitFirst() { runner1.runOneTest("mostSignificantBitFirst") }
  @Test def test_littleEndianLeastFirstLTR() { runner1.runOneTest("littleEndianLeastFirstLTR") }
  @Test def test_littleEndianLeastFirstRTL() { runner1.runOneTest("littleEndianLeastFirstRTL") }

  @Test def test_bugReportParse1() { runner4.runOneTest("dateTimeTest") }
  @Test def test_bugReportUnparse1() { runner4.runOneTest("unparseDateTimeTest") }

  @Test def test_bugReportTemplateParse1() { runner5.runOneTest("dateTimeTest") }
  @Test def test_bugReportTemplateUnparse1() { runner5.runOneTest("unparseDateTimeTest") }

}
