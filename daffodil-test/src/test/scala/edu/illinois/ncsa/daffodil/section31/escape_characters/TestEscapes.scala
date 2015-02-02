/* Copyright (c) 2012_2013 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.section31.escape_characters

import junit.framework.Assert._
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File

class TestEscapes {
  val testDir = "/edu/illinois/ncsa/daffodil/section31/escape_characters/"
  val tdml = testDir + "Escapes.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_escape_entry1() { runner.runOneTest("escape_entry1") }
  @Test def test_escape_entry2() { runner.runOneTest("escape_entry2") }
  @Test def test_escape_entry3() { runner.runOneTest("escape_entry3") }
  @Test def test_escape_entry4() { runner.runOneTest("escape_entry4") }
  @Test def test_escape_entry5() { runner.runOneTest("escape_entry5") }
  @Test def test_escape_entry6() { runner.runOneTest("escape_entry6") }
  @Test def test_escape_entry7() { runner.runOneTest("escape_entry7") }
  @Test def test_escape_entry8() { runner.runOneTest("escape_entry8") }
  @Test def test_escape_entry9() { runner.runOneTest("escape_entry9") }
  @Test def test_escape_entry10() { runner.runOneTest("escape_entry10") }
  @Test def test_escape_entry11() { runner.runOneTest("escape_entry11") }
  @Test def test_escape_entry12() { runner.runOneTest("escape_entry12") }

  @Test def test_escape_entry2_1() { runner.runOneTest("escape_entry2_1") }
  @Test def test_escape_entry2_2() { runner.runOneTest("escape_entry2_2") }
  @Test def test_escape_entry2_3() { runner.runOneTest("escape_entry2_3") }
  @Test def test_escape_entry2_4() { runner.runOneTest("escape_entry2_4") }
  @Test def test_escape_entry2_5() { runner.runOneTest("escape_entry2_5") }
  @Test def test_escape_entry2_6() { runner.runOneTest("escape_entry2_6") }
  @Test def test_escape_entry2_7() { runner.runOneTest("escape_entry2_7") }
  @Test def test_escape_entry2_8() { runner.runOneTest("escape_entry2_8") }
  @Test def test_escape_entry2_9() { runner.runOneTest("escape_entry2_9") }
  @Test def test_escape_entry2_10() { runner.runOneTest("escape_entry2_10") }
  @Test def test_escape_entry2_11() { runner.runOneTest("escape_entry2_11") }
  @Test def test_escape_entry2_12() { runner.runOneTest("escape_entry2_12") }
  @Test def test_escape_entry2_13() { runner.runOneTest("escape_entry2_13") }
  @Test def test_escape_entry2_14() { runner.runOneTest("escape_entry2_14") }
  @Test def test_escape_entry2_15() { runner.runOneTest("escape_entry2_15") }
  @Test def test_escape_entry2_16() { runner.runOneTest("escape_entry2_16") }
  @Test def test_escape_entry2_17() { runner.runOneTest("escape_entry2_17") }
  @Test def test_escape_entry2_18() { runner.runOneTest("escape_entry2_18") }

  @Test def test_escape_entry3_1() { runner.runOneTest("escape_entry3_1") }
  @Test def test_escape_entry3_2() { runner.runOneTest("escape_entry3_2") }
  @Test def test_escape_entry3_3() { runner.runOneTest("escape_entry3_3") }
  @Test def test_escape_entry3_4() { runner.runOneTest("escape_entry3_4") }
  @Test def test_escape_entry3_5() { runner.runOneTest("escape_entry3_5") }
  @Test def test_escape_entry3_6() { runner.runOneTest("escape_entry3_6") }
  @Test def test_escape_entry3_7() { runner.runOneTest("escape_entry3_7") }
  @Test def test_escape_entry3_8() { runner.runOneTest("escape_entry3_8") }
  @Test def test_escape_entry3_9() { runner.runOneTest("escape_entry3_9") }
  @Test def test_escape_entry3_10() { runner.runOneTest("escape_entry3_10") }
  @Test def test_escape_entry3_11() { runner.runOneTest("escape_entry3_11") }
  @Test def test_escape_entry3_12() { runner.runOneTest("escape_entry3_12") }
  @Test def test_escape_entry3_13() { runner.runOneTest("escape_entry3_13") }
  @Test def test_escape_entry3_14() { runner.runOneTest("escape_entry3_14") }
  @Test def test_escape_entry3_15() { runner.runOneTest("escape_entry3_15") }
  @Test def test_escape_entry3_16() { runner.runOneTest("escape_entry3_16") }
  @Test def test_escape_entry3_17() { runner.runOneTest("escape_entry3_17") }
  @Test def test_escape_entry3_18() { runner.runOneTest("escape_entry3_18") }
  @Test def test_escape_entry3_19() { runner.runOneTest("escape_entry3_19") }
  @Test def test_escape_entry3_20() { runner.runOneTest("escape_entry3_20") }
  @Test def test_escape_entry3_21() { runner.runOneTest("escape_entry3_21") }
  @Test def test_escape_entry3_22() { runner.runOneTest("escape_entry3_22") }
  @Test def test_escape_entry3_23() { runner.runOneTest("escape_entry3_23") }
  @Test def test_escape_entry3_24() { runner.runOneTest("escape_entry3_24") }
  @Test def test_escape_entry3_25() { runner.runOneTest("escape_entry3_25") }
  @Test def test_escape_entry3_26() { runner.runOneTest("escape_entry3_26") }
  @Test def test_escape_entry3_27() { runner.runOneTest("escape_entry3_27") }
  @Test def test_escape_entry3_28() { runner.runOneTest("escape_entry3_28") }
  @Test def test_escape_entry3_29() { runner.runOneTest("escape_entry3_29") }
  @Test def test_escape_entry3_30() { runner.runOneTest("escape_entry3_30") }
  @Test def test_escape_entry3_31() { runner.runOneTest("escape_entry3_31") }

  @Test def test_escape_entry4_1() { runner.runOneTest("escape_entry4_1") }
  @Test def test_escape_entry4_2() { runner.runOneTest("escape_entry4_2") }
  @Test def test_escape_entry4_3() { runner.runOneTest("escape_entry4_3") }
  @Test def test_escape_entry4_4() { runner.runOneTest("escape_entry4_4") }
  @Test def test_escape_entry4_5() { runner.runOneTest("escape_entry4_5") }
  @Test def test_escape_entry4_6() { runner.runOneTest("escape_entry4_6") }
  @Test def test_escape_entry4_7() { runner.runOneTest("escape_entry4_7") }
  @Test def test_escape_entry4_8() { runner.runOneTest("escape_entry4_8") }
  @Test def test_escape_entry4_9() { runner.runOneTest("escape_entry4_9") }
  @Test def test_escape_entry4_10() { runner.runOneTest("escape_entry4_10") }
  @Test def test_escape_entry4_11() { runner.runOneTest("escape_entry4_11") }
  @Test def test_escape_entry4_12() { runner.runOneTest("escape_entry4_12") }
  @Test def test_escape_entry4_13() { runner.runOneTest("escape_entry4_13") }
  @Test def test_escape_entry4_14() { runner.runOneTest("escape_entry4_14") }
  @Test def test_escape_entry4_15() { runner.runOneTest("escape_entry4_15") }
  @Test def test_escape_entry4_16() { runner.runOneTest("escape_entry4_16") }
  @Test def test_escape_entry4_17() { runner.runOneTest("escape_entry4_17") }
  @Test def test_escape_entry4_18() { runner.runOneTest("escape_entry4_18") }
  @Test def test_escape_entry4_19() { runner.runOneTest("escape_entry4_19") }
  @Test def test_escape_entry4_20() { runner.runOneTest("escape_entry4_20") }
  @Test def test_escape_entry4_21() { runner.runOneTest("escape_entry4_21") }
  @Test def test_escape_entry4_22() { runner.runOneTest("escape_entry4_22") }
  @Test def test_escape_entry4_23() { runner.runOneTest("escape_entry4_23") }
  @Test def test_escape_entry4_24() { runner.runOneTest("escape_entry4_24") }
  @Test def test_escape_entry4_25() { runner.runOneTest("escape_entry4_25") }
  @Test def test_escape_entry4_26() { runner.runOneTest("escape_entry4_26") }
  @Test def test_escape_entry4_27() { runner.runOneTest("escape_entry4_27") }

}
