package edu.illinois.ncsa.daffodil.section06.entities

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


import junit.framework.Assert._
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File

class TestEntities {
  val testDir = "/edu/illinois/ncsa/daffodil/section06/entities/"
  val tdml = testDir + "charClassEntities.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))
  
//  @Test def test_doubleNL() { runner.runOneTest("doubleNL") }

  @Test def test_entityInError() { runner.runOneTest("entityInError") }
  @Test def test_LineFeed() { runner.runOneTest("LineFeed") }
  @Test def test_CarriageReturn() { runner.runOneTest("CarriageReturn") }
  @Test def test_LineSeparator() { runner.runOneTest("LineSeparator") }
  @Test def test_NextLine() { runner.runOneTest("NextLine") }
  @Test def test_LineFeed_byte() { runner.runOneTest("LineFeed_byte") }
  @Test def test_CarriageReturn_byte() { runner.runOneTest("CarriageReturn_byte") }
  @Test def test_CRLF_byte() { runner.runOneTest("CRLF_byte") }
  @Test def test_LineSeparator_byte() { runner.runOneTest("LineSeparator_byte") }
  @Test def test_NextLine_byte() { runner.runOneTest("NextLine_byte") }
  @Test def test_FormFeed() { runner.runOneTest("FormFeed") }
  @Test def test_HexCodePoint() { runner.runOneTest("HexCodePoint") }

  val testDir_01 = "/edu/illinois/ncsa/daffodil/section06/entities/"
  val tdml_01 = testDir_01 + "Entities.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))

  @Test def test_text_entities_6_02() { runner_01.runOneTest("text_entities_6_02") }
  @Test def test_text_entities_6_03() { runner_01.runOneTest("text_entities_6_03") }
  @Test def test_text_entities_6_03b() { runner_01.runOneTest("text_entities_6_03b") }
  @Test def test_text_entities_6_04() { runner_01.runOneTest("text_entities_6_04") }
  @Test def test_byte_entities_6_01() { runner_01.runOneTest("byte_entities_6_01") }
  @Test def test_byte_entities_6_02() { runner_01.runOneTest("byte_entities_6_02") }
  @Test def test_byte_entities_6_03() { runner_01.runOneTest("byte_entities_6_03") }
  @Test def test_byte_entities_6_04() { runner_01.runOneTest("byte_entities_6_04") }
  @Test def test_byte_entities_6_05() { runner_01.runOneTest("byte_entities_6_05") }
  //@Test def test_byte_entities_6_06() { runner_01.runOneTest("byte_entities_6_06") }
  //@Test def test_byte_entities_6_07() { runner_01.runOneTest("byte_entities_6_07") }
  @Test def test_byte_entities_6_08() { runner_01.runOneTest("byte_entities_6_08") }
  
//  @Test def test_whitespace_01() { runner_01.runOneTest("whitespace_01") }

  val testDir_02 = "/edu/illinois/ncsa/daffodil/ibm-tests/"
  val tdml_02 = testDir_02 + "dpaext1.tdml"
  lazy val runner_02 = new DFDLTestSuite(Misc.getRequiredResource(tdml_02))
  @Test def test_syntax_entities_6_01() { runner_02.runOneTest("syntax_entities_6_01") }
  @Test def test_syntax_entities_6_02() { runner_02.runOneTest("syntax_entities_6_02") }
  // 
  // Needs dfdl:utf16Width='variable' implementation
  //  @Test def test_syntax_entities_6_03() { runner_02.runOneTest("syntax_entities_6_03") }

  val entity = testDir + "entities_01.tdml"
  lazy val runnerEntity = new DFDLTestSuite(Misc.getRequiredResource(entity))
  @Test def test_entity_fail_01() { runnerEntity.runOneTest("entity_fail_01") }
  @Test def test_entity_fail_02() { runnerEntity.runOneTest("entity_fail_02") }
  @Test def test_entity_fail_03() { runnerEntity.runOneTest("entity_fail_03") }
  @Test def test_entity_fail_04() { runnerEntity.runOneTest("entity_fail_04") }

}
