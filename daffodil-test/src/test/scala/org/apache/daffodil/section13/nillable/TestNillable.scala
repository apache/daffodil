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

package edu.illinois.ncsa.daffodil.section13.nillable

import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestNillable {
  val testDir = "/edu/illinois/ncsa/daffodil/section13/nillable/"
  val testDir_01 = "/edu/illinois/ncsa/daffodil/section06/entities/"

  val runnerAA = Runner(testDir, "nillable.tdml")
  val runnerLN = Runner(testDir, "literal-value-nils.tdml")
  val runnerLC = Runner(testDir, "literal-character-nils.tdml")
  val runnerEntity = Runner(testDir_01, "entities_01.tdml")

  @AfterClass def shutDown {
    runnerAA.reset
    runnerLN.reset
    runnerEntity.reset
    runnerLC.reset
  }

}

class TestNillable {

  import TestNillable._

  @Test def test_complex_nil() { runnerLN.runOneTest("test_complex_nil") }

  @Test def test_litNil1() { runnerAA.runOneTest("litNil1") }
  @Test def test_litNil2() { runnerAA.runOneTest("litNil2") }
  @Test def test_litNil3() { runnerAA.runOneTest("litNil3") }
  @Test def test_litNil4() { runnerAA.runOneTest("litNil4") }
  @Test def test_litNil4b() { runnerAA.runOneTest("litNil4b") }
  @Test def test_litNil5() { runnerAA.runOneTest("litNil5") }
  @Test def test_litNil6() { runnerAA.runOneTest("litNil6") }
  @Test def test_litNil7() { runnerAA.runOneTest("litNil7") }
  @Test def test_missing_scalar() { runnerAA.runOneTest("missing_scalar") }
  @Test def test_nillable1() { runnerAA.runOneTest("nillable1") }

  @Test def test_text_nil_characterClass_04_parse() = { runnerLN.runOneTest("text_nil_characterClass_04_parse") }

  //@Test def test_text_01() { runnerLN.runOneTest("text_01")}  This test is identical to litNil1.
  //@Test def test_text_02() { runnerLN.runOneTest("text_02")}  This test is identical to litNil2
  @Test def test_text_03() { runnerLN.runOneTest("text_03") }
  @Test def test_text_03ic() { runnerLN.runOneTest("text_03ic") }
  @Test def test_text_04() { runnerLN.runOneTest("text_04") }
  @Test def test_text_05() { runnerLN.runOneTest("text_05") }
  @Test def test_text_06() = { runnerLN.runOneTest("text_06") }
  @Test def test_binary_01() = { runnerLN.runOneTest("binary_01") }
  @Test def test_padded_nils() = { runnerLN.runOneTest("test_padded_nils") }

  @Test def test_nillable_ovc_01() = { runnerLN.runOneTest("nillable_ovc_01") }
  
  /* These should demonstrate that:
   * 	DFDL Char Classes are not allowed for literalCharacter
   *  DFDL Char Entities are allowed for literalCharacter
   *  Raw bytes entities are allowed for literalCharacter
   *  Only 1 character or byte are allowed for literalCharacter
   * 
   *  According to analysis doc, should also work for numeric
   *  and hex entities.
   * */
  @Test def test_text_lit_char_01() { runnerLC.runOneTest("text_01") }
  @Test def test_text_lit_char_01ic() { runnerLC.runOneTest("text_01ic") }
  @Test def test_text_lit_char_02() { runnerLC.runOneTest("text_02") }
  @Test def test_text_lit_char_03() { runnerLC.runOneTest("text_03") }
  @Test def test_text_lit_char_04() { runnerLC.runOneTest("text_04") }
  @Test def test_binary_lit_char_01() { runnerLC.runOneTest("binary_01") }

  @Test def test_entity_fail_05() { runnerEntity.runOneTest("entity_fail_05") }
  @Test def test_entity_fail_06() { runnerEntity.runOneTest("entity_fail_06") }
  @Test def test_entity_success_05() { runnerEntity.runOneTest("entity_success_05") }
  @Test def test_entity_success_06() { runnerEntity.runOneTest("entity_success_06") }

}
