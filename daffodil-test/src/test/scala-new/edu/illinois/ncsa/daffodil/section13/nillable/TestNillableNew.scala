/* Copyright (c) 2015 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass
import org.junit.Test

object TestNillableNew {
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

class TestNillableNew {

  import TestNillable._
  
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
  @Test def test_text_lit_char_02() { runnerLC.runOneTest("text_02") }
  @Test def test_text_lit_char_03() { runnerLC.runOneTest("text_03") }
  @Test def test_text_lit_char_04() { runnerLC.runOneTest("text_04") }
  @Test def test_binary_lit_char_01() { runnerLC.runOneTest("binary_01") }

}
