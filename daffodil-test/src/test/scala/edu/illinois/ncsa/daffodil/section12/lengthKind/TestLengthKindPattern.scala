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

package edu.illinois.ncsa.daffodil.section12.lengthKind

import junit.framework.Assert._
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File
import org.junit.AfterClass

object TestLengthKindPattern {
  val testDir = "/edu/illinois/ncsa/daffodil/section12/lengthKind/"
  val aa = testDir + "PatternTests.tdml"
  var runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  val ai = testDir + "AI.tdml"
  var runnerAI = new DFDLTestSuite(Misc.getRequiredResource(ai))

  @AfterClass def shutDown {
    runner = null
    runnerAI = null
  }
}
class TestLengthKindPattern {

  import TestLengthKindPattern._

  @Test def test_unmatchedPattern01() { runner.runOneTest("unmatchedPattern01") }
  @Test def test_unmatchedPattern02() { runner.runOneTest("unmatchedPattern02") }
  @Test def test_unmatchedPattern03() { runner.runOneTest("unmatchedPattern03") }

  @Test def test_invalid_pattern() { runner.runOneTest("invalid_pattern") }
  @Test def test_invalid_pattern2() { runner.runOneTest("invalid_pattern2") }
  @Test def test_invalid_pattern3() { runner.runOneTest("invalid_pattern3") }

  @Test def test_AI000_rev() { runner.runOneTest("AI000_rev") } // round trip
  @Test def test_LengthKindPattern() { runner.runOneTest("LengthKindPattern") } // round trip
  @Test def test_LengthKindPatternCompound() { runner.runOneTest("LengthKindPatternCompound") }
  @Test def test_LengthKindPatternCompound2() { runner.runOneTest("LengthKindPatternCompound2") } // round trip
  @Test def test_lengthKindPattern_01() { runner.runOneTest("lengthKindPattern_01") } // round trip
  @Test def test_lengthKindPattern_02() { runner.runOneTest("lengthKindPattern_02") } // round trip
  @Test def test_lengthKindPattern_03() { runner.runOneTest("lengthKindPattern_03") } // round trip
  @Test def test_lengthKindPattern_04() { runner.runOneTest("lengthKindPattern_04") } // round trip

  @Test def test_LengthPatternIllegalBits_01() { runner.runOneTest("LengthPatternIllegalBits_01") }
  @Test def test_LengthPatternLegalBits_01() { runner.runOneTest("LengthPatternLegalBits_01") }

  // DFDL-309
  @Test def test_LengthPatternIllegalBits_02_EncodingErrorPolicy_Replace() { runner.runOneTest("LengthPatternIllegalBits_02_EncodingErrorPolicy_Replace") }

  @Test def test_LengthPatternLegalBits_02() { runner.runOneTest("LengthPatternLegalBits_02") } // round trip
  @Test def test_lengthKindPatternFail() { runner.runOneTest("lengthKindPatternFail") }

  @Test def test_ComplexWithBinaryChild() { runner.runOneTest("ComplexWithBinaryChild") }

  @Test def test_AI000() { runnerAI.runOneTest("AI000") }

  @Test def test_LengthPatternNil_NoNil() { runner.runOneTest("LengthPatternNil_NoNil") } // round trip
  @Test def test_LengthPatternNil_FindsNil() { runner.runOneTest("LengthPatternNil_FindsNil") } // round trip
  @Test def test_LengthPatternNil_EmptyStringAllowed() { runner.runOneTest("LengthPatternNil_EmptyStringAllowed") }
  @Test def test_nested_patterns() { runner.runOneTest("nested_patterns") }
  @Test def test_nested_patterns_01() { runner.runOneTest("nested_patterns_01") }
  @Test def test_nested_patterns_02() { runner.runOneTest("nested_patterns_02") }
  @Test def test_nested_patterns_03() { runner.runOneTest("nested_patterns_03") }

}
