package edu.illinois.ncsa.daffodil.section14.sequence_groups

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

class TestSequenceGroups {
  val testDir = "/edu/illinois/ncsa/daffodil/ibm-tests/"
  val aa = testDir + "dpaext1.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_multiple_delimiters2() { runner.runOneTest("multiple_delimiters2") }

  val tdml2 = testDir + "dpaext2.tdml"
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))
  @Test def test_sequences_separated_14_03() { runner2.runOneTest("sequences_separated_14_03") }
  @Test def test_sequences_separated_14_05() { runner2.runOneTest("sequences_separated_14_05") }
  @Test def test_sequences_separated_14_06() { runner2.runOneTest("sequences_separated_14_06") }

  val testDir_01 = "/edu/illinois/ncsa/daffodil/section14/sequence_groups/"
  val tdml_01 = testDir_01 + "SequenceGroupDelimiters.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))

  @Test def test_SeqGrp_01() { runner_01.runOneTest("SeqGrp_01") }
  @Test def test_SeqGrp_02() { runner_01.runOneTest("SeqGrp_02") }
  @Test def test_SeqGrp_03() { runner_01.runOneTest("SeqGrp_03") }
  @Test def test_SeqGrp_04() { runner_01.runOneTest("SeqGrp_04") }
  @Test def test_prefix() { runner_01.runOneTest("prefix") }
  @Test def test_prefix_01() { runner_01.runOneTest("prefix_01") }
  @Test def test_NumSeq_02() { runner_01.runOneTest("NumSeq_02") }
  @Test def test_groupRefInheritProps() { runner_01.runOneTest("groupRefInheritProps") }
  @Test def test_sequenceWithinSequence() { runner_01.runOneTest("sequenceWithinSequence") }

  val tdml_02 = testDir_01 + "SequenceGroup.tdml"
  lazy val runner_02 = new DFDLTestSuite(Misc.getRequiredResource(tdml_02))
  
//  @Test def test_emptySequenceSDE() { runner_02.runOneTest("emptySequenceSDE") }
  @Test def test_complexEmptyContent() { runner_02.runOneTest("complexEmptyContent") }
//  @Test def test_noContentComplexSDE() { runner_02.runOneTest("noContentComplexSDE") }

  @Test def test_SeqGrp546() { runner_02.runOneTest("SeqGrp546") }

  @Test def test_SeqGrp_05() { runner_02.runOneTest("SeqGrp_05") }

  @Test def test_hiddenGroup1() { runner_02.runOneTest("hiddenGroup1") }
  @Test def test_hiddenGroupSchemaFail() { runner_02.runOneTest("hiddenGroupSchemaFail") }
  @Test def test_hiddenGroupWithAssert() { runner_02.runOneTest("hiddenGroupWithAssert") }
  @Test def test_hiddenGroupWithAssert2() { runner_02.runOneTest("hiddenGroupWithAssert2") }
  @Test def test_hiddenGroupNested() { runner_02.runOneTest("hiddenGroupNested") }
  @Test def test_hiddenGroupNested2() { runner_02.runOneTest("hiddenGroupNested2") }
  @Test def test_nestedGroupRefs() { runner_02.runOneTest("nestedGroupRefs") }
  @Test def test_nestedGroupRefs2() { runner_02.runOneTest("nestedGroupRefs2") }
  @Test def test_hiddenGroupChoice() { runner_02.runOneTest("hiddenGroupChoice") }
  @Test def test_hiddenGroupChoice2() { runner_02.runOneTest("hiddenGroupChoice2") }
  @Test def test_hiddenGroupIgnoredProps() { runner_02.runOneTest("hiddenGroupIgnoredProps") }
  @Test def test_hiddenGroupAttributeNotation() { runner_02.runOneTest("hiddenGroupAttributeNotation") }
  @Test def test_hiddenGroupElementNotation() { runner_02.runOneTest("hiddenGroupElementNotation") }
//  @Test def test_hiddenGroupLoop() { runner_02.runOneTest("hiddenGroupLoop") }

  @Test def test_AC000() { runner_02.runOneTest("AC000") }
  @Test def test_AD000() { runner_02.runOneTest("AD000") }
  @Test def test_AS000() { runner_02.runOneTest("AS000") }
  
}
