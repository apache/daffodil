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

package edu.illinois.ncsa.daffodil.section06.entities

import junit.framework.Assert._
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestEntitiesDebug {
  private val testDir = "/edu/illinois/ncsa/daffodil/section06/entities/"
  private val testDir_02 = "/edu/illinois/ncsa/daffodil/ibm-tests/"

  val runner = Runner(testDir, "charClassEntities.tdml")
  val runner_01 = Runner(testDir, "Entities.tdml")
  val runner_02 = Runner(testDir_02, "dpaext1.tdml")
  val runnerEntity = Runner(testDir, "entities_01.tdml")
  val runnerInvalid = Runner(testDir, "InvalidEntities.tdml")

  @AfterClass private def shutDown {
    runner.reset
    runner_01.reset
    runner_02.reset
    runnerEntity.reset
    runnerInvalid.reset
  }
}

class TestEntitiesDebug {

  import TestEntitiesDebug._

  // DFDL-378
  @Test def test_dataDumpEncoding() { runner_01.runOneTest("dataDumpEncoding") }

  // JIRA DFDL-1400 - separator in different encoding than terms being separated.
  @Test def test_text_entities_6_03b() { runner_01.runOneTest("text_entities_6_03b") }

  //DFDL-258 - raw byte entities
  @Test def test_byte_entities_6_10() { runner_01.runOneTest("byte_entities_6_10") }

  // Needs dfdl:utf16Width='variable' implementation
  @Test def test_syntax_entities_6_03() { runner_02.runOneTest("syntax_entities_6_03") }

}
