package edu.illinois.ncsa.daffodil.csv

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
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File
import edu.illinois.ncsa.daffodil.debugger.Debugger

import org.junit.contrib.java.lang.system.ExpectedSystemExit
import org.junit.contrib.java.lang.system.internal.CheckExitCalled
import org.junit.Rule
import edu.illinois.ncsa.daffodil.util.Misc
import java.io.ByteArrayInputStream
import edu.illinois.ncsa.daffodil.Main

class TestCSV extends JUnitSuite {
  val testDir = "/edu/illinois/ncsa/daffodil/csv/"
  val aa = testDir + "csv.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
  
  @Rule
  def exit = ExpectedSystemExit.none()

  @Test def test_csv_test() { runner.runOneTest("csv_test") }
//  @Test def test_csv_test_2() { runner.runOneTest("csv_test_2") }
  @Test def test_csv_test_3() { runner.runOneTest("csv_test_3") }

  val cli_schema = testDir + "csv.dfdl.xsd"

  @Test def test_csv_600k_perf() {
    try {
      val file = testDir + "csv_600k"
      val schema = Misc.getRequiredResource(cli_schema).getPath
      val infile = Misc.getRequiredResource(file).getPath
      val oldSysin = System.in

      Main.main(Array("parse", "-s", schema, infile))

      System.setIn(oldSysin)
    } catch {
      case c: CheckExitCalled => assertEquals(c.getStatus, 0)
    }
  }
<!-- 
  @Test def test_csv_2m_perf() {
    try {
      val file = testDir + "csv_2m"
      val schema = Misc.getRequiredResource(cli_schema).getPath
      val infile = Misc.getRequiredResource(file).getPath
      val oldSysin = System.in

      Main.main(Array("parse", "-s", schema, infile))

      System.setIn(oldSysin)
    } catch {
      case c: CheckExitCalled => assertEquals(c.getStatus, 0)
    }
  }
-->
}
