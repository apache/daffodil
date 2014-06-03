package edu.illinois.ncsa.daffodil.usertests

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
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.debugger.InteractiveDebugger
import edu.illinois.ncsa.daffodil.japi.debugger.TraceRunner
import edu.illinois.ncsa.daffodil.japi.debugger.JavaInteractiveDebuggerRunner
import edu.illinois.ncsa.daffodil.debugger.InteractiveDebuggerRunner
import edu.illinois.ncsa.daffodil.japi.debugger.DebuggerRunner

class TestUserSubmittedTests {
  val testDir = "/edu/illinois/ncsa/daffodil/usertests/"
  val aa = testDir + "UserSubmittedTests.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_prefix_separator_as_variable() {
    runner.runOneTest("test_prefix_separator_as_variable")
  }

  @Test def test_dfdl_782() = {
    val tr = new CustomTraceRunner
    tr.init
    val crunner = new CustomInteractiveDebuggerRunner(tr)
    val db = new InteractiveDebugger(crunner)
    Debugger.setDebugging(true)
    Debugger.setDebugger(db)

    runner.runOneTest("test_DFDL_782")

    // Comment out these two lines to see issue
    // documented in DFDL-790
    Debugger.setDebugging(false)
    Debugger.setDebugger(null)
  }

}

class CustomInteractiveDebuggerRunner(dr: DebuggerRunner)
  extends InteractiveDebuggerRunner {
  def init(id: InteractiveDebugger): Unit = dr.init
  def getCommand(): String = dr.getCommand
  def lineOutput(line: String): Unit = dr.lineOutput(line)
  def fini(): Unit = dr.fini
}

class CustomTraceRunner extends TraceRunner {
  private var _lines = List.empty[String]

  def getAllTheLines(): String = {
    val sb = new StringBuilder
    _lines.foreach(line => {
      if (line.length > 0) sb.append(line)
    })
    val allTheLines = sb.toString
    allTheLines
  }

  override def init: Unit = { _lines = List.empty[String] }
  override def lineOutput(line: String) = _lines ++ (line + "\n")

}

