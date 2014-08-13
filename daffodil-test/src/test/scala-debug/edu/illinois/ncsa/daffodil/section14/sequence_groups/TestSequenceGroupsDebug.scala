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
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.debugger.Debugger

class TestSequenceGroupsDebug {

  val testDir_01 = "/edu/illinois/ncsa/daffodil/section14/sequence_groups/"
  val tdml_02 = testDir_01 + "SequenceGroup.tdml"
  lazy val runner_02 = new DFDLTestSuite(Misc.getRequiredResource(tdml_02), validateTDMLFile = false)

  //  This test sets up an infinite loop of hidden groups. It is currently commented-out
  //  because it runs indefinitely and prevents the rest of the suite from executing  
  //  @Test def test_hiddenGroupLoop() { runner_03.runOneTest("hiddenGroupLoop") }

  @Test def test_emptySequenceSDE() { runner_02.runOneTest("emptySequenceSDE") }
  @Test def test_hiddenGroupEmpty() { runner_02.runOneTest("hiddenGroupEmpty") }
  @Test def test_sequenceWithComplexType() { runner_02.runOneTest("sequenceWithComplexType") }

  val tdml_01 = testDir_01 + "SequenceGroupInitiatedContent.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))

  /**
   * JIRA ticket DFDL-1038
   *
   * The test_baseline() below fails if you have dbg on it as here.
   * It passes if you don't have dbg on it.
   *
   * The problem is some interaction of the try/catch surrounding expression
   * evaluation at compile time and the InteractiveDebugger/Trace
   *
   * We run the expression and catch errors if it
   * ISNT constant, as a way of determining whether it is a constant. The
   * expression is { .. } which is NOT constant, and a throw of IllegalStateException
   * occurs which is caught (supposed to be anyway) very nearby where it is
   * thrown. But somehow, we end up exiting as if the exception was not caught.
   *
   * This happens on the final, 26th such throw. All earlier throws/catch work
   * fine. (If you set a breakpoint on IllegalStateException caught and uncaught
   * you can set the hit count to 26 and watch what happens here.
   *
   * I am mystified by this. -mikeb 2014-09-26
   */
  def dbg = {
    Debugger.withTracing(false)
    // LoggingDefaults.setLoggingLevel(LogLevel.Debug)
  }
  @Test def test_baseline() { dbg; runner_01.runOneTest("initiatedContentSeqBaseline") }
}

