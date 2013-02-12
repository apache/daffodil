package edu.illinois.ncsa.daffodil

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


import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import org.junit.contrib.java.lang.system.ExpectedSystemExit
import org.junit.contrib.java.lang.system.internal.CheckExitCalled
import org.junit.Test
import org.junit.Rule
import edu.illinois.ncsa.daffodil.util.Misc
import java.io.ByteArrayInputStream


class MainTests extends JUnitSuite {

  // This rule makes it so that System.exit throws an exception You are
  // supposed to be able to say exit.expectExitWithStatus(), but this doesn't
  // work, probably some scala problem, related to def exit. This is supposed
  // to be val exit, but it doesn't work with scala. Instead, we need to
  // manually catch CheckExitCalled and verify the status
  @Rule
  def exit = ExpectedSystemExit.none()

  @Test def test_required_mode() {
    try {
      Main.main(Array(""))
    } catch {
      case c: CheckExitCalled => assertEquals(c.getStatus, 1)
    }
  }
  
  @Test def test_require_schema_or_parser() {
    try {
      Main.main(Array("parse"))
    } catch {
      case c: CheckExitCalled => assertEquals(c.getStatus, 1)
    }
  }


  val testDir = "/test/cli/"
  val cli_choice= testDir + "choice.xsd"

  @Test def test_main_parse_simple() {
    try {
      val schema = Misc.getRequiredResource(cli_choice).getPath
      val data = "1;2;3;4;5;6;7;8"
      val is = new ByteArrayInputStream(data.getBytes())
      val oldSysin = System.in
      System.setIn(is)

      Main.main(Array("parse", "--schema", schema, "-"))

      System.setIn(oldSysin)
    } catch {
      case c: CheckExitCalled => assertEquals(c.getStatus, 0)
    }
  }
  
  @Test def test_main_parse_all_opts() {
    try {
      val schema = Misc.getRequiredResource(cli_choice).getPath
      val data = "1;2;3;4.4;5;6;7;8"
      val is = new ByteArrayInputStream(data.getBytes())
      val oldSysin = System.in
      System.setIn(is)
       
      Main.main(Array("parse", "--schema", schema,
                                        "--root", "ch1",
                                        "--namespace", "http://www.example.org/example1/",
                                        "-"
                                        ))
      System.setIn(oldSysin)
    } catch {
      case c: CheckExitCalled => assertEquals(c.getStatus, 0)
    }
  }

}
