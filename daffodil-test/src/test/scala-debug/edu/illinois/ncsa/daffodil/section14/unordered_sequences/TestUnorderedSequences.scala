/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.section14.unordered_sequences

import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.LoggingDefaults
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestUnorderedSequencesDebug {
  val testDir = "/edu/illinois/ncsa/daffodil/section14/unordered_sequences/"
  val runner = Runner(testDir, "UnorderedSequences.tdml")

  @AfterClass def shutDown {
    runner.reset
  }

}

class TestUnorderedSequencesDebug {

  import TestUnorderedSequencesDebug._

  //DFDL-1010
  @Test def test_simple = { runner.runOneTest("test_simple") }
  @Test def test_simple_fail_scalar = { runner.runOneTest("test_simple_fail_scalar") }
  @Test def test_simple_min_max_occurs = { runner.runOneTest("test_simple_min_max_occurs") }
  @Test def test_simple_min_max_occurs_fail = { runner.runOneTest("test_simple_min_max_occurs_fail") }
  @Test def test_simple_delimited = { runner.runOneTest("test_simple_delimited") }
  @Test def test_simple_nil = { runner.runOneTest("test_simple_nil") }
  @Test def test_simple_invalid_path_to_branch = { runner.runOneTest("test_simple_invalid_path_to_branch") }
  @Test def test_simple_invalid_path_to_branch_does_not_exist = { runner.runOneTest("test_simple_invalid_path_to_branch_does_not_exist") }
  @Test def test_nested_valid_path_to_branch = { runner.runOneTest("test_nested_valid_path_to_branch") }
  @Test def test_nested_multiple_valid_paths_to_branch = { runner.runOneTest("test_nested_multiple_valid_paths_to_branch") }
  @Test def test_nested_multiple_invalid_paths_to_branch = { runner.runOneTest("test_nested_multiple_invalid_paths_to_branch") }
  @Test def test_nested_path_evaluates_to_nodelist = { runner.runOneTest("test_nested_path_evaluates_to_nodelist") }
  @Test def test_nested_invalid_path_to_branch = { runner.runOneTest("test_nested_invalid_path_to_branch") }
  @Test def test_nested_invalid_path_to_branch_2 = { runner.runOneTest("test_nested_invalid_path_to_branch_2") }
  @Test def test_nested_invalid_path_to_branch_3 = { runner.runOneTest("test_nested_invalid_path_to_branch_3") }
  @Test def test_sde_element_element_ref = { runner.runOneTest("test_sde_element_element_ref") }
  @Test def test_sde_optional_array_ock_parsed = { runner.runOneTest("test_sde_optional_array_ock_parsed") }
  @Test def test_sde_unique_names_in_ns = { runner.runOneTest("test_sde_unique_names_in_ns") }
}
