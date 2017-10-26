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

package org.apache.daffodil.section08.property_scoping

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestPropertyScoping {
  val testDir = "/org/apache/daffodil/section08/property_scoping/"
  val runner = Runner(testDir, "PropertyScoping.tdml")
  val runner_01 = Runner(testDir, "PropertyScoping_01.tdml")

  @AfterClass def shutDown {
    runner.reset
    runner_01.reset
  }
}

class TestPropertyScoping {

  import TestPropertyScoping._

  @Test def test_defaultForm_01() { runner.runOneTest("defaultForm_01") }
  @Test def test_defaultForm_02() { runner.runOneTest("defaultForm_02") }
  @Test def test_defaultForm_03() { runner.runOneTest("defaultForm_03") }
  @Test def test_defaultForm_04() { runner.runOneTest("defaultForm_04") }

  @Test def test_localAnnotation_01() { runner.runOneTest("localAnnotation_01") }
  @Test def test_localAnnotation_02() { runner.runOneTest("localAnnotation_02") }
  @Test def test_localAnnotation_03() { runner.runOneTest("localAnnotation_03") }
  @Test def test_localAnnotation_04() { runner.runOneTest("localAnnotation_04") }

  //DFDL-1036 (was fixed) now DFDL-1159
  //@Test def test_localAnnotation_05() { runner.runOneTest("localAnnotation_05") }

  @Test def test_property_scoping_01() { runner.runOneTest("property_scoping_01") }
  @Test def test_unparse_property_scoping_01() { runner.runOneTest("unparse_property_scoping_01") }
  @Test def test_property_scoping_06() { runner.runOneTest("property_scoping_06") }
  @Test def test_unparse_property_scoping_06() { runner.runOneTest("unparse_property_scoping_06") }
  @Test def test_group_ref() { runner.runOneTest("group_ref") }
  @Test def test_multipleDefinition() { runner.runOneTest("multipleDefinition") }
  @Test def test_multipleDefinition2() { runner.runOneTest("multipleDefinition2") }
  @Test def test_multipleDefinition3() { runner.runOneTest("multipleDefinition3") }

  // Moved back to debug, because validation is no longer detecting the error it is
  // expecting
  // @Test def test_property_shortFormSchemaFail() { runner.runOneTest("shortFormSchemaFail") }

  @Test def test_format_nesting_01() { runner.runOneTest("format_nesting_01") }

  @Test def test_property_scoping_02() { runner_01.runOneTest("property_scoping_02") }
  //See DFDL-1337
  //@Test def test_unparse_property_scoping_02() { runner_01.runOneTest("unparse_property_scoping_02") }
  @Test def test_property_scoping_03() = { runner_01.runOneTest("property_scoping_03") }
  @Test def test_unparse_property_scoping_03() = { runner_01.runOneTest("unparse_property_scoping_03") }
  @Test def test_property_scoping_04() { runner_01.runOneTest("property_scoping_04") }
  @Test def test_property_scoping_05() { runner_01.runOneTest("property_scoping_05") }
  // See DFDL-1342
  //@Test def test_unparse_property_scoping_04() { runner_01.runOneTest("unparse_property_scoping_04") }
  //@Test def test_unparse_property_scoping_05() { runner_01.runOneTest("unparse_property_scoping_05") }
  @Test def test_property_scoping_07() { runner_01.runOneTest("property_scoping_07") }
  @Test def test_unparse_property_scoping_07() { runner_01.runOneTest("unparse_property_scoping_07") }
  @Test def test_property_scoping_08() { runner_01.runOneTest("property_scoping_08") }
  @Test def test_unparse_property_scoping_08() { runner_01.runOneTest("unparse_property_scoping_08") }
  @Test def test_property_scoping_09() { runner_01.runOneTest("property_scoping_09") }
  @Test def test_unparse_property_scoping_09() { runner_01.runOneTest("unparse_property_scoping_09") }
  @Test def test_property_scoping_10() { runner_01.runOneTest("property_scoping_10") }
  @Test def test_unparse_property_scoping_10() { runner_01.runOneTest("unparse_property_scoping_10") }
  @Test def test_property_scoping_11() { runner_01.runOneTest("property_scoping_11") }
  @Test def test_unparse_property_scoping_11() { runner_01.runOneTest("unparse_property_scoping_11") }
  @Test def test_unparse_property_scoping_12() { runner_01.runOneTest("unparse_property_scoping_12") }
  @Test def test_NearestEnclosingSequenceElementRef() { runner_01.runOneTest("NearestEnclosingSequenceElementRef") }

  @Test def test_property_refElementFormFail() = { runner_01.runOneTest("refElementFormFail") }

}
