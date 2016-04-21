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

package edu.illinois.ncsa.daffodil.section07.variables

import org.junit.Test
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import scala.math.Pi
import edu.illinois.ncsa.daffodil.util.Fakes
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestVariables {
  val testDir = "/edu/illinois/ncsa/daffodil/section07/variables/"
  val runner = Runner(testDir, "variables.tdml")
  val runner_01 = Runner(testDir, "variables_01.tdml")

  @AfterClass def shutDown {
    runner.reset
    runner_01.reset
  }

}

class TestVariables {
  import TestVariables._

  @Test def test_setVarAbsolutePath() { runner.runOneTest("setVarAbsolutePath") }
  @Test def test_varAsSeparator() { runner.runOneTest("varAsSeparator") }

  @Test def test_setVar1() { runner.runOneTest("setVar1") }
  @Test def test_doubleSetErr() { runner.runOneTest("doubleSetErr") }
  @Test def test_multiSetErr() { runner.runOneTest("multiSetErr") }
  // DFDL-1443 & DFDL-1448
  // @Test def test_setAfterReadErr() { runner.runOneTest("setAfterReadErr") }
  @Test def test_setVarChoice() { runner.runOneTest("setVarChoice") }
  @Test def test_unparse_setVarChoice() { runner.runOneTest("unparse_setVarChoice") }
  @Test def test_setVarOnSeqAndElemRef() { runner.runOneTest("setVarOnSeqAndElemRef") }
  @Test def test_unparse_setVarOnSeq() { runner.runOneTest("unparse_setVarOnSeq") }
  @Test def test_setVarOnGroupRef() { runner.runOneTest("setVarOnGroupRef") }
  //@Test def test_unparse_setVarOnGroupRef() { runner.runOneTest("unparse_setVarOnGroupRef") }
  @Test def test_setVarSimpleType() { runner.runOneTest("setVarSimpleType") }

  @Test def test_setVarValAttribute() { runner.runOneTest("setVarValAttribute") }
  @Test def test_setVarValAttribute2() { runner.runOneTest("setVarValAttribute2") }
  @Test def test_setVarTypeMismatch() { runner.runOneTest("setVarTypeMismatch") }
  @Test def test_setVarCurrVal() { runner.runOneTest("setVarCurrVal") }
  @Test def test_setVarMismatchRelative() { runner.runOneTest("setVarMismatchRelative") }
  @Test def test_setVarExpression() { runner.runOneTest("setVarExpression") }
  @Test def test_setVarExpression2() { runner.runOneTest("setVarExpression2") }
  @Test def test_setVarBadScope() { runner.runOneTest("setVarBadScope") }
  @Test def test_varAsSeparator2() { runner.runOneTest("varAsSeparator2") }
  @Test def test_setVarBadScope2() { runner.runOneTest("setVarBadScope2") }

  @Test def test_doubleEmptyDefault() { runner.runOneTest("doubleEmptyDefault") }
  @Test def test_emptyDefault() { runner.runOneTest("emptyDefault") }
  @Test def test_emptyDefault2() { runner.runOneTest("emptyDefault2") }

  @Test def test_var_end_path() { runner.runOneTest("var_end_path") }
  @Test def test_var_in_path() { runner.runOneTest("var_in_path") }

  @Test def test_doubleSetErr_d() { runner_01.runOneTest("doubleSetErr_d") }
  @Test def test_setVar1_d() { runner_01.runOneTest("setVar1_d") }
  // DFDL-1443 & DFDL-1448
  // @Test def test_setAfterReadErr_d() { runner_01.runOneTest("setAfterReadErr_d") }
  @Test def test_setVar1_d_unparse() { runner_01.runOneTest("setVar1_d_unparse") }

  /*****************************************************************/
  val tdmlVal = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example

  val context: SchemaComponent = Fakes.fakeSD

  val variables2 =
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdmlVal } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
      <tdml:defineSchema name="mySchema">
        <dfdl:format ref="tns:daffodilTest1"/>
        <dfdl:defineVariable name="pi" type="xs:double" defaultValue={ Pi.toString }/>
        <xs:element name="data" type="xs:double" dfdl:inputValueCalc="{ $tns:pi }"/>
      </tdml:defineSchema>
      <tdml:parserTestCase name="variables2" root="data" model="mySchema">
        <tdml:document/>
        <tdml:infoset>
          <tdml:dfdlInfoset>
            <tns:data>3.141592653589793</tns:data>
          </tdml:dfdlInfoset>
        </tdml:infoset>
      </tdml:parserTestCase>
    </tdml:testSuite>

  @Test def test_variables2() {
    val testSuite = variables2
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("variables2")
  }

  val variables3 =
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdmlVal } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
      <tdml:defineSchema name="mySchema">
        <dfdl:format ref="tns:daffodilTest1"/>
        <dfdl:defineVariable name="x" type="xs:double"/>
        <xs:element name="data">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="e1" type="xs:double" dfdl:lengthKind="delimited">
                <xs:annotation>
                  <xs:appinfo source="http://www.ogf.org/dfdl/">
                    <dfdl:setVariable ref="tns:x" value="{ . }"/>
                  </xs:appinfo>
                </xs:annotation>
              </xs:element>
              <xs:element name="e2" type="xs:double" dfdl:inputValueCalc="{ $tns:x - 0.141592653589793 }"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      </tdml:defineSchema>
      <tdml:parserTestCase name="variables3" root="data" model="mySchema">
        <tdml:document>3.141592653589793</tdml:document>
        <tdml:infoset>
          <tdml:dfdlInfoset>
            <tns:data><tns:e1>3.141592653589793</tns:e1><tns:e2>3.0</tns:e2></tns:data>
          </tdml:dfdlInfoset>
        </tdml:infoset>
      </tdml:parserTestCase>
    </tdml:testSuite>

  @Test def test_variables3() {
    // Debugger.setDebugging(true)
    val testSuite = variables3
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("variables3")
  }
}
