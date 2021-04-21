/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.section07.variables

import org.junit.Test
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.Implicits._
import scala.math.Pi
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestVariables {
  val testDir = "/org/apache/daffodil/section07/variables/"
  val runner = Runner(testDir, "variables.tdml")
  val runner_01 = Runner(testDir, "variables_01.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
    runner_01.reset
  }

}

class TestVariables {
  import TestVariables._

  @Test def test_setVarAbsolutePath(): Unit = { runner.runOneTest("setVarAbsolutePath") }
  @Test def test_varAsSeparator(): Unit = { runner.runOneTest("varAsSeparator") }

  @Test def test_setVar1(): Unit = { runner.runOneTest("setVar1") }
  @Test def test_doubleSetErr(): Unit = { runner.runOneTest("doubleSetErr") }
  @Test def test_multiSetErr(): Unit = { runner.runOneTest("multiSetErr") }
  // DFDL-1443 & DFDL-1448
  // @Test def test_setAfterReadErr() { runner.runOneTest("setAfterReadErr") }
  @Test def test_varInstance_01(): Unit = { runner.runOneTest("varInstance_01") }
  @Test def test_varInstance_02(): Unit = { runner.runOneTest("varInstance_02") }
  @Test def test_varInstance_03(): Unit = { runner.runOneTest("varInstance_03") }
  @Test def test_varInstance_04(): Unit = { runner.runOneTest("varInstance_04") }
  @Test def test_varInstance_05(): Unit = { runner.runOneTest("varInstance_05") }
  @Test def test_varInstance_06(): Unit = { runner.runOneTest("varInstance_06") }
  @Test def test_varInstance_07(): Unit = { runner.runOneTest("varInstance_07") }
  @Test def test_varInstance_08(): Unit = { runner.runOneTest("varInstance_08") }
  @Test def test_varInstance_09(): Unit = { runner.runOneTest("varInstance_09") }
  @Test def test_varInstance_10(): Unit = { runner.runOneTest("varInstance_10") }
  @Test def test_varInstance_11(): Unit = { runner.runOneTest("varInstance_11") }
  @Test def test_varInstance_12(): Unit = { runner.runOneTest("varInstance_12") }
  @Test def test_varInstance_13(): Unit = { runner.runOneTest("varInstance_13") }
  @Test def test_varInstance_14(): Unit = { runner.runOneTest("varInstance_14") }
  @Test def test_varInstance_15(): Unit = { runner.runOneTest("varInstance_15") }

  @Test def test_varDirection_1(): Unit = { runner.runOneTest("varDirection_1") }
  @Test def test_varDirection_2(): Unit = { runner.runOneTest("varDirection_2") }
  @Test def test_varDirection_3(): Unit = { runner.runOneTest("varDirection_3") }
  @Test def test_varDirection_err1(): Unit = { runner.runOneTest("varDirection_err1") }
  @Test def test_varDirection_err2(): Unit = { runner.runOneTest("varDirection_err2") }
  @Test def test_varDirection_setVar1(): Unit = { runner.runOneTest("varDirection_setVar1") }
  @Test def test_varDirection_setVar2(): Unit = { runner.runOneTest("varDirection_setVar2") }
  @Test def test_varDirection_nvi1(): Unit = { runner.runOneTest("varDirection_nvi1") }
  @Test def test_varDirection_nvi2(): Unit = { runner.runOneTest("varDirection_nvi2") }

  @Test def test_defineVariable_nonConstantExpression(): Unit = { runner.runOneTest("defineVariable_nonConstantExpression") }
  @Test def test_defineVariable_nonConstantExpression_unp(): Unit = { runner.runOneTest("defineVariable_nonConstantExpression_unp") }
  @Test def test_circular_defineVariable_err(): Unit = { runner.runOneTest("circular_defineVariable_err") }
  @Test def test_defineVariable_ref_noDefault_err(): Unit = { runner.runOneTest("defineVariable_ref_noDefault_err") }
  @Test def test_defineVariable_nonConstantExpression_setVar_err(): Unit = { runner.runOneTest("defineVariable_nonConstantExpression_setVar_err") }

  // DAFFODIL-2444 - This test triggers an unhandled NoSuchElement exception, which if handled then runs into an Assert.invariant
  //@Test def test_defineVariable_ref_infoset_err(): Unit = { runner.runOneTest("defineVariable_ref_infoset_err") }

  @Test def test_setVarChoice(): Unit = { runner.runOneTest("setVarChoice") }
  @Test def test_unparse_setVarChoice(): Unit = { runner.runOneTest("unparse_setVarChoice") }
  @Test def test_setVarOnSeqAndElemRef(): Unit = { runner.runOneTest("setVarOnSeqAndElemRef") }
  @Test def test_unparse_setVarOnSeq(): Unit = { runner.runOneTest("unparse_setVarOnSeq") }
  @Test def test_setVarOnGroupRef(): Unit = { runner.runOneTest("setVarOnGroupRef") }
  @Test def test_unparse_setVarOnGroupRef(): Unit = { runner.runOneTest("unparse_setVarOnGroupRef") }
  @Test def test_setVarSimpleType(): Unit = { runner.runOneTest("setVarSimpleType") }

  @Test def test_setVarValAttribute(): Unit = { runner.runOneTest("setVarValAttribute") }
  @Test def test_setVarValAttribute2(): Unit = { runner.runOneTest("setVarValAttribute2") }
  @Test def test_setVarTypeMismatch(): Unit = { runner.runOneTest("setVarTypeMismatch") }
  @Test def test_setVarCurrVal(): Unit = { runner.runOneTest("setVarCurrVal") }
  @Test def test_setVarMismatchRelative(): Unit = { runner.runOneTest("setVarMismatchRelative") }
  @Test def test_setVarExpression(): Unit = { runner.runOneTest("setVarExpression") }
  @Test def test_setVarExpression2(): Unit = { runner.runOneTest("setVarExpression2") }
  @Test def test_setVarBadScope(): Unit = { runner.runOneTest("setVarBadScope") }
  @Test def test_varAsSeparator2(): Unit = { runner.runOneTest("varAsSeparator2") }
  @Test def test_setVarBadScope2(): Unit = { runner.runOneTest("setVarBadScope2") }

  @Test def test_resetVar_01(): Unit = { runner.runOneTest("resetVar_01") }
  @Test def test_resetVar_02(): Unit = { runner.runOneTest("resetVar_02") }

  @Test def test_doubleEmptyDefault(): Unit = { runner.runOneTest("doubleEmptyDefault") }
  @Test def test_emptyDefault(): Unit = { runner.runOneTest("emptyDefault") }
  @Test def test_emptyDefault2(): Unit = { runner.runOneTest("emptyDefault2") }

  @Test def test_var_end_path(): Unit = { runner.runOneTest("var_end_path") }
  @Test def test_var_in_path(): Unit = { runner.runOneTest("var_in_path") }

  @Test def test_logical_default_values(): Unit = { runner.runOneTest("logical_default_values") }
  @Test def test_logical_default_values_err(): Unit = { runner.runOneTest("logical_default_values_err") }

  @Test def test_unsignedIntVarCast(): Unit = { runner.runOneTest("unsignedIntVarCast") }

  // DFDL-2354
  @Test def test_NVI_with_CDK1(): Unit = { runner.runOneTest("NVI_with_CDK1") }

  // DFDL-2374
  @Test def test_variables_nilled_element(): Unit = { runner.runOneTest("variables_nilled_element") }

  // DFDL-2375
  @Test def test_multipleBranchesWithNoElementsSetVariableError(): Unit = { runner.runOneTest("multipleBranchesWithNoElementsSetVariableError") }

  @Test def test_doubleSetErr_d(): Unit = { runner_01.runOneTest("doubleSetErr_d") }
  @Test def test_setVar1_d(): Unit = { runner_01.runOneTest("setVar1_d") }
  // DFDL-1443 & DFDL-1448
  // @Test def test_setAfterReadErr_d() { runner_01.runOneTest("setAfterReadErr_d") }
  @Test def test_setVar1_d_parse(): Unit = { runner_01.runOneTest("setVar1_d_parse") }
  @Test def test_setVar1_d_unparse(): Unit = { runner_01.runOneTest("setVar1_d_unparse") }

  @Test def test_escapeCharVars_01(): Unit = { runner.runOneTest("escapeCharVars_01") }
  @Test def test_escapeCharVars_02(): Unit = { runner.runOneTest("escapeCharVars_02") }

  @Test def test_multipleVarReadInPoU_01(): Unit = { runner.runOneTest("multipleVarReadInPoU_01") }


/*****************************************************************/
  val tdmlVal = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example

  val variables2 =
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdmlVal } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
      <tdml:defineSchema name="mySchema">
        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
        <dfdl:format ref="tns:GeneralFormat"/>
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

  @Test def test_variables2(): Unit = {
    val testSuite = variables2
    val runner = Runner(testSuite)
    runner.runOneTest("variables2")
    runner.reset
  }

  val variables3 =
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdmlVal } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
      <tdml:defineSchema name="mySchema">
        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
        <dfdl:format ref="tns:GeneralFormat"/>
        <dfdl:defineVariable name="x" type="xs:double"/>
        <xs:element name="data">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="e1" type="xs:double" dfdl:lengthKind="delimited" dfdl:textNumberPattern="#.###############">
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

  @Test def test_variables3(): Unit = {
    // Debugger.setDebugging(true)
    val testSuite = variables3
    val runner = Runner(testSuite)
    runner.runOneTest("variables3")
    runner.reset
  }
}
