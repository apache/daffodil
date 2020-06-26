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

package org.apache.daffodil.tdml

import org.apache.daffodil.xml.XMLUtils
import org.junit.Assert._
import org.junit.Test
import org.apache.daffodil.Implicits._

class TestTDMLCrossTest {

  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example
  val fn = XMLUtils.XPATH_FUNCTION_NAMESPACE

  @Test def testUnrecognizedImpl1(): Unit = {

    val testSuite =
      <ts:testSuite xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:ex={ example } xmlns:ts={ tdml } suiteName="theSuiteName" defaultImplementations="notAnImplName">
        <ts:defineSchema name="s">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format ref="ex:GeneralFormat"/>
          <xs:element name="r" type="xs:string" dfdl:lengthKind="delimited"/>
        </ts:defineSchema>
        <ts:parserTestCase ID="test1" name="test1" root="r" model="s" roundTrip="onePass">
          <ts:infoset>
            <ts:dfdlInfoset>
              <ex:r>foo</ex:r>
            </ts:dfdlInfoset>
          </ts:infoset>
          <ts:document>foo</ts:document>
        </ts:parserTestCase>
      </ts:testSuite>
    lazy val ts = new DFDLTestSuite(testSuite)
    val e = intercept[TDMLException] {
      ts.runOneTest("test1")
    }
    val msg = e.getMessage()
    assertTrue(msg.contains("notAnImplName"))
  }

}
