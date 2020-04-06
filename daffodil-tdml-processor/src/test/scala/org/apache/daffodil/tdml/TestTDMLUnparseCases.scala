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
import org.junit.Assert.assertEquals
import org.junit.Test
import org.apache.daffodil.Implicits._

class TestTDMLUnparseCases {

  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example

  @Test def testUnparseSuite1() {

    val testSuite = <ts:testSuite xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:ex={ example } xmlns:ts={ tdml } suiteName="theSuiteName">
                      <ts:defineSchema name="s">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="ex:GeneralFormat"/>
                        <xs:element name="bar" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
                      </ts:defineSchema>
                      <ts:unparserTestCase ID="test1" name="test1" root="bar" model="s">
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <ex:bar>Hello</ex:bar>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                        <ts:document>Hello</ts:document>
                      </ts:unparserTestCase>
                    </ts:testSuite>
    lazy val ts = new DFDLTestSuite(testSuite)
    val tc: UnparserTestCase = ts.unparserTestCases.find { utc => utc.tcName == "test1" }.get
    // println(tc)
    tc.document
    val is = tc.inputInfoset
    // println(is)
    val bar = is.dfdlInfoset.contents
    val scala.xml.Elem(pre, label, _, _, _) = bar
    assertEquals("ex", pre)
    assertEquals("bar", label)
    // ts.trace
    ts.runOneTest("test1")
  }

}
