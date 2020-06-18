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
import org.junit.Test
import org.apache.daffodil.xml.NS.implicitNStoString

class TestTDMLRunnerTutorial {

  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example
  val xhtml = XMLUtils.XHTML_NAMESPACE

  val tdmlWithTutorial =
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:xsi={ xsi } xmlns={ xhtml } xmlns:ex={ example }>
      <tdml:tutorial>
        <p>1</p>
      </tdml:tutorial>
      <tdml:defineSchema name="mySchema">
        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
        <dfdl:format ref="tns:GeneralFormat"/>
        <xs:element name="data" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(2) }"/>
      </tdml:defineSchema>
      <tdml:tutorial>
        <p>2</p>
      </tdml:tutorial>
      <tdml:parserTestCase name="testTutorialElementsParse" root="data" model="mySchema">
        <tdml:tutorial>
          <p>3</p>
        </tdml:tutorial>
        <tdml:document>37</tdml:document>
        <tdml:tutorial>
          <p>4</p>
        </tdml:tutorial>
        <tdml:infoset>
          <tdml:dfdlInfoset>
            <ex:data>37</ex:data>
          </tdml:dfdlInfoset>
        </tdml:infoset>
        <tdml:tutorial>
          <p>5</p>
        </tdml:tutorial>
      </tdml:parserTestCase>
      <tdml:tutorial>
        <p>6</p>
      </tdml:tutorial>
      <tdml:defineSchema name="s">
        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
        <dfdl:format ref="ex:GeneralFormat"/>
        <xs:element name="bar" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
      </tdml:defineSchema>
      <tdml:tutorial>
        <p>7</p>
      </tdml:tutorial>
      <tdml:unparserTestCase name="testTutorialElementsUnparse" root="bar" model="s">
        <tdml:tutorial>
          <p>8</p>
        </tdml:tutorial>
        <tdml:infoset>
          <tdml:dfdlInfoset>
            <ex:bar>Hello</ex:bar>
          </tdml:dfdlInfoset>
        </tdml:infoset>
        <tdml:tutorial>
          <p>9</p>
        </tdml:tutorial>
        <tdml:document>Hello</tdml:document>
        <tdml:tutorial>
          <p>10</p>
        </tdml:tutorial>
      </tdml:unparserTestCase>
      <tdml:tutorial>
        <p>11</p>
      </tdml:tutorial>
    </tdml:testSuite>

  @Test def testTutorialElementsParse(): Unit = {
    val testSuite = tdmlWithTutorial
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testTutorialElementsParse")
  }

  @Test def testTutorialElementsUnparse(): Unit = {
    val testSuite = tdmlWithTutorial
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testTutorialElementsUnparse")
  }
}
