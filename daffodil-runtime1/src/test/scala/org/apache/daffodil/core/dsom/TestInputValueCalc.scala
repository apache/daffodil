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

package org.apache.daffodil.core.dsom

import scala.xml.Node

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.util._
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Test

class TestInputValueCalc {

  // @Test
  @Test def testInputValueCalc1(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="data" type="xs:string" dfdl:inputValueCalc="{ xs:string(42) }" />
    )
    val (_, actual) = TestUtils.testString(testSchema, "")
    val expected: Node = <data>42</data>
    XMLUtils.compareAndReport(expected, actual)
  }

  // @Test
  @Test def testInputValueCalcString2(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii"/>,
      <xs:element name="data">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="e2" type="xs:string" dfdl:inputValueCalc="{ ../tns:e1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )

    val (_, actual) = TestUtils.testString(testSchema, "A")
    val expected: Node = <data><e1>A</e1><e2>A</e2></data>
    XMLUtils.compareAndReport(expected, actual)
  }

  // @Test
  @Test def testInputValueCalcInt3(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii"/>,
      <xs:element name="data">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="e1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="e2" type="xs:int" dfdl:inputValueCalc="{ ../tns:e1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )

    val (_, actual) = TestUtils.testString(testSchema, "8")
    val expected: Node = <data><e1>8</e1><e2>8</e2></data>
    XMLUtils.compareAndReport(expected, actual)
  }
}
