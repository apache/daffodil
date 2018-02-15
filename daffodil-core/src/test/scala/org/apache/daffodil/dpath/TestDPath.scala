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

package org.apache.daffodil.dpath

import org.junit.Test
import org.apache.daffodil.Implicits._; object INoWarn { ImplicitsSuppressUnusedImportWarning() }
import org.apache.daffodil.util.SchemaUtils
import org.apache.daffodil.util.TestUtils

class TestDPath {

  val testSchemaNoRef = SchemaUtils.dfdlTestSchemaUnqualified(
    <dfdl:format ref="tns:GeneralFormat" representation="binary" binaryNumberRep="binary" lengthUnits="bytes"/>,
    <xs:element name="a">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="b" type="xs:unsignedInt" dfdl:inputValueCalc="{ 4 }"/>
          <xs:element name="c">
            <xs:complexType>
              <xs:sequence>
                <xs:element name="d" type="xs:int" dfdl:inputValueCalc="{ ../../b }"/>
              </xs:sequence>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
    </xs:element>)

  @Test def test_twoUpwardSteps() {
    TestUtils.testString(testSchemaNoRef, "")
  }

  val testSchema = SchemaUtils.dfdlTestSchemaUnqualified(
    <dfdl:format ref="tns:GeneralFormat" representation="binary" binaryNumberRep="binary" lengthUnits="bytes"/>,
    <xs:element name="a">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="b" type="xs:unsignedInt" dfdl:inputValueCalc="{ 4 }"/>
          <xs:element ref="tns:c"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>
    <xs:element name="c">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="d" type="xs:int" dfdl:inputValueCalc="{ ../../b }"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>)

  @Test def test_twoUpwardStepsAcrossElementReference() {
    TestUtils.testString(testSchema, "")
  }

  val testSchema2 = SchemaUtils.dfdlTestSchemaUnqualified(
    <dfdl:format ref="tns:GeneralFormat" representation="binary" binaryNumberRep="binary" lengthUnits="bytes"/>,
    <xs:element name="a">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="b" type="xs:unsignedInt" dfdl:inputValueCalc="{ 4 }"/>
          <xs:element ref="tns:c"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>
    <xs:element name="c" type="xs:int" dfdl:inputValueCalc="{ ../b }"/>)

  @Test def test_oneUpwardStepsAcrossElementReference() {
    TestUtils.testString(testSchema2, "")
  }

}
