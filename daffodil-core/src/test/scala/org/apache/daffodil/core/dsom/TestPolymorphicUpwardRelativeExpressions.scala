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

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.util._
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Assert._
import org.junit.Test

class TestPolymorphicUpwardRelativeExpressions {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  /**
   * This test illustrates that even in the presence of compiling an expression
   * only once, we detect all places that polymorphic references look upward to, and
   * check that all such are consistently typed.
   */
  @Test def testPoly1(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" lengthKind="delimited"/>,
      <xs:element name="r" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="selector" type="xs:string" dfdl:terminator=";"/>
            <xs:choice>
              <xs:element ref="ex:e1"/>
              <xs:element ref="ex:e2"/>
            </xs:choice>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence>
              <xs:annotation>
                <xs:appinfo source="http://www.ogf.org/dfdl/">
                  <dfdl:discriminator test="{ ../selector eq 'e1' }"/>
                </xs:appinfo>
              </xs:annotation>
            </xs:sequence>
            <xs:element name="poly" type="xs:int" dfdl:terminator=";">
              <!--
                 This element ex:r/ex:e1/poly is instance 1. An int.
                -->
            </xs:element>
            <xs:element ref="ex:eShared"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="e2" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence>
              <xs:annotation>
                <xs:appinfo source="http://www.ogf.org/dfdl/">
                  <dfdl:discriminator test="{ ../selector eq 'e2' }"/>
                </xs:appinfo>
              </xs:annotation>
            </xs:sequence>
            <xs:element name="poly" type="xs:date" dfdl:terminator=";">
              <!--
                 This element ex:r/ex:e2/poly is instance 2. A date.

                 It will have a separate ERD and DPathElementCompileInfo object
                 from the prior one.
                -->
            </xs:element>
            <xs:element ref="ex:eShared"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="eShared" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <!--
                 This sequence's groupMembers will be compiled
                 once and shared.

                 So the expression inside eInner is compiled only once.
              -->
            <xs:element ref="ex:eInner"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="eInner" dfdl:lengthKind="implicit">
        <!--
        So since eInner appears only inside eShared's sequence, it will
        be compiled once.

        That means the assert and its expression is compiled once.

        When that expression is compiled, both e1/poly and e2/poly must be checked.
        -->
        <xs:complexType>
          <xs:sequence>
            <xs:sequence>
              <xs:annotation>
                <xs:appinfo source="http://www.ogf.org/dfdl/">
                  <dfdl:assert test="{ ../../poly eq 5 }">
                    <!--
                      This expression referring to poly could be referring
                      to either ex:r/ex:e1/poly or to ex:r/ex:e2/poly

                      Since e2/poly is an xs:date, the expression ../../poly eq 5
                      isn't even type-checked properly.

                      This should get a schema-compile-time type-error.
                     -->
                  </dfdl:assert>
                </xs:appinfo>
              </xs:annotation>
            </xs:sequence>
            <xs:element name="foo" type="xs:int" dfdl:terminator=";"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val e = intercept[Exception] {
      TestUtils.testString(testSchema, "e2;1961-02-01;6;", areTracing = false)
    }
    val msg = e.getMessage()
    val hasSDE = msg.contains("Schema Definition Error")
    val hasPolyInt = msg.contains("../../poly eq 5 with xs:int")
    val hasPolyDate = msg.contains("../../poly eq 5 with xs:date")
    val hasDifferentPhrase = msg.contains("different types at different points")
    assertTrue(hasSDE)
    assertTrue(hasPolyInt)
    assertTrue(hasPolyDate)
    assertTrue(hasDifferentPhrase)
  }

  @Test def testPoly2(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" lengthKind="delimited"/>,
      <xs:element name="r" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="selector" type="xs:string" dfdl:terminator=";"/>
            <xs:choice>
              <xs:element ref="ex:e1"/>
              <xs:element ref="ex:e2"/>
            </xs:choice>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence>
              <xs:annotation>
                <xs:appinfo source="http://www.ogf.org/dfdl/">
                  <dfdl:discriminator test="{ ../selector eq 'e1' }"/>
                </xs:appinfo>
              </xs:annotation>
            </xs:sequence>
            <xs:element name="poly" type="xs:int" dfdl:terminator=";">
              <!--
                 This element ex:r/ex:e1/poly is instance 1
                -->
            </xs:element>
            <xs:element ref="ex:eShared"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="e2" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence>
              <xs:annotation>
                <xs:appinfo source="http://www.ogf.org/dfdl/">
                  <dfdl:discriminator test="{ ../selector eq 'e2' }"/>
                </xs:appinfo>
              </xs:annotation>
            </xs:sequence>
            <xs:element name="poly" type="xs:int" dfdl:terminator=";">
              <!--
                 This element ex:r/ex:e2/poly is instance 2
                 It will have a separate ERD and DPathElementCompileInfo object
                 from the prior one.
                -->
            </xs:element>
            <xs:element ref="ex:eShared"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="eShared" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <!--
                 This sequence's groupMembers will be compiled
                 once and shared.
              -->
            <xs:element ref="ex:eInner"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="eInner" dfdl:lengthKind="implicit">
        <!--
        So since eInner appears only inside eShared's sequence, it will
        be compiled once.

        That means the assert and its expression is compiled once.

        However, both e1/poly and e2/poly which are separate elements with
        separate ERDs and separate DPathElementCompileInfo objects, are both
        being marked as referenced from expressions.
        -->
        <xs:complexType>
          <xs:sequence>
            <xs:sequence>
              <xs:annotation>
                <xs:appinfo source="http://www.ogf.org/dfdl/">
                  <dfdl:assert test="{ ../../poly eq 5 }">
                    <!--
                      This expression referring to poly could be referring
                      to either ex:r/ex:e1/poly or to ex:r/ex:e2/poly
                     -->
                  </dfdl:assert>
                </xs:appinfo>
              </xs:annotation>
            </xs:sequence>
            <xs:element name="foo" type="xs:int" dfdl:terminator=";"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val (_, actual) = TestUtils.testString(testSchema, "e2;5;6;", areTracing = false)
    val expected =
      <ex:r>
        <selector>e2</selector>
        <ex:e2>
          <poly>5</poly>
          <ex:eShared>
            <ex:eInner>
              <foo>6</foo>
            </ex:eInner>
          </ex:eShared>
        </ex:e2>
      </ex:r>
    XMLUtils.compareAndReport(expected, actual)
  }
}
