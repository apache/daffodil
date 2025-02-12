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

package org.apache.daffodil.core.outputValueCalc

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Assert._
import org.junit.Test

/*
 * These are all tests of OVC expressions that forward reference
 * (and backward. It's a mixture)
 */
class TestOutputValueCalcForwardReference {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testOutputValueCalcForwardReference1(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s2 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:e1 xmlns:ex={example}><s2>2</s2></ex:e1>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "22", areTracing)
  }

  @Test def testOutputValueCalcForwardReference2(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes" textTrimKind="padChar" textStringPadCharacter="%SP;"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s2 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s3 }"/>
            <xs:element name="s3" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:e1 xmlns:ex={example}><s3>3</s3></ex:e1>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "333", areTracing)
  }

  @Test def testOutputValueCalcForwardReference3(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s2 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="s3" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:e1 xmlns:ex={example}><s2>2</s2></ex:e1>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "222", areTracing)
  }

  @Test def testOutputValueCalcDeadlock(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s2 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s3 }"/>
            <xs:element name="s3" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:e1 xmlns:ex={example}></ex:e1>
    val areTracing = false

    try {
      TestUtils.testUnparsing(sch, infoset, "222", areTracing)
      fail("Expected SuspendedExpressionsDeadlockException")
    } catch {
      case e: Exception => {
        val msg = e.getMessage().toLowerCase()
        assertTrue(msg.contains("deadlocked"))
        assertTrue(msg.contains("runtime schema definition error"))
      }
    }

  }

  @Test def testOutputValueCalcConstant(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="foo" type="xs:string" dfdl:outputValueCalc='{ "abcde" }' dfdl:lengthKind="explicit" dfdl:length="5"/>
            <xs:element name="afterFoo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:bar xmlns:ex={example}><afterFoo>fghij</afterFoo></ex:bar>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "abcdefghij", areTracing)
  }

  @Test def testOutputValueCalcAfterOptional(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator=",%#x20;">
            <xs:element name="beforeFoo" type="xs:string" dfdl:initiator="beforeFoo=" dfdl:lengthKind="explicit" dfdl:length="5" minOccurs="0"/>
            <xs:element name="foo" type="xs:string" dfdl:outputValueCalc='{ "abcde" }' dfdl:initiator="foo=" dfdl:lengthKind="explicit" dfdl:length="5"/>
            <xs:element name="afterFoo" dfdl:initiator="afterFoo=" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:bar xmlns:ex={
      example
    }><beforeFoo>12345</beforeFoo><afterFoo>67890</afterFoo></ex:bar>
    val areTracing = false
    TestUtils.testUnparsing(
      sch,
      infoset,
      "beforeFoo=12345, foo=abcde, afterFoo=67890",
      areTracing
    )
  }

  @Test def testMultipleOutputValueCalcAndDefaultablePresent(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator=",%#x20;">
            <xs:element name="beforeFoo" type="xs:string" dfdl:initiator="beforeFoo=" dfdl:lengthKind="explicit" dfdl:length="5" minOccurs="0"/>
            <xs:element name="foo" dfdl:initiator="foo=" type="xs:string" dfdl:outputValueCalc='{ "abcde" }' dfdl:lengthKind="explicit" dfdl:length="5"/>
            <xs:element name="foo2" dfdl:initiator="foo2=" type="xs:string" default="fghij" dfdl:lengthKind="delimited" dfdl:terminator="!"/>
            <xs:element name="afterFoo" dfdl:initiator="afterFoo=" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:bar xmlns:ex={
      example
    }><beforeFoo>12345</beforeFoo><foo2>pqrst</foo2><afterFoo>67890</afterFoo></ex:bar>
    val areTracing = false
    TestUtils.testUnparsing(
      sch,
      infoset,
      "beforeFoo=12345, foo=abcde, foo2=pqrst!, afterFoo=67890",
      areTracing
    )
  }

}
