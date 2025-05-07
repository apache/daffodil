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

package org.apache.daffodil.core.general

import org.apache.daffodil.api.InfosetSimpleElement
import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.lib.Implicits.intercept
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetOutputter

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * Test InfosetOutputter that extends the ScalaXMLInfosetOutputter and redacts
 * words specified in the dfdlx:runtimeProperties extension
 */
class RedactingScalaXMLInfosetOutputter extends ScalaXMLInfosetOutputter {

  override def startSimple(se: InfosetSimpleElement): Unit = {
    super.startSimple(se)

    val runtimeProperties = se.metadata.runtimeProperties

    val redactions = Option(runtimeProperties.get("redact")).map { value => value.split(",") }
    if (redactions.isDefined) {
      val thisElem = stack.top.last.asInstanceOf[scala.xml.Elem]
      var text = thisElem.child(0).asInstanceOf[scala.xml.Text].data
      val redactWord = runtimeProperties.getOrDefault("redactWord", "")
      redactions.get.foreach { redaction =>
        text = text.replace(redaction, redactWord)
      }
      val newChild = Seq(new scala.xml.Text(text))
      val newElem = thisElem.copy(child = newChild)
      stack.top(stack.top.size - 1) = newElem
    }
  }
}

/**
 * Test InfosetInputter that extends the ScalaXMLInfosetInputter and redacts
 * words specified in the dfdlx:runtimeProperties extension
 */
class RedactingScalaXMLInfosetInputter(rootNode: scala.xml.Node)
  extends ScalaXMLInfosetInputter(rootNode) {

  override def getSimpleText(
    primType: NodeInfo.Kind,
    runtimeProperties: java.util.Map[String, String]
  ): String = {
    var text = super.getSimpleText(primType, runtimeProperties)

    val redactions = Option(runtimeProperties.get("redact")).map { value => value.split(",") }
    if (redactions.isDefined) {
      val redactWord = runtimeProperties.getOrDefault("redactWord", "")
      redactions.get.foreach { redaction =>
        text = text.replace(redaction, redactWord)
      }
    }
    text
  }
}

class TestRuntimeProperties {

  val testSchema1 = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="tns:GeneralFormat" lengthKind="delimited"/>,
    <xs:element name="root">
      <xs:complexType>
        <xs:sequence dfdl:separator=",">
          <xs:element name="filtered" type="xs:string" dfdlx:runtimeProperties="redact=foo,baz redactWord=REDACTED" />
          <xs:element name="unFiltered" type="xs:string" />
        </xs:sequence>
      </xs:complexType>
    </xs:element>
  )

  @Test def testRuntimeProperties_01(): Unit = {

    val data = "foo bar baz,foo bar baz"
    val expected =
      <root xmlns="http://example.com">
        <filtered>REDACTED bar REDACTED</filtered>
        <unFiltered>foo bar baz</unFiltered>
      </root>

    val input = InputSourceDataInputStream(data.getBytes("UTF-8"))
    val outputter = new RedactingScalaXMLInfosetOutputter()

    val dp = TestUtils.compileSchema(testSchema1)
    val pr = dp.parse(input, outputter)

    assertFalse(pr.isError)
    val actual = outputter.getResult()
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testRuntimeProperties_02(): Unit = {

    val infoset =
      <root xmlns="http://example.com">
        <filtered>foo bar baz</filtered>
        <unFiltered>foo bar baz</unFiltered>
      </root>
    val expected = "REDACTED bar REDACTED,foo bar baz"

    val inputter = new RedactingScalaXMLInfosetInputter(infoset)
    val output = new java.io.ByteArrayOutputStream()

    val dp = TestUtils.compileSchema(testSchema1)
    val ur = dp.unparse(inputter, output)

    assertFalse(ur.isError)

    val actual = output.toString("UTF-8")
    assertEquals(expected, actual)
  }

  val testSchemaBad1 = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="tns:GeneralFormat" lengthKind="delimited"/>,
    <xs:element name="root" dfdlx:runtimeProperties="redact=foo,baz">
      <xs:complexType>
        <xs:sequence dfdl:separator=",">
          <xs:element name="filtered" type="xs:string" />
          <xs:element name="unFiltered" type="xs:string" />
        </xs:sequence>
      </xs:complexType>
    </xs:element>
  )

  @Test def testRuntimeProperties_03(): Unit = {
    val ex = intercept[Exception] {
      TestUtils.compileSchema(testSchemaBad1)
    }
    val msg = ex.getMessage()

    assertTrue(msg.contains("Schema Definition Error"))
    assertTrue(msg.contains("dfdlx:runtimeProperties"))
    assertTrue(msg.contains("simple type"))
  }

  val testSchemaBad2 = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="tns:GeneralFormat" lengthKind="delimited"/>,
    <xs:element name="root">
      <xs:complexType>
        <xs:sequence dfdl:separator=",">
          <xs:element name="filtered" type="xs:string" dfdlx:runtimeProperties="redact=" />
          <xs:element name="unFiltered" type="xs:string" />
        </xs:sequence>
      </xs:complexType>
    </xs:element>
  )

  @Test def testRuntimeProperties_04(): Unit = {
    val ex = intercept[Exception] {
      TestUtils.compileSchema(testSchemaBad2)
    }
    val msg = ex.getMessage()

    assertTrue(msg.contains("Schema Definition Error"))
    assertTrue(msg.contains("dfdlx:runtimeProperties"))
    assertTrue(msg.contains("key=value"))
  }

  val testSchema2 = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="tns:GeneralFormat" lengthKind="delimited"/>,
    <xs:element name="root">
      <xs:complexType>
        <xs:sequence dfdl:separator=",">
          <xs:element name="filtered" type="xs:string">
            <xs:annotation>
              <xs:appinfo source="http://www.ogf.org/dfdl/">
                <dfdl:element>
                  <dfdl:property name="dfdlx:runtimeProperties">
                    redact=foo,baz
                    redactWord=REDACTED
                  </dfdl:property>
                </dfdl:element>
              </xs:appinfo>
            </xs:annotation>
          </xs:element>
          <xs:element name="unFiltered" type="xs:string" />
        </xs:sequence>
      </xs:complexType>
    </xs:element>
  )

  @Test def testRuntimeProperties_05(): Unit = {

    val data = "foo bar baz,foo bar baz"
    val expected =
      <root xmlns="http://example.com">
        <filtered>REDACTED bar REDACTED</filtered>
        <unFiltered>foo bar baz</unFiltered>
      </root>

    val input = InputSourceDataInputStream(data.getBytes("UTF-8"))
    val outputter = new RedactingScalaXMLInfosetOutputter()

    val dp = TestUtils.compileSchema(testSchema2)
    val pr = dp.parse(input, outputter)

    assertFalse(pr.isError)
    val actual = outputter.getResult()
    XMLUtils.compareAndReport(expected, actual)
  }

}
