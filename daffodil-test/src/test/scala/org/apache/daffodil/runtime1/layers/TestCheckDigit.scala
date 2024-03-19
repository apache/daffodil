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
package org.apache.daffodil.runtime1.layers

import scala.xml.Elem

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.Implicits.intercept
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.processors.parsers.ParseError
import org.apache.daffodil.runtime1.processors.unparsers.UnparseError
import org.apache.daffodil.tdml.Runner

import org.junit.AfterClass
import org.junit.Assert.assertTrue
import org.junit.Test

object TestCheckDigit {

  lazy val runner = Runner("/org/apache/daffodil/layers", "TestCheckDigit.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}

class TestCheckDigit {

  import TestCheckDigit._

  //
  // These are regular TDML tests
  //
  @Test def test_checkDigit_ok_01(): Unit = { runner.runOneTest("test_checkDigit_ok_01") }
  @Test def test_checkDigit_bad_parse_short_01(): Unit = {
    runner.runOneTest("test_checkDigit_bad_parse_short_01")
  }
  @Test def test_checkDigit_bad_invalid(): Unit = {
    runner.runOneTest("test_checkDigit_bad_invalid")
  }
  @Test def test_checkDigit_unparse_ok_01(): Unit = {
    runner.runOneTest("test_checkDigit_unparse_ok_01")
  }

  //
  // These don't use the TDML framework
  //
  private val example = XMLUtils.EXAMPLE_NAMESPACE.toString

  def cdSchema(flLayerLen: Int, layerEnc: String): Elem = {
    val topLevels = {
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
        <xs:import namespace="urn:org.apache.daffodil.layers.checkDigit"
                   schemaLocation="/org/apache/daffodil/layers/xsd/checkDigitLayer.dfdl.xsd"/>
      <xs:simpleType name="alwaysInvalid" dfdl:lengthKind="explicit" dfdl:length="0">
        <xs:restriction base="xs:string">
          <xs:minLength value="1"/>
        </xs:restriction>
      </xs:simpleType>
    }

    val dfdlTopLevels = {
      <dfdl:format ref="ex:GeneralFormat" lengthKind="delimited"/>
        <dfdl:defineVariable name="flLayerLen" type="xs:int" external="true" defaultValue={
        flLayerLen.toString
      }/>
        <dfdl:defineVariable name="cdLayerEnc" type="xs:string" external="true" defaultValue={
        layerEnc
      }/>
    }

    val theElement = {
      <xs:element name="e1" dfdl:lengthKind="implicit"
                  xmlns:cd="urn:org.apache.daffodil.layers.checkDigit"
                  xmlns:ex={example}
                  xmlns:xs="http://www.w3.org/2001/XMLSchema">
        <xs:complexType>
          <xs:sequence>
            <xs:annotation><xs:appinfo source="http://www.ogf.org/dfdl/">
              <dfdl:newVariableInstance ref="cd:checkDigit"/>
            </xs:appinfo></xs:annotation>
            <xs:sequence dfdlx:layer="cd:checkDigit">
              <xs:annotation><xs:appinfo source="http://www.ogf.org/dfdl/">
                <dfdl:newVariableInstance ref="cd:params" defaultValue="verbose"/>
                <dfdl:newVariableInstance ref="cd:digitEncoding" defaultValue="{ $ex:cdLayerEnc }"/>
                <dfdl:newVariableInstance ref="cd:length" defaultValue="{ $ex:flLayerLen }"/>
              </xs:appinfo></xs:annotation>
              <xs:element name="value" type="xs:string"/>
            </xs:sequence>
            <xs:element name="checkDigit" type="cd:checkDigitType" dfdl:initiator=":"
                     dfdl:outputValueCalc="{ $cd:checkDigit }"/>
            <xs:element name="computedCheckDigit" type="cd:checkDigitType"
                     dfdl:inputValueCalc="{ $cd:checkDigit }"/>
            <xs:element name="invalidCheckDigit" type="ex:alwaysInvalid" minOccurs="0"
                     dfdl:occursCountKind="expression"
                     dfdl:occursCount="{ if (../checkDigit eq ../computedCheckDigit) then 0 else 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    }

    SchemaUtils.dfdlTestSchema(
      topLevels,
      dfdlTopLevels,
      theElement,
      elementFormDefault = "unqualified",
      useDefaultNamespace = false,
      useTNS = false,
    )
  }

  val okInfoset: Elem =
    <ex:e1 xmlns="" xmlns:ex={example}>
      <value>2021-09-25</value>
      <checkDigit>1</checkDigit>
      <computedCheckDigit>1</computedCheckDigit>
    </ex:e1>

  @Test def testCheckDigitLayerOk1(): Unit = {
    val sch = cdSchema(10, "ascii")
    val data = "2021-09-25:1"
    val infoset = okInfoset

    val (_, actual) = TestUtils.testString(sch, data, areTracing = false)
    TestUtils.assertEqualsXMLElements(infoset, actual)

    TestUtils.testUnparsing(sch, infoset, data, areTracing = false)
  }

  @Test def testCheckDigitLayerBadLengthNeg1(): Unit = {
    val sch = cdSchema(-1, "ascii")
    val data = "2021-09-25:1"
    val infoset = okInfoset;
    // parse
    {
      val e = intercept[ParseError] {
        TestUtils.testString(sch, data, areTracing = false)
      }
      val msg = TestUtils.getAMessage(e)
      assertTrue(msg.contains("length is negative"))
    }
    // unparse
    {
      val e = intercept[UnparseError] {
        TestUtils.testUnparsing(sch, infoset, data, areTracing = false)
      }
      val msg = TestUtils.getAMessage(e)
      assertTrue(msg.contains("length is negative"))
    }
  }

  @Test def testCheckDigitLayerBadTooBig1(): Unit = {
    val sch = cdSchema(32768, "ascii")
    val data = "A" * 32768
    val infoset = okInfoset;
    // parse
    {
      val e = intercept[ParseError] {
        TestUtils.testString(sch, data, areTracing = false)
      }
      val msg = TestUtils.getAMessage(e)
      assertTrue(msg.contains("out of range for type"))
    }
    // unparse
    {
      val e = intercept[UnparseError] {
        TestUtils.testUnparsing(sch, infoset, data, areTracing = false)
      }
      val msg = TestUtils.getAMessage(e)
      assertTrue(msg.contains("out of range for type"))
    }
  }

  @Test def testCheckDigitLayerBadParseDataTooShort1(): Unit = {
    val sch = cdSchema(10, "ascii")
    val data = "ABC:1"
    val infoset = okInfoset;
    val e = intercept[ParseError] {
      TestUtils.testString(sch, data, areTracing = false)
    }
    val msg = TestUtils.getAMessage(e)
    assertTrue(msg.toLowerCase.contains("insufficient data"))
  }

  /**
   * Not actually an error case. Output data cannot be too long, but
   * it is allowed to be shorter than the length variable says.
   *
   * That won't be behavior that is utilized in many cases, but it
   * is available, and would be used along with dfdl:outputValueCalc
   * that populates data with new length values, thereby allowing the
   * unparsed data to be shorter and have the layer length that is
   * computed on reparse be correct for this shorter data.
   */
  @Test def testCheckDigitLayerOkUnparseDataTooShort1(): Unit = {
    val sch = cdSchema(10, "ascii")
    val data = "123:6"
    val infoset =
      <ex:e1 xmlns="" xmlns:ex={example}>
        <value>123</value>
        <checkDigit>0</checkDigit>
        <computedCheckDigit>0</computedCheckDigit>
      </ex:e1>;
    TestUtils.testUnparsing(sch, infoset, data, areTracing = false)
  }

  @Test def testCheckDigitLayerBadUnparseDataTooLong1(): Unit = {
    val sch = cdSchema(5, "ascii")
    val data = "1234567890:1"
    val infoset =
      <ex:e1 xmlns="" xmlns:ex={example}>
        <value>1234567890</value>
        <checkDigit>0</checkDigit>
        <computedCheckDigit>0</computedCheckDigit>
      </ex:e1>;
    val e = intercept[UnparseError] {
      TestUtils.testUnparsing(sch, infoset, data, areTracing = false)
    }
    val msg = TestUtils.getAMessage(e)
    assertTrue(msg.toLowerCase.contains("exceeded fixed layer length"))
  }

  @Test def testCheckDigitBadEncoding1(): Unit = {
    val sch = cdSchema(10, "notACharsetName")
    val data = "1234567890:1"
    val infoset =
      <ex:e1 xmlns="" xmlns:ex={example}>
        <value>1234567890</value>
        <checkDigit>0</checkDigit>
        <computedCheckDigit>0</computedCheckDigit>
      </ex:e1>;

    val e = intercept[Throwable] {
      TestUtils.testString(sch, data, areTracing = false)
    }
    val msg = TestUtils.getAMessage(e)
    assertTrue(msg.contains("notACharsetName"))

    val f = intercept[UnparseError] {
      TestUtils.testUnparsing(sch, infoset, data, areTracing = false)
    }
    val m = TestUtils.getAMessage(f)
    assertTrue(m.contains("notACharsetName"))
  }

}
