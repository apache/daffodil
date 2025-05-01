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

package org.apache.daffodil.core.layers

import scala.xml.Elem

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.Implicits.intercept
import org.apache.daffodil.lib.util._
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.processors.parsers.ParseError
import org.apache.daffodil.runtime1.processors.unparsers.UnparseError

import org.junit.Assert.assertTrue
import org.junit.Test

class TestFixedLengthLayer {
  val example = XMLUtils.EXAMPLE_NAMESPACE

  def fl1Schema(flLayerLen: Int): Elem =
    SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
      <xs:import namespace="urn:org.apache.daffodil.layers.fixedLength"
                 schemaLocation="/org/apache/daffodil/layers/xsd/fixedLengthLayer.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" lengthKind="delimited"/>
      <dfdl:defineVariable name="flLayerLen" type="xs:int" external="true" defaultValue={
        flLayerLen.toString
      }/>,
      <xs:element name="e1" dfdl:lengthKind="implicit"
                  xmlns:fl="urn:org.apache.daffodil.layers.fixedLength">
        <xs:complexType>
          <xs:sequence>
          <xs:sequence dfdlx:layer="fl:fixedLength">
              <xs:annotation><xs:appinfo source="http://www.ogf.org/dfdl/">
                <dfdl:newVariableInstance ref="fl:fixedLength" defaultValue="{ $ex:flLayerLen }"/>
              </xs:appinfo></xs:annotation>
                  <xs:element name="s1" type="xs:string" />
            </xs:sequence>
            <xs:element name="after" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )

  val abcInfoset =
    <ex:e1 xmlns:ex={example}>
      <s1>AB</s1>
      <after>C</after>
    </ex:e1>

  @Test def testfixedLengthLayerOk1(): Unit = {
    val sch = fl1Schema(2)
    val data = "ABC"
    val infoset = abcInfoset

    val (_, actual) = TestUtils.testString(sch, data, areTracing = false)
    XMLUtils.compareAndReport(infoset, actual)

    TestUtils.testUnparsing(sch, infoset, data, areTracing = false)
  }

  @Test def testfixedLengthLayerBadNegLen1(): Unit = {
    val sch = fl1Schema(-1)
    val data = "ABC"
    val infoset = abcInfoset

    val e = intercept[ParseError] {
      TestUtils.testString(sch, data, areTracing = false)
    }
    val msg = e.getMessage.toLowerCase()
    assertTrue(msg.contains("cannot convert '-1'"))

    val f = intercept[UnparseError] {
      TestUtils.testUnparsing(sch, infoset, data, areTracing = false)
    }
    val m = f.getMessage.toLowerCase
    assertTrue(m.contains("cannot convert '-1'"))
  }

  @Test def testfixedLengthLayerBadTooBigLen1(): Unit = {
    val sch = fl1Schema(65536)
    val data = "ABC"
    val infoset = abcInfoset

    val e = intercept[ParseError] {
      TestUtils.testString(sch, data, areTracing = false)
    }
    val msg = e.getMessage.toLowerCase()
    assertTrue(msg.contains("65536 is above the maximum"))

    val f = intercept[UnparseError] {
      TestUtils.testUnparsing(sch, infoset, data, areTracing = false)
    }
    val m = f.getMessage.toLowerCase
    assertTrue(m.contains("65536 is above the maximum"))
  }

  @Test def testfixedLengthLayerBadParseDataShort1(): Unit = {
    val sch = fl1Schema(2)
    val data = "A"
    val infoset = abcInfoset

    val e = intercept[ParseError] {
      TestUtils.testString(sch, data, areTracing = false)
    }
    val msg = e.getMessage.toLowerCase()
    assertTrue(msg.contains("insufficient data"))
  }

  @Test def testfixedLengthLayerBadUnparseDataShort1(): Unit = {
    val sch = fl1Schema(2)
    val data = "A"
    val infoset = <ex:e1 xmlns:ex={example}>
      <s1>A</s1>
      <after>D</after>
    </ex:e1>

    val e = intercept[UnparseError] {
      TestUtils.testUnparsing(sch, infoset, data, areTracing = false)
    }
    val msg = e.getMessage.toLowerCase()
    assertTrue(msg.contains("insufficient output data"))
  }

  @Test def testfixedLengthLayerBadUnparseDataLong1(): Unit = {
    val sch = fl1Schema(2)
    val data = "ABC"
    val infoset = <ex:e1 xmlns:ex={example}>
      <s1>ABC</s1>
      <after>D</after>
    </ex:e1>
    val f = intercept[UnparseError] {
      TestUtils.testUnparsing(sch, infoset, data, areTracing = false)
    }
    val m = f.getMessage.toLowerCase
    assertTrue(m.contains("exceeded fixed layer length of 2"))
  }
}
