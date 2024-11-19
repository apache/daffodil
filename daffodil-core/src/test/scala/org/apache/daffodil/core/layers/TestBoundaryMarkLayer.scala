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

class TestBoundaryMarkLayer {
  val example = XMLUtils.EXAMPLE_NAMESPACE

  def bm1Schema(bm: String, encoding: String = "iso-8859-1"): Elem =
    SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <xs:import namespace="urn:org.apache.daffodil.layers.boundaryMark"
                     schemaLocation="/org/apache/daffodil/layers/xsd/boundaryMarkLayer.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" lengthKind="delimited"/>
          <dfdl:defineVariable name="bmark" type="xs:string" external="true" defaultValue={
        bm
      }/>,
      <xs:element name="e1" dfdl:lengthKind="implicit"
                  xmlns:bm="urn:org.apache.daffodil.layers.boundaryMark">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence dfdlx:layer="bm:boundaryMark">
              <xs:annotation>
                <xs:appinfo source="http://www.ogf.org/dfdl/">
                  <dfdl:newVariableInstance ref="bm:boundaryMark" defaultValue="{ $ex:bmark }"/>
                  <dfdl:newVariableInstance ref="bm:layerEncoding" defaultValue={encoding}/>
                </xs:appinfo>
              </xs:annotation>
              <xs:element name="s1" type="xs:string"/>
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

  @Test def testBoundaryMarkLayerOk1(): Unit = {
    val sch = bm1Schema(";")
    val data = "AB;C"
    val infoset = abcInfoset

    val (_, actual) = TestUtils.testString(sch, data, areTracing = false)
    XMLUtils.compareAndReport(infoset, actual)

    TestUtils.testUnparsing(sch, infoset, data, areTracing = false)
  }

  @Test def testBoundaryMarkBadEmptyString1(): Unit = {
    val sch = bm1Schema("")
    val data = "AB;C"
    val infoset = abcInfoset

    val e = intercept[ParseError] {
      TestUtils.testString(sch, data, areTracing = false)
    }
    val msg = e.getMessage.toLowerCase()
    assertTrue(msg.contains("may not be empty string"))

    val f = intercept[UnparseError] {
      TestUtils.testUnparsing(sch, infoset, data, areTracing = false)
    }
    val m = f.getMessage.toLowerCase
    assertTrue(m.contains("may not be empty string"))
  }

  @Test def testBoundaryMarkBadTooLongString1(): Unit = {
    val sch = bm1Schema("!" * 32768)
    val data = "AB;C"
    val infoset = abcInfoset

    val e = intercept[ParseError] {
      TestUtils.testString(sch, data, areTracing = false)
    }
    val msg = e.getMessage.toLowerCase()
    assertTrue(msg.contains("length may not be greater than the limit"))

    val f = intercept[UnparseError] {
      TestUtils.testUnparsing(sch, infoset, data, areTracing = false)
    }
    val m = f.getMessage.toLowerCase
    assertTrue(m.contains("length may not be greater than the limit"))
  }

  @Test def testBoundaryMarkBadLayerEncoding1(): Unit = {
    val sch = bm1Schema(";", "notACharsetName")
    val data = "AB;C"
    val infoset = abcInfoset

    val e = intercept[ParseError] {
      TestUtils.testString(sch, data, areTracing = false)
    }
    val msg = e.getMessage
    assertTrue(msg.contains("notACharsetName"))

    val f = intercept[UnparseError] {
      TestUtils.testUnparsing(sch, infoset, data, areTracing = false)
    }
    val m = f.getMessage
    assertTrue(m.contains("notACharsetName"))
  }
}
