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

import org.junit.Assert._
import org.junit.Test
import org.apache.daffodil.util.SchemaUtils
import org.apache.daffodil.compiler._
import scala.util.parsing.combinator.Parsers
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.xml.GlobalQName
import org.apache.daffodil.Implicits._; object INoWarn2 { ImplicitsSuppressUnusedImportWarning() }
import org.apache.daffodil.processors.parsers.PState
import org.apache.daffodil.util.TestUtils
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.infoset.NullInfosetOutputter
import org.apache.daffodil.infoset.TestInfoset
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.infoset.InfosetDocument

class TestDFDLExpressionEvaluation extends Parsers {

  def testExpr(testSchema: scala.xml.Elem, infosetAsXML: scala.xml.Elem, expr: String)(body: Any => Unit): Unit = {
    val schemaCompiler = Compiler().withTunable("allowExternalPathExpressions", "true")
    val pf = schemaCompiler.compileNode(testSchema).asInstanceOf[ProcessorFactory]
    val sset = pf.sset
    if (pf.isError) fail("pf compile errors")
    val dp = pf.onPath("/").asInstanceOf[DataProcessor]
    val infosetRootElem = TestInfoset.elem2Infoset(infosetAsXML, dp)
    val erd = infosetRootElem.erd
    val qn = GlobalQName(Some("daf"), "testExpr", XMLUtils.dafintURI)
    val exprCompiler = new DFDLPathExpressionParser[AnyRef](qn, NodeInfo.AnyType, testSchema.scope, erd.dpathCompileInfo, false, sset)
    val compiledExpr = exprCompiler.compile(expr)
    val doc = infosetRootElem.parent.asInstanceOf[InfosetDocument]

    val dis = InputSourceDataInputStream(java.nio.ByteBuffer.allocate(0)) // fake. Zero bits available.
    val outputter = new NullInfosetOutputter()
    val pstate = PState.createInitialPState(doc, erd, dis, outputter, dp, areDebugging = false)
    val result = compiledExpr.evaluate(pstate)
    body(result)
  }

  @Test def test_a() = {
    val schema = SchemaUtils.dfdlTestSchemaUnqualified(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="a" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(5) }"/>)

    val data = <a xmlns="http://example.com">aaaaa</a>
    testExpr(schema, data, "{ /tns:a }") { (res: Any) =>
      assertEquals("aaaaa", res)
    }
  }

  @Test def test_ba(): Unit = {
    val schema = SchemaUtils.dfdlTestSchemaUnqualified(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="b">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(5) }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val ex = "http://example.com"
    val data = <ex:b xmlns:ex={ ex }><a>aaaaa</a></ex:b>
    testExpr(schema, data, "{ /tns:b/a }") { (res: Any) =>
      assertEquals("aaaaa", res)
    }
  }

  @Test def test_arrayCount1(): Unit = {
    val schema = SchemaUtils.dfdlTestSchemaUnqualified(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="b">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" maxOccurs="2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(5) }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val data = <ex:b xmlns:ex="http://example.com"><a>aaaaa</a><a>bbbbb</a></ex:b>
    testExpr(schema, data, "{ fn:count(/tns:b/a) }") { (res: Any) =>
      assertEquals(2L, res)
    }
  }

  @Test def test_arrayIndex1(): Unit = {
    val schema = SchemaUtils.dfdlTestSchemaUnqualified(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="b">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" maxOccurs="2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(5) }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val data = <ex:b xmlns:ex="http://example.com"><a>aaaaa</a><a>bbbbb</a></ex:b>
    testExpr(schema, data, "{ /tns:b/a[1] }") { (res: Any) =>
      assertEquals("aaaaa", res)
    }
  }

  @Test def test_absPathWithArrayIndex1(): Unit = {
    val schema = SchemaUtils.dfdlTestSchemaUnqualified(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="b">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" maxOccurs="2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(5) }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val data = <ex:b xmlns:ex="http://example.com"><a>aaaaa</a><a>bbbbb</a></ex:b>
    testExpr(schema, data, "{ /tns:b/a[2] }") { (res: Any) =>
      assertEquals("bbbbb", res)
    }
  }

  @Test def test_ivc1(): Unit = {
    val schema = SchemaUtils.dfdlTestSchemaUnqualified(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="b">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="i" type="xs:long" dfdl:inputValueCalc="{ 5 }"/>
            <xs:element name="a" type="xs:long" dfdl:inputValueCalc="{ ../i }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    TestUtils.testString(schema, "")
  }
}
