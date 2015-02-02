/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.dpath

import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.processors.Infoset
import edu.illinois.ncsa.daffodil.util.SchemaUtils
import scala.xml.Utility
import edu.illinois.ncsa.daffodil.processors.EmptyVariableMap
import edu.illinois.ncsa.daffodil.compiler._
import scala.util.parsing.combinator.Parsers
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.xml.NoNamespace
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.xml.StepQName
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.util.TestUtils

class TestDFDLExpressionEvaluation extends Parsers {

  def testExpr(testSchema: scala.xml.Elem, infosetAsXML: scala.xml.Elem, expr: String)(body: (Any, VariableMap) => Unit) {
    val schemaCompiler = Compiler()
    val pf = schemaCompiler.compileNode(testSchema).asInstanceOf[ProcessorFactory]
    if (pf.isError) fail("pf compile errors")
    val dp = pf.onPath("/")
    val sset = pf.sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.forRoot()
    val erd = decl.elementRuntimeData
    val infosetRootElem = Infoset.elem2Infoset(erd, infosetAsXML)

    val exprCompiler = new DFDLPathExpressionParser(NodeInfo.AnyType, testSchema.scope, erd.dpathCompileInfo)
    val compiledExpr = exprCompiler.compile(expr)
    val doc = Infoset.newDocument(erd)
    doc.setRootElement(infosetRootElem)

    val pstate = PState.createInitialState(doc, erd, null, dp)
    val (result, newVMap) = compiledExpr.evaluate(pstate)
    body(result, newVMap)
  }

  @Test def test_a() = {
    val schema = SchemaUtils.dfdlTestSchemaUnqualified(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="a" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(5) }"/>)

    val data = <a xmlns="http://example.com">aaaaa</a>
    testExpr(schema, data, "{ /tns:a }") { (res: Any, vmap: VariableMap) =>
      assertEquals("aaaaa", res)
    }
  }

  @Test def test_ba() {
    val schema = SchemaUtils.dfdlTestSchemaUnqualified(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="b">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(5) }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val data = <b xmlns="http://example.com"><a>aaaaa</a></b>
    testExpr(schema, data, "{ /tns:b/a }") { (res: Any, vmap: VariableMap) =>
      assertEquals("aaaaa", res)
    }
  }

  @Test def test_arrayCount1() {
    val schema = SchemaUtils.dfdlTestSchemaUnqualified(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="b">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" maxOccurs="2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(5) }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val data = <b xmlns="http://example.com"><a>aaaaa</a><a>bbbbb</a></b>
    testExpr(schema, data, "{ fn:count(/tns:b/a) }") { (res: Any, vmap: VariableMap) =>
      assertEquals(2L, res)
    }
  }

  @Test def test_arrayIndex1() {
    val schema = SchemaUtils.dfdlTestSchemaUnqualified(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="b">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" maxOccurs="2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(5) }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val data = <b xmlns="http://example.com"><a>aaaaa</a><a>bbbbb</a></b>
    testExpr(schema, data, "{ /tns:b/a[1] }") { (res: Any, vmap: VariableMap) =>
      assertEquals("aaaaa", res)
    }
  }

  @Test def test_absPathWithArrayIndex1() {
    val schema = SchemaUtils.dfdlTestSchemaUnqualified(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="b">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" maxOccurs="2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(5) }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val data = <b xmlns="http://example.com"><a>aaaaa</a><a>bbbbb</a></b>
    testExpr(schema, data, "{ /tns:b/a[2] }") { (res: Any, vmap: VariableMap) =>
      assertEquals("bbbbb", res)
    }
  }

  @Test def test_ivc1() {
    val schema = SchemaUtils.dfdlTestSchemaUnqualified(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="b">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="i" type="xs:long" dfdl:inputValueCalc="{ 5 }"/>
            <xs:element name="a" type="xs:long" dfdl:inputValueCalc="{ ../i }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val res = TestUtils.testString(schema, "")
    println(res.result)
  }
}