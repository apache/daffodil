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
import edu.illinois.ncsa.daffodil.util.SchemaUtils
import edu.illinois.ncsa.daffodil.compiler._
import scala.util.parsing.combinator.Parsers
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.GlobalQName
import edu.illinois.ncsa.daffodil.Implicits._; object INoWarn2 { ImplicitsSuppressUnusedImportWarning() }
import edu.illinois.ncsa.daffodil.processors.parsers.PState
import edu.illinois.ncsa.daffodil.util.TestUtils
import edu.illinois.ncsa.daffodil.io.ByteBufferDataInputStream
import edu.illinois.ncsa.daffodil.infoset.DIDocument
import edu.illinois.ncsa.daffodil.infoset.NullInfosetOutputter
import edu.illinois.ncsa.daffodil.infoset.TestInfoset

class TestDFDLExpressionEvaluation extends Parsers {

  def testExpr(testSchema: scala.xml.Elem, infosetAsXML: scala.xml.Elem, expr: String)(body: Any => Unit) {
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
    val infosetRootElem = TestInfoset.elem2Infoset(erd, infosetAsXML)
    val qn = GlobalQName(Some("daf"), "testExpr", XMLUtils.dafintURI)
    val exprCompiler = new DFDLPathExpressionParser[AnyRef](qn, NodeInfo.AnyType, testSchema.scope, erd.dpathCompileInfo, false, sset)
    val compiledExpr = exprCompiler.compile(expr)
    val doc = infosetRootElem.parent.asInstanceOf[DIDocument]

    val dis = ByteBufferDataInputStream(java.nio.ByteBuffer.allocate(0), 0L) // fake. Zero bits available.
    val outputter = new NullInfosetOutputter()
    val pstate = PState.createInitialPState(doc, erd, dis, outputter, dp)
    val result = compiledExpr.evaluate(pstate)
    body(result)
  }

  @Test def test_a() = {
    val schema = SchemaUtils.dfdlTestSchemaUnqualified(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="a" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(5) }"/>)

    val data = <a xmlns="http://example.com">aaaaa</a>
    testExpr(schema, data, "{ /tns:a }") { (res: Any) =>
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
    val ex = "http://example.com"
    val data = <ex:b xmlns:ex={ ex }><a>aaaaa</a></ex:b>
    testExpr(schema, data, "{ /tns:b/a }") { (res: Any) =>
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
    val data = <ex:b xmlns:ex="http://example.com"><a>aaaaa</a><a>bbbbb</a></ex:b>
    testExpr(schema, data, "{ fn:count(/tns:b/a) }") { (res: Any) =>
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
    val data = <ex:b xmlns:ex="http://example.com"><a>aaaaa</a><a>bbbbb</a></ex:b>
    testExpr(schema, data, "{ /tns:b/a[1] }") { (res: Any) =>
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
    val data = <ex:b xmlns:ex="http://example.com"><a>aaaaa</a><a>bbbbb</a></ex:b>
    testExpr(schema, data, "{ /tns:b/a[2] }") { (res: Any) =>
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

    TestUtils.testString(schema, "")
  }
}
