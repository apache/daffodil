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