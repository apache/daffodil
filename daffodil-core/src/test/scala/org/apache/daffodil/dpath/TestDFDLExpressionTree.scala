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
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.xml.StepQName
import edu.illinois.ncsa.daffodil.Implicits._
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInt }

class TestDFDLExpressionTree extends Parsers {
  val qn = GlobalQName(Some("daf"), "testExpr", XMLUtils.dafintURI)

  val dummySchema = SchemaUtils.dfdlTestSchema(
    <dfdl:format ref="tns:daffodilTest1"/>,
    <xs:element name="title" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(5) }"/>)

  def testExpr(testSchema: scala.xml.Elem, expr: String)(body: Expression => Unit) {
    val schemaCompiler = Compiler()
    val sset = schemaCompiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.forRoot()
    val erd = decl.elementRuntimeData

    val exprCompiler = new DFDLPathExpressionParser(qn, NodeInfo.String, testSchema.scope, erd.dpathCompileInfo, false, sset)
    val result = exprCompiler.getExpressionTree(expr, sset)
    body(result)
  }

  def testExpr2(testSchema: scala.xml.Elem, expr: String)(body: (Expression, ElementRuntimeData) => Unit) {
    val schemaCompiler = Compiler()
    val sset = schemaCompiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.forRoot()
    val erd = decl
    val exprCompiler = new DFDLPathExpressionParser(qn, NodeInfo.AnyType, testSchema.scope, decl.dpathCompileInfo, false, sset)
    val result = exprCompiler.getExpressionTree(expr, sset)
    body(result, erd.elementRuntimeData)
  }

  val testSchema = SchemaUtils.dfdlTestSchemaUnqualified(
    <dfdl:format ref="tns:daffodilTest1"/>,
    <xs:element name="bookstore">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="book">
            <xs:complexType>
              <xs:sequence>
                <xs:element name="title" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(5) }"/>
              </xs:sequence>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
    </xs:element>)

  val aSchema = SchemaUtils.dfdlTestSchema(
    <dfdl:format ref="tns:daffodilTest1"/>,
    <xs:element name="a" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(5) }"/>)

  @Test def test_a() = {
    testExpr2(aSchema, "{ /tns:a }") { (ce, erd) =>
      val WholeExpression(_, RootPathExpression(Some(RelativePathExpression(steps, _))), _, _, _) = ce
      val List(n1) = steps
      val n1p @ NamedStep("tns:a", None) = n1; assertNotNull(n1p)
      assertFalse(n1.isArray)
      assertEquals(NodeInfo.AnyType, n1.targetType)
      assertEquals(NodeInfo.String, n1.inherentType)
      assertTrue(n1.isLastStep)
      val List(DownElement(erd)) = n1.compiledDPath.ops.toList
      assertEquals(erd, n1.stepElement)
    }
  }

  val bSchema = SchemaUtils.dfdlTestSchemaUnqualified(
    <dfdl:format ref="tns:daffodilTest1"/>,
    <xs:element name="b">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="i" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="2"/>
          <xs:element name="a" maxOccurs="2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(5) }"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>)

  @Test def test_a_pred() {
    testExpr2(bSchema, "{ /tns:b/a[/tns:b/i] }") { (ce, erd) =>
      val w @ WholeExpression(_, RootPathExpression(Some(RelativePathExpression(steps, _))), _, _, _) = ce
      val List(b, a) = steps
      val bp @ NamedStep("tns:b", None) = b; assertNotNull(bp)
      val NamedStep("a", Some(PredicateExpression(i))) = a
      val RootPathExpression(Some(RelativePathExpression(List(_, istep), _))) = i
      val isp @ NamedStep("i", None) = istep; assertNotNull(isp)
      val ops = i.compiledDPath.ops.toList
      val List(ToRoot, DownElement(berd), DownElement(_), IntToLong, LongToArrayIndex) = ops
      val List(ToRoot, DownElement(berd2), DownArrayOccurrence(_, _)) =
        w.compiledDPath.ops.toList
      assertEquals(berd, berd2)
    }
  }

  @Test def test_a_predPred() {
    testExpr(dummySchema, "{ a[i[j]] }") { actual =>
      val WholeExpression(_, RelativePathExpression(steps, _), _, _, _) = actual
      val List(n1) = steps
      val NamedStep("a", Some(PredicateExpression(rel))) = n1
      val RelativePathExpression(List(n2), _) = rel
      val NamedStep("i", Some(PredicateExpression(j))) = n2
      val RelativePathExpression(List(n3), _) = j
      val n3p @ NamedStep("j", None) = n3; assertNotNull(n3p)
    }
  }

  @Test def test_relativePath() = {
    testExpr(dummySchema, "{ ../..[2]/bookstore }") { actual =>
      val WholeExpression(_, RelativePathExpression(steps, _), _, _, _) = actual
      val List(n1, n2, n3) = steps
      val u @ Up(None) = n1; assertNotNull(u)
      val u2 @ Up(Some(PredicateExpression(LiteralExpression(two: JBigInt)))) = n2; assertNotNull(u2)
      assertEquals(BigDecimal(2), BigDecimal(two))
      val n3p @ NamedStep("bookstore", None) = n3; assertNotNull(n3p)
    }
  }

  /**
   * Permutations of stepQName to local element NamedQName matching are:
   * 1) elementFormDefault qualified or unqualified
   * 2) schema document containing element referenced by the step has
   * a target namespace or no target namespace,
   * 3) schemaDocument where the path appears has a default namespace
   *  or no default namespace
   *
   * qualified, target, default - ../foo works, ../tns:foo works - step has namespace from default, elem has qualified name
   * unqualified, target, default ../foo fails as it gets default namespace
   * qualified, noTarget, default XXX makes no sense qualified, but no targetNS
   * unqualified noTarget, default XXX default namespace can't be NoNamespace
   * qualified, target, no default  ../tns:foo works ../foo fails
   * unqualified, target, no default ../foo works ../tns:foo fails (or resolves to wrong thing)
   * qualified, noTarget, no default XXXX qualified but no TargetNS
   * unqualified noTarget, no default ../foo works ../tns:foo fails as tns can't be NoNamespace
   *
   */

  @Test def test_absolutePath() = {
    testExpr2(testSchema, "{ /tns:bookstore/book/title }") { (actual, erd) =>
      val WholeExpression(_, RootPathExpression(Some(RelativePathExpression(steps, _))), _, _, _) = actual
      val List(n1, n2, n3) = steps
      val n1p @ NamedStep("tns:bookstore", None) = n1; assertNotNull(n1p)
      assertTrue(n1.isFirstStep)
      assertFalse(n1.isLastStep)
      assertEquals(0, n1.positionInStepsSequence)
      assertFalse(n1.priorStep.isDefined)
      //
      // The dfdlTestSchema function used to create the schema for this test
      // has elementFormDefault="qualified" (TODO: should be unqualified - run a test on 0.14.0
      // to see what breaks if we change this). It also has a targetNamespace
      // and sets the default namespace xmlns="http://example.com" which matches
      // the target namespace.
      //
      // So the first step resolves to using the example.com namespace by way
      // of the default namespace, AND this matches the qualified local element name.
      //
      val ex = NS("http://example.com")
      assertEquals(StepQName(Some("tns"), "bookstore", ex), n1.stepQName)
      assertEquals(erd.dpathElementCompileInfo, n1.stepElement)
      assertEquals(NS("http://example.com"), erd.targetNamespace)

      val n2p @ NamedStep("book", None) = n2; assertNotNull(n2p)
      assertFalse(n2.isFirstStep)
      assertFalse(n2.isLastStep)
      assertEquals(1, n2.positionInStepsSequence) // zero based
      assertTrue(n2.priorStep.isDefined)
      assertEquals(StepQName(None, "book", NoNamespace), n2.stepQName)
      val bookERD = erd.childERDs.find { _.name == "book" }.get
      assertEquals(bookERD.dpathElementCompileInfo, n2.stepElement)
      val n3p @ NamedStep("title", None) = n3; assertNotNull(n3p)
      assertFalse(n3.isFirstStep)
      assertTrue(n3.isLastStep)
      assertEquals(2, n3.positionInStepsSequence) // zero based
      assertTrue(n3.priorStep.isDefined)
      assertEquals(StepQName(None, "title", NoNamespace), n3.stepQName)
      val titleERD = bookERD.childERDs.find { _.name == "title" }.get
      assertEquals(titleERD.dpathElementCompileInfo, n3.stepElement)
    }
  }

  @Test def test_pathNoSuchElement1() {
    testExpr2(testSchema, "{ /thereIsNoElementWithThisName }") { (actual, erd) =>
      val WholeExpression(_, RootPathExpression(Some(RelativePathExpression(List(step), _))), _, _, _) = actual
      val exc = intercept[SchemaDefinitionError] {
        step.stepElement
      }
      val msg = exc.getMessage()
      assertTrue(msg.contains("thereIsNoElementWithThisName")) // the error
      if (!msg.contains("bookstore"))
        println(msg)
      assertTrue(msg.contains("bookstore")) // the possibilities
    }
  }

  @Test def test_predPath() = {
    testExpr(dummySchema, "{ /bookstore[$i]/book/title }") { actual =>
      val WholeExpression(_, RootPathExpression(Some(RelativePathExpression(steps, _))), _, _, _) = actual
      val List(n1, n2, n3) = steps
      val NamedStep("bookstore", pred) = n1
      // println(pred)
      NS("http://example.com")
      val p @ Some(PredicateExpression(VariableRef("i"))) = pred; assertNotNull(p)
      val n2p @ NamedStep("book", None) = n2; assertNotNull(n2p)
      val n3p @ NamedStep("title", None) = n3; assertNotNull(n3p)
    }
  }

  @Test def testAddConstants() {
    testExpr(dummySchema, "{ 1 + 2 }") { actual =>
      val a @ WholeExpression(_, AdditiveExpression("+", List(LiteralExpression(one: JBigInt), LiteralExpression(two: JBigInt))), _, _, _) = actual; assertNotNull(a)
      assertEquals(BigDecimal(1), BigDecimal(one))
      assertEquals(BigDecimal(2), BigDecimal(two))
    }
  }

  @Test def testFnTrue() {
    testExpr(dummySchema, "{ fn:true() }") { actual =>
      val a @ WholeExpression(_, FunctionCallExpression("fn:true", Nil), _, _, _) = actual; assertNotNull(a)
    }
  }

  @Test def test_numbers1() = {
    testExpr(dummySchema, "0.") { actual: Expression =>
      val res = BigDecimal("0.0")
      val a @ WholeExpression(_, LiteralExpression(actualRes), _, _, _) = actual; assertNotNull(a)
      val bd = BigDecimal(actualRes.asInstanceOf[JBigDecimal])
      assertEquals(res, bd)
    }
  }

  @Test def test_numbers() = {

    testExpr(dummySchema, "5.0E2") { case WholeExpression(_, LiteralExpression(500.0), _, _, _) => /* ok */ ; }
    testExpr(dummySchema, "5E2") { case WholeExpression(_, LiteralExpression(500.0), _, _, _) => /* ok */ ; }
    testExpr(dummySchema, ".2E2") { case WholeExpression(_, LiteralExpression(20.0), _, _, _) => /* ok */ ; }
    testExpr(dummySchema, ".2E-3") { case WholeExpression(_, LiteralExpression(0.0002), _, _, _) => /* ok */ ; }
    testExpr(dummySchema, ".2E+3") { case WholeExpression(_, LiteralExpression(200.0), _, _, _) => /* ok */ ; }

    //    testExpr(dummySchema, "0.") { actual =>
    //      val res = new JBigDecimal("0.0")
    //      val a @ WholeExpression(_, LiteralExpression(`res`), _, _, _) = actual; assertNotNull(a)
    //    }
    testExpr(dummySchema, ".1") { actual =>
      val res = new JBigDecimal("0.1")
      val a @ WholeExpression(_, LiteralExpression(`res`), _, _, _) = actual; assertNotNull(a)
    }
    testExpr(dummySchema, "982304892038409234982304892038409234.0909808908982304892038409234") { actual =>
      val res = new JBigDecimal("982304892038409234982304892038409234.0909808908982304892038409234")
      val WholeExpression(_, LiteralExpression(r: JBigDecimal), _, _, _) = actual
      assertEquals(res, r)
    }

    testExpr(dummySchema, "0") { actual =>
      val res = scala.math.BigInt(0)
      val a @ WholeExpression(_, LiteralExpression(actualRes: JBigInt), _, _, _) = actual; assertNotNull(a)
      assertEquals(res, BigInt(actualRes))
    }
    testExpr(dummySchema, "9817239872193792873982173948739879128370982398723897921370") { actual =>
      val res = scala.math.BigInt("9817239872193792873982173948739879128370982398723897921370")
      val a @ WholeExpression(_, LiteralExpression(actualRes: JBigInt), _, _, _) = actual; assertNotNull(a)
      assertEquals(res, BigInt(actualRes))
    }

  }

  @Test def test_stringLit() = {

    def testStringLit(expr: String) {
      testExpr(dummySchema, expr) {
        case WholeExpression(_, LiteralExpression(expr), _, _, _) => /* ok */
      }
    }
    testStringLit("'abc'")
    testStringLit("\"def\"")
  }

  @Test def test_funCall1() = {

    val testSchema = dummySchema

    val schemaCompiler = Compiler()
    val sset = schemaCompiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.forRoot()
    decl.elementRuntimeData

    def testFn(expr: String)(body: FunctionCallExpression => Unit) {
      testExpr(dummySchema, expr) { actual =>
        val WholeExpression(_, fnExpr: FunctionCallExpression, _, _, _) = actual
        body(fnExpr)
      }
    }

    testFn("fn:true()") { actual => assertEquals("true", actual.functionQName.local) }
    testFn("fn:concat('a', 'b')") { actual => assertEquals("concat", actual.functionQName.local) }

  }

  @Test def test_binaryOps() = {
    testExpr(dummySchema, """{
  if (a or b and c gt d)
  then if (c gt d)
    then e + f * g
    else -h
  else +i
        }""") { actual =>
      val WholeExpression(_, IfExpression(Seq(pred, thenPart, elsePart)), _, _, _) = actual
      val p @ OrExpression(
        List(RelativePathExpression(List(NamedStep("a", None)), _),
          AndExpression(
            List(RelativePathExpression(List(NamedStep("b", None)), _),
              ComparisonExpression("gt",
                List(RelativePathExpression(List(NamedStep("c", None)), _),
                  RelativePathExpression(List(NamedStep("d", None)), _)))
              )))) = pred
      assertNotNull(p)
      val th @ IfExpression(
        List(ComparisonExpression("gt",
          List(RelativePathExpression(List(NamedStep("c", None)), _),
            RelativePathExpression(List(NamedStep("d", None)), _))),
          AdditiveExpression("+",
            List(RelativePathExpression(List(NamedStep("e", None)), _),
              MultiplicativeExpression("*",
                List(RelativePathExpression(List(NamedStep("f", None)), _),
                  RelativePathExpression(List(NamedStep("g", None)), _)))
              )),
          UnaryExpression("-", RelativePathExpression(List(NamedStep("h", None)), _)))) = thenPart
      assertNotNull(th)
      val el @ UnaryExpression("+", RelativePathExpression(List(NamedStep("i", None)), _)) = elsePart
      assertNotNull(el)
    }
  }
}
