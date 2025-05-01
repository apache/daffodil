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

package org.apache.daffodil.core.dpath

import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInt }
import scala.util.parsing.combinator.Parsers

import org.apache.daffodil.core.compiler._
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml._
import org.apache.daffodil.runtime1.dpath._
import org.apache.daffodil.runtime1.processors.ElementRuntimeData

import org.junit.Assert._
import org.junit.Test

class TestDFDLExpressionTree extends Parsers {
  val qn = GlobalQName(Some("daf"), "testExpr", XMLUtils.dafintURI)

  val dummySchema = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="tns:GeneralFormat"/>,
    <xs:element name="title" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(5) }"/>
  )

  def testExpr(testSchema: scala.xml.Elem, expr: String)(body: Expression => Unit): Unit = {
    val schemaCompiler = Compiler()
    val sset = schemaCompiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val schemaDoc = schema.schemaDocuments.head
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.asRoot
    val erd = decl.elementRuntimeData

    val exprCompiler = new DFDLPathExpressionParser(
      qn,
      NodeInfo.String,
      testSchema.scope,
      schemaDoc.noPrefixNamespace,
      erd.dpathCompileInfo,
      false,
      sset
    )
    val result = exprCompiler.getExpressionTree(expr)
    body(result)
  }

  def testExpr2(testSchema: scala.xml.Elem, expr: String)(
    body: (Expression, ElementRuntimeData) => Unit
  ): Unit = {
    val schemaCompiler = Compiler()
    val sset = schemaCompiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val schemaDoc = schema.schemaDocuments.head
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.asRoot
    val erd = decl
    val exprCompiler = new DFDLPathExpressionParser(
      qn,
      NodeInfo.AnyType,
      testSchema.scope,
      schemaDoc.noPrefixNamespace,
      decl.dpathCompileInfo,
      false,
      sset
    )
    val result = exprCompiler.getExpressionTree(expr)
    body(result, erd.elementRuntimeData)
  }

  val testSchema = SchemaUtils.dfdlTestSchemaUnqualified(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="tns:GeneralFormat"/>,
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
    </xs:element>
  )

  val aSchema = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="tns:GeneralFormat"/>,
    <xs:element name="a" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ xs:string(xs:unsignedInt(5)) }"/>
  )

  @Test def test_a() = {
    testExpr2(aSchema, "{ /tns:a }") { (ce, erd) =>
      val WholeExpression(
        _,
        RootPathExpression(Some(RelativePathExpression(steps, _))),
        _,
        _,
        _,
        _
      ) = ce
      val List(n1: DownStepExpression) = steps
      val n1p @ NamedStep("tns:a", None) = n1;
      assertNotNull(n1p)
      assertFalse(n1.isArray)
      assertEquals(NodeInfo.AnyType, n1.targetType)
      assertEquals(NodeInfo.String, n1.inherentType)
      assertTrue(n1.isLastStep)
      val List(DownElement(nqn: NamedQName)) = n1.compiledDPath.ops.toList
      assertEquals(nqn, n1.stepElements.head.namedQName)
    }
  }

  val bSchema = SchemaUtils.dfdlTestSchemaUnqualified(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="tns:GeneralFormat"/>,
    <xs:element name="b">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="i" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="2"/>
          <xs:element name="a" maxOccurs="2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ xs:string(xs:unsignedInt(5)) }"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>
  )

  @Test def test_a_pred(): Unit = {
    testExpr2(bSchema, "{ /tns:b/a[/tns:b/i] }") { (ce, erd) =>
      val w @ WholeExpression(
        _,
        RootPathExpression(Some(RelativePathExpression(steps, _))),
        _,
        _,
        _,
        _
      ) = ce
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

  @Test def test_a_predPred(): Unit = {
    testExpr(dummySchema, "{ a[i[j]] }") { actual =>
      val WholeExpression(_, RelativePathExpression(steps, _), _, _, _, _) = actual
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
      val WholeExpression(_, RelativePathExpression(steps, _), _, _, _, _) = actual
      val List(n1, n2, n3) = steps
      val u @ Up(None) = n1; assertNotNull(u)
      val u2 @ Up(Some(PredicateExpression(LiteralExpression(two: JBigInt)))) = n2;
      assertNotNull(u2)
      assertEquals(JBigInt.valueOf(2), two)
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
      val WholeExpression(
        _,
        RootPathExpression(Some(RelativePathExpression(steps, _))),
        _,
        _,
        _,
        _
      ) = actual
      val List(n1, n2, n3) = steps.asInstanceOf[List[NamedStep]]
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
      assertEquals(erd.dpathElementCompileInfo, n1.stepElements.head)
      assertEquals(NS("http://example.com"), erd.targetNamespace)

      val n2p @ NamedStep("book", None) = n2; assertNotNull(n2p)
      assertFalse(n2.isFirstStep)
      assertFalse(n2.isLastStep)
      assertEquals(1, n2.positionInStepsSequence) // zero based
      assertTrue(n2.priorStep.isDefined)
      assertEquals(StepQName(None, "book", NoNamespace), n2.stepQName)
      val bookERD = erd.childERDs.find { _.name == "book" }.get
      assertEquals(bookERD.dpathElementCompileInfo, n2.stepElements.head)
      val n3p @ NamedStep("title", None) = n3; assertNotNull(n3p)
      assertFalse(n3.isFirstStep)
      assertTrue(n3.isLastStep)
      assertEquals(2, n3.positionInStepsSequence) // zero based
      assertTrue(n3.priorStep.isDefined)
      assertEquals(StepQName(None, "title", NoNamespace), n3.stepQName)
      val titleERD = bookERD.childERDs.find { _.name == "title" }.get
      assertEquals(titleERD.dpathElementCompileInfo, n3.stepElements.head)
    }
  }

  @Test def test_pathNoSuchElement1(): Unit = {
    testExpr2(testSchema, "{ /thereIsNoElementWithThisName }") { (actual, erd) =>
      val WholeExpression(
        _,
        RootPathExpression(Some(RelativePathExpression(List(step: NamedStep), _))),
        _,
        _,
        _,
        _
      ) = actual
      val exc = intercept[Exception] {
        step.stepElements
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
      val WholeExpression(
        _,
        RootPathExpression(Some(RelativePathExpression(steps, _))),
        _,
        _,
        _,
        _
      ) = actual
      val List(n1, n2, n3) = steps
      val NamedStep("bookstore", pred) = n1
      // println(pred)
      NS("http://example.com")
      val p @ Some(PredicateExpression(VariableRef("i"))) = pred; assertNotNull(p)
      val n2p @ NamedStep("book", None) = n2; assertNotNull(n2p)
      val n3p @ NamedStep("title", None) = n3; assertNotNull(n3p)
    }
  }

  @Test def testAddConstants(): Unit = {
    testExpr(dummySchema, "{ 1 + 2 }") { actual =>
      val a @ WholeExpression(
        _,
        AdditiveExpression(
          "+",
          List(LiteralExpression(one: JBigInt), LiteralExpression(two: JBigInt))
        ),
        _,
        _,
        _,
        _
      ) = actual; assertNotNull(a)
      assertEquals(JBigInt.valueOf(1), one)
      assertEquals(JBigInt.valueOf(2), two)
    }
  }

  @Test def testFnTrue(): Unit = {
    testExpr(dummySchema, "{ fn:true() }") { actual =>
      val a @ WholeExpression(_, FunctionCallExpression("fn:true", Nil), _, _, _, _) = actual;
      assertNotNull(a)
    }
  }

  @Test def test_numbers1() = {
    testExpr(dummySchema, "{ 0. }") { (actual: Expression) =>
      val res = JBigDecimal.ZERO
      val a @ WholeExpression(_, LiteralExpression(actualRes: JBigDecimal), _, _, _, _) =
        actual;
      assertNotNull(a)
      assertEquals(res, actualRes)
    }
  }

  @Test def test_numbers() = {

    testExpr(dummySchema, "{ 5.0E2 }") {
      case WholeExpression(_, LiteralExpression(500.0), _, _, _, _) => /* ok */ ;
    }
    testExpr(dummySchema, "{ 5E2 }") {
      case WholeExpression(_, LiteralExpression(500.0), _, _, _, _) => /* ok */ ;
    }
    testExpr(dummySchema, "{ .2E2 }") {
      case WholeExpression(_, LiteralExpression(20.0), _, _, _, _) => /* ok */ ;
    }
    testExpr(dummySchema, "{ .2E-3 }") {
      case WholeExpression(_, LiteralExpression(0.0002), _, _, _, _) => /* ok */ ;
    }
    testExpr(dummySchema, "{ .2E+3 }") {
      case WholeExpression(_, LiteralExpression(200.0), _, _, _, _) => /* ok */ ;
    }

    //    testExpr(dummySchema, "0.") { actual =>
    //      val res = new JBigDecimal("0.0")
    //      val a @ WholeExpression(_, LiteralExpression(`res`), _, _, _, _) = actual; assertNotNull(a)
    //    }
    testExpr(dummySchema, "{ .1 }") { actual =>
      val res = new JBigDecimal("0.1")
      val a @ WholeExpression(_, LiteralExpression(`res`), _, _, _, _) = actual;
      assertNotNull(a)
    }
    testExpr(
      dummySchema,
      "{ 982304892038409234982304892038409234.0909808908982304892038409234 }"
    ) { actual =>
      val res =
        new JBigDecimal("982304892038409234982304892038409234.0909808908982304892038409234")
      val WholeExpression(_, LiteralExpression(r: JBigDecimal), _, _, _, _) = actual
      assertEquals(res, r)
    }

    testExpr(dummySchema, "{ 0 }") { actual =>
      val res = JBigInt.ZERO
      val a @ WholeExpression(_, LiteralExpression(actualRes: JBigInt), _, _, _, _) = actual;
      assertNotNull(a)
      assertEquals(res, actualRes)
    }
    testExpr(dummySchema, "{ 9817239872193792873982173948739879128370982398723897921370 }") {
      actual =>
        val res = new JBigInt("9817239872193792873982173948739879128370982398723897921370")
        val a @ WholeExpression(_, LiteralExpression(actualRes: JBigInt), _, _, _, _) = actual;
        assertNotNull(a)
        assertEquals(res, actualRes)
    }

  }

  @Test def test_stringLit() = {

    def testStringLit(expr: String): Unit = {
      testExpr(dummySchema, expr) {
        case WholeExpression(_, LiteralExpression(expr), _, _, _, _) => /* ok */
      }
    }
    testStringLit("{ 'abc' }")
    testStringLit("{ \"def\" }")
  }

  @Test def test_funCall1() = {

    val testSchema = dummySchema

    val schemaCompiler = Compiler()
    val sset = schemaCompiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val schemaDoc = schema.schemaDocuments.head
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.asRoot
    decl.elementRuntimeData

    def testFn(expr: String)(body: FunctionCallExpression => Unit): Unit = {
      testExpr(dummySchema, expr) { actual =>
        val WholeExpression(_, fnExpr: FunctionCallExpression, _, _, _, _) = actual
        body(fnExpr)
      }
    }

    testFn("{ fn:true() }") { actual => assertEquals("true", actual.functionQName.local) }
    testFn("{ fn:concat('a', 'b') }") { actual =>
      assertEquals("concat", actual.functionQName.local)
    }

  }

  @Test def test_binaryOps() = {
    testExpr(
      dummySchema,
      """{
  if (a or b and c gt d)
  then if (c gt d)
    then e + f * g
    else -h
  else +i
        }"""
    ) { actual =>
      val WholeExpression(_, IfExpression(Seq(pred, thenPart, elsePart)), _, _, _, _) = actual
      val p @ OrExpression(
        List(
          RelativePathExpression(List(NamedStep("a", None)), _),
          AndExpression(
            List(
              RelativePathExpression(List(NamedStep("b", None)), _),
              ComparisonExpression(
                "gt",
                List(
                  RelativePathExpression(List(NamedStep("c", None)), _),
                  RelativePathExpression(List(NamedStep("d", None)), _)
                )
              )
            )
          )
        )
      ) = pred
      assertNotNull(p)
      val th @ IfExpression(
        List(
          ComparisonExpression(
            "gt",
            List(
              RelativePathExpression(List(NamedStep("c", None)), _),
              RelativePathExpression(List(NamedStep("d", None)), _)
            )
          ),
          AdditiveExpression(
            "+",
            List(
              RelativePathExpression(List(NamedStep("e", None)), _),
              MultiplicativeExpression(
                "*",
                List(
                  RelativePathExpression(List(NamedStep("f", None)), _),
                  RelativePathExpression(List(NamedStep("g", None)), _)
                )
              )
            )
          ),
          UnaryExpression("-", RelativePathExpression(List(NamedStep("h", None)), _))
        )
      ) = thenPart
      assertNotNull(th)
      val el @ UnaryExpression("+", RelativePathExpression(List(NamedStep("i", None)), _)) =
        elsePart
      assertNotNull(el)
    }
  }
}
