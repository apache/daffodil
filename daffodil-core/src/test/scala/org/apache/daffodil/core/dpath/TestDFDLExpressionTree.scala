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
import org.apache.daffodil.core.util.TestUtils.intercept
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
      val steps = ce match {
        case WholeExpression(
              _,
              RootPathExpression(Some(RelativePathExpression(steps, _))),
              _,
              _,
              _,
              _
            ) =>
          steps
        case _ => fail(); null
      }
      val n1: DownStepExpression = steps match {
        case List(n1: DownStepExpression) => n1
        case _ => fail(); null
      }
      val n1p = n1 match {
        case n1p @ NamedStep("tns:a", None) => n1p
        case _ => fail(); null
      }
      assertNotNull(n1p)
      assertFalse(n1.isArray)
      assertEquals(NodeInfo.AnyType, n1.targetType)
      assertEquals(NodeInfo.String, n1.inherentType)
      assertTrue(n1.isLastStep)
      val nqn = n1.compiledDPath.ops.toList match {
        case List(DownElement(nqn: NamedQName)) => nqn
        case _ => fail(); null
      }
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
      val (w, steps) = ce match {
        case w @ WholeExpression(
              _,
              RootPathExpression(Some(RelativePathExpression(steps, _))),
              _,
              _,
              _,
              _
            ) =>
          (w, steps)
        case _ => fail(); null
      }
      val List(b, a) = steps
      val bp = b match {
        case bp @ NamedStep("tns:b", None) => bp
        case _ => fail(); null
      }; assertNotNull(bp)
      val i = a match {
        case NamedStep("a", Some(PredicateExpression(i))) => i
        case _ => fail(); null
      }
      val istep = i match {
        case RootPathExpression(Some(RelativePathExpression(List(_, istep), _))) => istep
        case _ => fail(); null
      }
      val isp = istep match {
        case isp @ NamedStep("i", None) => isp
        case _ => fail(); null
      }; assertNotNull(isp)
      val ops = i.compiledDPath.ops.toList
      val berd = ops match {
        case List(ToRoot, DownElement(berd), DownElement(_), IntToLong, LongToArrayIndex) =>
          berd
        case _ => fail(); null
      }
      val berd2 =
        w.compiledDPath.ops.toList match {
          case List(ToRoot, DownElement(berd2), DownArrayOccurrence(_, _)) => berd2
          case _ => fail(); null
        }
      assertEquals(berd, berd2)
    }
  }

  @Test def test_a_predPred(): Unit = {
    testExpr(dummySchema, "{ a[i[j]] }") { actual =>
      val steps = actual match {
        case WholeExpression(_, RelativePathExpression(steps, _), _, _, _, _) => steps
        case _ => fail(); null
      }
      val List(n1) = steps
      val rel = n1 match {
        case NamedStep("a", Some(PredicateExpression(rel))) => rel
        case _ => fail(); null
      }
      val n2 = rel match {
        case RelativePathExpression(List(n2), _) => n2
        case _ => fail(); null
      }
      val j = n2 match {
        case NamedStep("i", Some(PredicateExpression(j))) => j
        case _ => fail(); null
      }
      val n3 = j match {
        case RelativePathExpression(List(n3), _) => n3
        case _ => fail(); null
      }
      val n3p = n3 match {
        case n3p @ NamedStep("j", None) => n3p
        case _ => fail(); null
      }; assertNotNull(n3p)
    }
  }

  @Test def test_relativePath() = {
    testExpr(dummySchema, "{ ../..[2]/bookstore }") { actual =>
      val steps = actual match {
        case WholeExpression(_, RelativePathExpression(steps, _), _, _, _, _) => steps
        case _ => fail(); null
      }
      val List(n1, n2, n3) = steps
      val u = n1 match {
        case u @ Up(None) => u
        case _ => fail(); null
      }; assertNotNull(u)
      val (u2, two) = n2 match {
        case u2 @ Up(Some(PredicateExpression(LiteralExpression(two: JBigInt)))) => (u2, two)
        case _ => fail(); null
      }; assertNotNull(u2)
      assertEquals(JBigInt.valueOf(2), two)
      val n3p = n3 match {
        case n3p @ NamedStep("bookstore", None) => n3p
        case _ => fail(); null
      }; assertNotNull(n3p)
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
      val steps = actual match {
        case WholeExpression(
              _,
              RootPathExpression(Some(RelativePathExpression(steps, _))),
              _,
              _,
              _,
              _
            ) =>
          steps
        case _ => fail(); null
      }
      val List(n1, n2, n3) = steps.asInstanceOf[List[NamedStep]]
      val n1p = n1 match {
        case n1p @ NamedStep("tns:bookstore", None) => n1p
        case _ => fail(); null
      }; assertNotNull(n1p)
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

      val n2p = n2 match {
        case n2p @ NamedStep("book", None) => n2p
        case _ => fail(); null
      }; assertNotNull(n2p)
      assertFalse(n2.isFirstStep)
      assertFalse(n2.isLastStep)
      assertEquals(1, n2.positionInStepsSequence) // zero based
      assertTrue(n2.priorStep.isDefined)
      assertEquals(StepQName(None, "book", NoNamespace), n2.stepQName)
      val bookERD = erd.childERDs.find { _.name == "book" }.get
      assertEquals(bookERD.dpathElementCompileInfo, n2.stepElements.head)
      val n3p = n3 match {
        case n3p @ NamedStep("title", None) => n3p
        case _ => fail(); null
      }; assertNotNull(n3p)
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
      val step = actual match {
        case WholeExpression(
              _,
              RootPathExpression(Some(RelativePathExpression(List(step: NamedStep), _))),
              _,
              _,
              _,
              _
            ) =>
          step
        case _ => fail(); null
      }
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
      val steps = actual match {
        case WholeExpression(
              _,
              RootPathExpression(Some(RelativePathExpression(steps, _))),
              _,
              _,
              _,
              _
            ) =>
          steps
        case _ => fail(); null
      }
      val List(n1, n2, n3) = steps
      val pred = n1 match {
        case NamedStep("bookstore", pred) => pred
        case _ => fail(); null
      }
      // println(pred)
      NS("http://example.com")
      val p = pred match {
        case p @ Some(PredicateExpression(VariableRef("i"))) => p
        case _ => fail(); null
      }; assertNotNull(p)
      val n2p = n2 match {
        case n2p @ NamedStep("book", None) => n2p
        case _ => fail(); null
      }; assertNotNull(n2p)
      val n3p = n3 match {
        case n3p @ NamedStep("title", None) => n3p
        case _ => fail(); null
      }; assertNotNull(n3p)
    }
  }

  @Test def testAddConstants(): Unit = {
    testExpr(dummySchema, "{ 1 + 2 }") { actual =>
      val (a, one, two) = actual match {
        case a @ WholeExpression(
              _,
              AdditiveExpression(
                "+",
                List(LiteralExpression(one: JBigInt), LiteralExpression(two: JBigInt))
              ),
              _,
              _,
              _,
              _
            ) =>
          (a, one, two)
        case _ => fail(); null
      }; assertNotNull(a)
      assertEquals(JBigInt.valueOf(1), one)
      assertEquals(JBigInt.valueOf(2), two)
    }
  }

  @Test def testFnTrue(): Unit = {
    testExpr(dummySchema, "{ fn:true() }") { actual =>
      val a = actual match {
        case a @ WholeExpression(_, FunctionCallExpression("fn:true", Nil), _, _, _, _) => a
        case _ => fail(); null
      }; assertNotNull(a)
    }
  }

  @Test def test_numbers1() = {
    testExpr(dummySchema, "{ 0. }") { (actual: Expression) =>
      val res = JBigDecimal.ZERO
      val (a, actualRes) =
        actual match {
          case a @ WholeExpression(_, LiteralExpression(actualRes: JBigDecimal), _, _, _, _) =>
            (a, actualRes)
          case _ => fail(); null
        }; assertNotNull(a)
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
      val a = actual match {
        case a @ WholeExpression(_, LiteralExpression(`res`), _, _, _, _) => a
        case _ => fail(); null
      }; assertNotNull(a)
    }
    testExpr(
      dummySchema,
      "{ 982304892038409234982304892038409234.0909808908982304892038409234 }"
    ) { actual =>
      val res =
        new JBigDecimal("982304892038409234982304892038409234.0909808908982304892038409234")
      val r = actual match {
        case WholeExpression(_, LiteralExpression(r: JBigDecimal), _, _, _, _) => r
        case _ => fail(); null
      }
      assertEquals(res, r)
    }

    testExpr(dummySchema, "{ 0 }") { actual =>
      val res = JBigInt.ZERO
      val (a, actualRes) = actual match {
        case a @ WholeExpression(_, LiteralExpression(actualRes: JBigInt), _, _, _, _) =>
          (a, actualRes)
        case _ => fail(); null
      }; assertNotNull(a)
      assertEquals(res, actualRes)
    }
    testExpr(dummySchema, "{ 9817239872193792873982173948739879128370982398723897921370 }") {
      actual =>
        val res = new JBigInt("9817239872193792873982173948739879128370982398723897921370")
        val (a, actualRes) = actual match {
          case a @ WholeExpression(_, LiteralExpression(actualRes: JBigInt), _, _, _, _) =>
            (a, actualRes)
          case _ => fail(); null
        }; assertNotNull(a)
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
        val fnExpr = actual match {
          case WholeExpression(_, fnExpr: FunctionCallExpression, _, _, _, _) => fnExpr
          case _ => fail(); null
        }
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
      val (pred, thenPart, elsePart) = actual match {
        case WholeExpression(_, IfExpression(Seq(pred, thenPart, elsePart)), _, _, _, _) =>
          (pred, thenPart, elsePart)
        case _ => fail(); null
      }
      val p = pred match {
        case p @ OrExpression(
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
            ) =>
          p
        case _ => fail(); null
      }
      assertNotNull(p)
      val th = thenPart match {
        case th @ IfExpression(
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
            ) =>
          th
        case _ => fail(); null
      }
      assertNotNull(th)
      val el =
        elsePart match {
          case el @ UnaryExpression(
                "+",
                RelativePathExpression(List(NamedStep("i", None)), _)
              ) =>
            el
          case _ => fail(); null
        }
      assertNotNull(el)
    }
  }
}
