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

import java.lang.{ Double => JDouble }
import java.math.{ BigDecimal => JBigDecimal }
import java.math.{ BigInteger => JBigInt }
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader
import scala.xml.NamespaceBinding

import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.lib.xml.NS
import org.apache.daffodil.lib.xml.NamedQName
import org.apache.daffodil.lib.xml.QNameRegex
import org.apache.daffodil.runtime1.BasicComponent
import org.apache.daffodil.runtime1.dpath._
import org.apache.daffodil.runtime1.dsom.CompiledExpression
import org.apache.daffodil.runtime1.dsom.ConstantExpression
import org.apache.daffodil.runtime1.dsom.DPathCompileInfo

/**
 * Parses DPath expressions. Most real analysis is done later. This is
 * just the syntax being legal so that we can build the abstract syntax
 * tree (of ElementBase-derived classes).
 *
 * Use isEvaluatedAbove for expressions that are evaluated in a parent context
 * around the element where they are expressed (e.g., occursCount)
 *
 * One goal of this object, and the reason it is yet another separate
 * compiler object, is that it uses Scala's Combinator parsers, which
 * have been known to cause memory leaks. This class is transient. We never
 * save it. So hopefully that discards all the state of the combinator
 * stuff as well.
 */
class DFDLPathExpressionParser[T <: AnyRef](
  qn: NamedQName,
  nodeInfoKind: NodeInfo.Kind,
  namespaces: NamespaceBinding,
  noPrefixNamespace: NS,
  context: DPathCompileInfo,
  isEvaluatedAbove: Boolean,
  host: BasicComponent
) extends RegexParsers {

  def compile(expr: String): CompiledExpression[T] = {
    val tree = getExpressionTree(expr)

    val recipe =
      try {
        tree.compiledDPath // if we cannot get one this will fail by throwing out of here.
      } catch {
        case e: PathExpressionNoContextError => {
          host.SDW(
            WarnID.ExpressionCompilationSkipped,
            s"Expression compilation skipped due to path expression in unreferenced element, group, or complex type: $expr"
          )
          new CompiledDPath(RuntimeAbortOp(expr))
        }
      }

    val value =
      recipe.runExpressionForConstant(context.schemaFileLocation, context, host.tunable)
    val res: CompiledExpression[T] = value.getOptionAnyRef match {
      case Some(constantValue) => {
        Assert.invariant(constantValue != null)
        val res = new ConstantExpression[T](qn, nodeInfoKind, constantValue.asInstanceOf[T])
        res
      }
      case None => {
        val contentReferencedElementInfos = tree.contentReferencedElementInfos
        val valueReferencedElementInfos = tree.valueReferencedElementInfos
        new RuntimeExpressionDPath[T](
          qn,
          nodeInfoKind,
          recipe,
          expr,
          context,
          isEvaluatedAbove,
          contentReferencedElementInfos,
          valueReferencedElementInfos
        )
      }
    }
    res
  }

  override val skipWhitespace = true

  /**
   *  We need to override ~ of Parser so that we can consume/omit
   *  whitespace separating sub-expressions.
   *
   *  Before this fix the following would occur:
   *
   *  Given:
   *   { ../../e1 eq 1 }
   *
   *  Was interpreted as:
   *   { ../../e1eq1 }
   *
   *  This was incorrect.
   */
  override def Parser[T](f: Input => ParseResult[T]): Parser[T] = new Parser[T] {
    def apply(in: Input) = f(in)

    def WS: Parser[String] = """\s""".r
    def OptRepWhiteSpace: Parser[Any] = WS.*

    override def ~[U](q: => Parser[U]): Parser[~[T, U]] = {
      lazy val p = q // lazy argument
      (for (a <- this; x <- OptRepWhiteSpace; b <- p) yield new ~(a, b)).named("~")
    }
  }

  /**
   * A helper method that turns a `Parser` into one that will
   *  print debugging information to stdout before and after
   *  being applied.
   */
  val verboseParse = false // true if you want to see the expression parsing

  override def log[T](p: => Parser[T])(name: String): Parser[T] =
    if (!verboseParse) p
    else
      Parser { in =>
        Logger.log.trace(s"trying $name at $in")
        val r = p(in)
        Logger.log.trace(s"end $name --> $in")
        r
      }

  def getExpressionTree(expr: String): WholeExpression = {
    // This wrapping of phrase() prevents a memory leak in the scala parser
    // combinators in scala 2.10. See the following bug for more information:
    //
    //   https://issues.scala-lang.org/browse/SI-4929
    //
    // The phrase() function does not have the memory leak problem, however, it
    // requires that the parse consumes all the data and succeeds. So we have
    // to fake that. This is done by wrapping the real result in a Success
    // (this is done by the wrapAsSuccess() function). It also adds a
    // SucessAtEnd parser which always succeeds and always consumes all of its
    // data (which is just the empty string). This way, the whole parse succeeds
    // and it appears to have consumed all the data. Once this is complete, we
    // then unwrap the captured result (which may be a Success or NoSuccess)
    // and inspect that to determine that actual result of the parse.
    //
    // Once the above bug is fixed, the phrase, wrapAsSuccess, and SuccessAtEnd
    // stuff should all go away.

    val pResult = this.parse(phrase(wrapAsSuccess(TopLevel) ~ SuccessAtEnd), expr)

    val realResult = pResult match {
      case Success(res, _) => {
        val wrappedResult ~ _ = res
        wrappedResult
      }
      case ns: NoSuccess => ns // This should never happen
    }

    realResult match {
      case Success(expressionAST, next) => {
        expressionAST.init()
        expressionAST
      }
      case NoSuccess.I(msg, next) => {
        val nextString = convertNextToString(next)
        context.SDE(
          "Unable to parse expression. Message: %s\nNext: %s.",
          msg,
          nextString
        )
      }
    }
  }

  // blech - no easy way to just grab up to 30 chars from a Reader[Char]
  def convertNextToString(next: Input): String = {
    var nextRdr = next
    var i = 0
    val nextString = new StringBuilder()
    while (!nextRdr.atEnd && i < 30) {
      nextString.append(nextRdr.first)
      nextRdr = nextRdr.rest
      i += 1
    }
    nextString.toString()
  }

  def wrapAsSuccess[T](p: => Parser[T]): Parser[ParseResult[T]] = Parser { in =>
    p(in) match {
      case ns: NoSuccess => Success(ns, in)
      case _ @s => Success(s, in)
    }
  }

  /*
   * The grammar defined below does not match up with the expression language
   * grammar in the DFDL spec. The one in the spec is specifically designed
   * to just be a variation on XPath's published grammar. But DFDL expressions
   * are simpler and so a simpler grammar will suffice.
   */

  def ContextItemExpr = "."
  def AbbrevReverseStep = ".."

  def SupportedForwardAxis = (("child" <~ "::") | ("self" <~ "::"))
  def UnsupportedForwardAxis =
    (("descendant" <~ "::") | ("attribute" <~ "::") | ("descendant-or-self" <~ "::") |
      ("following-sibling" <~ "::") | ("following" <~ "::") |
      ("namespace" <~ "::"))
  def SupportedReverseAxis = ("parent" <~ "::")
  def UnsupportedReverseAxis = (("ancestor" <~ "::") |
    ("preceding-sibling" <~ "::") | ("preceding" <~ "::") |
    ("ancestor-or-self" <~ "::"))

  def EqualityComp = "eq" | "ne" | "!=" | "="
  def NumberComp = "lt" | "le" | "gt" | "ge" | "<=" | ">=" | "<" | ">"
  def Comp = EqualityComp | NumberComp

  def TopLevel: Parser[WholeExpression] = ("{" ~> Expr <~ "}") ^^ { xpr =>
    WholeExpression(nodeInfoKind, xpr, namespaces, noPrefixNamespace, context, host)
  }

  val SuccessAtEnd = Parser { in => Success(in, new CharSequenceReader("")) }

  def Expr: Parser[Expression] = ExprSingle
  def ExprSingle: Parser[Expression] = IfExpr | OrExpr

  def IfExpr: Parser[Expression] =
    log("if" ~> "(" ~> (Expr <~ ")") ~ ("then" ~> ExprSingle) ~ ("else" ~> ExprSingle) ^^ {
      case tst ~ th ~ els =>
        IfExpression(List(tst, th, els))
    })("if")
  //
  // I think structuring the grammar rules this way implements proper
  // operator precedence for XPath (and DPath is the same).
  //
  def OrExpr: Parser[Expression] = log(AndExpr ~ ("or" ~> AndExpr).* ^^ {
    case a1 ~ Nil => a1
    case a1 ~ aMore => aMore.foldLeft(a1) { case (a, b) => OrExpression(List(a, b)) }
  })("or")

  def AndExpr: Parser[Expression] = log(ComparisonExpr ~ ("and" ~> ComparisonExpr).* ^^ {
    case a1 ~ Nil => a1
    case a1 ~ aMore => aMore.foldLeft(a1) { case (a, b) => AndExpression(List(a, b)) }
  })("and")

  def ComparisonExpr = log(AdditiveExpr ~ (Comp ~ AdditiveExpr).? ^^ { x =>
    x match {
      case a1 ~ Some(vc ~ a2) => ComparisonExpression(vc, List(a1, a2))
      case a1 ~ None => a1
    }
  })("compare")

  def AdditiveExpr: Parser[Expression] =
    log(MultiplicativeExpr ~ (("+" | "-") ~ MultiplicativeExpr).* ^^ { case m1 ~ mMore =>
      mMore.foldLeft(m1) { case (a, op ~ b) => AdditiveExpression(op, List(a, b)) }
    })("add")

  def MultiplicativeExpr: Parser[Expression] =
    log(UnaryExpr ~ (("*" | "div" | "idiv" | "mod") ~ UnaryExpr).* ^^ { case u1 ~ uMore =>
      uMore.foldLeft(u1) { case (a, op ~ b) => MultiplicativeExpression(op, List(a, b)) }
    })("mult")

  def UnaryExpr: Parser[Expression] = log(("-" | "+").? ~ ValueExpr ^^ {
    case Some(op) ~ v => UnaryExpression(op, v)
    case None ~ v => v
  })("unary")

  def ValueExpr = log(PrimaryExpr | PathExpr)("value")

  def PathExpr: Parser[PathExpression] = log(
    ("//" ~> RelativePathExpr) ^^ { _ =>
      context.SDE("'//' is unsupported in DFDL Expression Syntax.")
    } |
      ("/" ~> RelativePathExpr) ^^ { r => RootPathExpression(Some(r)) } |
      ("/") ^^ { r => RootPathExpression(None) } |
      RelativePathExpr
  )("path")

  def RelativePathExpr: Parser[RelativePathExpression] = log(
    StepExpr ~ ("/" ~> StepExpr).* ^^ { case s1 ~ moreSteps =>
      RelativePathExpression(s1 :: moreSteps, isEvaluatedAbove)
    } |
      StepExpr ~ ("//" ~> StepExpr).* ^^ { _ =>
        context.SDE("'//' is unsupported in DFDL Expression Syntax.")
      }
  )("relativePath")

  def StepExpr: Parser[StepExpression] = log(AxisStep | VarRef ^^ { varRef =>
    this.context
      .SDE("Variables cannot be used in path expressions.  Error: $%s", varRef.qnameString)
  })("step")

  def AxisStep: Parser[StepExpression] =
    Reverse | Forward

  def Reverse = AbbrevReverseStep ~> Predicate.? ^^ { Up(_) } |
    (SupportedReverseAxis ~> NodeTest) ~ Predicate.? ^^ { case qn ~ p => { Up2(qn, p) } } |
    (UnsupportedReverseAxis ~ NodeTest) ~ Predicate.? ^^ { case name ~ _ =>
      context.SDE("'%s::' is an unsupported axis in DFDL Expression Syntax.", name)
    }

  def Forward = ContextItemExpr ~> Predicate.? ^^ { Self(_) } |
    SupportedForwardAxis ~ NodeTest ~ Predicate.? ^^ {
      case "self" ~ qn ~ p => Self2(qn, p)
      case "child" ~ qn ~ p => NamedStep(qn, p)
      // $COVERAGE-OFF$
      case _ => Assert.impossible()
      // $COVERAGE-ON$
    } |
    UnsupportedForwardAxis ~ Predicate.? ^^ { case name ~ _ =>
      context.SDE("'%s::' is an unsupported axis in DFDL Expression Syntax.", name)
    } |
    NodeTest ~ Predicate.? ^^ { case qn ~ p => { NamedStep(qn, p) } }

  def Predicate: Parser[PredicateExpression] = log("[" ~> Expr <~ "]" ^^ {
    PredicateExpression(_)
  })("predicate")

  def PrimaryExpr: Parser[PrimaryExpression] =
    log(FunctionCall | Literal | VarRef | ParenthesizedExpr)("primary")

  def Literal = log((StringLiteral | NumericLiteral) ^^ { LiteralExpression(_) })("literal")

  def NumericLiteral: Parser[Number] = DoubleLiteral | DecimalLiteral | IntegerLiteral

  def VarRef = "$" ~> RefName ^^ { VariableRef(_) }

  def ParenthesizedExpr = "(" ~> Expr <~ ")" ^^ { ParenthesizedExpression(_) }

  def FunctionCall: Parser[FunctionCallExpression] = log((RefName ~ ArgList) ^^ {
    case qn ~ arglist => FunctionCallExpression(qn, arglist)
  })("functionCall")

  def ArgList = log(
    "(" ~ ")" ^^ { _ => Nil } |
      "(" ~> (ExprSingle ~ (("," ~> ExprSingle).*)) <~ ")" ^^ { case e1 ~ moreEs =>
        e1 :: moreEs
      }
  )("argList")

  def StepName = log(
    QualifiedName |
      Wildcard ^^ { wc =>
        context.SDE(
          "Wildcard is unsupported in DFDL Expression Syntax. Offending value was '%s'.",
          wc
        )
      }
  )("stepName")

  def Wildcard = "*" |
    (QNameRegex.NCName ~ ":" ~ "*") ^^ { case ncname ~ c ~ wc => ncname + c + wc } |
    ("*" ~ ":" ~ QNameRegex.NCName) ^^ { case wc ~ c ~ ncname => wc + c + ncname }
  def NodeTest = KindTest | NameTest
  def NameTest = StepName // aka QName | Wildcard
  def KindTest =
    log(ProcessingInstructionTest | CommentTest | TextTest | AnyKindTest)("kindTest")
  def ProcessingInstructionTest =
    log("processing-instruction" ~ "(" ~ StringLiteral.? ~ ")" ^^ { _ =>
      context.SDE("Use of processing-instruction() is unsupported in DFDL Expression Syntax.")
    })("processingInstructionTest")
  def CommentTest = log("comment" ~ "(" ~ ")" ^^ { _ =>
    context.SDE("Use of comment() is unsupported in DFDL Expression Syntax.")
  })("commentTest")
  def TextTest = log("text" ~ "(" ~ ")" ^^ { _ =>
    context.SDE("Use of text() is unsupported in DFDL Expression Syntax.")
  })("textTest")
  def AnyKindTest = log("node" ~ "(" ~ ")" ^^ { _ =>
    context.SDE("Use of node() is unsupported in DFDL Expression Syntax.")
  })("anyKindTest")

  def RefName = log(QualifiedName)("refName")

  def QualifiedName: Parser[String] = PrefixedName | UnprefixedName

  def PrefixedName = QNameRegex.QName
  def UnprefixedName = QNameRegex.NCName

  def IntegerLiteral: Parser[JBigInt] = Digits ^^ { new JBigInt(_) }

  val Digits = """[0-9]+""".r
  val optDigits: Parser[String] = """[0-9]*""".r
  val Expon: Parser[String] = """[eE][+-]?[0-9]{1,3}""".r
  val plusMinus: Parser[String] = """[+-]?""".r

  val DecimalLiteral: Parser[java.math.BigDecimal] =
    ("." ~> Digits) ^^ { case dig => new JBigDecimal("0." + dig) } |
      (Digits ~ ("." ~> optDigits)) ^^ { case digit ~ optDig =>
        new JBigDecimal(digit + "." + optDig)
      }

  val DoubleLiteral: Parser[JDouble] = ("." ~> Digits ~ Expon ^^ {
    case fraction ~ exp => {
      "0." + fraction + exp
    }
  } |
    Digits ~ (("." ~> optDigits).?) ~ Expon ^^ { case intPart ~ fraction ~ exp =>
      intPart + "." + fraction.getOrElse("0") + exp
    }) ^^ { str => java.lang.Double.parseDouble(str) }

  /**
   * String literal must be one regex, not separate combinators combined.
   *
   * This is to avoid whitespace collapsing inside string literals. We want
   * whitespace to be ignored outside string literals, but not inside them.
   */

  val StringLiteral: Parser[String] = {
    val escapeQuot = """\"\""""
    val notQuot = """[^"]"""
    val escapeApos = """''"""
    val notApos = """[^']"""
    def quotedBody(esc: String, not: String) = """(%s|%s)*""".format(esc, not)
    val doubleQuotedBody = quotedBody(escapeQuot, notQuot)
    val singleQuotedBody = quotedBody(escapeApos, notApos)
    val stringLit = """(\"%s\")|(\'%s\')""".format(doubleQuotedBody, singleQuotedBody)
    val stringLitRegex = stringLit.r
    stringLitRegex ^^ { sl =>
      if (sl.length == 2) "" // turns into empty string
      else sl.substring(1, sl.length - 1) // 2nd arg is endPos, not how many chars.
    }
  }

}
