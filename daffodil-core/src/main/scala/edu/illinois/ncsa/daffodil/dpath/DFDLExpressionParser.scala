package edu.illinois.ncsa.daffodil.dpath

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

import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.OOLAGHost
import edu.illinois.ncsa.daffodil.util.DebugRegexParsers
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.dsom._
import scala.xml.NamespaceBinding
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.processors._
import scala.util.parsing.input.CharSequenceReader

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
class DFDLPathExpressionParser(
  nodeInfoKind: NodeInfo.Kind,
  namespaces: NamespaceBinding,
  context: DPathCompileInfo,
  isEvaluatedAbove: Boolean = false) extends DebugRegexParsers {

  def compile(expr: String): CompiledExpression = {
    val tree = getExpressionTree(expr)

    //
    // TODO: call tree.isError and if there are errors, grab all the diagnostics
    // and somehow get them back to the OOLAG world - e.g., by throwing a
    // special DPathExpressionError(diags) which takes a whole list
    // of diagnostics. Then when this is caught, it can be treated specially 
    // and the individual diagnostics take out, or they might do better all in 
    // one.
    tree.isError // forces the requiredEvaluations in a useful order for debug

    val recipe = tree.compiledDPath // if we cannot get one this will fail by throwing out of here.

    val value = recipe.runExpressionForConstant(context.schemaFileLocation)
    val res = value match {
      case Some(constantValue) => {
        Assert.invariant(constantValue != null)
        val res = new ConstantExpression(nodeInfoKind, constantValue)
        res
      }
      case None => {
        new RuntimeExpressionDPath(nodeInfoKind, recipe, expr, context, isEvaluatedAbove)
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
   * 	{ ../../e1 eq 1 }
   *
   *  Was interpreted as:
   * 	{ ../../e1eq1 }
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
    else Parser { in =>
      Console.out.println("trying %s at %s".format(name, in))
      val r = p(in)
      Console.out.println("end %s --> %s".format(name, r))
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
      case NoSuccess(msg, next) => {
        // blech - no easy way to just grab up to 30 chars from a Reader[Char]
        var nextRdr = next
        var nextString = new StringBuffer()
        var i = 0
        while (!nextRdr.atEnd & i < 30) {
          nextString.append(nextRdr.first)
          nextRdr = nextRdr.rest
          i += 1
        }
        context.SDE("Unable to parse expression. Message: %s\nNext: %s.", msg, nextString.toString())
      }
    }
  }

  def wrapAsSuccess[T](p: => Parser[T]): Parser[ParseResult[T]] = Parser { in =>
    p(in) match {
      case ns: NoSuccess => Success(ns, in)
      case _@ s => Success(s, in)
    }
  }

  /*
   * The grammar defined below does not match up with the expression language
   * grammar in the DFDL spec. The one in the spec is specifically designed
   * to just be a variation on XPath's published grammar. But DFDL expressions
   * are simpler and so a simpler grammar will suffice.
   */

  def ContextItemExpr = "." ^^ { expr => Self(None) }
  def AbbrevReverseStep = ".." ^^ { expr => Up(None) }
  // TODO support forward axis syntax some day
  // def ForwardAxis = ("child" ~ "::") | ("self" ~ "::") 
  def EqualityComp = "eq" | "ne" | "!=" | "="
  def NumberComp = "lt" | "le" | "gt" | "ge" | "<=" | ">=" | "<" | ">"
  def Comp = EqualityComp | NumberComp
  //
  // we don't care if it has braces around it or not.
  def TopLevel: Parser[WholeExpression] = ("{" ~> Expr <~ "}" | Expr) ^^ { xpr =>
    WholeExpression(nodeInfoKind, xpr, namespaces, context)
  }

  val SuccessAtEnd = Parser { in => Success(in, new CharSequenceReader("")) }

  def Expr: Parser[Expression] = ExprSingle
  def ExprSingle: Parser[Expression] = IfExpr | OrExpr

  def IfExpr: Parser[Expression] = log(
    "if" ~> "(" ~> (Expr <~ ")") ~ ("then" ~> ExprSingle) ~ ("else" ~> ExprSingle) ^^ {
      case tst ~ th ~ els =>
        IfExpression(List(tst, th, els))
    })("if")
  //
  // I think structuring the grammar rules this way implements proper
  // operator precedence for XPath (and DPath is the same).
  //
  def OrExpr: Parser[Expression] = log(
    AndExpr ~ ("or" ~> AndExpr).* ^^ {
      case a1 ~ Nil => a1
      case a1 ~ aMore => aMore.foldLeft(a1) { case (a, b) => OrExpression(List(a, b)) }
    })("or")

  def AndExpr: Parser[Expression] = log(
    ComparisonExpr ~ ("and" ~> ComparisonExpr).* ^^ {
      case a1 ~ Nil => a1
      case a1 ~ aMore => aMore.foldLeft(a1) { case (a, b) => AndExpression(List(a, b)) }
    })("and")

  def ComparisonExpr = log(AdditiveExpr ~ (Comp ~ AdditiveExpr).? ^^ { x =>
    x match {
      case a1 ~ Some(vc ~ a2) => {
        if (List("eq", "ne", "=", "!=").contains(vc))
          EqualityComparisonExpression(vc, List(a1, a2))
        else
          ComparisonExpression(vc, List(a1, a2))
      }
      case a1 ~ None => a1
    }
  })("compare")

  def AdditiveExpr: Parser[Expression] = log(
    MultiplicativeExpr ~ (("+" | "-") ~ MultiplicativeExpr).* ^^ {
      case m1 ~ mMore => mMore.foldLeft(m1) { case (a, op ~ b) => AdditiveExpression(op, List(a, b)) }
    })("add")

  def MultiplicativeExpr: Parser[Expression] = log(
    UnaryExpr ~ (("*" | "div" | "idiv" | "mod") ~ UnaryExpr).* ^^ {
      case u1 ~ uMore => uMore.foldLeft(u1) { case (a, op ~ b) => MultiplicativeExpression(op, List(a, b)) }
    })("mult")

  def UnaryExpr: Parser[Expression] = log(
    ("-" | "+").? ~ ValueExpr ^^ {
      case Some(op) ~ v => UnaryExpression(op, v)
      case None ~ v => v
    })("unary")

  def ValueExpr = log(PrimaryExpr | PathExpr)("value")

  def PathExpr: Parser[PathExpression] = log(
    ("/" ~> RelativePathExpr) ^^ { r => RootPathExpression(Some(r)) } |
      ("/") ^^ { r => RootPathExpression(None) } |
      RelativePathExpr)("path")

  def RelativePathExpr: Parser[RelativePathExpression] = log(
    StepExpr ~ ("/" ~> StepExpr).* ^^ { case s1 ~ moreSteps => RelativePathExpression(s1 :: moreSteps, isEvaluatedAbove) })("relativePath")

  def StepExpr: Parser[StepExpression] = log(AxisStep | VarRef ^^ { varRef => this.context.SDE("Variables cannot be used in path expressions.  Error: $%s", varRef.qnameString) })("step")
  def AxisStep: Parser[StepExpression] =
    ".." ~> Predicate.? ^^ { Up(_) } |
      "." ~> Predicate.? ^^ { Self(_) } |
      StepName ~ Predicate.? ^^ { case qn ~ p => { NamedStep(qn, p) } }

  def Predicate: Parser[PredicateExpression] = log(
    "[" ~> Expr <~ "]" ^^ { PredicateExpression(_) })("predicate")

  def PrimaryExpr: Parser[PrimaryExpression] = log(
    FunctionCall | Literal | VarRef | ParenthesizedExpr)("primary")

  def Literal = log((StringLiteral | NumericLiteral) ^^ { LiteralExpression(_) })("literal")

  def NumericLiteral = DoubleLiteral | DecimalLiteral | IntegerLiteral

  def VarRef = "$" ~> RefName ^^ { VariableRef(_) }

  def ParenthesizedExpr = "(" ~> Expr <~ ")" ^^ { ParenthesizedExpression(_) }

  def FunctionCall: Parser[FunctionCallExpression] = log(
    (RefName ~ ArgList) ^^ {
      case qn ~ arglist => FunctionCallExpression(qn, arglist)
    })("functionCall")

  def ArgList = log(
    "(" ~ ")" ^^ { _ => Nil } |
      "(" ~> (ExprSingle ~ (("," ~> ExprSingle).*)) <~ ")" ^^ { case e1 ~ moreEs => e1 :: moreEs })("argList")

  def StepName = log(QualifiedName)("stepName")

  def RefName = log(QualifiedName)("refName")

  def QualifiedName: Parser[String] = PrefixedName | UnprefixedName

  def PrefixedName = QNameRegex.QName
  def UnprefixedName = QNameRegex.NCName

  def IntegerLiteral: Parser[BigInt] = Digits ^^ { BigInt(_) }

  val Digits = """[0-9]+""".r
  val optDigits: Parser[String] = """[0-9]*""".r
  val Expon: Parser[String] = """[eE][+-]?[0-9]{1,3}""".r
  val plusMinus: Parser[String] = """[+-]?""".r

  val DecimalLiteral: Parser[BigDecimal] =
    ("." ~> Digits) ^^ { case dig => BigDecimal("0." + dig) } |
      (Digits ~ ("." ~> optDigits)) ^^ { case digit ~ optDig => BigDecimal(digit + "." + optDig) }

  val DoubleLiteral: Parser[Double] = (
    "." ~> Digits ~ Expon ^^ {
      case fraction ~ exp => {
        "0." + fraction + exp
      }
    } |
    Digits ~ (("." ~> optDigits).?) ~ Expon ^^ {
      case intPart ~ fraction ~ exp => intPart + "." + fraction.getOrElse("0") + exp
    }) ^^ { str => println(str); java.lang.Double.parseDouble(str) }

  /**
   * String literal must be one regex, not separate combinators combined.
   *
   * This is to avoid whitespace collapsing inside string literals. We want
   * whitespace to be ignored outside string literals, but not inside them.
   */
  //  val StringLiteral: Parser[String] =
  //    ("\"" ~> (EscapeQuot | notQuot).* <~ "\"") ^^ { case values => values.mkString } |
  //      ("'" ~> (EscapeApos | notApos).* <~ "'") ^^ { case values => values.mkString }
  //
  //  val EscapeQuot = "\"\""
  //  val EscapeApos = "''"
  //  val notQuot: Parser[String] = """[^"]""".r
  //  val notApos: Parser[String] = """[^']""".r

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
