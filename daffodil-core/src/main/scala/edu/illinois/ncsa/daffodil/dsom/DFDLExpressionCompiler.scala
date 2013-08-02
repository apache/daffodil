package edu.illinois.ncsa.daffodil.dsom

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable

import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.OOLAGHost
import edu.illinois.ncsa.daffodil.processors.EmptyVariableMap
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.util.DebugRegexParsers
import edu.illinois.ncsa.daffodil.xml.XMLUtils

class DFDLExpressionError(schemaContext: Option[SchemaComponentBase],
  kind: String,
  args: Any*)
  extends SchemaDefinitionDiagnosticBase(
    schemaContext, None, None, kind, args: _*) {

  override def isError = false
  val diagnosticKind = "Error"

  override def contextInfo(msg: String,
    diagnosticKind: String,
    schContextLocDescription: String,
    annContextLocDescription: String,
    schemaContext: Option[SchemaComponentBase]): String = {

    val res = "DFDLExpressionError " + diagnosticKind + ": " + msg +
      "\nSchema context: " + Some(schemaContext).getOrElse("top level") + "." +
      // TODO: should be one or the other, never(?) both
      schContextLocDescription +
      annContextLocDescription

    res
  }

}

// For debugging to indent text
object StepCounter {
  private var cntr: Int = 0
  def plusOne(): Int = {
    cntr = cntr + 1
    cntr
  }
  def minusOne(): Int = {
    if (cntr > 0) cntr = cntr - 1
    cntr
  }
}

abstract class ExpressionBase(val str: String) extends OOLAGHost {

  override def toString(): String = str
  def isLiteral: Boolean

  def name: String
  def hasPredicate: Boolean
  def children: List[ExpressionBase]
  def setContextsForChildren() { children.foreach { c => c.setOOLAGContext(this); c.setContextsForChildren(); } }
  def rethrowAsDiagnostic(th: Throwable) = Error("%s", th)

  def Error(str: String, args: Any*) = {
    ExecutionMode.requireCompilerMode

    val rsde = new DFDLExpressionError(None, str, args: _*)
    throw (rsde)
  }
  protected def indent = {
    val numTabs = StepCounter.plusOne
    val indent = new StringBuilder
    for (i <- 1 to numTabs) {
      indent.append(".")
    }
    indent.toString
  }
  protected def unindent = StepCounter.minusOne
  protected def retrievePathExpressions: List[ExpressionBase] = {
    //val indentStr = indent
    var lst: List[ExpressionBase] = children.toList.filter(c => c.isInstanceOf[PathTypeExpression]).filter(p => p.isLiteral)
    //Console.err.println(indentStr + "S_" + name + "\t" + str)
    val childPaths = children.foreach(c => lst = lst ++ c.retrievePathExpressions)
    //Console.err.println(indentStr + "E_" + name + "\t" + str + "\t" + lst)
    //unindent
    lst
  }
  def getPathExpressions: List[ExpressionBase] = retrievePathExpressions
}

abstract class ExpressionLists(override val str: String, val lst: List[ExpressionBase])
  extends ExpressionBase(str) with HasExpressionLists {

  lazy val hasPredicate = lst.filter(expr => expr.hasPredicate).length > 0
  val children = lst
}

trait HasExpressionLists { self: ExpressionLists =>
  val isLiteral = {
    var allLiteral: Boolean = true
    breakable {
      self.lst.foreach(e => {
        if (!e.isLiteral) allLiteral = false
        break
      })
    }
    allLiteral
  }
}

case class Expression(override val str: String, ifor: ExpressionBase)
  extends ExpressionBase(str) {

  val name = "Expression"
  lazy val hasPredicate = ifor.hasPredicate
  val children = List(ifor)
  val isLiteral = ifor.isLiteral
}
case class IfExpression(override val str: String, ifthenelse: List[ExpressionBase])
  extends ExpressionLists(str, ifthenelse) {

  val name = "IfExpression"
}
case class OrExpression(override val str: String, ands: List[AndExpression])
  extends ExpressionLists(str, ands) {

  val name = "OrExpression"
}
case class AndExpression(override val str: String, comps: List[ComparisonExpression])
  extends ExpressionLists(str, comps) {
  val name = "AndExpression"
}
case class ComparisonExpression(override val str: String, adds: List[AdditiveExpression])
  extends ExpressionLists(str, adds) {

  val name = "ComparisonExpression"
}
case class AdditiveExpression(override val str: String, mults: List[MultiplicativeExpression])
  extends ExpressionLists(str, mults) {

  val name = "AdditiveExpression"
}
case class MultiplicativeExpression(override val str: String, unarys: List[UnaryExpression])
  extends ExpressionLists(str, unarys) {

  val name = "MultiplicativeExpression"
}
case class UnaryExpression(override val str: String, pathExp: PathExpression)
  extends ExpressionBase(str) {

  val name = "UnaryExpression"
  lazy val hasPredicate = pathExp.hasPredicate
  val children = List(pathExp)
  val isLiteral = pathExp.isLiteral
}
case class PathExpression(override val str: String, pathExp: PathTypeExpression)
  extends ExpressionBase(str) {

  val name = "PathExpression"
  lazy val hasPredicate: Boolean = pathExp.hasPredicate
  val children = List(pathExp)
  val isLiteral = pathExp.isLiteral
}
abstract class PathTypeExpression(str: String)
  extends ExpressionBase(str) {

  val pathWithoutPredicate = {
    val pattern = """[^\[]+""".r
    pattern.findFirstIn(str).get
  }
}
case class RootPathExpression(override val str: String, relPath: Option[RelativePathExpression])
  extends PathTypeExpression(str) {

  val name = "RootPathExpression"
  lazy val hasPredicate: Boolean = {
    if (!relPath.isDefined) false
    else relPath.get.hasPredicate
  }
  val hasRelativePath: Boolean = relPath.isDefined
  val hasLiteralPrimaryExpr: Boolean = relPath.get.hasLiteralPrimaryExpr

  val children: List[ExpressionBase] = relPath match {
    case None => List.empty
    case Some(p) => List(p)
  }
  val isLiteral = {
    if (hasRelativePath) relPath.get.isLiteral
    else true
  }
}
case class RelativePathExpression(override val str: String, steps: List[StepExpression])
  extends PathTypeExpression(str) {

  val name = "RelativePathExpression"
  lazy val hasPredicate: Boolean = {
    steps.filter(s => s.hasPredicate).length > 0
  }
  val hasLiteralPrimaryExpr: Boolean = false
  val children: List[ExpressionBase] = steps

  val isLiteral = {
    var allLiteral: Boolean = true
    breakable {
      steps.foreach(e => {
        if (!e.isLiteral) allLiteral = false
        break
      })
    }
    allLiteral
  }
}
case class StepExpression(override val str: String, pred: Option[PredicateExpression])
  extends ExpressionBase(str) {

  val name = "StepExpression"
  val hasPredicate: Boolean = pred.isDefined
  val children: List[ExpressionBase] = pred match {
    case None => List.empty
    case Some(p) => List(p)
  }
  val isLiteral = {
    if (hasPredicate) pred.get.isLiteral
    else true
  }
}
case class PredicateExpression(override val str: String, ifOr: ExpressionBase)
  extends ExpressionBase(str) {

  val name = "PredicateExpression"
  lazy val hasPredicate = ifOr.hasPredicate
  val children: List[ExpressionBase] = List(ifOr)
  val isLiteral = ifOr.isLiteral
}
case class FilterExpression(override val str: String, prim: PrimaryExpression, pred: Option[PredicateExpression])
  extends PathTypeExpression(str) {

  val name = "FilterExpression"

  lazy val hasPredicate: Boolean = {
    if (pred.isDefined) true
    else false
  }
  val hasLiteralPrim: Boolean = prim.isLiteral
  val hasLiteralPred: Boolean = if (hasPredicate) pred.get.isLiteral else true
  val children: List[ExpressionBase] = pred match {
    case None => List(prim)
    case Some(p) => List(prim, p)
  }
  val isLiteral = {
    hasLiteralPrim && hasLiteralPred
  }
}
abstract class PrimaryExpression(override val str: String, expressions: Option[List[ExpressionBase]])
  extends ExpressionBase(str) {

  val name = "PrimaryExpression"
  lazy val hasPredicate = {
    expressions match {
      case Some(exprs) => exprs.filter(expr => expr.hasPredicate).length > 0
      case None => false
    }
  }
  val isLiteral: Boolean = { !expressions.isDefined }
  val children: List[ExpressionBase] = expressions.getOrElse(List.empty)
}
case class PrimExpression(override val str: String)
  extends PrimaryExpression(str, None)
case class VariableRef(override val str: String, val theQName: String,
  referringContext: SchemaComponent, var variableMap: VariableMap)
  extends PrimaryExpression(str, None) {

  override val name = "VariableRef"
  val evaluatedValue: Option[ExpressionBase] = {
    val (ns, localPart) = XMLUtils.QName(referringContext.xml, theQName, referringContext.schemaDocument)
    val expandedVarName = XMLUtils.expandedQName(ns, localPart)
    val compiler = new DFDLPathExpressionCompiler(referringContext)
    val (value, newVMap) = variableMap.readVariable(expandedVarName, referringContext)
    variableMap = newVMap
    val (result, newerVMap) = compiler.getExpressionTree(value.toString, newVMap)
    variableMap = newerVMap
    result
  }
  override def toString(): String = evaluatedValue.getOrElse(str).toString()
}
case class FunctionCallExpression(override val str: String, expressions: Option[List[Expression]])
  extends PrimaryExpression(str, expressions) {
  override val name = "FunctionCallExpression"
}
case class ParenthesizedExpression(override val str: String, expressions: Option[List[ExpressionBase]])
  extends PrimaryExpression(str, expressions) {
  override val name = "ParenthesizedExpression"
}

class DFDLPathExpressionCompiler(context: SchemaComponent) extends DebugRegexParsers {

  override val skipWhitespace = false //true

  private val expressionCompiler = new ExpressionCompiler(context)

  private var variableMap: VariableMap = EmptyVariableMap

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

  // Uncomment for testing!
  //  /**
  //   * A helper method that turns a `Parser` into one that will
  //   *  print debugging information to stdout before and after
  //   *  being applied.
  //   */
  //  override def log[T](p: => Parser[T])(name: String): Parser[T] = Parser { in =>
  //    Console.out.println("trying %s at %s".format(name, in))
  //    val r = p(in)
  //    Console.out.println("end %s --> %s".format(name, r))
  //    r
  //  }

  def getPathsFromExpression(expr: String, vmap: VariableMap): Either[(String, VariableMap), (List[ExpressionBase], VariableMap)] = {
    variableMap = vmap
    val pResult = this.parse(this.log(DFDLExpression)("getPathsFromExpression"), expr)
    pResult match {
      case Success(paths, next) => Right(paths.getPathExpressions, variableMap)
      case NoSuccess(msg, next) => Left(msg, variableMap)
    }
  }

  def getPathsFromExpressionAsCompiledExpressions(expr: String, vmap: VariableMap): Either[(String, VariableMap), (List[CompiledExpression], VariableMap)] = {
    variableMap = vmap
    val pResult = this.parse(this.log(DFDLExpression)("getPathsFromExpressionAsCompiledExpressions"), expr)
    pResult match {
      case Success(paths, next) => {
        val ces = paths.getPathExpressions.map { p =>
          {
            val exp = "{ " + p.toString + " }"
            val f = Found(exp, context)
            val ce = expressionCompiler.compile(ConvertToType.String, f)
            ce
          }
        }.filterNot(ce => ce.isConstant) // Paths are not constant
        Right(ces, variableMap)
      }
      case NoSuccess(msg, next) => Left(msg, variableMap)
    }
  }

  def getExpressionTree(expr: String, vmap: VariableMap): (Option[ExpressionBase], VariableMap) = {
    variableMap = vmap
    val pResult = this.parse(this.log(DFDLExpression)("getExpressionTree"), expr)
    pResult match {
      case Success(paths, next) => (Some(paths), variableMap)
      case NoSuccess(msg, next) => (None, variableMap)
    }
  }

  def ContextItemExpr = "."
  def AbbrevReverseStep = ".."
  def ForwardAxis = ("child" ~ "::") | ("self" ~ "::")
  def ValueComp = "eq" | "ne" | "lt" | "le" | "gt" | "ge"

  def DFDLExpression: Parser[ExpressionBase] = ("{" ~ Expr ~ "}") ^^ {
    case lb ~ exp ~ rb => exp
  } ||| Expr
  def Expr: Parser[ExpressionBase] = ExprSingle
  def ExprSingle: Parser[Expression] = (IfExpr ||| OrExpr) ^^ { xpr => Expression(xpr.toString, xpr) }
  def IfExpr: Parser[IfExpression] = "if" ~ "(" ~ Expr ~ ")" ~ "then" ~ ExprSingle ~ "else" ~ ExprSingle ^^ {
    case iff ~ op ~ xpr ~ cp ~ th ~ xpr2 ~ els ~ xpr3 =>
      IfExpression(iff + op + xpr + cp + th + xpr2 + els + xpr3, List(xpr, xpr2, xpr3))
  }
  def OrExpr: Parser[OrExpression] = AndExpr ~ ("or" ~ AndExpr ^^ { case or ~ ae => ae }).* ^^ {
    case ae ~ ae2 => {
      val lst = ae :: ae2
      OrExpression(ae + ae2.mkString(" or "), lst)
    }
  }
  def AndExpr: Parser[AndExpression] = ComparisonExpr ~ ("and" ~ ComparisonExpr ^^ {
    case l ~ ce => ce
  }).* ^^ {
    case ce ~ ce2 => {
      val lst = ce :: ce2
      AndExpression(ce + ce2.mkString(" and "), lst)
    }
  }
  def ComparisonExpr: Parser[ComparisonExpression] = AdditiveExpr ~ ((ValueComp) ~ AdditiveExpr ^^ { case vc ~ ae => (vc, ae) }).? ^^ {
    case ae ~ Some((vc, ae2)) => ComparisonExpression(ae + vc + ae2, List(ae, ae2))
    case ae ~ None => ComparisonExpression(ae.toString, List(ae))
  }
  def AdditiveExpr: Parser[AdditiveExpression] = MultiplicativeExpr ~ ((("+" | "-") ~ MultiplicativeExpr) ^^ {
    case op ~ exp => (op, exp)
  }).* ^^ {
    case e1 ~ e2 => {
      val str = e2.map { case (op, exp) => " " + op + " " + exp }.mkString
      val lst = e1 :: e2.map(x => x._2).toList
      AdditiveExpression(e1 + str, lst)
    }
  }
  def MultiplicativeExpr: Parser[MultiplicativeExpression] = UnaryExpr ~ (("*" | "div" | "idiv" | "mod") ~ UnaryExpr ^^ {
    case op ~ e => (op, e)
  }).* ^^ {
    case e1 ~ e2 => {
      val str = e2.map { case (op, exp) => " " + op + " " + exp }.mkString
      val lst = e1 :: e2.map(x => x._2).toList
      MultiplicativeExpression(e1 + str, lst)
    }
  }
  def UnaryExpr: Parser[UnaryExpression] = ("-" | "+").* ~ ValueExpr ^^ { case op ~ v => UnaryExpression(op.mkString + v, v) }
  def ValueExpr = PathExpr
  def PathExpr: Parser[PathExpression] = (("/" ~ RelativePathExpr.?) ^^ {
    case s ~ Some(p) => {
      RootPathExpression(s + p, Some(p))
    }
    case s ~ None => {
      RootPathExpression(s, None)
    }
  } ||| RelativePathExpr ||| FilterExpr) ^^ { case p => PathExpression(p.toString, p) }
  def RelativePathExpr: Parser[RelativePathExpression] =
    StepExpr ~ (("/") ~ StepExpr ^^ {
      case s ~ expr => (s, expr)
    }).* ^^ {
      case s ~ opt => {
        val str = opt.map { case (op, exp) => op + exp }.mkString
        val lst = s :: opt.map(x => x._2)
        RelativePathExpression(s + str, lst)
      }
    }
  def StepExpr: Parser[StepExpression] = AxisStep
  def AxisStep: Parser[StepExpression] = (ReverseStep ||| ForwardStep) ~ Predicate.? ^^ {
    case s ~ Some(p) => {
      StepExpression(s + p, Some(p))
    }
    case s ~ None => {
      StepExpression(s, None)
    }
  }
  def ForwardStep = (ForwardAxis ~ NodeTest) ^^ { case fa ~ nt => fa + nt } ||| AbbrevForwardStep
  def AbbrevForwardStep = NodeTest ||| ContextItemExpr
  def ReverseStep = (ReverseAxis ~ NodeTest) ^^ { case ra ~ nt => ra + nt } ||| AbbrevReverseStep
  def ReverseAxis = ("parent" ~ "::") ^^ { case p ~ cc => p + cc }
  def NodeTest = NameTest
  def NameTest = QName
  def FilterExpr: Parser[FilterExpression] = PrimaryExpr ~ Predicate.? ^^ {
    case expr ~ Some(pr) => {
      FilterExpression(expr.toString + pr.toString, expr, Some(pr))
    }
    case expr ~ None => {
      FilterExpression(expr.toString, expr, None)
    }
  }
  def Predicate: Parser[PredicateExpression] = "[" ~ Expr ~ "]" ^^ { case ob ~ xpr ~ cb => PredicateExpression(ob + xpr + cb, xpr) }
  def PrimaryExpr: Parser[PrimaryExpression] = (Literal ||| VarRef ||| ParenthesizedExpr |||
    ContextItemExpr ||| FunctionCall) ^^ {
      case s: String => PrimExpression(s)
      case pe: PrimaryExpression => pe
    }
  def Literal = NumericLiteral | StringLiteral
  def NumericLiteral = IntegerLiteral ||| DecimalLiteral ||| DoubleLiteral
  def VarRef = "$" ~ VarName ^^ {
    case d ~ vn => {
      val vref = VariableRef(d + vn, vn, context, variableMap)
      variableMap = vref.variableMap
      vref
    }
  }
  def VarName = QName
  def ParenthesizedExpr = "(" ~ Expr ~ ")" ^^ {
    case op ~ expr ~ cp =>
      ParenthesizedExpression(op + expr + cp, Some(List(expr)))
  }
  def FunctionCall: Parser[FunctionCallExpression] =
    QName ~ "(" ~ ((ExprSingle ~ ("," ~ ExprSingle ^^ {
      case c ~ e => (c, e)
    }).*) ^^ {
      case e1 ~ opt => {
        val str = opt.map { case (op, exp) => op + " " + exp }.mkString
        val lst = e1 :: opt.map(x => x._2)
        (e1 + str, lst)
      }
    }).? ~ ")" ^^ {
      case qnm ~ op ~ Some(optValue) ~ cp => {
        val (optStr, optExpr) = optValue
        val str = qnm + op + optStr + cp
        new FunctionCallExpression(str, Some(optExpr))
      }
      case qnm ~ op ~ None ~ cp => FunctionCallExpression(qnm + op + cp, None)
    }
  def QName = PrefixedName ||| UnprefixedName
  def PrefixedName = Prefix ~ ":" ~ LocalPart ^^ { case p ~ c ~ l => p + c + l }
  def UnprefixedName = LocalPart
  def Prefix = NCName
  def LocalPart = NCName
  def NCName = NCNameStartChar ~ (NCNameChar).* ^^ { case ncstart ~ ncchar => ncstart + ncchar.mkString }
  def IntegerLiteral = Digits

  val Digits = """[0-9]+""".r
  val optDigits: Parser[String] = """[0-9]*""".r
  val exp: Parser[String] = """[eE]""".r
  val plusMinus: Parser[String] = """[+-]?""".r

  val DecimalLiteral: Parser[String] = ("." ~ Digits) ^^ { case dot ~ dig => dot + dig } |||
    (Digits ~ "." ~ optDigits) ^^ { case digit ~ dot ~ optDig => digit + dot + optDig }

  val DoubleLiteral: Parser[String] = ((("." ~ Digits) ^^ { case dot ~ dig => dot + dig } |||
    (Digits ~ ("." ~ optDigits ^^ { case dot ~ dig => dot + dig }).?) ^^ {
      case dig ~ Some(rest) => dig + rest
      case dig ~ None => dig
    }) ~ exp ~ plusMinus ~ Digits) ^^ { case first ~ e ~ pm ~ d => first + e + pm + d }

  val notQuot: Parser[String] = """[^"]""".r
  val notApos: Parser[String] = """[^']""".r

  val StringLiteral: Parser[String] = ("\"" ~ (EscapeQuot ||| notQuot).* ~ "\"") ^^ {
    case bq ~ values ~ eq => bq + values.mkString + eq
  } ||| ("'" ~ (EscapeApos ||| notApos).* ~ "'") ^^ {
    case ba ~ values ~ ea => ba + values.mkString + ea
  }

  val EscapeQuot = "\"\""
  val EscapeApos = "''"

  val xC0_D6 = ("""[\x{C0}-\x{D6}]""").r
  val xD8_F6 = """[\x{D8}-\x{F6}]""".r
  val xF8_2FF = """[\x{F8}-\x{2FF}]""".r
  val x370_37D = """[\x{370}-\x{37D}]""".r
  val x37F_1FFF = """[\x{37F}-\x{1FFF}]"""
  val x200C_200D = """\x{200c}|\x{200d}""".r
  val x2070_218F = """[\x{2070}-\x{218F}]""".r
  val x2C00_2FEF = """[\x{2C00}-\x{2FEF}]""".r
  val x3001_D7FF = """[\x{3001}-\x{D7FF}]""".r
  val xF900_FDCF = """[\x{F900}-\x{FDCF}]""".r
  val xFDF0_FFFD = """[\x{FDF0}-\x{FFFD}]""".r
  val x10000_EFFFF = """[\x{10000}-\x{EFFFF}]""".r
  val range0_9 = """[0-9]""".r
  val xB7 = """\xB7""".r
  val x0300_036F = """[\x{0300}-\x{036F}]""".r
  val x203F_2040 = """[\x{203F}-\x{2040}]""".r

  def NCNameStartChar = """[A-Z]|_|[a-z]""".r | xC0_D6 | xD8_F6 | xF8_2FF | x37F_1FFF | x200C_200D |
    x2070_218F | x2C00_2FEF | x3001_D7FF | xF900_FDCF | xFDF0_FFFD | x10000_EFFFF
  def NCNameChar = NCNameStartChar | "-" | "." | range0_9 | xB7 |
    x0300_036F | x203F_2040
}
