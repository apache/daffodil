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
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.dsom._
import scala.xml.NamespaceBinding
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.xml.RefQName
import edu.illinois.ncsa.daffodil.api.Diagnostic

abstract class Expression extends OOLAGHost
  with SchemaFileLocatable
  with ImplementsThrowsSDE {

  /**
   * Use several calls instead of one, because then OOLAG will try them
   * all separately, and perhaps if you're lucky report more errors.
   */
  requiredEvaluations(parent)
  requiredEvaluations(targetType)
  requiredEvaluations(inherentType)
  requiredEvaluations(isTypeCorrect)
  requiredEvaluations(compiledDPath)

  // This split allows overrides of this lazy val to still reuse this
  // (super.isTypeCorrect doesn't work inside an overridden lazy val.)
  lazy val isTypeCorrect: Boolean = checkTypeCorrectness
  //
  // override for other checking beyond what is needed to do conversions
  //
  protected def checkTypeCorrectness = true
  //    lazy val parentString = 
  //      if (parent == null) this.text
  //    else parent.text
  //    typeCheck(inherentType, targetType, this.text, parentString)
  //  }
  //
  //  def typeCheck(inherentType: NodeInfo.Kind,
  //    targetType: NodeInfo.Kind,
  //    inherentExprText: => String,
  //    enclosingExprText: => String): Boolean = {
  //    if (targetType == NodeInfo.Array) {
  //      // other places verify that an expression produces an array.
  //      return true
  //    }
  //    if (!inherentType.isSubtypeOf(targetType)) {
  //      SDE("Types incompatible: got %s (from %s), but required %s (for %s)",
  //        inherentType.name, inherentExprText, targetType.name, enclosingExprText)
  //    }
  //    true
  //  }

  def text: String

  /**
   * TODO: get more precise line and column information for
   * pointing at sub-regions of large DPath expressions
   *
   * We're parsing them, so we should have access to specific locations
   * within the expression.
   */
  def lineAttribute: Option[String] = schemaComponent.lineAttribute
  def columnAttribute: Option[String] = schemaComponent.columnAttribute
  def fileAttribute: Option[String] = schemaComponent.fileAttribute
  def fileName = schemaComponent.fileName

  lazy val conversions = {
    val inh = inherentType
    val tt = targetType
    val res = Conversion.conversionOps(inh, tt, this)
    res
  }

  def compiledDPath: DPathRecipe

  final lazy val parentOpt = Option(parent)

  final lazy val parent: Expression = {
    if (!this.hasOOLAGRootSetup)
      Assert.invariantFailed("not setup")
    this match {
      case w: WholeExpression => null
      case _ => this.oolagContext.asInstanceOf[Expression]
    }
  }

  lazy val schemaComponent: DPathCompileInfo = // override in WholeExpression
    parent.schemaComponent

  final lazy val enclosingElementCompileInfo: Option[DPathElementCompileInfo] =
    schemaComponent.enclosingElementCompileInfo

  final lazy val rootElement: DPathElementCompileInfo = {
    enclosingElementCompileInfo.map {
      _.rootElement
    }.getOrElse {
      schemaComponent.elementCompileInfo.getOrElse {
        Assert.invariantFailed("root doesn't have compile info")
      }
    }
  }

  lazy val namespaces: NamespaceBinding = parent.namespaces

  def children: Seq[Expression]

  def setContextsForChildren(context: OOLAGHost = this) {
    children.foreach {
      c =>
        c.setOOLAGContext(context);
        c.setContextsForChildren(c);
    }
  }

  /**
   * The target type is defined for simple types. It gives the type that
   * the expression must return. It is the type that the context is expecting
   * or in attribute-grammar terms, the inherited type.
   * <p>
   * For inputValueCalc, this is the type of the element carrying that property.
   * <p>
   * For setVariable and newVariableInstance, it is the type of the variable.
   * <p>
   * For dfdl:length and dfdl:occursCount it is UInt
   * <p>
   * For the test properties of assert/discriminator it is Boolean
   * <p>
   * For the following properties, when their value is an expression
   * the xsdTargetType is NonEmptyString
   * byteOrder,
   * encoding,
   * outputNewLine,
   * escapeCharacter,
   * escapeEscapeCharacter,
   * initiator,
   * terminator,
   * separator,
   * textStandardDecimalSeparator,
   * textStandardGroupingSeparator,
   * textStandardExponentRep,
   * binaryFloatRep,
   * textBooleanFalseRep,
   * textBooleanTrueRep
   */
  lazy val targetType: NodeInfo.Kind = {
    // this will only be called for expressions that have a parent
    Assert.usage(parentOpt.isDefined)
    val p = parentOpt.get
    val res = p.targetTypeForSubexpression(this)
    res
  }

  def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind

  /**
   * The inherent type is the bottom-up type. That is, the type that
   * is determined by examining the expression, or in attribute-grammar speak,
   * the synthetic type.
   *
   * For "foo" it is string, for
   * 5.0 it is double, and for 5 it is integer. For an identifier such
   * as b in /a/b, it is the simple type of the element declaration for b.
   *
   * The inherent type can be a complex type, as in the foo in /foo/bar.
   * That is why this is not the same system of type-kind values as the
   * xsdTargetType
   */
  def inherentType: NodeInfo.Kind

  //  protected def indent = {
  //    val numTabs = StepCounter.plusOne
  //    val indent = new StringBuilder
  //    for (i <- 1 to numTabs) {
  //      indent.append(".")
  //    }
  //    indent.toString
  //  }
  //  protected def unindent = StepCounter.minusOne
  // protected def retrievePathExpressions: List[Expression] = 
  //  {
  //    //val indentStr = indent
  //    var lst: List[Expression] = children.toList.filter(c => c.isInstanceOf[PathTypeExpression]).filter(p => p.isLiteral)
  //    //Console.err.println(indentStr + "S_" + name + "\t" + str)
  //    val childPaths = children.foreach(c => lst = lst ++ c.retrievePathExpressions)
  //    //Console.err.println(indentStr + "E_" + name + "\t" + str + "\t" + lst)
  //    //unindent
  //    lst
  //  }
  // def getPathExpressions: List[Expression] = retrievePathExpressions

  def resolveRef(qnameString: String) = {
    QName.resolveRef(qnameString, namespaces).getOrElse {
      SDE("The prefix of '%s' has no corresponding namespace definition.", qnameString)
    }
  }
}

abstract class ExpressionLists(val lst: List[Expression])
  extends Expression {
  override lazy val children = lst
}

trait BinaryExpMixin { self: ExpressionLists =>
  def left = children(0)
  def right = children(1)
  def op: String
  def text = left.text + " " + op + " " + right
}

trait BooleanExpression extends BinaryExpMixin {
  self: ExpressionLists =>

  override lazy val compiledDPath = {
    val leftDPath = left.compiledDPath
    val rightDPath = right.compiledDPath
    val c = conversions
    val res = new DPathRecipe(BooleanOp(op, leftDPath, rightDPath) +: c)
    res
  }

  override def targetTypeForSubexpression(subexp: Expression): NodeInfo.Kind = NodeInfo.Boolean
  override lazy val inherentType: NodeInfo.Kind = NodeInfo.Boolean

}

case class NumberComparisonExpression(op: String, adds: List[Expression])
  extends ExpressionLists(adds) with BooleanExpression {

  lazy val compareOp: NumberCompareOp = {
    import NodeInfo._
    (op, convergedArgType) match {
      case ("<", Decimal) => LT_Decimal
      case ("lt", Decimal) => LT_Decimal
      case (">", Decimal) => GT_Decimal
      case ("gt", Decimal) => GT_Decimal
      case ("<=", Decimal) => LE_Decimal
      case ("le", Decimal) => LE_Decimal
      case (">=", Decimal) => GE_Decimal
      case ("ge", Decimal) => GE_Decimal

      case ("<", Integer) => LT_Integer
      case ("lt", Integer) => LT_Integer
      case (">", Integer) => GT_Integer
      case ("gt", Integer) => GT_Integer
      case ("<=", Integer) => LE_Integer
      case ("le", Integer) => LE_Integer
      case (">=", Integer) => GE_Integer
      case ("ge", Integer) => GE_Integer

      case ("<", NonNegativeInteger) => LT_NonNegativeInteger
      case ("lt", NonNegativeInteger) => LT_NonNegativeInteger
      case (">", NonNegativeInteger) => GT_NonNegativeInteger
      case ("gt", NonNegativeInteger) => GT_NonNegativeInteger
      case ("<=", NonNegativeInteger) => LE_NonNegativeInteger
      case ("le", NonNegativeInteger) => LE_NonNegativeInteger
      case (">=", NonNegativeInteger) => GE_NonNegativeInteger
      case ("ge", NonNegativeInteger) => GE_NonNegativeInteger

      case ("<", UnsignedLong) => LT_UnsignedLong
      case ("lt", UnsignedLong) => LT_UnsignedLong
      case (">", UnsignedLong) => GT_UnsignedLong
      case ("gt", UnsignedLong) => GT_UnsignedLong
      case ("<=", UnsignedLong) => LE_UnsignedLong
      case ("le", UnsignedLong) => LE_UnsignedLong
      case (">=", UnsignedLong) => GE_UnsignedLong
      case ("ge", UnsignedLong) => GE_UnsignedLong

      case ("<", Long) => LT_Long
      case ("lt", Long) => LT_Long
      case (">", Long) => GT_Long
      case ("gt", Long) => GT_Long
      case ("<=", Long) => LE_Long
      case ("le", Long) => LE_Long
      case (">=", Long) => GE_Long
      case ("ge", Long) => GE_Long

      case ("<", UnsignedInt) => LT_UnsignedInt
      case ("lt", UnsignedInt) => LT_UnsignedInt
      case (">", UnsignedInt) => GT_UnsignedInt
      case ("gt", UnsignedInt) => GT_UnsignedInt
      case ("<=", UnsignedInt) => LE_UnsignedInt
      case ("le", UnsignedInt) => LE_UnsignedInt
      case (">=", UnsignedInt) => GE_UnsignedInt
      case ("ge", UnsignedInt) => GE_UnsignedInt

      case ("<", ArrayIndex) => LT_UnsignedInt
      case ("lt", ArrayIndex) => LT_UnsignedInt
      case (">", ArrayIndex) => GT_UnsignedInt
      case ("gt", ArrayIndex) => GT_UnsignedInt
      case ("<=", ArrayIndex) => LE_UnsignedInt
      case ("le", ArrayIndex) => LE_UnsignedInt
      case (">=", ArrayIndex) => GE_UnsignedInt
      case ("ge", ArrayIndex) => GE_UnsignedInt

      case ("<", Int) => LT_Int
      case ("lt", Int) => LT_Int
      case (">", Int) => GT_Int
      case ("gt", Int) => GT_Int
      case ("<=", Int) => LE_Int
      case ("le", Int) => LE_Int
      case (">=", Int) => GE_Int
      case ("ge", Int) => GE_Int

      case ("<", UnsignedShort) => LT_UnsignedShort
      case ("lt", UnsignedShort) => LT_UnsignedShort
      case (">", UnsignedShort) => GT_UnsignedShort
      case ("gt", UnsignedShort) => GT_UnsignedShort
      case ("<=", UnsignedShort) => LE_UnsignedShort
      case ("le", UnsignedShort) => LE_UnsignedShort
      case (">=", UnsignedShort) => GE_UnsignedShort
      case ("ge", UnsignedShort) => GE_UnsignedShort

      case ("<", Short) => LT_Short
      case ("lt", Short) => LT_Short
      case (">", Short) => GT_Short
      case ("gt", Short) => GT_Short
      case ("<=", Short) => LE_Short
      case ("le", Short) => LE_Short
      case (">=", Short) => GE_Short
      case ("ge", Short) => GE_Short

      case ("<", UnsignedByte) => LT_UnsignedByte
      case ("lt", UnsignedByte) => LT_UnsignedByte
      case (">", UnsignedByte) => GT_UnsignedByte
      case ("gt", UnsignedByte) => GT_UnsignedByte
      case ("<=", UnsignedByte) => LE_UnsignedByte
      case ("le", UnsignedByte) => LE_UnsignedByte
      case (">=", UnsignedByte) => GE_UnsignedByte
      case ("ge", UnsignedByte) => GE_UnsignedByte

      case ("<", Byte) => LT_Byte
      case ("lt", Byte) => LT_Byte
      case (">", Byte) => GT_Byte
      case ("gt", Byte) => GT_Byte
      case ("<=", Byte) => LE_Byte
      case ("le", Byte) => LE_Byte
      case (">=", Byte) => GE_Byte
      case ("ge", Byte) => GE_Byte

      case ("<", Float) => LT_Float
      case ("lt", Float) => LT_Float
      case (">", Float) => GT_Float
      case ("gt", Float) => GT_Float
      case ("<=", Float) => LE_Float
      case ("le", Float) => LE_Float
      case (">=", Float) => GE_Float
      case ("ge", Float) => GE_Float

      case ("<", Double) => LT_Double
      case ("lt", Double) => LT_Double
      case (">", Double) => GT_Double
      case ("gt", Double) => GT_Double
      case ("<=", Double) => LE_Double
      case ("le", Double) => LE_Double
      case (">=", Double) => GE_Double
      case ("ge", Double) => GE_Double

      case _ => subsetError("Unsupported operation '%s' on type %s.", op, convergedArgType)
    }
  }

  override lazy val compiledDPath = {
    val leftDPath = left.compiledDPath
    val rightDPath = right.compiledDPath
    val c = conversions
    val res = new DPathRecipe(NumberCompareOperator(compareOp, leftDPath, rightDPath) +: c)
    res
  }

  override def targetTypeForSubexpression(child: Expression): NodeInfo.Kind = convergedArgType

  lazy val (convergedArgType, _) = (left.inherentType, right.inherentType) match {
    case (left: NodeInfo.Numeric.Kind, right: NodeInfo.Numeric.Kind) =>
      Conversion.numericBinaryOpTargetTypes(op, left, right)
    case (left: NodeInfo.Numeric.Kind, r) =>
      SDE("Right operand for operator '%s' must have numeric type. Type was: %s.", op, r)
    case (l, r: NodeInfo.Numeric.Kind) =>
      SDE("Left operand for operator '%s' must have numeric type. Type was: %s.", op, l)
    case (l, r) =>
      SDE("Operands for operator '%s' must have numeric type. Types were: %s and %s.", op, l, r)
  }

}

trait NumericExpression extends BinaryExpMixin {
  self: ExpressionLists =>

  lazy val numericOp: NumericOp = {
    import NodeInfo._
    (op, convergedArgType) match {
      case ("+", Decimal) => PlusDecimal
      case ("-", Decimal) => MinusDecimal
      case ("*", Decimal) => TimesDecimal
      case ("div", Decimal) => DivDecimal
      case ("idiv", Decimal) => IDivDecimal
      case ("mod", Decimal) => ModDecimal

      case ("+", Integer) => PlusInteger
      case ("-", Integer) => MinusInteger
      case ("*", Integer) => TimesInteger
      case ("div", Integer) => DivInteger
      case ("idiv", Integer) => IDivInteger
      case ("mod", Integer) => ModInteger

      case ("+", NonNegativeInteger) => PlusNonNegativeInteger
      case ("-", NonNegativeInteger) => MinusNonNegativeInteger
      case ("*", NonNegativeInteger) => TimesNonNegativeInteger
      case ("div", NonNegativeInteger) => DivNonNegativeInteger
      case ("idiv", NonNegativeInteger) => IDivNonNegativeInteger
      case ("mod", NonNegativeInteger) => ModNonNegativeInteger

      case ("+", UnsignedLong) => PlusUnsignedLong
      case ("-", UnsignedLong) => MinusUnsignedLong
      case ("*", UnsignedLong) => TimesUnsignedLong
      case ("div", UnsignedLong) => DivUnsignedLong
      case ("idiv", UnsignedLong) => IDivUnsignedLong
      case ("mod", UnsignedLong) => ModUnsignedLong

      case ("+", Long) => PlusLong
      case ("-", Long) => MinusLong
      case ("*", Long) => TimesLong
      case ("div", Long) => DivLong
      case ("idiv", Long) => IDivLong
      case ("mod", Long) => ModLong

      case ("+", UnsignedInt) => PlusUnsignedInt
      case ("-", UnsignedInt) => MinusUnsignedInt
      case ("*", UnsignedInt) => TimesUnsignedInt
      case ("div", UnsignedInt) => DivUnsignedInt
      case ("idiv", UnsignedInt) => IDivUnsignedInt
      case ("mod", UnsignedInt) => ModUnsignedInt

      case ("+", ArrayIndex) => PlusUnsignedInt
      case ("-", ArrayIndex) => MinusUnsignedInt
      case ("*", ArrayIndex) => TimesUnsignedInt
      case ("div", ArrayIndex) => DivUnsignedInt
      case ("idiv", ArrayIndex) => IDivUnsignedInt
      case ("mod", ArrayIndex) => ModUnsignedInt

      case ("+", Int) => PlusInt
      case ("-", Int) => MinusInt
      case ("*", Int) => TimesInt
      case ("div", Int) => DivInt
      case ("idiv", Int) => IDivInt
      case ("mod", Int) => ModInt

      case ("+", UnsignedShort) => PlusUnsignedShort
      case ("-", UnsignedShort) => MinusUnsignedShort
      case ("*", UnsignedShort) => TimesUnsignedShort
      case ("div", UnsignedShort) => DivUnsignedShort
      case ("idiv", UnsignedShort) => IDivUnsignedShort
      case ("mod", UnsignedShort) => ModUnsignedShort

      case ("+", Short) => PlusShort
      case ("-", Short) => MinusShort
      case ("*", Short) => TimesShort
      case ("div", Short) => DivShort
      case ("idiv", Short) => IDivShort
      case ("mod", Short) => ModShort

      case ("+", UnsignedByte) => PlusUnsignedByte
      case ("-", UnsignedByte) => MinusUnsignedByte
      case ("*", UnsignedByte) => TimesUnsignedByte
      case ("div", UnsignedByte) => DivUnsignedByte
      case ("idiv", UnsignedByte) => IDivUnsignedByte
      case ("mod", UnsignedByte) => ModUnsignedByte

      case ("+", Byte) => PlusByte
      case ("-", Byte) => MinusByte
      case ("*", Byte) => TimesByte
      case ("div", Byte) => DivByte
      case ("idiv", Byte) => IDivByte
      case ("mod", Byte) => ModByte

      case ("+", Float) => PlusFloat
      case ("-", Float) => MinusFloat
      case ("*", Float) => TimesFloat
      case ("div", Float) => DivFloat
      case ("idiv", Float) => IDivFloat
      case ("mod", Float) => ModFloat

      case ("+", Double) => PlusDouble
      case ("-", Double) => MinusDouble
      case ("*", Double) => TimesDouble
      case ("div", Double) => DivDouble
      case ("idiv", Double) => IDivDouble
      case ("mod", Double) => ModDouble
      case _ => subsetError("Unsupported operation '%s' on type %s.", op, convergedArgType)
    }
  }

  override lazy val compiledDPath = {
    val leftDPath = left.compiledDPath
    val rightDPath = right.compiledDPath
    val c = conversions
    new DPathRecipe(NumericOperator(numericOp, leftDPath, rightDPath) +: c)
  }

  /**
   * The DPath operator, such as, "+", or "idiv"
   */
  def op: String

  override def inherentType: NodeInfo.Kind = convergedResult

  override def targetTypeForSubexpression(child: Expression): NodeInfo.Kind = convergedArgType

  lazy val (convergedArgType, convergedResult) = (left.inherentType, right.inherentType) match {
    case (left: NodeInfo.Numeric.Kind, right: NodeInfo.Numeric.Kind) =>
      Conversion.numericBinaryOpTargetTypes(op, left, right)
    case (left: NodeInfo.Numeric.Kind, r) =>
      SDE("Right operand for operator '%s' must have numeric type. Type was: %s.", op, r)
    case (l, r: NodeInfo.Numeric.Kind) =>
      SDE("Left operand for operator '%s' must have numeric type. Type was: %s.", op, l)
    case (l, r) =>
      SDE("Operands for operator '%s' must have numeric type. Types were: %s and %s.", op, l, r)
  }

}

/**
 * A whole expression
 */
case class WholeExpression(
  nodeInfoKind: NodeInfo.Kind,
  ifor: Expression,
  nsBindingForPrefixResolution: NamespaceBinding,
  sc: DPathCompileInfo)
  extends Expression {

  def init() {
    sc match {
      case comp: OOLAGHost => this.setOOLAGContext(comp)
      case _ => {
        // it's a runtime object. Shouldn't be except if we're compiling
        // from the debugger.
        this.setOOLAGContext(null) // we are the root.
      }
    }

    this.setContextsForChildren()
  }

  override lazy val namespaces = nsBindingForPrefixResolution

  override def text = ifor.text

  override lazy val targetType = nodeInfoKind

  override def targetTypeForSubexpression(subExpr: Expression): NodeInfo.Kind = {
    //
    // Note: the subExpr might not be exactly our ifor child expression
    // because if ifor was a function call, then the resulting function 
    // object will have been created and given this as its parent pointer.
    //
    Assert.invariant(subExpr == ifor | ifor.isInstanceOf[FunctionCallExpression])
    targetType
  }

  override lazy val inherentType = ifor.inherentType

  override lazy val schemaComponent = sc

  override lazy val compiledDPath = ifor.compiledDPath

  override lazy val children = List(ifor)

}

case class IfExpression(ifthenelse: List[Expression])
  extends ExpressionLists(ifthenelse) {

  override lazy val compiledDPath = new DPathRecipe(IF(predicate.compiledDPath,
    thenPart.compiledDPath, elsePart.compiledDPath))

  override def text = "if (" + predicate.text + ") then " + thenPart.text + " else " + elsePart.text
  lazy val List(predicate, thenPart, elsePart) = ifthenelse
  val op = "if"

  override def targetTypeForSubexpression(subexp: Expression) = {
    Assert.invariant(children.contains(subexp))
    if (subexp == predicate)
      NodeInfo.Boolean
    else
      this.targetType
  }

  override lazy val inherentType = {
    // we need the type which is the generalization of the thenPart and
    // elsePart inherent types, but it's an error if they have no generalization.
    // 
    NodeInfo.generalize(thenPart, elsePart)
  }
}

case class OrExpression(ands: List[Expression])
  extends ExpressionLists(ands) with BooleanExpression {
  val op = "or"
}
case class AndExpression(comps: List[Expression])
  extends ExpressionLists(comps) with BooleanExpression {
  val op = "and"
}

case class EqualityComparisonExpression(op: String, adds: List[Expression])
  extends ExpressionLists(adds) with BinaryExpMixin {

  override lazy val compiledDPath = {
    val leftDPath = left.compiledDPath
    val rightDPath = right.compiledDPath
    val c = conversions
    val res = new DPathRecipe(EqualityCompareOp(op, leftDPath, rightDPath) +: c)
    res
  }
  override lazy val inherentType = NodeInfo.Boolean

  override def targetTypeForSubexpression(subexp: Expression) = NodeInfo.AnyAtomic
}

case class AdditiveExpression(op: String, mults: List[Expression])
  extends ExpressionLists(mults) with NumericExpression {

}

case class MultiplicativeExpression(op: String, unarys: List[Expression])
  extends ExpressionLists(unarys) with NumericExpression

case class UnaryExpression(op: String, exp: Expression)
  extends ExpressionLists(List(exp)) {

  override def text = "( " + op + " (" + exp.text + "))"

  override def inherentType: NodeInfo.Kind = exp.inherentType

  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind =
    if (op == "+") targetType
    else {
      exp.inherentType // negate the inherent type, then we'll convert to whatever the target wants.
    }

  override lazy val compiledDPath =
    if (op == "+") {
      // no conversions because we passed on target type to subexpression
      exp.compiledDPath
    } else {
      new DPathRecipe(NegateOp(exp.compiledDPath) +: conversions)
    }
}

abstract class PathExpression()
  extends Expression {
  def steps: Seq[StepExpression]

  override def text = steps.map { _.text }.mkString("/")

  override lazy val inherentType: NodeInfo.Kind = steps.last.inherentType

  override def targetTypeForSubexpression(subexp: Expression) = {
    parent.targetTypeForSubexpression(this)
  }

  /**
   * Path to a single array element without a [N] predicate qualifying it.
   *
   * That is, the kind of path expression used to access information
   * about an array such as its current count.
   */
  lazy val isPathToOneWholeArray: Boolean = {
    if (steps == Nil) false // root is never an array
    else steps.last.isArray &&
      !steps.last.pred.isDefined && // last cannot have a [N] pred
      steps.dropRight(1).forall {
        // for all the prior steps 
        // if they mention an array element, there
        // must be a [N] predicate.
        step =>
          if (step.isInstanceOf[Up]) true
          else if (step.isArray) step.pred.isDefined
          else true
      }
  }
}

case class RootPathExpression(relPath: Option[RelativePathExpression])
  extends PathExpression() {

  override def text = "/" + super.text

  lazy val steps = relPath.map { _.steps }.getOrElse(Nil)

  override lazy val inherentType = {
    if (!(steps == Nil)) steps.last.inherentType
    else {
      rootElement.optPrimType match {
        case Some(pt) => NodeInfo.fromPrimType(pt)
        case None => {
          // this is the more common case. Only in unit tests will the
          // root element be simple type. 
          NodeInfo.Complex
        }
      }
    }
  }

  override def targetTypeForSubexpression(subexp: Expression) = {
    Assert.usage(relPath.isDefined)
    Assert.invariant(subexp == relPath.get)
    super.targetTypeForSubexpression(this)
  }

  override lazy val children = relPath.toSeq

  override lazy val compiledDPath = {
    val rel = relPath.map { rp => rp.compiledDPath.ops.toList }.getOrElse(Nil)
    val cdp = new DPathRecipe(ToRoot +: rel)
    cdp
  }
}

case class RelativePathExpression(steps1: List[StepExpression], isEvaluatedAbove: Boolean)
  extends PathExpression() {

  lazy val isAbsolutePath = {
    parent != null && parent.isInstanceOf[RootPathExpression]
  }

  /**
   * We must adjust the steps for the isEvaluatedAbove case.
   * That's when something like dfdl:occursCount is written as { ../c }.
   * In that case, it's written on an element decl as if it were accessing a
   * peer, but in fact the expression is evaluated before any instances of
   * the array are even allocated, so we must remove an ".." up move from
   * every relative path contained inside the expression, except when
   * part of an absolute path.
   */
  private lazy val adjustedSteps = {
    if (parent.isInstanceOf[RootPathExpression]) steps2
    else {
      // not an absolute path.
      if (isEvaluatedAbove) {
        // in this case, the relative path must begin with ".." to be
        // meaningful. 
        Assert.invariant(steps2(0).isInstanceOf[Up])
        steps2.tail // trim off the UP move at the start.
      } else steps2
    }
  }

  override lazy val compiledDPath: DPathRecipe = {
    val cps = adjustedSteps.map {
      _.compiledDPath
    }
    val ops = cps.map {
      _.ops.toList
    }
    val res = new DPathRecipe(ops.flatten)
    res
  }

  override lazy val steps = {
    steps2
  }

  // remove any spurious "." in relative paths so "./.././.." becomes "../.."
  val steps2 =
    if (steps1.length == 1) steps1
    else steps1.filter { case Self(None) => false; case _ => true }

  override lazy val children: List[Expression] = steps

}

sealed abstract class StepExpression(val step: String, val pred: Option[PredicateExpression])
  extends Expression {

  requiredEvaluations(priorStep)
  requiredEvaluations(schemaComponent)
  requiredEvaluations(stepElement)

  // override def toString = text

  // Note: all instances are distinct regardless of contents.
  override def equals(x: Any) = x match {
    case ar: AnyRef => this eq ar
    case _ => false
  }

  override def hashCode() = super.hashCode()

  lazy val stepQName = QName.resolveStep(step, namespaces).getOrElse {
    SDE("Step %s prefix has no corresponding namespace.", step)
  }

  override lazy val children = pred.toList

  def stepElement: DPathElementCompileInfo

  lazy val priorStep: Option[StepExpression] = {
    val res =
      if (isFirstStep)
        None
      else {
        val pos = positionInStepsSequence - 1
        val step = relPathParent.steps(pos)
        Some(step)
      }
    res
  }

  lazy val relPathParent = {
    parentOpt match {
      case Some(rel: RelativePathExpression) => rel
      case Some(x) => Assert.invariantFailed("StepExpression must have RelativePathExpression parent.")
      case None => Assert.invariantFailed("StepExpression must have parent.")
    }
  }

  lazy val isAbsolutePath = relPathParent.isAbsolutePath

  lazy val positionInStepsSequence = {
    var steps = relPathParent.steps
    steps.indexOf(this)
  }

  lazy val isFirstStep = {
    val res = (positionInStepsSequence == 0)
    res
  }

  lazy val isLastStep = {
    val res = positionInStepsSequence == (relPathParent.steps.length - 1)
    res
  }

  lazy val isArray: Boolean = stepElement.isArray

  /**
   * Used when there is no match as part of error diagnostic message.
   * It searches the local xml scope for a prefix binding for this
   * namespace. Returns Nil if none, a list of strings for all the available
   * prefixes for this namespace (because there can be more than one)
   */
  def suggestedPossiblePrefixes(ns: NS): Seq[String] = {
    val res = NS.allPrefixes(ns, schemaComponent.namespaces)
    res
  }

  override lazy val targetType: NodeInfo.Kind = {
    if (isLastStep) parent.targetTypeForSubexpression(this)
    else NodeInfo.Complex
  }

  override def targetTypeForSubexpression(subexp: Expression) = {
    Assert.invariant(pred.isDefined)
    Assert.invariant(pred.get == subexp)
    NodeInfo.ArrayIndex
  }

  override lazy val inherentType: NodeInfo.Kind = {
    if (!isLastStep) NodeInfo.Complex
    else {
      if (stepElement.optPrimType.isDefined) {
        // simple type, so 
        val pt = stepElement.optPrimType.get
        val nt = NodeInfo.fromPrimType(pt)
        nt
      } else {
        NodeInfo.Complex
      }
    }
  }

}

//TODO: Is ".[i]" ever a valid expression in DFDL?

case class Self(predArg: Option[PredicateExpression]) extends StepExpression(null, predArg) {

  override lazy val compiledDPath = new DPathRecipe(SelfMove +: conversions)

  override def text = "." // + "{" + stepElement.path + "}"

  override lazy val stepElement: DPathElementCompileInfo =
    priorStep.map { _.stepElement }.getOrElse {
      //  no prior step, so we're the first step 
      this.schemaComponent.elementCompileInfo.getOrElse {
        SDE("Relative path .. past root element.")
      }
    }
}

case class Up(predArg: Option[PredicateExpression]) extends StepExpression(null, predArg) {
  override lazy val compiledDPath = new DPathRecipe(UpMove)

  override def text = ".." // + "{" + stepElement.path + "}"

  override lazy val stepElement: DPathElementCompileInfo = {
    if (isFirstStep) {
      Assert.invariant(!isAbsolutePath)
      val rpe = this.relPathParent
      val sc = this.schemaComponent
      // if we are some component inside an element then we 
      // need to get the element surrounding first, then go up one.
      val e = sc.elementCompileInfo
      val e1 = e.getOrElse {
        SDE("No enclosing element.")
      }
      val e2 = e1.enclosingElementCompileInfo
      val e3 = e2.getOrElse {
        SDE("Relative path .. past root element.")
      }
      e3
    } else {
      // not first, so 
      val ps = priorStep
      val ps2 = ps.map { _.stepElement }
      val ps3 = ps2.getOrElse {
        SDE("Relative path .. past root element.")
      }
      val ps4 = ps3.enclosingElementCompileInfo
      val ps5 = ps4.getOrElse {
        SDE("Relative path .. past root element.")
      }
      ps5
    }
  }
}

case class NamedStep(s: String, predArg: Option[PredicateExpression])
  extends StepExpression(s, predArg) {

  requiredEvaluations(stepQName)

  override lazy val compiledDPath = {
    val d = downwardStep
    val conv = conversions
    val c = (if (isLastStep) conv else Nil)
    val res = new DPathRecipe(d +: c)
    res
  }

  lazy val erd = stepElement

  lazy val downwardStep = {
    if (stepElement.isArray && pred.isDefined) {
      Assert.invariant(pred.get.targetType == NodeInfo.ArrayIndex)
      val indexRecipe = pred.get.compiledDPath
      new DownArrayOccurrence(erd, indexRecipe)
    } else if (stepElement.isArray) {
      schemaDefinitionUnless(targetType == NodeInfo.Array, "Query-style paths not supported. Must have '[...]' after array-element's name. Offending path step: '%s'.", step)
      new DownArray(erd)
    } else {
      Assert.invariant(!pred.isDefined)
      new DownElement(erd)
    }
  }

  override def text = step // + "{" + stepElement.path + "}"

  private def die = {
    Assert.invariantFailed("should have thrown")
  }
  /*
   * The ERD of the element that corresponds to this path step
   * (or SDE trying to find it.)
   */
  override lazy val stepElement: DPathElementCompileInfo = {
    val stepElem = if (isFirstStep) {
      if (isAbsolutePath) {
        // has to be the root element, but we have to make sure the name matches.
        rootElement.elementCompileInfo.map { r =>
          if (!r.namedQName.matches(stepQName))
            r.noMatchError(stepQName)
        }.getOrElse(die)
        rootElement
      } else {
        // since we're first we start from the element, or nearest enclosing
        val e = schemaComponent.elementCompileInfo.map { _.findNamedChild(stepQName) }.getOrElse(die)
        e
      }
    } else {
      // not first step so we are extension of prior step
      val e = priorStep.map { _.stepElement }.map { _.findNamedChild(stepQName) }.getOrElse(die)
      e
    }
    stepElem
  }
}

/**
 * The thing in square brackets is indexing in DFDL, but XPath
 * calls it a "predicate".
 */
case class PredicateExpression(ifOr: Expression)
  extends Expression {

  override def text = "[" + ifOr.text + "]"

  override lazy val inherentType = NodeInfo.ArrayIndex
  override def targetTypeForSubexpression(subexp: Expression) = {
    Assert.invariant(ifOr == subexp)
    NodeInfo.ArrayIndex
  }

  override lazy val compiledDPath = ifOr.compiledDPath

  override lazy val children: List[Expression] = List(ifOr)
}

abstract class PrimaryExpression(expressions: List[Expression])
  extends ExpressionLists(expressions) {
}

abstract class LiteralExpressionBase(value: Any)
  extends PrimaryExpression(Nil) {

  override def text = {
    value match {
      case s: String => s
      case _ => value.toString
    }
  }

  override def toString = text

  /**
   * Convert to regular types from the pessimistic BigInt
   * and BigDecimal that come in from the parser.
   */
  lazy val litValue = value match {
    case s: String => s
    case i: BigInt => {
      if (i.isValidInt) i.toInt
      else if (i.isValidLong) i.toLong
      else i
    }
    case bd: BigDecimal => {
      if (bd.isValidLong) bd.toLongExact
      else if (bd.isValidDouble) bd.toDouble
      else bd
    }
    case f: Float => f.toDouble
    case d: Double => d
    case b: Boolean => b // there are no literal booleans, but fn:true() and fn:false() turns into one.
    case _ => Assert.invariantFailed("value not one of the expected types")
  }

  override lazy val compiledDPath: DPathRecipe = {
    new DPathRecipe(Literal(litValue) +: conversions)
  }

  override lazy val inherentType = {
    litValue match {
      case s: String => NodeInfo.String
      case i: BigInt => NodeInfo.Integer
      case d: BigDecimal => NodeInfo.Decimal
      case df: Double => NodeInfo.Double
      case l: Long => NodeInfo.Long
      case i: Int => NodeInfo.Int
      case b: Boolean => NodeInfo.Boolean
      case _ => Assert.invariantFailed("value not one of the expected types " + litValue.getClass())
    }
  }

  override def targetTypeForSubexpression(subexp: Expression) =
    Assert.usageError("literal expressions have no subexpressions")
}

case class LiteralExpression(v: Any) extends LiteralExpressionBase(v)

/**
 * This is a literal as used to replace the fn:true() and fn:false() functions.
 *
 * It has to behave like a function call with respect to target type.
 */
case class LiteralBooleanExpression(v: Any) extends LiteralExpressionBase(v) {
  override lazy val targetType = {
    val res = parent.targetType
    res
  }
}

case class VariableRef(val qnameString: String)
  extends PrimaryExpression(Nil) {

  override lazy val compiledDPath = {
    val vrefOp = VRef(theQName, vrd)
    new DPathRecipe(vrefOp +: conversions)
  }
  override def text = "$" + qnameString

  lazy val theQName: RefQName = resolveRef(qnameString)

  lazy val vrd = schemaComponent.variableMap.getVariableRuntimeData(theQName).getOrElse(
    SDE("Undefined variable: %s", text))

  lazy val varType = {
    val vt = vrd.primType
    val ni = NodeInfo.fromPrimType(vt)
    ni
  }

  override lazy val inherentType: NodeInfo.Kind = varType
  override def targetTypeForSubexpression(subexp: Expression) =
    Assert.usageError("variable reference expressions have no subexpressions")

  // lazy val evaluatedValue: Option[Expression] =
  //  {
  //    val (ns, localPart) = XMLUtils.QName(referringContext.namespaces, theQName, referringContext)
  //    val expandedVarName = XMLUtils.expandedQName(ns, localPart)
  //    val compiler = new DFDLPathExpressionCompiler(referringContext)
  //    val (value, newVMap) = variableMap.readVariable(expandedVarName, referringContext)
  //    variableMap = newVMap
  //    // FIXME: Why are we re-parsing the value of a variable?
  //    val (result, newerVMap) = compiler.getExpressionTree(value.toString, newVMap)
  //    variableMap = newerVMap
  //    result
  //  }
}

/*
 * Functions and operators that exist in tests as of 2014-08-05
 * 
 * fn:dateTime
 * xs:dateTime
 * xs:time
 * xs:date
 * xs:hexBinary
 */

case class FunctionCallExpression(functionQNameString: String, expressions: List[Expression])
  extends PrimaryExpression(expressions) {

  override def text = functionObject.text

  lazy val functionQName: RefQName = resolveRef(functionQNameString)

  override lazy val compiledDPath = functionObject.compiledDPath

  def inherentType: NodeInfo.Kind = functionObject.inherentType

  def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind = {
    // This is called by the arguments to the function to get their target types.
    // However, this is not the right parent. They are arguments to the function object
    // once the FunctionCallExpression creates that function object, so 
    // we delegate this back to the function object
    functionObject.targetTypeForSubexpression(childExpr)
  }

  lazy val functionObject: Expression = {
    val DFDL = XMLUtils.DFDL_NAMESPACE
    val FUNC = XMLUtils.XPATH_FUNCTION_NAMESPACE
    val XSD = XMLUtils.XSD_NAMESPACE
    val DAF = XMLUtils.DAFFODIL_EXTENSION_NAMESPACE
    val funcObj = (functionQName, expressions) match {

      case (RefQName(_, "trace", DAF), args) =>
        DAFTraceExpr(functionQNameString, functionQName, args)

      case (RefQName(_, "error", DAF), args) =>
        DAFErrorExpr(functionQNameString, functionQName, args)

      case (RefQName(_, "not", FUNC), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.Boolean, NodeInfo.Boolean, FNNot(_, _))

      case (RefQName(_, "ends-with", FUNC), args) =>
        FNTwoArgsExpr(functionQNameString, functionQName, args,
          NodeInfo.Boolean,
          NodeInfo.String, NodeInfo.String, FNEndsWith(_))

      // FIXME: Which of these two xs:string(...) implementations??
      case (RefQName(_, "string", XSD), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.String, NodeInfo.AnyAtomic, XSString(_, _))

      case (RefQName(_, "string", XSD), args) =>
        XSConverterExpr(functionQNameString, functionQName, args, NodeInfo.String)

      case (RefQName(_, "dateTime", FUNC), args) =>
        FNTwoArgsExpr(functionQNameString, functionQName, args,
          NodeInfo.DateTime, NodeInfo.Date, NodeInfo.Time, FNDateTime(_))

      case (RefQName(_, "dateTime", XSD), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.DateTime, NodeInfo.AnyAtomic, XSDateTime(_, _))

      case (RefQName(_, "date", XSD), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.Date, NodeInfo.AnyAtomic, XSDate(_, _))

      case (RefQName(_, "time", XSD), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.Time, NodeInfo.AnyAtomic, XSTime(_, _))

      case (RefQName(_, "hexBinary", XSD), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.HexBinary, NodeInfo.AnyAtomic, XSHexBinary(_, _))

      case (RefQName(_, "nilled", FUNC), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.Boolean, NodeInfo.Nillable, FNNilled(_, _))

      case (RefQName(_, "exists", FUNC), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.Boolean, NodeInfo.AnyType, FNExists(_, _))

      case (RefQName(_, "ceiling", FUNC), args) =>
        FNOneArgMathExpr(functionQNameString, functionQName, args,
          FNCeiling(_, _))

      case (RefQName(_, "floor", FUNC), args) =>
        FNOneArgMathExpr(functionQNameString, functionQName, args,
          FNFloor(_, _))

      case (RefQName(_, "round", FUNC), args) =>
        FNOneArgMathExpr(functionQNameString, functionQName, args,
          FNRound(_, _))

      case (RefQName(_, "abs", FUNC), args) =>
        FNOneArgMathExpr(functionQNameString, functionQName, args,
          FNAbs(_, _))

      case (RefQName(_, "concat", FUNC), args) =>
        FNArgListExpr(functionQNameString, functionQName, args,
          NodeInfo.String, NodeInfo.String, FNConcat(_))

      // Note: there is no string-join function in DFDL
      //      case (RefQName(_, "string-join", FUNC), args) =>
      //        FNTwoArgsExpr(functionQNameString, functionQName, args,
      //          NodeInfo.Array, NodeInfo.String, NodeInfo.String, FNStringJoin(_))

      case (RefQName(_, "substring", FUNC), args) if args.length == 2 =>
        FNTwoArgsExpr(functionQNameString, functionQName, args,
          NodeInfo.String, NodeInfo.String, NodeInfo.Double, FNSubstring2(_))
      case (RefQName(_, "substring", FUNC), args) if args.length == 3 =>
        FNThreeArgsExpr(functionQNameString, functionQName, args,
          NodeInfo.String, NodeInfo.String, NodeInfo.Double, NodeInfo.Double, FNSubstring3(_))

      case (RefQName(_, "true", FUNC), Nil) => LiteralBooleanExpression(true)
      case (RefQName(_, "false", FUNC), Nil) => LiteralBooleanExpression(false)
      case (RefQName(_, "count", FUNC), args) => {
        subset(args.length == 1, "The count function requires a single argument.")
        args(0) match {
          case pe: PathExpression if pe.isPathToOneWholeArray => FNCountExpr(functionQNameString, functionQName, pe)
          case pe: PathExpression => subsetError("The count function must reference a single array.")
          case _ => subsetError("The count function must contain a path.")
        }
      }

      case (RefQName(_, "year-from-dateTime", FUNC), args) => FNOneArgExprConversionDisallowed(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.DateTime, FNYearFromDateTime(_, _))
      case (RefQName(_, "month-from-dateTime", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.DateTime, FNMonthFromDateTime(_, _))
      case (RefQName(_, "day-from-dateTime", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.DateTime, FNDayFromDateTime(_, _))
      case (RefQName(_, "hours-from-dateTime", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.DateTime, FNHoursFromDateTime(_, _))
      case (RefQName(_, "minutes-from-dateTime", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.DateTime, FNMinutesFromDateTime(_, _))
      case (RefQName(_, "seconds-from-dateTime", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.DateTime, FNSecondsFromDateTime(_, _))
      case (RefQName(_, "year-from-date", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.Date, FNYearFromDate(_, _))
      case (RefQName(_, "month-from-date", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.Date, FNMonthFromDate(_, _))
      case (RefQName(_, "day-from-date", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.Date, FNDayFromDate(_, _))
      case (RefQName(_, "hours-from-time", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.Time, FNHoursFromTime(_, _))
      case (RefQName(_, "minutes-from-time", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.Time, FNMinutesFromTime(_, _))
      case (RefQName(_, "seconds-from-time", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.Time, FNSecondsFromTime(_, _))

      case (RefQName(_, "occursIndex", DFDL), args) => {
        DFDLOccursIndexExpr(functionQNameString, functionQName, args)
      }
      case (RefQName(_, "checkConstraints", DFDL), args) => {
        DFDLCheckConstraintsExpr(functionQNameString, functionQName, args)
      }
      case (RefQName(_, "decodeDFDLEntities", DFDL), args) => {
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.String, NodeInfo.String, DFDLDecodeDFDLEntities(_, _))
      }
      case (RefQName(_, "encodeDFDLEntities", DFDL), args) => {
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.String, NodeInfo.String, DFDLEncodeDFDLEntities(_, _))
      }
      case (RefQName(_, "containsDFDLEntities", DFDL), args) => {
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.Boolean, NodeInfo.String, DFDLContainsDFDLEntities(_, _))
      }

      case (RefQName(_, "testBit", DFDL), args) => {
        DFDLTestBitExpr(functionQNameString, functionQName, args)
      }
      case (RefQName(_, "setBits", DFDL), args) => {
        DFDLSetBitsExpr(functionQNameString, functionQName, args)
      }

      case (RefQName(_, "round-half-to-even", FUNC), args) =>
        FNRoundHalfToEvenExpr(functionQNameString, functionQName, args)

      case (RefQName(_, "string-length", FUNC), args) => {
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.ArrayIndex, NodeInfo.String, FNStringLength(_, _))
      }

      case (RefQName(_, "contentLength", DFDL), _) =>
        SDE("dfdl:contentLength is not valid during parsing.")

      case (RefQName(_, "valueLength", DFDL), _) =>
        SDE("dfdl:valueLength is not valid during parsing.")

      case (RefQName(_, "lower-case", FUNC), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.String, NodeInfo.String, FNLowerCase(_, _))

      case (RefQName(_, "upper-case", FUNC), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.String, NodeInfo.String, FNUpperCase(_, _))

      // conversion functions
      case (RefQName(_, "integer", XSD), args) =>
        XSConverterExpr(functionQNameString, functionQName, args, NodeInfo.Integer)

      case (RefQName(_, "decimal", XSD), args) =>
        XSConverterExpr(functionQNameString, functionQName, args, NodeInfo.Decimal)

      case (RefQName(_, "boolean", XSD), args) =>
        XSConverterExpr(functionQNameString, functionQName, args, NodeInfo.Boolean)

      case (RefQName(_, "float", XSD), args) =>
        XSConverterExpr(functionQNameString, functionQName, args, NodeInfo.Float)

      case (RefQName(_, "double", XSD), args) =>
        XSConverterExpr(functionQNameString, functionQName, args, NodeInfo.Float)

      case (RefQName(_, "long", XSD), args) =>
        XSConverterExpr(functionQNameString, functionQName, args, NodeInfo.Long)

      case (RefQName(_, "unsignedLong", XSD), args) =>
        XSConverterExpr(functionQNameString, functionQName, args, NodeInfo.UnsignedLong)

      case (RefQName(_, "int", XSD), args) =>
        XSConverterExpr(functionQNameString, functionQName, args, NodeInfo.Int)

      case (RefQName(_, "unsignedInt", XSD), args) =>
        XSConverterExpr(functionQNameString, functionQName, args, NodeInfo.UnsignedInt)

      case (RefQName(_, "short", XSD), args) =>
        XSConverterExpr(functionQNameString, functionQName, args, NodeInfo.Short)

      case (RefQName(_, "unsignedShort", XSD), args) =>
        XSConverterExpr(functionQNameString, functionQName, args, NodeInfo.UnsignedShort)

      case (RefQName(_, "nonNegativeInteger", XSD), args) =>
        XSConverterExpr(functionQNameString, functionQName, args, NodeInfo.NonNegativeInteger)

      case (RefQName(_, "byte", XSD), args) =>
        XSConverterExpr(functionQNameString, functionQName, args, NodeInfo.Byte)

      case (RefQName(_, "unsignedByte", XSD), args) =>
        XSConverterExpr(functionQNameString, functionQName, args, NodeInfo.UnsignedByte)

      case (RefQName(_, "hexBinary", XSD), args) =>
        XSConverterExpr(functionQNameString, functionQName, args, NodeInfo.HexBinary)

      case _ => SDE("Unsupported function: %s", functionQName)
    }
    funcObj.setOOLAGContext(this)
    funcObj
  }
}
// xs constructors
/*
xs:string($arg as xs:anyAtomicType) as xs:string
xs:boolean($arg as xs:anyAtomicType) as xs:boolean
xs:decimal($arg as xs:anyAtomicType) as xs:decimal
xs:float($arg as xs:anyAtomicType) as xs:float
xs:double($arg as xs:anyAtomicType) as xs:double
xs:dateTime($arg as xs:anyAtomicType) as xs:dateTime
xs:time($arg as xs:anyAtomicType) as xs:time
xs:date($arg as xs:anyAtomicType) as xs:date
xs:hexBinary($arg as xs:anyAtomicType) as xs:hexBinary
xs:integer($arg as xs:anyAtomicType) as xs:integer
xs:long($arg as xs:anyAtomicType) as xs:long
xs:short($arg as xs:anyAtomicType) as xs:short
xs:byte($arg as xs:anyAtomicType) as xs:byte
xs:nonNegativeInteger($arg as xs:anyAtomicType) as xs:nonNegativeInteger
xs:unsignedLong($arg as xs:anyAtomicType) as xs:unsignedLong
xs:unsignedInt($arg as xs:anyAtomicType) as xs:unsignedInt
xs:unsignedShort($arg as xs:anyAtomicType) as xs:unsignedShort
xs:unsignedByte($arg as xs:anyAtomicType) as xs:unsignedByte
*/
/* special fn constructor for dateTime
      fn:dateTime($arg1 as xs:date, $arg2 as xs:time) as xs:dateTime
      */
// boolean functions
/* fn:not(arg) */
// numeric functions
/*
fn:abs($arg as numeric)	Returns the absolute value of the argument. 
fn:ceiling($arg as numeric)	Returns the smallest number with no fractional part that is greater than or equal to the argument.
fn:floor($arg as numeric)	Returns the largest number with no fractional part that is less than or equal to the argument.
fn:round($arg as numeric)	Rounds to the nearest number with no fractional part. When the value is x.5, it rounds toward positive infinity.

*/
// string functions
/*
fn:concat( $arg1 as xs:anyAtomicType, $arg2 as xs:anyAtomicType, ... ) 	Concatenates two or more xs:anyAtomicType arguments cast to xs:string.
fn:substring($sourceString as xs:string, $startingLoc as xs:double) 
fn:substring($sourceString as xs:string, $startingLoc as xs:double, $length as xs:double)	Returns the xs:string located at a specified place within an argument xs:string.
fn:upper-case($arg as xs:string)  	Returns the upper-cased value of the argument.
fn:lower-case($arg as xs:string) 	Returns the lower-cased value of the argument.
fn:contains($arg1 as xs:string, $arg2 as xs:string)	Returns xs:boolean indicating whether one xs:string contains another xs:string.
fn:starts-with($arg1 as xs:string, $arg2 as xs:string)	Returns xs:boolean indicating whether the value of one xs:string begins with the characters of another xs:string. 
fn:ends-with($arg1 as xs:string, $arg2 as xs:string)	Returns xs:boolean indicating whether the value of one xs:string ends with the characters of another xs:string. 
fn:substring-before($arg1 as xs:string, $arg2 as xs:string)	Returns the characters of one xs:string that precede in that xs:string the characters of another xs:string.
fn:substring-after($arg1 as xs:string, $arg2 as xs:string)	Returns the characters of xs:string that follow in that xs:string the characters of another xs:string. 
*/
// date and time functions
/*
fn:year-from-dateTime($arg as xs:dateTime)	Returns the year from an xs:dateTime value as an xs:integer.
fn:month-from-dateTime($arg as xs:dateTime)	Returns the month from an xs:dateTime value as an xs:integer.
fn:day-from-dateTime($arg as xs:dateTime)	Returns the day from an xs:dateTime value as an xs:integer.
fn:hours-from-dateTime($arg as xs:dateTime)	Returns the hours from an xs:dateTime value as an xs:integer.
fn:minutes-from-dateTime($arg as xs:dateTime)	Returns the minutes from an xs:dateTime value as an xs:integer.
fn:seconds-from-dateTime($arg as xs:dateTime)	Returns the seconds from an xs:dateTime value as an xs:decimal.
fn:year-from-date($arg as xs:date)	Returns the year from an xs:date value as an xs:integer.
fn:month-from-date($arg as xs:date)	Returns the month from an xs:date value as an xs:integer.
fn:day-from-date($arg as xs:date)	Returns the day from an xs:date value as an xs:integer.
fn:hours-from-time($arg as xs:time) Returns the hours from an xs:time value as an xs:integer.
fn:minutes-from-time($arg as xs:time) Returns the minutes from an xs:time value as an xs:integer.
fn:seconds-from-time($arg as xs:time) Returns the seconds from an xs:time value as an xs:decimal.
*/

/*
 * 23.5.2.5	Node Sequence Test Functions
The following functions are defined on sequences. (Note that DFDL v1.0 does not support sequences of length > 1.)
Function	Meaning
fn:empty($arg?)	Indicates whether or not the provided sequence is empty.
fn:exists($arg?)	Indicates whether or not the provided sequence is not empty.
fn:exactly-one($arg?)	True if the provided sequence contains exactly one node/value.

Table 65 Node Sequence Test Functions
23.5.2.6	Node functions
This section discusses functions and operators on nodes.
Function	Meaning
fn:local-name()
fn:local-name($arg)	Returns the local name of the context node or the specified node as an xs:string.
fn:namespace-uri()
fn:namespace-uri($arg)	Returns the namespace URI as an xs:string for the argument node or the context node if the argument is omitted. Returns empty string if the argument/context node is in no namespace.
Table 66 Node functions
23.5.2.7	Nillable Element Functions
This section discusses functions related to nillable elements.
Function	Meaning
fn:nilled($arg?)	Returns an xs:boolean true when the argument node Infoset member [nilled] is true and false when [nilled] is false. If the argument is not an element node, returns the empty sequence. If the argument is the empty sequence, returns the empty sequence. If the argument is an element node and [nilled] has no value returns the empty sequence.
Table 67 Nillable Element Functions
*/
// DFDL functions
/*
dfdl:testBit($data, $bitPos) 	Returns Boolean true if the bit number given by the xs:nonNegativeInteger $bitPos is set on in the xs:unsignedByte given by $data, otherwise returns Boolean false.
dfdl:setBits($bit1, $bit2, ... $bit8)	Returns an unsigned byte being the value of the bit positions provided by the Boolean arguments, where true is1, false is 0. The number of arguments must be 8.

dfdl:encodeDFDLEntities($arg) 	Returns a string containing a DFDL string literal constructed from the $arg string argument. If $arg contains any '%' and/or space characters, then the return value replaces each '%' with '%%' and each space with '%SP;', otherwise $arg is returned unchanged. 
dfdl:decodeDFDLEntities ($arg)	Returns a string constructed from the $arg string argument. If $arg contains syntax matching DFDL Character Entities syntax, then the corresponding characters are used in the result.  Any characters in $arg not matching the DFDL Character Entities syntax remain unchanged in the result.
dfdl:containsDFDLEntities($arg) 	Returns a Boolean indicating whether the $arg string argument contains one or more DFDL entities. 
dfdl:timeZoneFromDateTime($arg) 
dfdl:timeZoneFromDate($arg)
dfdl:timeZoneFromTime ($arg)	Returns the timezone component, if any, of $arg as an xs:string. The $arg is of type xs:dateTime, xs:date and xs:time respectively.
If $arg has a timezone component, then the result is a string in the format of an ISO Time zone designator. Interpreted as an offset from UTC, its value may range from +14:00 to -14:00 hours, both inclusive. The UTC time zone is represented as "+00:00". If the $arg has no timezone component, then "" (empty string) is returned.
 * 
 */
// dfdl constructors
/*
dfdl:byte ($arg) 
dfdl:unsignedByte ($arg) 
dfdl:short ($arg) 
dfdl:unsignedShort ($arg) 
dfdl:int ($arg) 
dfdl:unsignedInt ($arg) 
dfdl:long ($arg) 
dfdl:unsignedLong ($arg) 
dfdl:hexBinary ($arg) 
 * 
 */

abstract class FunctionCallBase(functionQNameString: String,
  functionQName: RefQName,
  expressions: List[Expression]) extends ExpressionLists(expressions) {
  override def text = functionQNameString + "(" + expressions.map { _.text }.mkString(", ") + ")"

  override lazy val targetType = {
    val res = parent.targetType
    res
  }
}
/**
 * Tells the sub-expression that we want an array out of it.
 */
case class FNCountExpr(nameAsParsed: String, fnQName: RefQName, arrPath: PathExpression)
  extends FunctionCallBase(nameAsParsed, fnQName, List(arrPath)) {

  override lazy val isTypeCorrect = {
    arrPath.isPathToOneWholeArray
  }

  override lazy val inherentType: NodeInfo.Kind = NodeInfo.Long
  override def targetTypeForSubexpression(subExp: Expression): NodeInfo.Kind = NodeInfo.Array

  override lazy val compiledDPath = {
    new DPathRecipe((arrPath.compiledDPath.ops.toList :+ FNCount) ++ conversions)
  }
}

case class FNRoundHalfToEvenExpr(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression])
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  val (valueExpr, precisionExpr) = args match {
    case List(ve) => (ve, LiteralExpression(0))
    case List(ve, pe) => (ve, pe)
    case _ => Assert.usageError("must have one or two args in the argument list.")
  }

  override lazy val children = List(valueExpr, precisionExpr)

  override lazy val inherentType = valueExpr.inherentType

  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind = {
    if (childExpr == valueExpr) valueExpr.inherentType
    else if (childExpr == precisionExpr) NodeInfo.Int
    else Assert.invariantFailed("subexpression isn't one of the expected.")
  }

  override lazy val compiledDPath =
    new DPathRecipe(
      FNRoundHalfToEven(
        valueExpr.compiledDPath,
        precisionExpr.compiledDPath) +: conversions)
}

/**
 * Preserves the inherent type of the argument to the function as the type of
 * the result, that is if the argument type is a numeric type, and if
 * the argument type is a subtype thereof.
 */
case class FNOneArgMathExpr(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression], constructor: (DPathRecipe, NodeInfo.Kind) => RecipeOp)
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val inherentType = {
    schemaDefinitionUnless(argInherentType.isSubtypeOf(NodeInfo.Numeric),
      "Argument must be of numeric type but was %s.", argInherentType)
    argInherentType
  }

  lazy val argInherentType = {
    schemaDefinitionUnless(args.length == 1, "Function %s takes 1 argument.",
      fnQName.toPrettyString)
    args(0).inherentType
  }

  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind = inherentType

  override lazy val compiledDPath = {
    val arg0Recipe = args(0).compiledDPath
    val arg0Type = args(0).inherentType
    val c = conversions
    val res = new DPathRecipe(constructor(arg0Recipe, arg0Type) +: c)
    res
  }
}

case class FNOneArgExpr(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression], resultType: NodeInfo.Kind, argType: NodeInfo.Kind, constructor: (DPathRecipe, NodeInfo.Kind) => RecipeOp)
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val inherentType = resultType

  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind = argType

  override lazy val compiledDPath = {
    schemaDefinitionUnless(args.length == 1, "Function %s takes 1 argument.",
      fnQName.toPrettyString)
    val arg0Recipe = args(0).compiledDPath
    val arg0Type = args(0).inherentType
    val c = conversions
    val res = new DPathRecipe(constructor(arg0Recipe, arg0Type) +: c)
    res
  }
}

case class FNOneArgExprConversionDisallowed(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression], resultType: NodeInfo.Kind, argType: NodeInfo.Kind, constructor: (DPathRecipe, NodeInfo.Kind) => RecipeOp)
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  schemaDefinitionUnless(args.length == 1, "Function %s takes 1 argument.",
    fnQName.toPrettyString)

  override lazy val inherentType = resultType

  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind = argType

  override lazy val compiledDPath = {
    val arg0Recipe = args(0).compiledDPath
    val arg0Type = args(0).inherentType
    val c = Nil //conversions
    val res = new DPathRecipe(constructor(arg0Recipe, arg0Type) +: c)
    res
  }
}

case class FNTwoArgsExpr(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression], resultType: NodeInfo.Kind, arg1Type: NodeInfo.Kind, arg2Type: NodeInfo.Kind,
  constructor: List[DPathRecipe] => RecipeOp)
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  lazy val checkArgs = {
    schemaDefinitionUnless(args.length == 2, "Function %s takes 2 arguments.", fnQName.toPrettyString)
  }

  override lazy val inherentType = resultType

  lazy val List(arg1, arg2) = { checkArgs; args }

  override def targetTypeForSubexpression(subexp: Expression): NodeInfo.Kind = {
    if (subexp == arg1)
      arg1Type
    else if (subexp == arg2)
      arg2Type
    else
      Assert.invariantFailed("subexpression %s is not an argument.".format(subexp))
  }

  override lazy val compiledDPath = {
    val arg1Recipe = arg1.compiledDPath
    val arg2Recipe = arg2.compiledDPath
    val res = new DPathRecipe(constructor(List(arg1Recipe, arg2Recipe)) +: conversions)
    res
  }
}

case class FNThreeArgsExpr(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression], resultType: NodeInfo.Kind,
  arg1Type: NodeInfo.Kind, arg2Type: NodeInfo.Kind, arg3Type: NodeInfo.Kind,
  constructor: List[DPathRecipe] => RecipeOp)
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  lazy val checkArgs = schemaDefinitionUnless(args.length == 3, "Function %s takes 3 arguments.",
    fnQName.toPrettyString)

  override lazy val inherentType = resultType

  lazy val List(arg1, arg2, arg3) = { checkArgs; args }

  override def targetTypeForSubexpression(subexp: Expression): NodeInfo.Kind = {
    if (subexp == arg1) arg1Type
    else if (subexp == arg2) arg2Type
    else {
      Assert.invariant(subexp == arg3)
      arg3Type
    }
  }

  override lazy val compiledDPath = {
    val arg1Path = arg1.compiledDPath
    val arg2Path = arg2.compiledDPath
    val arg3Path = arg3.compiledDPath
    val c = conversions
    val res = new DPathRecipe(constructor(
      List(arg1Path, arg2Path, arg3Path)) +: c)
    res
  }
}

case class XSConverterExpr(nameAsParsed: String, fnQName: RefQName, args: List[Expression],
  resultType: NodeInfo.Kind)
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  lazy val checkArgs = schemaDefinitionUnless(args.length == 1, "Function %s takes 1 argument.", fnQName.toPrettyString)

  override lazy val inherentType = resultType

  /*
   * By using the result type as the target for the expression, conversions will
   * do the work of converting into that type. We don't need to put down any 
   * additional recipe operator to convert anything.
   */
  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind = resultType // NodeInfo.AnyType

  // override lazy val compiledDPath = new DPathRecipe(XSConverter(args(0).compiledDPath, args(0).inherentType, resultType) +: conversions)

  // TODO: this should work... why do we need to call an additional converter. The
  // args(0).compiledDPath should already have taken into account converting into 
  // their target types which are the same as this conversion's output result type.

  override lazy val compiledDPath = {
    checkArgs
    val arg0Recipe = args(0).compiledDPath
    val c = conversions
    val res = new DPathRecipe(arg0Recipe.ops.toList ++ c)
    res
  }
}

case class FNArgListExpr(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression], resultType: NodeInfo.Kind, argType: NodeInfo.Kind,
  constructor: List[DPathRecipe] => RecipeOp)
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val inherentType = resultType

  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind = argType

  override lazy val compiledDPath = {
    schemaDefinitionUnless(args.length > 1, "Function %s called with no arguments.", fnQName.toPrettyString)
    new DPathRecipe(constructor(args.map { _.compiledDPath }) +: conversions)
  }
}

case class DFDLOccursIndexExpr(nameAsParsed: String, fnQName: RefQName, args: List[Expression])
  extends FunctionCallBase(nameAsParsed, fnQName, args) {
  // Note that even though there are no arguments, this cannot be a case object
  // since it gets wired into expressions, and so the parent pointer must
  // be set. So it's not a singleton.

  override lazy val children = Nil
  override def text = "dfdl:occursIndex"

  override lazy val compiledDPath = {
    schemaDefinitionUnless(args.length == 0, "Function %s takes no arguments.", fnQName.toPrettyString)
    new DPathRecipe(DFDLOccursIndex +: conversions)
  }

  override lazy val inherentType = NodeInfo.ArrayIndex
  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind =
    Assert.usageError("No subexpressions")
}

case class DFDLTestBitExpr(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression]) extends FunctionCallBase(nameAsParsed, fnQName, args) {

  lazy val checkArgs = schemaDefinitionUnless(args.length == 2, "Function %s takes 2 arguments.",
    fnQName.toPrettyString)

  val List(data, bitPos) = { checkArgs; args }

  override lazy val inherentType = NodeInfo.Boolean

  override def targetTypeForSubexpression(subexpr: Expression): NodeInfo.Kind = {
    subexpr match {
      case `data` => NodeInfo.UnsignedByte
      case `bitPos` => NodeInfo.UnsignedByte
      case _ => Assert.invariantFailed("wasn't one of the subexpressions.")
    }
  }

  override lazy val compiledDPath =
    new DPathRecipe(DFDLTestBit(data.compiledDPath, bitPos.compiledDPath) +: conversions)

}

case class DFDLSetBitsExpr(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression]) extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val inherentType = NodeInfo.UnsignedByte
  override def targetTypeForSubexpression(subexpr: Expression): NodeInfo.Kind = NodeInfo.UnsignedByte

  override lazy val compiledDPath = {
    schemaDefinitionUnless(args.length == 8, "Function %s takes 8 arguments.", fnQName.toPrettyString)
    new DPathRecipe(DFDLSetBits(args.map { _.compiledDPath }) +: conversions)
  }
}

case class DFDLCheckConstraintsExpr(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression]) extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val children = args

  override lazy val compiledDPath = {
    schemaDefinitionUnless(args.length == 1, "Function %s takes 1 argument.", fnQName.toPrettyString)
    val argDPath = args(0).compiledDPath
    val c = conversions
    val res = new DPathRecipe(DFDLCheckConstraints(argDPath) +: c)
    res
  }
  override def targetTypeForSubexpression(subexpr: Expression): NodeInfo.Kind = NodeInfo.AnyAtomic

  override lazy val inherentType = NodeInfo.Boolean

}

/**
 * Really this just delegates anything bottom-up to the contained expression
 * and anything top-down to the parent
 */
case class ParenthesizedExpression(expression: Expression)
  extends PrimaryExpression(List(expression)) {

  override def inherentType: NodeInfo.Kind = expression.inherentType
  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind = targetType
  override def text: String = "( " + expression.text + " )"
  override lazy val compiledDPath = expression.compiledDPath // no conversions because we passed on targetType to subexp
}

case class DAFTraceExpr(nameAsParsed: String, fnQName: RefQName, args: List[Expression])
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  requiredEvaluations(realArg)
  requiredEvaluations(msgText)

  lazy val (realArg, msgText) = args match {
    case List(arg0, LiteralExpression(txt: String)) => (arg0, txt)
    case List(arg0, other) =>
      SDE("The second argument to %n must be a string literal, but was %s.",
        nameAsParsed, other.text)
    case _ => {
      SDE("The %n function requires 2 arguments.", nameAsParsed)
    }
  }

  override lazy val inherentType: NodeInfo.Kind = realArg.inherentType
  override def targetTypeForSubexpression(subExp: Expression): NodeInfo.Kind = targetType

  override lazy val compiledDPath = {
    new DPathRecipe(DAFTrace(realArg.compiledDPath, msgText) +: conversions)
  }
}

case class DAFErrorExpr(nameAsParsed: String, fnQName: RefQName, args: List[Expression])
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val inherentType: NodeInfo.Kind = NodeInfo.Nothing
  override def targetTypeForSubexpression(subExp: Expression): NodeInfo.Kind =
    Assert.invariantFailed("no subexpressions")

  override lazy val compiledDPath = {
    Assert.invariant(conversions == Nil)
    new DPathRecipe(DAFError)
  }
}
