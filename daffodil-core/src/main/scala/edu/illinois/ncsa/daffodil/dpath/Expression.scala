/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.dsom._
import scala.xml.NamespaceBinding
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.xml.RefQName
import scala.util.{ Success, Failure }
import edu.illinois.ncsa.daffodil.dsom.RelativePathPastRootError
import edu.illinois.ncsa.daffodil.equality._
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInt }
import edu.illinois.ncsa.daffodil.util.Numbers

/**
 * Root class of the type hierarchy for the AST nodes used when we
 * compile a DPath Expression.
 *
 * All the work in 'compiling' the DPath expressions happens on methods
 * of these objects.
 *
 * This is the OOLAG pattern again.
 */
abstract class Expression extends OOLAGHostImpl()
  with ImplementsThrowsOrSavesSDE {

  /**
   * Use several calls instead of one, because then OOLAG will try them
   * all separately, and perhaps if you're lucky report more errors.
   */
  requiredEvaluations(parent)
  requiredEvaluations(targetType)
  requiredEvaluations(inherentType)
  requiredEvaluations(isTypeCorrect)
  requiredEvaluations(compiledDPath_)

  def tunable = compileInfo.tunable

  /**
   * Override where we traverse/access elements.
   */
  def leafContentLengthReferencedElements = ReferencedElementInfos.None
  def leafValueLengthReferencedElements = ReferencedElementInfos.None

  def contentReferencedElementInfos: Set[DPathElementCompileInfo] = {
    val clds = children
    val cldsreis: Set[DPathElementCompileInfo] =
      clds.foldLeft(ReferencedElementInfos.None) {
        (s, item) =>
          {
            val ireis = item.contentReferencedElementInfos
            s.union(ireis)
          }
      }
    val res = cldsreis ++
      leafContentLengthReferencedElements
    res
  }

  def valueReferencedElementInfos: Set[DPathElementCompileInfo] = {
    val clds = children
    val cldsreis: Set[DPathElementCompileInfo] =
      clds.foldLeft(ReferencedElementInfos.None) {
        (s, item) =>
          {
            val ireis = item.valueReferencedElementInfos
            s.union(ireis)
          }
      }
    val res = cldsreis ++
      leafValueLengthReferencedElements
    res
  }

  // This split allows overrides of this lazy val to still reuse this
  // (super.isTypeCorrect doesn't work inside an overridden lazy val.)
  lazy val isTypeCorrect: Boolean = checkTypeCorrectness
  //
  // override for other checking beyond what is needed to do conversions
  //
  protected def checkTypeCorrectness = true

  def text: String

  def hasReferenceTo(elem: DPathElementCompileInfo): Boolean = {
    children.exists(_.hasReferenceTo(elem))
  }

  /**
   * TODO: get more precise line and column information for
   * pointing at sub-regions of large DPath expressions
   *
   * We're parsing them, so we should have access to specific locations
   * within the expression.
   */
  lazy val schemaFileLocation = compileInfo.schemaFileLocation

  lazy val conversions = {
    val inh = inherentType
    val tt = targetType
    val res = Conversion.conversionOps(inh, tt, this)
    res
  }

  private def compiledDPath_ = LV('compiledDPath) { compiledDPath }
  def compiledDPath: CompiledDPath

  final lazy val parentOpt = Option(parent)

  final lazy val parent: Expression = {
    if (!this.hasOOLAGRootSetup)
      Assert.invariantFailed("not setup")
    this match {
      case w: WholeExpression => null
      case _ => this.oolagContext.asInstanceOf[Expression]
    }
  }

  final lazy val wholeExpression: Expression = {
    parent match {
      case w: WholeExpression => w
      case null => this
      case _ => parent.wholeExpression
    }
  }

  final lazy val wholeExpressionText = wholeExpression.text

  lazy val compileInfo: DPathCompileInfo = // override in WholeExpression
    parent.compileInfo

  final lazy val enclosingElementCompileInfo: Option[DPathElementCompileInfo] =
    compileInfo.enclosingElementCompileInfo

  final lazy val rootElement: DPathElementCompileInfo = {
    enclosingElementCompileInfo.map {
      _.rootElement
    }.getOrElse {
      compileInfo.elementCompileInfo.getOrElse {
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
   * For dfdl:length and dfdl:occursCount it is UnsignedInt
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

  def resolveRef(qnameString: String) = {
    QName.resolveRef(qnameString, namespaces, tunable).recover {
      case _: Throwable =>
        SDE("The prefix of '%s' has no corresponding namespace definition.", qnameString)
    }.get
  }

}

abstract class ExpressionLists(val lst: List[Expression])
  extends Expression {
  override lazy val children = lst
}

trait BinaryExpMixin { self: ExpressionLists =>
  Assert.invariant(children.length == 2)
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
    val res = new CompiledDPath(BooleanOp(op, leftDPath, rightDPath) +: c)
    res
  }

  override def targetTypeForSubexpression(subexp: Expression): NodeInfo.Kind = NodeInfo.Boolean
  override lazy val inherentType: NodeInfo.Kind = NodeInfo.Boolean

}

case class ComparisonExpression(op: String, adds: List[Expression])
  extends ExpressionLists(adds) with BooleanExpression {

  lazy val compareOp: CompareOpBase = {
    import NodeInfo.PrimType._
    import NodeInfo.ArrayIndex
    (op, convergedArgType) match {
      case ("<", _) => subsetError("Unsupported operation '%s'. Use 'lt' instead.", op)
      case (">", _) => subsetError("Unsupported operation '%s'. Use 'gt' instead.", op)
      case ("<=", _) => subsetError("Unsupported operation '%s'. Use 'le' instead.", op)
      case (">=", _) => subsetError("Unsupported operation '%s'. Use 'ge' instead.", op)
      case ("=", _) => subsetError("Unsupported operation '%s'. Use 'eq' instead.", op)
      case ("!=", _) => subsetError("Unsupported operation '%s'. Use 'ne' instead.", op)

      case ("eq", HexBinary) => EQ_CompareByteArray
      case ("ne", HexBinary) => NE_CompareByteArray
      case ("eq", _) => EQ_Compare
      case ("ne", _) => NE_Compare

      case ("lt", Boolean) => LT_Boolean
      case ("gt", Boolean) => GT_Boolean
      case ("le", Boolean) => LE_Boolean
      case ("ge", Boolean) => GE_Boolean

      case ("lt", Date) => LT_Date
      case ("gt", Date) => GT_Date
      case ("le", Date) => LE_Date
      case ("ge", Date) => GE_Date

      case ("lt", Time) => LT_Time
      case ("gt", Time) => GT_Time
      case ("le", Time) => LE_Time
      case ("ge", Time) => GE_Time

      case ("lt", DateTime) => LT_DateTime
      case ("gt", DateTime) => GT_DateTime
      case ("le", DateTime) => LE_DateTime
      case ("ge", DateTime) => GE_DateTime

      case ("lt", String) => LT_String
      case ("gt", String) => GT_String
      case ("le", String) => LE_String
      case ("ge", String) => GE_String

      case ("lt", Decimal) => LT_Decimal
      case ("gt", Decimal) => GT_Decimal
      case ("le", Decimal) => LE_Decimal
      case ("ge", Decimal) => GE_Decimal

      case ("lt", Integer) => LT_Integer
      case ("gt", Integer) => GT_Integer
      case ("le", Integer) => LE_Integer
      case ("ge", Integer) => GE_Integer

      case ("lt", NonNegativeInteger) => LT_NonNegativeInteger
      case ("gt", NonNegativeInteger) => GT_NonNegativeInteger
      case ("le", NonNegativeInteger) => LE_NonNegativeInteger
      case ("ge", NonNegativeInteger) => GE_NonNegativeInteger

      case ("lt", UnsignedLong) => LT_UnsignedLong
      case ("gt", UnsignedLong) => GT_UnsignedLong
      case ("le", UnsignedLong) => LE_UnsignedLong
      case ("ge", UnsignedLong) => GE_UnsignedLong

      case ("lt", Long) => LT_Long
      case ("gt", Long) => GT_Long
      case ("le", Long) => LE_Long
      case ("ge", Long) => GE_Long

      case ("lt", UnsignedInt) => LT_UnsignedInt
      case ("gt", UnsignedInt) => GT_UnsignedInt
      case ("le", UnsignedInt) => LE_UnsignedInt
      case ("ge", UnsignedInt) => GE_UnsignedInt

      case ("lt", ArrayIndex) => LT_UnsignedInt
      case ("gt", ArrayIndex) => GT_UnsignedInt
      case ("le", ArrayIndex) => LE_UnsignedInt
      case ("ge", ArrayIndex) => GE_UnsignedInt

      case ("lt", Int) => LT_Int
      case ("gt", Int) => GT_Int
      case ("le", Int) => LE_Int
      case ("ge", Int) => GE_Int

      case ("lt", UnsignedShort) => LT_UnsignedShort
      case ("gt", UnsignedShort) => GT_UnsignedShort
      case ("le", UnsignedShort) => LE_UnsignedShort
      case ("ge", UnsignedShort) => GE_UnsignedShort

      case ("lt", Short) => LT_Short
      case ("gt", Short) => GT_Short
      case ("le", Short) => LE_Short
      case ("ge", Short) => GE_Short

      case ("lt", UnsignedByte) => LT_UnsignedByte
      case ("gt", UnsignedByte) => GT_UnsignedByte
      case ("le", UnsignedByte) => LE_UnsignedByte
      case ("ge", UnsignedByte) => GE_UnsignedByte

      case ("lt", Byte) => LT_Byte
      case ("gt", Byte) => GT_Byte
      case ("le", Byte) => LE_Byte
      case ("ge", Byte) => GE_Byte

      case ("lt", Float) => LT_Float
      case ("gt", Float) => GT_Float
      case ("le", Float) => LE_Float
      case ("ge", Float) => GE_Float

      case ("lt", Double) => LT_Double
      case ("gt", Double) => GT_Double
      case ("le", Double) => LE_Double
      case ("ge", Double) => GE_Double

      case _ => subsetError("Unsupported operation '%s' on type %s.", op, convergedArgType)
    }
  }

  override lazy val compiledDPath = {
    val leftDPath = left.compiledDPath
    val rightDPath = right.compiledDPath
    val c = conversions
    val res = new CompiledDPath(CompareOperator(compareOp, leftDPath, rightDPath) +: c)
    res
  }

  override def targetTypeForSubexpression(child: Expression): NodeInfo.Kind = convergedArgType

  lazy val convergedArgType = (left.inherentType, right.inherentType) match {
    // String => Numeric conversions are not allowed for comparison Ops.
    //
    // See http://www.w3.org/TR/xpath20/#mapping
    //
    case (left: NodeInfo.String.Kind, right: NodeInfo.String.Kind) =>
      NodeInfo.String
    case (left: NodeInfo.Numeric.Kind, right: NodeInfo.Numeric.Kind) =>
      NodeInfoUtils.generalizeArgTypesForComparisonOp(op, left, right)
    case (left: NodeInfo.Date.Kind, right: NodeInfo.Date.Kind) =>
      NodeInfo.Date
    case (left: NodeInfo.Time.Kind, right: NodeInfo.Time.Kind) =>
      NodeInfo.Time
    case (left: NodeInfo.DateTime.Kind, right: NodeInfo.DateTime.Kind) =>
      NodeInfo.DateTime
    case (left: NodeInfo.Boolean.Kind, right: NodeInfo.Boolean.Kind) =>
      NodeInfo.Boolean
    case (left: NodeInfo.HexBinary.Kind, right: NodeInfo.HexBinary.Kind) =>
      NodeInfo.HexBinary
    case (l, r) =>
      SDE("Cannot compare %s with %s for operator '%s'", l, r, op)
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
    new CompiledDPath(NumericOperator(numericOp, leftDPath, rightDPath) +: c)
  }

  /**
   * The DPath operator, such as, "+", or "idiv"
   */
  def op: String

  override def inherentType: NodeInfo.Kind = convergedResult

  override def targetTypeForSubexpression(child: Expression): NodeInfo.Kind = convergedArgType

  lazy val (convergedArgType, convergedResult) = (left.inherentType, right.inherentType) match {
    case (left: NodeInfo.Numeric.Kind, right: NodeInfo.Numeric.Kind) =>
      NodeInfoUtils.generalizeArgAndResultTypesForNumericOp(op, left, right)
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
  ci: DPathCompileInfo,
  host: OOLAGHost)
  extends Expression {

  def init() {
    this.setOOLAGContext(host) // we are the root of expression, but we propagate diagnostics further.
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

  override lazy val compileInfo = ci

  override lazy val compiledDPath = ifor.compiledDPath

  override lazy val children = List(ifor)

}

case class IfExpression(ifthenelse: List[Expression])
  extends ExpressionLists(ifthenelse) {

  override lazy val compiledDPath = new CompiledDPath(IF(predicate.compiledDPath,
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
    (thenPart.inherentType, elsePart.inherentType) match {
      case (left: NodeInfo.Numeric.Kind, right: NodeInfo.Numeric.Kind) =>
        NodeInfoUtils.generalizeArgAndResultTypesForNumericOp(op, left, right)._2
      case (left, right) if left == right => left
      case (left, right) if right == NodeInfo.Nothing => left
      case (left, right) if left == NodeInfo.Nothing => right
      case (left, right) =>
        SDE("If-expression branches must have similar types, but were %s and %s ", left, right)
    }
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
      new CompiledDPath(NegateOp(exp.compiledDPath) +: conversions)
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
        case Some(pt) => pt
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
    val cdp = new CompiledDPath(ToRoot +: rel)
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

  override lazy val compiledDPath: CompiledDPath = {
    val cps = adjustedSteps.map {
      _.compiledDPath
    }
    val ops = cps.map {
      _.ops.toList
    }
    val res = new CompiledDPath(ops.flatten)
    res
  }

  override lazy val steps = {
    steps2
  }

  // remove any spurious "." in relative paths so "./.././.." becomes "../.."
  // corner case "././././." should behave like "."
  val steps2 = {
    val noSelfSteps = steps1.filter { case Self(None) => false; case _ => true }
    val res =
      if (noSelfSteps.length == 0) List(Self(None)) // we need one "."
      else noSelfSteps
    res
  }

  override lazy val children: List[Expression] = steps

}

sealed abstract class StepExpression(val step: String, val pred: Option[PredicateExpression])
  extends Expression {

  def relPathErr() = {
    val err = new RelativePathPastRootError(this.schemaFileLocation,
      "Relative path '%s' past root element.", this.wholeExpressionText)
    toss(err)
  }
  requiredEvaluations(priorStep)
  requiredEvaluations(compileInfo)
  requiredEvaluations(stepElement)

  // override def toString = text

  // Note: all instances are distinct regardless of contents.
  override def equals(x: Any) = x match {
    case ar: AnyRef => this _eq_ ar
    case _ => false
  }

  override def hashCode() = super.hashCode()

  override def hasReferenceTo(elem: DPathElementCompileInfo): Boolean = {
    stepElement =:= elem
  }

  lazy val stepQName = {
    val e = QName.resolveStep(step, namespaces, tunable)
    e match {
      case Failure(th) => SDE("Step %s prefix has no corresponding namespace.", step)
      case Success(v) => v
    }
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
    val steps = relPathParent.steps
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
    val res = NS.allPrefixes(ns, compileInfo.namespaces)
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
        pt
      } else {
        NodeInfo.Complex
      }
    }
  }

}

//TODO: Is ".[i]" ever a valid expression in DFDL?

case class Self(predArg: Option[PredicateExpression]) extends StepExpression(null, predArg) {

  override lazy val compiledDPath = new CompiledDPath(SelfMove +: conversions)

  override def text = "."

  override lazy val stepElement: DPathElementCompileInfo =
    priorStep.map { _.stepElement }.getOrElse {
      //  no prior step, so we're the first step
      this.compileInfo.elementCompileInfo.getOrElse {
        relPathErr()
      }
    }
}

/**
 * Different from Self in that it verifies the qName (s) is
 * the name of the context.
 */
case class Self2(s: String, predArg: Option[PredicateExpression])
  extends StepExpression(s, predArg) {

  requiredEvaluations(stepQName)

  override lazy val compiledDPath = new CompiledDPath(SelfMove +: conversions)

  override def text = "."

  override lazy val stepElement: DPathElementCompileInfo = {
    val ci = priorStep.map { _.stepElement }.getOrElse {
      //  no prior step, so we're the first step
      this.compileInfo.elementCompileInfo.getOrElse {
        relPathErr()
      }
    }
    if (!ci.namedQName.matches(stepQName))
      ci.noMatchError(stepQName)
    ci
  }

}

case class Up(predArg: Option[PredicateExpression]) extends StepExpression(null, predArg) {
  override lazy val compiledDPath = {
    if (isLastStep && stepElement.isArray && targetType == NodeInfo.Array) {
      new CompiledDPath(UpMoveArray)
    } else {
      new CompiledDPath(UpMove)
    }
  }

  override def text = ".." // + "{" + stepElement.path + "}"

  override lazy val stepElement: DPathElementCompileInfo = {
    if (isFirstStep) {
      Assert.invariant(!isAbsolutePath)
      val sc = this.compileInfo
      // if we are some component inside an element then we
      // need to get the element surrounding first, then go up one.
      val e = sc.elementCompileInfo
      val e1 = e.getOrElse {
        SDE("No enclosing element.")
      }
      val e2 = e1.enclosingElementCompileInfo
      val e3 = e2.getOrElse {
        relPathErr()
      }
      e3
    } else {
      // not first, so
      val ps = priorStep
      val ps2 = ps.map { _.stepElement }
      val ps3 = ps2.getOrElse {
        relPathErr()
      }
      val ps4 = ps3.enclosingElementCompileInfo
      val ps5 = ps4.getOrElse {
        relPathErr()
      }
      ps5
    }
  }
}

/**
 * Different from Up in that it verifies the qName (s) is the
 * name of the parent node.
 */
case class Up2(s: String, predArg: Option[PredicateExpression])
  extends StepExpression(s, predArg) {
  override lazy val compiledDPath = new CompiledDPath(UpMove)

  requiredEvaluations(stepQName)

  override def text = ".." // + "{" + stepElement.path + "}"

  override lazy val stepElement: DPathElementCompileInfo = {
    val ci = if (isFirstStep) {
      Assert.invariant(!isAbsolutePath)
      val sc = this.compileInfo
      // if we are some component inside an element then we
      // need to get the element surrounding first, then go up one.
      val e = sc.elementCompileInfo
      val e1 = e.getOrElse {
        SDE("No enclosing element.")
      }
      val e2 = e1.enclosingElementCompileInfo
      val e3 = e2.getOrElse {
        relPathErr()
      }
      e3
    } else {
      // not first, so
      val ps = priorStep
      val ps2 = ps.map { _.stepElement }
      val ps3 = ps2.getOrElse {
        relPathErr()
      }
      val ps4 = ps3.enclosingElementCompileInfo
      val ps5 = ps4.getOrElse {
        relPathErr()
      }
      ps5
    }
    if (!ci.namedQName.matches(stepQName))
      ci.noMatchError(stepQName)
    ci
  }
}

case class NamedStep(s: String, predArg: Option[PredicateExpression])
  extends StepExpression(s, predArg) {

  requiredEvaluations(stepQName)

  override lazy val compiledDPath = {
    val d = downwardStep
    val conv = conversions
    val c = (if (isLastStep) conv else Nil)
    val res = new CompiledDPath(d +: c)
    res
  }

  lazy val dpathElementCompileInfo = stepElement

  lazy val downwardStep = {
    if (stepElement.isArray && pred.isDefined) {
      Assert.invariant(pred.get.targetType == NodeInfo.ArrayIndex)
      val indexRecipe = pred.get.compiledDPath
      new DownArrayOccurrence(dpathElementCompileInfo, indexRecipe)
    } else if (stepElement.isArray && targetType == NodeInfo.Exists) {
      new DownArrayExists(dpathElementCompileInfo)
    } else if (stepElement.isArray) {
      schemaDefinitionUnless(targetType == NodeInfo.Array, "Query-style paths not supported. Must have '[...]' after array-element's name. Offending path step: '%s'.", step)
      new DownArray(dpathElementCompileInfo)
    } else {
      //
      // Note: DFDL spec allows a[exp] if a is not an array, but it's a processing
      // error if exp doesn't evaluate to 1.
      // TODO: Implement this.
      if (pred.isDefined) subsetError("Indexing is only allowed on arrays. Offending path step: '%s%s'.", step, pred.get.text)
      new DownElement(dpathElementCompileInfo)
    }
  }

  override def text = step

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
        rootElement.findRoot(stepQName, this)
        rootElement
      } else {
        // since we're first we start from the element, or nearest enclosing
        val nc = compileInfo.elementCompileInfo.getOrElse {
          // happens for example if you have defaultValue="false" since false looks like a path step, but is really illegal. should be fn:false().
          compileInfo.SDE("The expression path step '%s' has no defined enclosing element.", s)
        }.findNamedChild(stepQName, this)
        nc
      }
    } else {
      // not first step so we are extension of prior step
      val e = priorStep.map { ps =>
        val psse = ps.stepElement
        psse
      }.map { se =>
        val nc = se.findNamedChild(stepQName, this)
        nc
      }.getOrElse(die)
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
    case i: Int => i
    case i: BigInt => {
      Assert.usageError("Expected java.math.BigInteger but received BigInt.")
    }
    case i: JBigInt => {
      if (Numbers.isValidInt(i)) i.intValue()
      else if (Numbers.isValidLong(i)) i.longValue()
      else i
    }
    case bd: BigDecimal => {
      Assert.usageError("Expected java.math.BigDecimal but received scala BigDecimal.")
    }
    case bd: JBigDecimal => {
      // since we got a JBigDecimal, we know it at least wasn't parsed
      // into a JBigInteger, so it has a fraction component, even if it is ".0"
      // We want a double if it fits in one exactly. Otherwise keep a decimal.
      //
      // So consider 0.2. That's going to become a double float since it can be
      // converted to/from without loss of information.
      //
      // But 0.20000000000 is going to stay a BigDecimal.
      //
      if (Numbers.isDecimalDouble(bd)) bd.doubleValue()
      else bd
    }
    case f: Float => f.toDouble
    case d: Double => d
    case b: Boolean => b // there are no literal booleans, but fn:true() and fn:false() turns into one.
    case _ => Assert.invariantFailed("value not one of the expected types")
  }

  override lazy val compiledDPath: CompiledDPath = {
    new CompiledDPath(Literal(litValue) +: conversions)
  }

  override lazy val inherentType = {
    litValue match {
      case s: String => NodeInfo.String
      case i: BigInt => Assert.usageError("Expected java.math.BigInteger but got BigInt.")
      case i: JBigInt => NodeInfo.Integer
      case d: BigDecimal => Assert.usageError("Expected java.math.BigDecimal but got package.BigDecimal.")
      case d: JBigDecimal => NodeInfo.Decimal
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
    val vrefOp = VRef(vrd, compileInfo)
    new CompiledDPath(vrefOp +: conversions)
  }
  override def text = "$" + qnameString

  lazy val theQName: GlobalQName = {
    val refQ = resolveRef(qnameString)
    val global = QName.createGlobal(refQ.local, refQ.namespace, namespaces)
    global
  }

  lazy val vrd = compileInfo.variableMap.getVariableRuntimeData(theQName).getOrElse(
    SDE("Undefined variable: %s", text))

  lazy val varType = {
    val vt = vrd.primType
    vt
  }

  override lazy val inherentType: NodeInfo.Kind = varType
  override def targetTypeForSubexpression(subexp: Expression) =
    Assert.usageError("variable reference expressions have no subexpressions")
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

  final override def leafContentLengthReferencedElements = functionObject.leafContentLengthReferencedElements
  final override def leafValueLengthReferencedElements = functionObject.leafValueLengthReferencedElements

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
    val MATH = XMLUtils.XPATH_MATH_NAMESPACE
    val XSD = XMLUtils.XSD_NAMESPACE
    val DAF = XMLUtils.EXT_NS
    val funcObj = (functionQName, expressions) match {

      case (RefQName(_, "trace", DAF), args) =>
        DAFTraceExpr(functionQNameString, functionQName, args)

      case (RefQName(_, "error", DAF), args) => {
        SDW("Expression daf:error is deprecated. Use fn:error instead")
        DAFErrorExpr(functionQNameString, functionQName, args)
      }

      case (RefQName(_, "error", FUNC), args) => {
        FNErrorExpr(functionQNameString, functionQName, args)
      }

      case (RefQName(_, "not", FUNC), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.Boolean, NodeInfo.AnyAtomic, FNNot(_, _))

      case (RefQName(_, "empty", FUNC), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.Boolean, NodeInfo.Exists, FNEmpty(_, _))

      case (RefQName(_, "contains", FUNC), args) =>
        FNTwoArgsExpr(functionQNameString, functionQName, args,
          NodeInfo.Boolean,
          NodeInfo.String, NodeInfo.String, FNContains(_))

      case (RefQName(_, "starts-with", FUNC), args) =>
        FNTwoArgsExpr(functionQNameString, functionQName, args,
          NodeInfo.Boolean,
          NodeInfo.String, NodeInfo.String, FNStartsWith(_))

      case (RefQName(_, "ends-with", FUNC), args) =>
        FNTwoArgsExpr(functionQNameString, functionQName, args,
          NodeInfo.Boolean,
          NodeInfo.String, NodeInfo.String, FNEndsWith(_))

      case (RefQName(_, "local-name", FUNC), args) if args.length == 0 =>
        FNZeroArgExpr(functionQNameString, functionQName,
          NodeInfo.String, NodeInfo.AnyAtomic, FNLocalName0(_, _))

      case (RefQName(_, "local-name", FUNC), args) if args.length == 1 =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.String, NodeInfo.AnyAtomic, FNLocalName1(_, _))

      case (RefQName(_, "string", XSD), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.String, NodeInfo.AnyAtomic, XSString(_, _))

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

      case (RefQName(_, "hexBinary", DFDL), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.HexBinary, NodeInfo.AnyAtomic, DFDLHexBinary(_, _))

      case (RefQName(_, "byte", DFDL), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.Byte, NodeInfo.AnyAtomic, DFDLByte(_, _))

      case (RefQName(_, "unsignedByte", DFDL), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.UnsignedByte, NodeInfo.AnyAtomic, DFDLUnsignedByte(_, _))

      case (RefQName(_, "short", DFDL), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.Short, NodeInfo.AnyAtomic, DFDLShort(_, _))

      case (RefQName(_, "unsignedShort", DFDL), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.UnsignedShort, NodeInfo.AnyAtomic, DFDLUnsignedShort(_, _))

      case (RefQName(_, "int", DFDL), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.Int, NodeInfo.AnyAtomic, DFDLInt(_, _))

      case (RefQName(_, "unsignedInt", DFDL), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.UnsignedInt, NodeInfo.AnyAtomic, DFDLUnsignedInt(_, _))

      case (RefQName(_, "long", DFDL), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.Long, NodeInfo.AnyAtomic, DFDLLong(_, _))

      case (RefQName(_, "unsignedLong", DFDL), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.UnsignedLong, NodeInfo.AnyAtomic, DFDLUnsignedLong(_, _))

      case (RefQName(_, "nilled", FUNC), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.Boolean, NodeInfo.Nillable, FNNilled(_, _))

      case (RefQName(_, "exists", FUNC), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.Boolean, NodeInfo.Exists, FNExists(_, _))

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
      // keep this as we may put this in as a daffodil extension
      //      case (RefQName(_, "string-join", FUNC), args) =>
      //        FNTwoArgsExpr(functionQNameString, functionQName, args,
      //          NodeInfo.Array, NodeInfo.String, NodeInfo.String, FNStringJoin(_))

      case (RefQName(_, "substring", FUNC), args) if args.length == 2 =>
        FNTwoArgsExpr(functionQNameString, functionQName, args,
          NodeInfo.String, NodeInfo.String, NodeInfo.Double, FNSubstring2(_))
      case (RefQName(_, "substring", FUNC), args) if args.length == 3 =>
        FNThreeArgsExpr(functionQNameString, functionQName, args,
          NodeInfo.String, NodeInfo.String, NodeInfo.Double, NodeInfo.Double, FNSubstring3(_))

      case (RefQName(_, "substring-before", FUNC), args) =>
        FNTwoArgsExpr(functionQNameString, functionQName, args,
          NodeInfo.String, NodeInfo.String, NodeInfo.String, FNSubstringBefore(_))

      case (RefQName(_, "substring-after", FUNC), args) =>
        FNTwoArgsExpr(functionQNameString, functionQName, args,
          NodeInfo.String, NodeInfo.String, NodeInfo.String, FNSubstringAfter(_))

      case (RefQName(_, "true", FUNC), Nil) => LiteralBooleanExpression(true)
      case (RefQName(_, "false", FUNC), Nil) => LiteralBooleanExpression(false)

      case (RefQName(_, "count", FUNC), args) =>
        FNCountExpr(functionQNameString, functionQName, args)

      case (RefQName(_, "exactly-one", FUNC), args) =>
        FNExactlyOneExpr(functionQNameString, functionQName, args)

      case (RefQName(_, "year-from-dateTime", FUNC), args) => FNOneArgExprConversionDisallowed(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.DateTime, FNYearFromDateTime(_, _))
      case (RefQName(_, "month-from-dateTime", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.DateTime, FNMonthFromDateTime(_, _))
      case (RefQName(_, "day-from-dateTime", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.DateTime, FNDayFromDateTime(_, _))
      case (RefQName(_, "hours-from-dateTime", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.DateTime, FNHoursFromDateTime(_, _))
      case (RefQName(_, "minutes-from-dateTime", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.DateTime, FNMinutesFromDateTime(_, _))
      case (RefQName(_, "seconds-from-dateTime", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Decimal, NodeInfo.DateTime, FNSecondsFromDateTime(_, _))
      case (RefQName(_, "year-from-date", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.Date, FNYearFromDate(_, _))
      case (RefQName(_, "month-from-date", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.Date, FNMonthFromDate(_, _))
      case (RefQName(_, "day-from-date", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.Date, FNDayFromDate(_, _))
      case (RefQName(_, "hours-from-time", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.Time, FNHoursFromTime(_, _))
      case (RefQName(_, "minutes-from-time", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Long, NodeInfo.Time, FNMinutesFromTime(_, _))
      case (RefQName(_, "seconds-from-time", FUNC), args) => FNOneArgExpr(functionQNameString, functionQName, args, NodeInfo.Decimal, NodeInfo.Time, FNSecondsFromTime(_, _))

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

      case (RefQName(_, "round-half-to-even", FUNC), args) if args.length == 1 => {
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.Numeric, NodeInfo.Numeric, FNRoundHalfToEven1(_, _))
      }

      case (RefQName(_, "round-half-to-even", FUNC), args) => {
        FNTwoArgsExpr(functionQNameString, functionQName, args,
          NodeInfo.Numeric, NodeInfo.Numeric, NodeInfo.Integer, FNRoundHalfToEven2(_))
      }

      case (RefQName(_, "string-length", FUNC), args) => {
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.ArrayIndex, NodeInfo.String, FNStringLength(_, _))
      }

      case (RefQName(_, "contentLength", DFDL), args) =>
        ContentLengthExpr(functionQNameString, functionQName, args,
          NodeInfo.Long, NodeInfo.Exists, NodeInfo.String, DFDLContentLength(_))
      // The above specifies Exists, because we want to know a node exists, but
      // it has to be a node, not a simple value. E.g., dfdl:contentLength("foobar", "bits") makes no sense.
      // The first argument has to be a path to a node, otherwise we don't have format properties and so
      // can't determine a content length.
      //
      // One might argue that dfdl:contentLength("foobar", "characters") is meaningful and should be 6.
      // But that's fairly pointless.

      case (RefQName(_, "valueLength", DFDL), args) => {
        ValueLengthExpr(functionQNameString, functionQName, args,
          NodeInfo.Long, NodeInfo.Exists, NodeInfo.String, DFDLValueLength(_))
      }

      case (RefQName(_, "lower-case", FUNC), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.String, NodeInfo.String, FNLowerCase(_, _))

      case (RefQName(_, "upper-case", FUNC), args) =>
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.String, NodeInfo.String, FNUpperCase(_, _))

      case (RefQName(_, "timeZoneFromDateTime", DFDL), args) => {
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.String, NodeInfo.DateTime, DFDLTimeZoneFromDFDLCalendar(_, _))
      }
      case (RefQName(_, "timeZoneFromDate", DFDL), args) => {
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.String, NodeInfo.Date, DFDLTimeZoneFromDFDLCalendar(_, _))
      }
      case (RefQName(_, "timeZoneFromTime", DFDL), args) => {
        FNOneArgExpr(functionQNameString, functionQName, args,
          NodeInfo.String, NodeInfo.Time, DFDLTimeZoneFromDFDLCalendar(_, _))
      }
      case (RefQName(_, "pow", MATH), args) => {
        FNTwoArgsExpr(functionQNameString, functionQName, args,
          NodeInfo.Double, NodeInfo.Double, NodeInfo.Numeric, MATHPow(_))
      }

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
        XSConverterExpr(functionQNameString, functionQName, args, NodeInfo.Double)

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

      case _ => SDE("Unsupported function: %s", functionQName)
    }
    funcObj.setOOLAGContext(this)
    funcObj
  }
}

abstract class FunctionCallBase(functionQNameString: String,
  functionQName: RefQName,
  expressions: List[Expression]) extends ExpressionLists(expressions) {
  override def text = functionQNameString + "(" + expressions.map { _.text }.mkString(", ") + ")"

  override lazy val targetType = {
    val res = parent.targetType
    res
  }

  final def checkArgCount(n: Int) {
    if (expressions.length != n) argCountErr(n)
  }
  final def argCountErr(n: Int) = {
    SDE("The %s function requires %s argument(s).", functionQName.toPrettyString, n)
  }

  final def argCountTooFewErr(n: Int) = {
    //
    // Digression: below illustrates what is a hard problem in internationalization
    // of software, which is called pluralization.
    //
    // See that "(s)" fudge here where we mean - add plural 's' if the number is not one.
    // Well there's no one consistent way to do that.

    // Even consider zero. In English zero is plural. In French it is singular.

    // In many languages there are different endings for 0, x1, x2, x3, etc.
    // Much the way ordinal numbers work in English where we have 1st (st ending)
    // 2nd (nd ending for 2nd 22nd, 32nd, 572nd - but not 12 - special case 12th has th ending).
    // We seldom use ordinals in English, but in many languages CARDINAL numbers work
    // this way, and must match the quantity and there's 3 or 4 different endings
    // with exceptions for 11, 12, 13, etc.
    //
    SDE("The %s function requires at least %s argument(s).", functionQName.toPrettyString, n)
  }

  final def argCountTooManyErr(n: Int) = {
    SDE("The %s function requires no more than %s argument(s).", functionQName.toPrettyString, n)
  }
}

/**
 * Tells the sub-expression that we want an array out of it.
 */
abstract class FunctionCallArrayBase(nameAsParsed: String, fnQName: RefQName, args: List[Expression])
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  def funcName: String

  lazy val arrPath = args(0) match {
    case pe: PathExpression => pe
    case _ => subsetError("The %s function must contain a path.", funcName)
  }

  override lazy val isTypeCorrect = {
    checkArgCount(1)
    arrPath.isPathToOneWholeArray
  }

  override def targetTypeForSubexpression(subExp: Expression): NodeInfo.Kind = NodeInfo.Array

}
/**
 * Tells the sub-expression that we want an array out of it.
 */
case class FNCountExpr(nameAsParsed: String, fnQName: RefQName, args: List[Expression])
  extends FunctionCallArrayBase(nameAsParsed, fnQName, args) {

  val funcName: String = "fn:count"

  override lazy val inherentType: NodeInfo.Kind = NodeInfo.Long

  override lazy val compiledDPath = {
    checkArgCount(1)
    val arg0Recipe = args(0).compiledDPath
    val arg0Type = args(0).inherentType
    val c = conversions
    val res = new CompiledDPath(FNCount(arg0Recipe, arg0Type) +: c)
    res
  }
}

case class FNExactlyOneExpr(nameAsParsed: String, fnQName: RefQName, args: List[Expression])
  extends FunctionCallArrayBase(nameAsParsed, fnQName, args) {

  val funcName: String = "fn:exactly-one"

  override lazy val inherentType: NodeInfo.Kind = NodeInfo.AnyType

  override lazy val compiledDPath = {
    checkArgCount(1)
    subsetError("fn:exactly-one is not supported.")
    //new CompiledDPath((arrPath.compiledDPath.ops.toList :+ FNExactlyOne) ++ conversions)
  }
}

case class FNRoundHalfToEvenExpr(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression])
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  val (valueExpr, precisionExpr) = args match {
    case List(ve) => (ve, LiteralExpression(0))
    case List(ve, pe) => (ve, pe)
    case _ => SDE("The %n function requires 1 or 2 arguments. But %s were found.",
      fnQName.toPrettyString, args.length)
  }

  override lazy val children = List(valueExpr, precisionExpr)

  override lazy val inherentType = valueExpr.inherentType

  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind = {
    if (childExpr == valueExpr) valueExpr.inherentType
    else if (childExpr == precisionExpr) NodeInfo.Int
    else Assert.invariantFailed("subexpression isn't one of the expected.")
  }

  override lazy val compiledDPath =
    new CompiledDPath(
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
  args: List[Expression], constructor: (CompiledDPath, NodeInfo.Kind) => RecipeOp)
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
    val res = new CompiledDPath(constructor(arg0Recipe, arg0Type) +: c)
    res
  }
}

case class FNZeroArgExpr(nameAsParsed: String, fnQName: RefQName,
  resultType: NodeInfo.Kind, argType: NodeInfo.Kind, constructor: (CompiledDPath, NodeInfo.Kind) => RecipeOp)
  extends FunctionCallBase(nameAsParsed, fnQName, Nil) {

  override lazy val inherentType = resultType

  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind = argType

  override lazy val compiledDPath = {
    checkArgCount(0)
    val c = conversions
    val res = new CompiledDPath(constructor(null, null) +: c)
    res
  }
}

case class FNOneArgExpr(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression], resultType: NodeInfo.Kind, argType: NodeInfo.Kind, constructor: (CompiledDPath, NodeInfo.Kind) => RecipeOp)
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val inherentType = resultType

  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind = argType

  override lazy val compiledDPath = {
    checkArgCount(1)
    val arg0Recipe = args(0).compiledDPath
    val arg0Type = args(0).inherentType
    val c = conversions
    val res = new CompiledDPath(constructor(arg0Recipe, arg0Type) +: c)
    res
  }
}

case class FNOneArgExprConversionDisallowed(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression], resultType: NodeInfo.Kind, argType: NodeInfo.Kind, constructor: (CompiledDPath, NodeInfo.Kind) => RecipeOp)
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val inherentType = resultType

  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind = argType

  override lazy val conversions = Nil

  override lazy val compiledDPath = {
    checkArgCount(1)
    val arg0Recipe = args(0).compiledDPath
    val arg0Type = args(0).inherentType
    val c = Nil //conversions
    val res = new CompiledDPath(constructor(arg0Recipe, arg0Type) +: c)
    res
  }
}

abstract class FNTwoArgsExprBase(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression], resultType: NodeInfo.Kind, arg1Type: NodeInfo.Kind, arg2Type: NodeInfo.Kind,
  constructor: List[CompiledDPath] => RecipeOp)
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val inherentType = resultType

  lazy val List(arg1, arg2) = { checkArgCount(2); args }

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
    val res = new CompiledDPath(constructor(List(arg1Recipe, arg2Recipe)) +: conversions)
    res
  }
}

case class FNTwoArgsExpr(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression], resultType: NodeInfo.Kind, arg1Type: NodeInfo.Kind, arg2Type: NodeInfo.Kind,
  constructor: List[CompiledDPath] => RecipeOp)
  extends FNTwoArgsExprBase(nameAsParsed, fnQName, args, resultType, arg1Type, arg2Type, constructor)

sealed abstract class LengthExprBase(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression], resultType: NodeInfo.Kind, arg1Type: NodeInfo.Kind, arg2Type: NodeInfo.Kind,
  constructor: List[CompiledDPath] => RecipeOp)
  extends FNTwoArgsExprBase(nameAsParsed, fnQName, args, resultType, arg1Type, arg2Type, constructor) {

  protected final def leafReferencedElements = {
    val arg = args(0).asInstanceOf[PathExpression]
    val steps = arg.steps
    val lst = steps.last
    val elem = lst.stepElement
    val res = Set(elem)
    res
  }

}

case class ContentLengthExpr(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression], resultType: NodeInfo.Kind, arg1Type: NodeInfo.Kind, arg2Type: NodeInfo.Kind,
  constructor: List[CompiledDPath] => RecipeOp)
  extends LengthExprBase(nameAsParsed, fnQName, args, resultType, arg1Type, arg2Type, constructor) {

  override lazy val leafContentLengthReferencedElements = leafReferencedElements
}

case class ValueLengthExpr(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression], resultType: NodeInfo.Kind, arg1Type: NodeInfo.Kind, arg2Type: NodeInfo.Kind,
  constructor: List[CompiledDPath] => RecipeOp)
  extends LengthExprBase(nameAsParsed, fnQName, args, resultType, arg1Type, arg2Type, constructor) {

  override lazy val leafValueLengthReferencedElements = leafReferencedElements
}

case class FNThreeArgsExpr(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression], resultType: NodeInfo.Kind,
  arg1Type: NodeInfo.Kind, arg2Type: NodeInfo.Kind, arg3Type: NodeInfo.Kind,
  constructor: List[CompiledDPath] => RecipeOp)
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val inherentType = resultType

  lazy val List(arg1, arg2, arg3) = { checkArgCount(3); args }

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
    val res = new CompiledDPath(constructor(
      List(arg1Path, arg2Path, arg3Path)) +: c)
    res
  }
}

case class XSConverterExpr(nameAsParsed: String, fnQName: RefQName, args: List[Expression],
  resultType: NodeInfo.Kind)
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val inherentType = resultType

  /*
   * By using the result type as the target for the expression, conversions will
   * do the work of converting into that type. We don't need to put down any
   * additional recipe operator to convert anything.
   */
  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind = resultType // NodeInfo.AnyType

  // TODO: this should work... why do we need to call an additional converter. The
  // args(0).compiledDPath should already have taken into account converting into
  // their target types which are the same as this conversion's output result type.

  override lazy val compiledDPath = {
    checkArgCount(1)
    val arg0Recipe = args(0).compiledDPath
    val c = conversions
    val res = new CompiledDPath(arg0Recipe.ops.toList ++ c)
    res
  }
}

case class FNArgListExpr(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression], resultType: NodeInfo.Kind, argType: NodeInfo.Kind,
  constructor: List[CompiledDPath] => RecipeOp)
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val inherentType = resultType

  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind = argType

  override lazy val compiledDPath = {
    if (args.length < 1) argCountTooFewErr(1)
    new CompiledDPath(constructor(args.map { _.compiledDPath }) +: conversions)
  }
}

case class FNErrorExpr(nameAsParsed: String, fnQName: RefQName, args: List[Expression])
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val inherentType: NodeInfo.Kind = NodeInfo.Nothing

  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind = NodeInfo.String

  override lazy val compiledDPath = {
    if (args.length > 3) argCountTooManyErr(3)
    Assert.invariant(conversions == Nil)
    new CompiledDPath(FNError(args.map { _.compiledDPath }))
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
    new CompiledDPath(DFDLOccursIndex +: conversions)
  }

  override lazy val inherentType = NodeInfo.ArrayIndex
  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind =
    Assert.usageError("No subexpressions")
}

case class DFDLTestBitExpr(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression]) extends FunctionCallBase(nameAsParsed, fnQName, args) {

  lazy val List(data, bitPos) = { checkArgCount(2); args }

  override lazy val inherentType = NodeInfo.Boolean

  override def targetTypeForSubexpression(subexpr: Expression): NodeInfo.Kind = {
    subexpr match {
      case `data` => NodeInfo.UnsignedByte
      case `bitPos` => NodeInfo.UnsignedByte
      case _ => Assert.invariantFailed("wasn't one of the subexpressions.")
    }
  }

  override lazy val compiledDPath =
    new CompiledDPath(DFDLTestBit(data.compiledDPath, bitPos.compiledDPath) +: conversions)

}

case class DFDLSetBitsExpr(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression]) extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val inherentType = NodeInfo.UnsignedByte
  override def targetTypeForSubexpression(subexpr: Expression): NodeInfo.Kind = NodeInfo.UnsignedByte

  override lazy val compiledDPath = {
    checkArgCount(8)
    new CompiledDPath(DFDLSetBits(args.map { _.compiledDPath }) +: conversions)
  }
}

case class DFDLCheckConstraintsExpr(nameAsParsed: String, fnQName: RefQName,
  args: List[Expression]) extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val children = args

  override lazy val compiledDPath = {
    checkArgCount(1)
    val argDPath = args(0).compiledDPath
    val c = conversions
    val res = new CompiledDPath(DFDLCheckConstraints(argDPath) +: c)
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
      argCountErr(2)
    }
  }

  override lazy val inherentType: NodeInfo.Kind = realArg.inherentType
  override def targetTypeForSubexpression(subExp: Expression): NodeInfo.Kind = targetType

  override lazy val compiledDPath = {
    new CompiledDPath(DAFTrace(realArg.compiledDPath, msgText) +: conversions)
  }
}

case class DAFErrorExpr(nameAsParsed: String, fnQName: RefQName, args: List[Expression])
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val inherentType: NodeInfo.Kind = NodeInfo.Nothing
  override def targetTypeForSubexpression(subExp: Expression): NodeInfo.Kind =
    Assert.invariantFailed("no subexpressions")

  override lazy val compiledDPath = {
    checkArgCount(0)
    Assert.invariant(conversions == Nil)
    new CompiledDPath(DAFError)
  }
}
