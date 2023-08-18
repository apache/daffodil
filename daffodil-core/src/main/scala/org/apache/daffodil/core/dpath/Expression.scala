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

import java.lang.{ Boolean => JBoolean, Double => JDouble, Integer => JInt, Long => JLong }
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInt }
import scala.util.{ Failure, Success }
import scala.xml.NamespaceBinding

import org.apache.daffodil.lib.api.DaffodilTunables
import org.apache.daffodil.lib.api.UnqualifiedPathStepPolicy
import org.apache.daffodil.lib.api.WarnID
import org.apache.daffodil.lib.equality._
import org.apache.daffodil.lib.exceptions._
import org.apache.daffodil.lib.oolag.OOLAG.OOLAGHost
import org.apache.daffodil.lib.oolag.OOLAG.OOLAGHostImpl
import org.apache.daffodil.lib.util.Numbers
import org.apache.daffodil.lib.xml.RefQName
import org.apache.daffodil.lib.xml._
import org.apache.daffodil.runtime1.BasicComponent
import org.apache.daffodil.runtime1.dpath._
import org.apache.daffodil.runtime1.dsom.RelativePathPastRootError
import org.apache.daffodil.runtime1.dsom._
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.processors._
import org.apache.daffodil.runtime1.udf.UserDefinedFunctionService

/**
 * Root class of the type hierarchy for the AST nodes used when we
 * compile a DPath Expression.
 *
 * All the work in 'compiling' the DPath expressions happens on methods
 * of these objects.
 *
 * This is the OOLAG pattern again.
 */
abstract class Expression extends OOLAGHostImpl() with BasicComponent {

  override lazy val tunable: DaffodilTunables = parent.tunable
  override lazy val unqualifiedPathStepPolicy: UnqualifiedPathStepPolicy =
    parent.unqualifiedPathStepPolicy

  /**
   * Override where we traverse/access elements.
   */
  def leafContentLengthReferencedElements = ReferencedElementInfos.None
  def leafValueLengthReferencedElements = ReferencedElementInfos.None

  def contentReferencedElementInfos: Set[DPathElementCompileInfo] = {
    val clds = children
    val cldsreis: Set[DPathElementCompileInfo] =
      clds.foldLeft(ReferencedElementInfos.None) { (s, item) =>
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
      clds.foldLeft(ReferencedElementInfos.None) { (s, item) =>
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
  protected lazy val checkTypeCorrectness =
    children.forall { child =>
      child.isTypeCorrect
    }

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

  private lazy val compiledDPath_ = LV('compiledDPath) { compiledDPath }
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

  final lazy val enclosingElementCompileInfos: Seq[DPathElementCompileInfo] =
    compileInfo.enclosingElementCompileInfos

  final lazy val rootElement: DPathElementCompileInfo = {
    val ecis = compileInfo.elementCompileInfos
    if (ecis.length == 0) {
      Assert.invariantFailed("Element doesn't have compile info")
    }
    ecis.head.rootElement
  }

  lazy val namespaces: NamespaceBinding = parent.namespaces

  def children: Seq[Expression]

  def setContextsForChildren(context: OOLAGHost = this): Unit = {
    children.foreach { c =>
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
    QName
      .resolveRef(qnameString, namespaces, tunable.unqualifiedPathStepPolicy)
      .recover { case _: Throwable =>
        SDE("The prefix of '%s' has no corresponding namespace definition.", qnameString)
      }
      .get
  }
}

abstract class ExpressionLists(val lst: List[Expression]) extends Expression {
  override lazy val children = lst
}

trait BinaryExpMixin { self: ExpressionLists =>
  Assert.invariant(children.length == 2)
  def left = children(0)
  def right = children(1)
  def op: String
  def text = left.text + " " + op + " " + right.text
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
  extends ExpressionLists(adds)
  with BooleanExpression {

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
  host: BasicComponent,
) extends Expression {

  final override lazy val tunable: DaffodilTunables = host.tunable
  final override lazy val unqualifiedPathStepPolicy: UnqualifiedPathStepPolicy =
    host.unqualifiedPathStepPolicy

  def init(): Unit = {
    this.setOOLAGContext(
      host,
    ) // we are the root of expression, but we propagate diagnostics further.
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
    Assert.invariant(subExpr == ifor || ifor.isInstanceOf[FunctionCallExpression])
    // The result of this function will be used to coerce the result of the
    // expression to the target type. However, we do not always want to allow
    // type coercion even when it might be possible. For example, if the
    // targetType is xs:string and the expression is { 5 }, we do not want to
    // coerce that to the string "5", but instead want to throw an SDE
    // signifying that we expected a string but the expression result was a
    // numeric. But we sometimes do want coercion for usability purposes. For
    // example, if the result of an expression is a int but the type is long,
    // then we should still allow that coercion. Below we allow coercion
    // between decimal-like types where precision would not be lost, and all
    // integer-like types (which check for precision loss when evaluated).
    val allowCoercion = (inherentType, targetType) match {
      case (_, _) if inherentType == targetType => true
      case (_, _)
          if (inherentType
            .isSubtypeOf(NodeInfo.String) && targetType.isSubtypeOf(NodeInfo.String)) =>
        true
      case (_, _)
          if (inherentType
            .isSubtypeOf(NodeInfo.Integer) && targetType.isSubtypeOf(NodeInfo.Integer)) =>
        true
      case (_, NodeInfo.Float) if (inherentType.isSubtypeOf(NodeInfo.Integer)) => true
      case (_, NodeInfo.Double) if (inherentType.isSubtypeOf(NodeInfo.Integer)) => true
      case (_, NodeInfo.Decimal) if (inherentType.isSubtypeOf(NodeInfo.Integer)) => true
      case (NodeInfo.Float, NodeInfo.Double) => true
      case (NodeInfo.Float, NodeInfo.Decimal) => true
      case (NodeInfo.Double, NodeInfo.Decimal) => true
      case (NodeInfo.Nothing, _) => true
      case (_, NodeInfo.AnyType) => true
      case _ => false
    }

    if (!allowCoercion) {
      if (tunable.allowExpressionResultCoercion) {
        SDW(
          WarnID.DeprecatedExpressionResultCoercion,
          "In expression %s, result type (%s) should be manually cast to the expected type (%s) with the appropriate constructor." +
            "Performing deprecated automatic conversion.",
          wholeExpressionText,
          inherentType,
          targetType,
        )
      } else {
        SDE(
          "In expression %s, result type (%s) must be manually cast to the expected type (%s) with the approrpriate constructor.",
          wholeExpressionText,
          inherentType,
          targetType,
        )
      }
    }

    targetType
  }

  override lazy val inherentType = ifor.inherentType

  override lazy val compileInfo = ci

  override lazy val compiledDPath = ifor.compiledDPath

  override lazy val children = List(ifor)

}

case class IfExpression(ifthenelse: List[Expression]) extends ExpressionLists(ifthenelse) {

  override lazy val compiledDPath = new CompiledDPath(
    IF(predicate.compiledDPath, thenPart.compiledDPath, elsePart.compiledDPath),
  )

  override def text =
    "if (" + predicate.text + ") then " + thenPart.text + " else " + elsePart.text
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
  extends ExpressionLists(ands)
  with BooleanExpression {
  val op = "or"
}
case class AndExpression(comps: List[Expression])
  extends ExpressionLists(comps)
  with BooleanExpression {
  val op = "and"
}

case class AdditiveExpression(op: String, mults: List[Expression])
  extends ExpressionLists(mults)
  with NumericExpression {}

case class MultiplicativeExpression(op: String, unarys: List[Expression])
  extends ExpressionLists(unarys)
  with NumericExpression

case class UnaryExpression(op: String, exp: Expression) extends ExpressionLists(List(exp)) {

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

abstract class PathExpression() extends Expression {
  def steps: List[StepExpression]

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
    if (steps == Nil) false // root is never an array
    else if (isSelf) false // self path is never an array
    else
      steps.last.isArray &&
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

  def isSelf = steps match {
    case List(Self(_)) => true
    case _ => false
  }
}

case class RootPathExpression(relPath: Option[RelativePathExpression])
  extends PathExpression() {

  override def text = "/" + super.text

  override lazy val steps = relPath.map { _.steps }.getOrElse(Nil)

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
    // note that no conversion is needed here since the compiledDPath of the
    // relative path includes the necessary conversions
    val cdp = new CompiledDPath(ToRoot +: rel)
    cdp
  }
}

case class RelativePathExpression(stepsRaw: List[StepExpression], isEvaluatedAbove: Boolean)
  extends PathExpression() {

  lazy val isAbsolutePath = {
    parent != null && parent.isInstanceOf[RootPathExpression]
  }

  override lazy val compiledDPath: CompiledDPath = {

    val stepsToEvaluate = {
      if (parent.isInstanceOf[RootPathExpression] || !isEvaluatedAbove) steps
      else {
        // This expression a relative expression that is actually evaluated
        // above the element it is defined on. This means it's something like
        // dfdl:occursCount with an expression like { ../c }. In cases like
        // this, the property is written on an element decl as if it were
        // accessing a peer, but in fact the expression is evaluated before any
        // instances of the array are even allocated, so the first up ".." move
        // in this relative path is implied and so should not actually be
        // evaluated. Such an expression must begin with an upward step to even
        // be considered valid, so we also error if it doesn't exist.
        if (!steps(0).isInstanceOf[Up]) {
          SDE(
            """Path expression must be absolute or begin with a "../" upward step: %s""",
            this.text,
          )
        }
        steps.tail
      }
    }

    // All the steps are individual CompiledDPaths, let's optmize those out and
    // just create a single CompiledDpath that contains all those flattened
    // operations.
    val ops = stepsToEvaluate.flatMap { _.compiledDPath.ops }

    // add the appropriate conversions based on the inherent type of this path
    // expression. Individual steps do not need a conversion, we only need to
    // convert the result of the path expression
    val res = new CompiledDPath(ops ++ conversions)
    res
  }

  override lazy val steps = {
    // Optimize out all self steps, since those don't change the expression at all
    val noSelfSteps = stepsRaw.filter { case Self(None) => false; case _ => true }
    if (noSelfSteps.length == 0) {
      // If this path expression was all self steps, all steps were removed. We
      // still need at least one step, so replace it with a self step
      List(Self(None))
    } else {
      noSelfSteps
    }
  }

  override lazy val children: List[Expression] = steps

}

sealed abstract class StepExpression(val step: String, val pred: Option[PredicateExpression])
  extends Expression {

  def relPathErr() = {
    // This path expression cannot be compiled because we went past the root. This normally
    // should be an SDE with a RelativePathPastRootError. However, if we don't have any element
    // compile infos or the current root is not the distinguished root that Daffodil is
    // compiling, it means this expression is used on a term that is not a descendent of the
    // distinguished root. The expression might make sense if a different distinguished root were
    // compiled and used the term, but we can't tell. But it doesn't matter since this
    // expression will never be used because it's not a descendent of the distinguished root. So
    // we can just throw a "no context" exception which outputs a warning and allows Daffodil to
    // continue compilation.
    val err =
      if (compileInfo.elementCompileInfos.isEmpty || !rootElement.isDistinguishedRoot) {
        new PathExpressionNoContextError()
      } else {
        new RelativePathPastRootError(
          this.schemaFileLocation,
          "Relative path '%s' past root element.",
          this.wholeExpressionText,
        )
      }
    toss(err)
  }

  def verifyQNames(cis: Seq[DPathElementCompileInfo]): Unit = {
    cis.foreach { ci =>
      if (!ci.namedQName.matches(stepQName))
        ci.noMatchError(stepQName)
    }
  }
  // Combination of lazy val and a protected def is an idiom
  // that enables a lazy calculation to call super.
  final lazy val stepElements = {
    val res = stepElementDefs
    // We cannot check Assert.invariant(res.isDefinedAt(0))
    // since that would prevent us from detecting illegal paths past root
    // for example.
    res
  }

  protected def stepElementDefs: Seq[DPathElementCompileInfo]

  // Note: all instances are distinct regardless of contents.
  override def equals(x: Any) = x match {
    case ar: AnyRef => this._eq_(ar)
    case _ => false
  }

  override def hashCode() = super.hashCode()

  final override def hasReferenceTo(elem: DPathElementCompileInfo): Boolean = {
    stepElements.contains(elem)
  }

  lazy val stepQName = {
    val e = QName.resolveStep(step, namespaces, tunable.unqualifiedPathStepPolicy)
    e match {
      case Failure(th) => SDE("Step %s prefix has no corresponding namespace.", step)
      case Success(v) => v
    }
  }

  override lazy val children = pred.toList

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
      case Some(x) =>
        Assert.invariantFailed("StepExpression must have RelativePathExpression parent.")
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

  final lazy val isArray: Boolean = {
    val (arrays, scalars) = stepElements.partition { _.isArray }
    (arrays.length, scalars.length) match {
      case (a, s) if (a > 0 && s > 0) =>
        arrays.head.SDE(
          "Path step is ambiguous. It can be to an array or a non-array element.\n" +
            "One of the non-arrays is %s",
          scalars.head.schemaFileLocation.toString,
        )
      case (a, s) if (a == 0) => false
      case _ => true
    }
  }

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

}

sealed abstract class DownStepExpression(s: String, predArg: Option[PredicateExpression])
  extends StepExpression(s, predArg) {

  /**
   * Since an expression can be in a reused group, type, or element, the downward path
   * step may actually represent a step downward to distinct elements that happen to
   * just have the same name/namespace, but they could be different types.
   *
   * If that is the case, for the expression to be meaningful, the path step must be
   * ultimately result in an element of type suitable for the next operation that
   * occurs on the element.
   *
   * To insure we have referential transparency, that requires that each distinct type
   * result in its own compilation of the expression. In a complex expression involving
   * multiple such paths.... gets complicated.
   *
   * For now, we just insist all the types are equal, and issue a SDE if they are not.
   */
  override lazy val inherentType: NodeInfo.Kind = {
    val allTypes = stepElements.map { _.typeNode }.distinct
    Assert.invariant(allTypes.length > 0)
    if (allTypes.length > 1) {
      val (relevantExpr, details) = polymorphicExpressionErrorDetails(this)
      subsetError(
        "Expression %s has different types at different points of usage.%s",
        relevantExpr.text,
        details,
      )
    } else {
      allTypes.head
    }
  }

  final def polymorphicExpressionErrorDetails(context: Expression): (Expression, String) = {
    val (relevantExpr: Expression, detailMsg: String) =
      context match {
        case stepE: StepExpression => {
          val typeNodeGroups = stepE.stepElements.groupBy { _.typeNode }
          Assert.invariant(typeNodeGroups.size > 0)
          val rpp = stepE.relPathParent
          val wholePath =
            if (rpp.isAbsolutePath)
              rpp.parent.asInstanceOf[RootPathExpression]
            else
              rpp
          if (typeNodeGroups.size > 1) {
            //
            // types are not consistent for all step elements
            //
            val detailStrings: Seq[String] = {
              typeNodeGroups.flatMap { case (tn, cis) =>
                val tname = tn.globalQName.toQNameString
                val perUsePointStrings = cis.map { ci =>
                  val sfl = ci.schemaFileLocation.locationDescription
                  val qn = ci.namedQName.toQNameString
                  val msg = "element %s in expression %s with %s type at %s".format(
                    qn,
                    wholePath.wholeExpressionText,
                    tname,
                    sfl,
                  )
                  msg
                }
                perUsePointStrings
              }
            }.toSeq
            val detailPart: Seq[String] =
              if (detailStrings.length > 4)
                detailStrings.take(4) :+ "..."
              else
                detailStrings
            val detailMessage: String =
              "\nThe inconsistent type usages are listed here:\n%s".format(
                detailPart.mkString("\n"),
              )
            (wholePath, detailMessage)
          } else {
            (wholePath, "")
          }
        }
        case _ => (context, "")
      }
    (relevantExpr, detailMsg)
  }
}

// TODO: Is ".[i]" ever a valid expression in DFDL?
// Perhaps. Doesn't work currently though. See DAFFODIL-2182

sealed abstract class SelfStepExpression(s: String, predArg: Option[PredicateExpression])
  extends DownStepExpression(s, predArg) {

  override lazy val compiledDPath = new CompiledDPath(SelfMove)
  override def text = "."

  protected def stepElementDefs: Seq[DPathElementCompileInfo] = {
    if (this.isFirstStep) {
      val ecs = compileInfo.elementCompileInfos
      if (ecs.isEmpty) {
        Assert.invariantFailed("Self expression step '.' but there is no enclosing element")
      }
      ecs
    } else
      priorStep.get match {
        case priorUp: UpStepExpression => SDE("Path '../.' is not allowed.")
        case priorDown: DownStepExpression => priorDown.stepElements
        case x => Assert.invariantFailed("Not recognized: " + x)
      }
  }
}

case class Self(predArg: Option[PredicateExpression]) extends SelfStepExpression(null, predArg)

/**
 * Different from Self in that it verifies the qName (s) is
 * the name of the context.
 */
case class Self2(s: String, predArg: Option[PredicateExpression])
  extends SelfStepExpression(s, predArg) {

  override def stepElementDefs: Seq[DPathElementCompileInfo] = {
    val cis = super.stepElementDefs
    verifyQNames(cis)
    cis
  }
}

sealed abstract class UpStepExpression(s: String, predArg: Option[PredicateExpression])
  extends StepExpression(s, predArg) {

  override def text = ".."

  final override lazy val compiledDPath = {
    val areAllArrays = isLastStep && stepElements.forall {
      _.isArray
    } && targetType == NodeInfo.Array
    if (areAllArrays) {
      new CompiledDPath(UpMoveArray)
    } else {
      new CompiledDPath(UpMove)
    }
  }

  protected def stepElementDefs: Seq[DPathElementCompileInfo] = {
    val res =
      if (isFirstStep) {
        Assert.invariant(!isAbsolutePath)
        //
        // This looks like 2 hops up, but it is really 1 hop up.
        // first position ourselves on nearest enclosing element, or self if
        // we are an element.
        // Then take the enclosing elements to get ".." upward move.
        //
        val p = compileInfo.elementCompileInfos.flatMap { _.enclosingElementCompileInfos }
        if (p.isEmpty) relPathErr()
        p
      } else {
        val ps = priorStep.get
        ps.stepElements.flatMap { _.enclosingElementCompileInfos }
      }
    res
  }

  override lazy val inherentType: NodeInfo.Kind = NodeInfo.Complex
}

case class Up(predArg: Option[PredicateExpression]) extends UpStepExpression(null, predArg)

/**
 * Different from Up in that it verifies the qName (s) is the
 * name of the parent node.
 */
case class Up2(s: String, predArg: Option[PredicateExpression])
  extends UpStepExpression(s, predArg) {

  override def text = ".."

  override protected def stepElementDefs: Seq[DPathElementCompileInfo] = {
    val cis = super.stepElementDefs
    verifyQNames(cis)
    cis
  }

}

/**
 * Exception that could be thrown by a NamedStep if it is determined that there is not
 * enough context to evaluate the step.
 */
class PathExpressionNoContextError extends ThinException

case class NamedStep(s: String, predArg: Option[PredicateExpression])
  extends DownStepExpression(s, predArg) {

  override lazy val compiledDPath = {
    val d = downwardStep
    val res = new CompiledDPath(d)
    res
  }

  lazy val dpathElementCompileInfos = stepElements

  lazy val downwardStep = {
    val nqn = dpathElementCompileInfos.head.namedQName
    if (isArray) {
      if (pred.isDefined) {
        Assert.invariant(pred.get.targetType == NodeInfo.ArrayIndex)
        val indexRecipe = pred.get.compiledDPath
        new DownArrayOccurrence(nqn, indexRecipe)
      } else if (targetType == NodeInfo.Exists) {
        new DownArrayExists(nqn)
      } else {
        schemaDefinitionUnless(
          targetType == NodeInfo.Array,
          "Query-style paths not supported. Must have '[...]' after array-element's name. Offending path step: '%s'.",
          step,
        )
        new DownArray(nqn)
      }
    } else {
      //
      // Note: DFDL spec allows a[exp] if a is not an array, but it's a processing
      // error if exp doesn't evaluate to 1.
      // TODO: Implement this.
      if (pred.isDefined)
        subsetError(
          "Indexing is only allowed on arrays. Offending path step: '%s%s'.",
          step,
          pred.get.text,
        )
      // the downward element step must be the same for all the possible elements, so
      // we can pick the first one arbitrarily.
      new DownElement(nqn)
    }
  }

  override def text = step

  /*
   * The ERDs of the elements that correspond to this path step
   * (or SDE trying to find them.)
   *
   * There are multiple step elements because an upward relative path can be say
   * from an element inside a global group, upward to multiple group
   * references where the relative path then steps downward to
   * different elements having the same name.
   *
   * E.g., ../../b must type check for all possible b that this path
   * can refer to. If that path appears inside a global group definition,
   * then every place that group is used must have a corresponding b
   * child element that satisfies this path, and which type-checks.
   *
   * So long as they all type check, the we can generate the
   * downward step we need in a way that works universally for all
   * uses of the path.
   */
  override protected def stepElementDefs: Seq[DPathElementCompileInfo] = {
    val res: Seq[DPathElementCompileInfo] =
      if (isFirstStep) {
        if (isAbsolutePath) {
          if (compileInfo.elementCompileInfos.isEmpty || !rootElement.isDistinguishedRoot) {
            // If we don't have any element compile infos or the current root is not the
            // distinguished root that Daffodil is compiling, it means this expression is used
            // on a term that is not a descendent of the distinguished root. The expression might
            // make sense if a different distinguished root were compiled and used the term, but
            // we can't tell. But it doesn't matter since this expression will never be used
            // because it's not a descendent of the distinguished root. So we can just throw a
            // "no context" exception which outputs a warning and allows Daffodil to continue
            // compilation.
            throw new PathExpressionNoContextError()
          } else {
            // Our root is the distinguished root, so ensure that the named step exists as the
            // root element
            rootElement.findRoot(stepQName, this)
          }
          Seq(rootElement)
        } else {
          // Since we're first we start from the element, or nearest enclosing
          // for this path expression.

          // That can already be multiple such, because this expression could be
          // say on the initiator property of an inner sequence inside a global sequence
          // group def. That global def could be used inside the complex type of
          // 3 different elements. So we would have 3 elementCompileInfos for this
          // step.
          //
          // Also since we're the first step, and this is a named step, this has
          // to be downward to a child. Assertion check here is just in case
          // somebody refactors this code elsewhere, as it is a very strong
          // assumption for this algorithm.
          Assert.invariant(this.isInstanceOf[NamedStep])
          //
          // All enclosing elements for this path step must be able to
          // find a named child that matches.
          //
          val nc = compileInfo.elementCompileInfos.map {
            _.findNamedChild(stepQName, this) // will SDE on not found.
          }

          // If the Seq of named children is empty, that means the elementCompileInfos Seq
          // must have been empty. This only happens when the context of this path
          // expression is unknown. An example of this is a path expression in a global
          // group where the group is never referenced. Because it is not referenced we
          // have no element context, and so cannot evaluate the path for correctness. In
          // cases like this, we throw an exception that is caught elsewere to handle this
          // issue
          if (nc.isEmpty) throw new PathExpressionNoContextError()

          nc
        }
      } else {
        // This is not first step so we are extension of prior step
        val ps = priorStep.get
        //
        // Now our current step is one that we can start
        // look for our downward named step.
        //
        // This downward step must make sense name and type-wise
        val possibles = ps.stepElements.flatMap { d =>
          val possibleChildren = d.elementChildrenCompileInfo
          val matches = d.findNamedChildren(stepQName, possibleChildren)
          if (matches.isEmpty)
            d.noMatchError(stepQName)
          if (matches.length > 1) {
            // expression is ambiguous. A path like ../foo, but for one of the possible ".." there's multiple foo.
            d.queryMatchWarning(stepQName, matches, this)
          }
          matches
        }
        if (possibles.isEmpty) relPathErr()
        possibles
      }
    res
  }
}

/**
 * The thing in square brackets is indexing in DFDL, but XPath
 * calls it a "predicate".
 */
case class PredicateExpression(ifOr: Expression) extends Expression {

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
  extends ExpressionLists(expressions) {}

abstract class LiteralExpressionBase(value: Any) extends PrimaryExpression(Nil) {

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
  lazy val litValue: DataValuePrimitive = value match {
    case s: String => s
    case i: Int => i
    case i: JBigInt => {
      if (Numbers.isValidInt(i)) i.intValue()
      else if (Numbers.isValidLong(i)) i.longValue()
      else i
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
    case b: Boolean =>
      b // there are no literal booleans, but fn:true() and fn:false() turns into one.
    case _ => Assert.invariantFailed("value not one of the expected types")
  }

  override lazy val compiledDPath: CompiledDPath = {
    litValue
    new CompiledDPath(Literal(litValue) +: conversions)
  }

  override lazy val inherentType = {
    litValue.getAnyRef match {
      case s: String => NodeInfo.String
      case i: JBigInt => NodeInfo.Integer
      case d: JBigDecimal => NodeInfo.Decimal
      case df: JDouble => NodeInfo.Double
      case l: JLong => NodeInfo.Long
      case i: JInt => NodeInfo.Int
      case b: JBoolean => NodeInfo.Boolean
      case _ =>
        Assert.invariantFailed("value not one of the expected types " + litValue.getClass())
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

case class VariableRef(val qnameString: String) extends PrimaryExpression(Nil) {

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

  lazy val vrd = compileInfo.variableMap
    .getVariableRuntimeData(theQName)
    .getOrElse(SDE("Undefined variable: %s", text))

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

  final override def leafContentLengthReferencedElements =
    functionObject.leafContentLengthReferencedElements
  final override def leafValueLengthReferencedElements =
    functionObject.leafValueLengthReferencedElements

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
    val DFDLX = XMLUtils.DFDLX_NAMESPACE
    val FUNC = XMLUtils.XPATH_FUNCTION_NAMESPACE
    val MATH = XMLUtils.XPATH_MATH_NAMESPACE
    val XSD = XMLUtils.XSD_NAMESPACE
    val DAF_NCSA = XMLUtils.EXT_NS_NCSA
    val DAF_APACHE = XMLUtils.EXT_NS_APACHE
    val funcObj = (functionQName, expressions) match {

      case (RefQName(_, "trace", DFDLX | DAF_NCSA | DAF_APACHE), args) => {
        if (functionQName.namespace != DFDLX) {
          SDW(
            WarnID.DeprecatedFunctionDAFError,
            "Expression %s is deprecated. Use dfdlx:trace instead",
            functionQNameString,
          )
        }
        DFDLXTraceExpr(functionQNameString, functionQName, args)
      }

      case (RefQName(_, "error", DFDLX | DAF_NCSA | DAF_APACHE), args) => {
        SDW(
          WarnID.DeprecatedFunctionDAFError,
          "Expression %s is deprecated. Use fn:error instead",
          functionQNameString,
        )
        DAFErrorExpr(functionQNameString, functionQName, args)
      }

      case (RefQName(_, "error", FUNC), args) => {
        FNErrorExpr(functionQNameString, functionQName, args)
      }

      case (RefQName(_, "not", FUNC), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Boolean,
          NodeInfo.AnyAtomic,
          FNNot(_, _),
        )

      case (RefQName(_, "empty", FUNC), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Boolean,
          NodeInfo.Exists,
          FNEmpty(_, _),
        )

      case (RefQName(_, "contains", FUNC), args) =>
        FNTwoArgsExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Boolean,
          NodeInfo.String,
          NodeInfo.String,
          FNContains(_),
        )

      case (RefQName(_, "starts-with", FUNC), args) =>
        FNTwoArgsExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Boolean,
          NodeInfo.String,
          NodeInfo.String,
          FNStartsWith(_),
        )

      case (RefQName(_, "ends-with", FUNC), args) =>
        FNTwoArgsExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Boolean,
          NodeInfo.String,
          NodeInfo.String,
          FNEndsWith(_),
        )

      case (RefQName(_, "local-name", FUNC), args) if args.length == 0 =>
        FNZeroArgExpr(
          functionQNameString,
          functionQName,
          NodeInfo.String,
          NodeInfo.Exists,
          FNLocalName0(_, _),
        )

      case (RefQName(_, "local-name", FUNC), args) if args.length == 1 =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.String,
          NodeInfo.Exists,
          FNLocalName1(_, _),
        )

      case (RefQName(_, "namespace-uri", FUNC), args) if args.length == 0 =>
        FNZeroArgExpr(
          functionQNameString,
          functionQName,
          NodeInfo.String,
          NodeInfo.Exists,
          FNNamespaceUri0(_, _),
        )

      case (RefQName(_, "namespace-uri", FUNC), args) if args.length == 1 =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.String,
          NodeInfo.Exists,
          FNNamespaceUri1(_, _),
        )

      case (RefQName(_, "string", XSD), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.String,
          NodeInfo.AnyAtomic,
          XSString(_, _),
        )

      case (RefQName(_, "dateTime", FUNC), args) =>
        FNTwoArgsExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.DateTime,
          NodeInfo.Date,
          NodeInfo.Time,
          FNDateTime(_),
        )

      case (RefQName(_, "dateTime", XSD), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.DateTime,
          NodeInfo.AnyAtomic,
          XSDateTime(_, _),
        )

      case (RefQName(_, "date", XSD), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Date,
          NodeInfo.AnyAtomic,
          XSDate(_, _),
        )

      case (RefQName(_, "time", XSD), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Time,
          NodeInfo.AnyAtomic,
          XSTime(_, _),
        )

      case (RefQName(_, "hexBinary", XSD), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.HexBinary,
          NodeInfo.AnyAtomic,
          XSHexBinary(_, _),
        )

      case (RefQName(_, "hexBinary", DFDL), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.HexBinary,
          NodeInfo.AnyAtomic,
          DFDLHexBinary(_, _),
        )

      case (RefQName(_, "byte", DFDL), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Byte,
          NodeInfo.AnyAtomic,
          DFDLByte(_, _),
        )

      case (RefQName(_, "unsignedByte", DFDL), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.UnsignedByte,
          NodeInfo.AnyAtomic,
          DFDLUnsignedByte(_, _),
        )

      case (RefQName(_, "short", DFDL), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Short,
          NodeInfo.AnyAtomic,
          DFDLShort(_, _),
        )

      case (RefQName(_, "unsignedShort", DFDL), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.UnsignedShort,
          NodeInfo.AnyAtomic,
          DFDLUnsignedShort(_, _),
        )

      case (RefQName(_, "int", DFDL), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Int,
          NodeInfo.AnyAtomic,
          DFDLInt(_, _),
        )

      case (RefQName(_, "unsignedInt", DFDL), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.UnsignedInt,
          NodeInfo.AnyAtomic,
          DFDLUnsignedInt(_, _),
        )

      case (RefQName(_, "long", DFDL), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Long,
          NodeInfo.AnyAtomic,
          DFDLLong(_, _),
        )

      case (RefQName(_, "unsignedLong", DFDL), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.UnsignedLong,
          NodeInfo.AnyAtomic,
          DFDLUnsignedLong(_, _),
        )

      case (RefQName(_, "nilled", FUNC), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Boolean,
          NodeInfo.AnyType,
          FNNilled(_, _),
        )

      case (RefQName(_, "exists", FUNC), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Boolean,
          NodeInfo.Exists,
          FNExists(_, _),
        )

      case (RefQName(_, "ceiling", FUNC), args) =>
        FNOneArgMathExpr(functionQNameString, functionQName, args, FNCeiling(_, _))

      case (RefQName(_, "floor", FUNC), args) =>
        FNOneArgMathExpr(functionQNameString, functionQName, args, FNFloor(_, _))

      case (RefQName(_, "round", FUNC), args) =>
        FNOneArgMathExpr(functionQNameString, functionQName, args, FNRound(_, _))

      case (RefQName(_, "abs", FUNC), args) =>
        FNOneArgMathExpr(functionQNameString, functionQName, args, FNAbs(_, _))

      case (RefQName(_, "concat", FUNC), args) =>
        FNArgListExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.String,
          NodeInfo.String,
          FNConcat(_),
        )

      // Note: there is no string-join function in DFDL
      // keep this as we may put this in as a daffodil extension
      //      case (RefQName(_, "string-join", FUNC), args) =>
      //        FNTwoArgsExpr(functionQNameString, functionQName, args,
      //          NodeInfo.Array, NodeInfo.String, NodeInfo.String, FNStringJoin(_))

      case (RefQName(_, "substring", FUNC), args) if args.length == 2 =>
        FNTwoArgsExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.String,
          NodeInfo.String,
          NodeInfo.Double,
          FNSubstring2(_),
        )
      case (RefQName(_, "substring", FUNC), args) if args.length == 3 =>
        FNThreeArgsExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.String,
          NodeInfo.String,
          NodeInfo.Double,
          NodeInfo.Double,
          FNSubstring3(_),
        )

      case (RefQName(_, "substring-before", FUNC), args) =>
        FNTwoArgsExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.String,
          NodeInfo.String,
          NodeInfo.String,
          FNSubstringBefore(_),
        )

      case (RefQName(_, "substring-after", FUNC), args) =>
        FNTwoArgsExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.String,
          NodeInfo.String,
          NodeInfo.String,
          FNSubstringAfter(_),
        )

      case (RefQName(_, "true", FUNC), Nil) => LiteralBooleanExpression(true)
      case (RefQName(_, "false", FUNC), Nil) => LiteralBooleanExpression(false)

      case (RefQName(_, "count", FUNC), args) =>
        FNCountExpr(functionQNameString, functionQName, args)

      case (RefQName(_, "exactly-one", FUNC), args) =>
        FNExactlyOneExpr(functionQNameString, functionQName, args)

      case (RefQName(_, "leftShift", DFDLX), args) =>
        DFDLXShiftExpr(functionQNameString, functionQName, args, DFDLXLeftShift(_, _))
      case (RefQName(_, "rightShift", DFDLX), args) =>
        DFDLXShiftExpr(functionQNameString, functionQName, args, DFDLXRightShift(_, _))
      case (RefQName(_, "bitXor", DFDLX), args) =>
        DFDLXBitBinaryExpr(functionQNameString, functionQName, args, DFDLXBitXor(_, _))
      case (RefQName(_, "bitAnd", DFDLX), args) =>
        DFDLXBitBinaryExpr(functionQNameString, functionQName, args, DFDLXBitAnd(_, _))
      case (RefQName(_, "bitOr", DFDLX), args) =>
        DFDLXBitBinaryExpr(functionQNameString, functionQName, args, DFDLXBitOr(_, _))
      case (RefQName(_, "bitNot", DFDLX), args) =>
        DFDLXBitUnaryExpr(functionQNameString, functionQName, args, DFDLXBitNot(_, _))

      case (RefQName(_, "year-from-dateTime", FUNC), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Integer,
          NodeInfo.DateTime,
          FNYearFromDateTime(_, _),
        )
      case (RefQName(_, "month-from-dateTime", FUNC), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Integer,
          NodeInfo.DateTime,
          FNMonthFromDateTime(_, _),
        )
      case (RefQName(_, "day-from-dateTime", FUNC), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Integer,
          NodeInfo.DateTime,
          FNDayFromDateTime(_, _),
        )
      case (RefQName(_, "hours-from-dateTime", FUNC), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Integer,
          NodeInfo.DateTime,
          FNHoursFromDateTime(_, _),
        )
      case (RefQName(_, "minutes-from-dateTime", FUNC), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Integer,
          NodeInfo.DateTime,
          FNMinutesFromDateTime(_, _),
        )
      case (RefQName(_, "seconds-from-dateTime", FUNC), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Decimal,
          NodeInfo.DateTime,
          FNSecondsFromDateTime(_, _),
        )
      case (RefQName(_, "year-from-date", FUNC), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Integer,
          NodeInfo.Date,
          FNYearFromDate(_, _),
        )
      case (RefQName(_, "month-from-date", FUNC), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Integer,
          NodeInfo.Date,
          FNMonthFromDate(_, _),
        )
      case (RefQName(_, "day-from-date", FUNC), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Integer,
          NodeInfo.Date,
          FNDayFromDate(_, _),
        )
      case (RefQName(_, "hours-from-time", FUNC), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Integer,
          NodeInfo.Time,
          FNHoursFromTime(_, _),
        )
      case (RefQName(_, "minutes-from-time", FUNC), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Integer,
          NodeInfo.Time,
          FNMinutesFromTime(_, _),
        )
      case (RefQName(_, "seconds-from-time", FUNC), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Decimal,
          NodeInfo.Time,
          FNSecondsFromTime(_, _),
        )

      case (RefQName(_, "occursIndex", DFDL), args) => {
        DFDLOccursIndexExpr(functionQNameString, functionQName, args)
      }
      case (RefQName(_, "checkConstraints", DFDL), args) => {
        DFDLCheckConstraintsExpr(functionQNameString, functionQName, args)
      }
      case (RefQName(_, "decodeDFDLEntities", DFDL), args) => {
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.String,
          NodeInfo.String,
          DFDLDecodeDFDLEntities(_, _),
        )
      }
      case (RefQName(_, "encodeDFDLEntities", DFDL), args) => {
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.String,
          NodeInfo.String,
          DFDLEncodeDFDLEntities(_, _),
        )
      }
      case (RefQName(_, "containsDFDLEntities", DFDL), args) => {
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Boolean,
          NodeInfo.String,
          DFDLContainsDFDLEntities(_, _),
        )
      }

      case (RefQName(_, "testBit", DFDL), args) => {
        DFDLTestBitExpr(functionQNameString, functionQName, args)
      }
      case (RefQName(_, "setBits", DFDL), args) => {
        DFDLSetBitsExpr(functionQNameString, functionQName, args)
      }

      // Begin DFDLX functions

      case (RefQName(_, "doubleFromRawLong", DFDLX), args) => {
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Double,
          NodeInfo.Long,
          DFDLXDoubleFromRawLong(_, _),
        )
      }

      case (RefQName(_, "doubleToRawLong", DFDLX), args) => {
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Long,
          NodeInfo.Double,
          DFDLXDoubleToRawLong(_, _),
        )
      }

      case (RefQName(_, "lookAhead", DFDLX), args) =>
        FNTwoArgsExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.NonNegativeInteger,
          NodeInfo.UnsignedInt,
          NodeInfo.UnsignedInt,
          DFDLXLookAhead(_),
        )

      // End DFDLX functions

      case (RefQName(_, "round-half-to-even", FUNC), args) if args.length == 1 => {
        FNOneArgMathExpr(functionQNameString, functionQName, args, FNRoundHalfToEven1(_, _))
      }

      case (RefQName(_, "round-half-to-even", FUNC), args) => {
        FNTwoArgsMathExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Integer,
          FNRoundHalfToEven2(_),
        )
      }

      case (RefQName(_, "string-length", FUNC), args) => {
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.ArrayIndex,
          NodeInfo.String,
          FNStringLength(_, _),
        )
      }

      case (RefQName(_, "contentLength", DFDL), args) =>
        ContentLengthExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Long,
          NodeInfo.Exists,
          NodeInfo.String,
          DFDLContentLength(_),
        )
      // The above specifies Exists, because we want to know a node exists, but
      // it has to be a node, not a simple value. E.g., dfdl:contentLength("foobar", "bits") makes no sense.
      // The first argument has to be a path to a node, otherwise we don't have format properties and so
      // can't determine a content length.
      //
      // One might argue that dfdl:contentLength("foobar", "characters") is meaningful and should be 6.
      // But that's fairly pointless.

      case (RefQName(_, "valueLength", DFDL), args) => {
        ValueLengthExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Long,
          NodeInfo.Exists,
          NodeInfo.String,
          DFDLValueLength(_),
        )
      }

      case (RefQName(_, "lower-case", FUNC), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.String,
          NodeInfo.String,
          FNLowerCase(_, _),
        )

      case (RefQName(_, "upper-case", FUNC), args) =>
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.String,
          NodeInfo.String,
          FNUpperCase(_, _),
        )

      case (RefQName(_, "timeZoneFromDateTime", DFDL), args) => {
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.String,
          NodeInfo.DateTime,
          DFDLTimeZoneFromDFDLCalendar(_, _),
        )
      }
      case (RefQName(_, "timeZoneFromDate", DFDL), args) => {
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.String,
          NodeInfo.Date,
          DFDLTimeZoneFromDFDLCalendar(_, _),
        )
      }
      case (RefQName(_, "timeZoneFromTime", DFDL), args) => {
        FNOneArgExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.String,
          NodeInfo.Time,
          DFDLTimeZoneFromDFDLCalendar(_, _),
        )
      }
      case (RefQName(_, "pow", MATH), args) => {
        FNTwoArgsExpr(
          functionQNameString,
          functionQName,
          args,
          NodeInfo.Double,
          NodeInfo.Double,
          NodeInfo.Numeric,
          MATHPow(_),
        )
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

      case (_: RefQName, args) => {
        val namespace = functionQName.namespace.toString()
        val fName = functionQName.local

        val udfCallingInfo =
          UserDefinedFunctionService.lookupUserDefinedFunctionCallingInfo(namespace, fName)

        if (udfCallingInfo.isEmpty) {
          SDE("Unsupported function: %s", functionQName)
        } else {
          val UserDefinedFunctionService.UserDefinedFunctionCallingInfo(udf, ei) =
            udfCallingInfo.get
          val UserDefinedFunctionService.EvaluateMethodInfo(
            evaluateMethod,
            evaluateParamTypes,
            evaluateReturnType,
          ) =
            ei

          UserDefinedFunctionCallExpr(
            functionQNameString,
            functionQName,
            args,
            evaluateParamTypes,
            evaluateReturnType,
            UserDefinedFunctionCall(_, _, udf, evaluateMethod),
          )
        }
      }
    }
    funcObj.setOOLAGContext(this)
    funcObj
  }

}

abstract class FunctionCallBase(
  functionQNameString: String,
  functionQName: RefQName,
  expressions: List[Expression],
) extends ExpressionLists(expressions) {
  override def text =
    functionQNameString + "(" + expressions.map { _.text }.mkString(", ") + ")"

  override lazy val targetType = {
    val res = parent.targetType
    res
  }

  final def checkArgCount(n: Int): Unit = {
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
    SDE(
      "The %s function requires no more than %s argument(s).",
      functionQName.toPrettyString,
      n,
    )
  }

}

/**
 * Tells the sub-expression that we want an array out of it.
 */
abstract class FunctionCallArrayBase(
  nameAsParsed: String,
  fnQName: RefQName,
  args: List[Expression],
) extends FunctionCallBase(nameAsParsed, fnQName, args) {

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
  }
}

case class FNRoundHalfToEvenExpr(
  nameAsParsed: String,
  fnQName: RefQName,
  args: List[Expression],
) extends FunctionCallBase(nameAsParsed, fnQName, args) {

  val (valueExpr, precisionExpr) = args match {
    case List(ve) => (ve, LiteralExpression(0))
    case List(ve, pe) => (ve, pe)
    case _ =>
      SDE(
        "The %n function requires 1 or 2 arguments. But %s were found.",
        fnQName.toPrettyString,
        args.length,
      )
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
      FNRoundHalfToEven(valueExpr.compiledDPath, precisionExpr.compiledDPath) +: conversions,
    )
}

/**
 * Preserves the inherent type of the argument to the function as the type of
 * the result, that is if the argument type is a numeric type, and if
 * the argument type is a subtype thereof.
 */
case class FNOneArgMathExpr(
  nameAsParsed: String,
  fnQName: RefQName,
  args: List[Expression],
  constructor: (CompiledDPath, NodeInfo.Kind) => RecipeOp,
) extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val inherentType = {
    schemaDefinitionUnless(
      argInherentType.isSubtypeOf(NodeInfo.Numeric),
      "Argument must be of numeric type but was %s.",
      argInherentType,
    )
    argInherentType
  }

  lazy val argInherentType = {
    schemaDefinitionUnless(
      args.length == 1,
      "Function %s takes 1 argument.",
      fnQName.toPrettyString,
    )
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

/**
 * Preserves the inherent type of the argument to the function as the type of
 * the result, that is if the argument type is a numeric type, and if the
 * argument type is a subtype thereof. The inherent type comes from the first
 * argument
 */
case class FNTwoArgsMathExpr(
  nameAsParsed: String,
  fnQName: RefQName,
  args: List[Expression],
  arg2Type: NodeInfo.Kind,
  constructor: List[CompiledDPath] => RecipeOp,
) extends FNTwoArgsExprBase(
    nameAsParsed,
    fnQName,
    args,
    NodeInfo.Numeric,
    NodeInfo.Numeric,
    arg2Type,
    constructor,
  ) {

  override lazy val inherentType = {
    schemaDefinitionUnless(
      argInherentType.isSubtypeOf(NodeInfo.Numeric),
      "First argument must be of numeric type but was %s.",
      argInherentType,
    )
    argInherentType
  }

  lazy val argInherentType = {
    schemaDefinitionUnless(
      args.length == 2,
      "Function %s takes 2 arguments.",
      fnQName.toPrettyString,
    )
    args(0).inherentType
  }

  override def targetTypeForSubexpression(subexp: Expression): NodeInfo.Kind = {
    if (subexp == arg1)
      inherentType
    else if (subexp == arg2)
      arg2Type
    else
      Assert.invariantFailed("subexpression %s is not an argument.".format(subexp))
  }
}

case class DFDLXShiftExpr(
  nameAsParsed: String,
  fnQName: RefQName,
  args: List[Expression],
  constructor: (List[CompiledDPath], NodeInfo.Kind) => RecipeOp,
) extends FunctionCallBase(nameAsParsed, fnQName, args) {
  override lazy val inherentType = {
    val argInherentType = args(0).inherentType
    schemaDefinitionUnless(
      argInherentType.isSubtypeOf(NodeInfo.PrimType.UnsignedLong) ||
        argInherentType.isSubtypeOf(NodeInfo.PrimType.Long),
      "First argument for %s must be either xs:unsignedLong or xs:long or a subtype of those, but was %s.",
      nameAsParsed,
      argInherentType.globalQName,
    )
    argInherentType
  }

  override def targetTypeForSubexpression(subexp: Expression): NodeInfo.Kind = {
    if (subexp == args(0))
      inherentType
    else if (subexp == args(1))
      NodeInfo.UnsignedInt
    else
      // $COVERAGE-OFF$
      Assert.invariantFailed("subexpression %s is not an argument.".format(subexp))
    // $COVERAGE-ON$
  }

  override def compiledDPath: CompiledDPath = {
    checkArgCount(2)
    val arg0Recipe = args(0).compiledDPath
    val arg0Type = args(0).inherentType
    val arg1Recipe = args(1).compiledDPath
    val c = conversions
    val res = new CompiledDPath(constructor(List(arg0Recipe, arg1Recipe), arg0Type) +: c)
    res
  }
}
case class DFDLXBitBinaryExpr(
  nameAsParsed: String,
  fnQName: RefQName,
  args: List[Expression],
  constructor: (List[CompiledDPath], NodeInfo.Kind) => RecipeOp,
) extends FunctionCallBase(nameAsParsed, fnQName, args) {
  override lazy val inherentType = {
    val arg0Type = args(0).inherentType
    val arg1Type = args(1).inherentType
    val argInherentType = if (arg1Type.isSubtypeOf(arg0Type)) arg0Type else arg1Type
    schemaDefinitionUnless(
      argInherentType.isSubtypeOf(NodeInfo.PrimType.UnsignedLong) || argInherentType
        .isSubtypeOf(NodeInfo.PrimType.Long),
      "Both arguments for %s must be either xs:unsignedLong or xs:long or a subtype of those, but was %s.",
      nameAsParsed,
      argInherentType.globalQName,
    )
    argInherentType
  }
  override def targetTypeForSubexpression(subexp: Expression): NodeInfo.Kind = inherentType

  override def compiledDPath: CompiledDPath = {
    checkArgCount(2)
    val argType = inherentType
    val arg0Recipe = args(0).compiledDPath
    val arg1Recipe = args(1).compiledDPath
    val c = conversions
    val res = new CompiledDPath(constructor(List(arg0Recipe, arg1Recipe), argType) +: c)
    res
  }
}

case class DFDLXBitUnaryExpr(
  nameAsParsed: String,
  fnQName: RefQName,
  args: List[Expression],
  constructor: (CompiledDPath, NodeInfo.Kind) => RecipeOp,
) extends FunctionCallBase(nameAsParsed, fnQName, args) {
  override lazy val inherentType = {
    val arg0Type = args(0).inherentType
    schemaDefinitionUnless(
      arg0Type.isSubtypeOf(NodeInfo.PrimType.UnsignedLong) || arg0Type.isSubtypeOf(
        NodeInfo.PrimType.Long,
      ),
      "The argument passed for %s must be either xs:unsignedLong or xs:long or a subtype of those, but was %s.",
      nameAsParsed,
      arg0Type.globalQName,
    )
    arg0Type
  }
  override def targetTypeForSubexpression(subexp: Expression): NodeInfo.Kind = inherentType

  override def compiledDPath: CompiledDPath = {
    checkArgCount(1)
    val argType = inherentType
    val arg0Recipe = args(0).compiledDPath
    val c = conversions
    val res = new CompiledDPath(constructor(arg0Recipe, argType) +: c)
    res
  }
}

case class FNZeroArgExpr(
  nameAsParsed: String,
  fnQName: RefQName,
  resultType: NodeInfo.Kind,
  argType: NodeInfo.Kind,
  constructor: (CompiledDPath, NodeInfo.Kind) => RecipeOp,
) extends FunctionCallBase(nameAsParsed, fnQName, Nil) {

  override lazy val inherentType = resultType

  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind = argType

  override lazy val compiledDPath = {
    checkArgCount(0)
    val c = conversions
    val res = new CompiledDPath(constructor(null, null) +: c)
    res
  }
}

case class FNOneArgExpr(
  nameAsParsed: String,
  fnQName: RefQName,
  args: List[Expression],
  resultType: NodeInfo.Kind,
  argType: NodeInfo.Kind,
  constructor: (CompiledDPath, NodeInfo.Kind) => RecipeOp,
) extends FunctionCallBase(nameAsParsed, fnQName, args) {

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

case class FNOneArgExprConversionDisallowed(
  nameAsParsed: String,
  fnQName: RefQName,
  args: List[Expression],
  resultType: NodeInfo.Kind,
  argType: NodeInfo.Kind,
  constructor: (CompiledDPath, NodeInfo.Kind) => RecipeOp,
) extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val inherentType = resultType

  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind = argType

  override lazy val conversions = Nil

  override lazy val compiledDPath = {
    checkArgCount(1)
    val arg0Recipe = args(0).compiledDPath
    val arg0Type = args(0).inherentType
    val c = Nil // conversions
    val res = new CompiledDPath(constructor(arg0Recipe, arg0Type) +: c)
    res
  }
}

abstract class FNTwoArgsExprBase(
  nameAsParsed: String,
  fnQName: RefQName,
  args: List[Expression],
  resultType: NodeInfo.Kind,
  arg1Type: NodeInfo.Kind,
  arg2Type: NodeInfo.Kind,
  constructor: List[CompiledDPath] => RecipeOp,
) extends FunctionCallBase(nameAsParsed, fnQName, args) {

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
    val recipe = constructor(List(arg1Recipe, arg2Recipe))
    val res = new CompiledDPath(recipe +: conversions)
    res
  }
}

case class FNTwoArgsExpr(
  nameAsParsed: String,
  fnQName: RefQName,
  args: List[Expression],
  resultType: NodeInfo.Kind,
  arg1Type: NodeInfo.Kind,
  arg2Type: NodeInfo.Kind,
  constructor: List[CompiledDPath] => RecipeOp,
) extends FNTwoArgsExprBase(
    nameAsParsed,
    fnQName,
    args,
    resultType,
    arg1Type,
    arg2Type,
    constructor,
  )

/*
 * Used when the underlying constructor does not inherantly know its arguement types will be.
 * Note that we do not actually "infer" the types here, as we can get them from the args parameter
 * as easily as the caller, and the caller might be aware of more constraints than we are.
 * (Eg. in the DFDLXTypeInputCalc functions, only the second parameter is infered, the first
 * must be a String).
 *
 * Note that, for arguements that should be inferred, the caller should use arg.inherentType,
 * not arg.targetType, as the latter will look to this class to make its determination.
 */
case class FNTwoArgsExprInferedArgType(
  nameAsParsed: String,
  fnQName: RefQName,
  args: List[Expression],
  resultType: NodeInfo.Kind,
  arg1Type: NodeInfo.Kind,
  arg2Type: NodeInfo.Kind,
  constructor: List[(CompiledDPath, NodeInfo.Kind)] => RecipeOp,
) extends {
    private val constructor_ : List[CompiledDPath] => RecipeOp =
      (subExprs: List[CompiledDPath]) => {
        Assert.invariant(subExprs.length == args.length)
        val types = args.map(_.targetType)
        val typedSubExprs = subExprs.zip(types)
        constructor(typedSubExprs)
      }
  }
  with FNTwoArgsExprBase(
    nameAsParsed,
    fnQName,
    args,
    resultType,
    arg1Type,
    arg2Type,
    constructor_,
  )

sealed abstract class LengthExprBase(
  nameAsParsed: String,
  fnQName: RefQName,
  args: List[Expression],
  resultType: NodeInfo.Kind,
  arg1Type: NodeInfo.Kind,
  arg2Type: NodeInfo.Kind,
  constructor: List[CompiledDPath] => RecipeOp,
) extends FNTwoArgsExprBase(
    nameAsParsed,
    fnQName,
    args,
    resultType,
    arg1Type,
    arg2Type,
    constructor,
  ) {

  protected final def leafReferencedElements = {
    val path = args(0) match {
      case arg: PathExpression => arg
      case _ => {
        // $COVERAGE-OFF$
        Assert.invariantFailed("NodeInfo.Exists, but arg was not a PathExpression.")
        // $COVERAGE-ON$
      }
    }
    if (path.isPathToOneWholeArray)
      SDE(s"First argument to ${fnQName} cannot be a path to an array")
    val steps = path.steps
    val lst = steps.last
    val res = lst.stepElements.toSet
    res
  }
}

case class ContentLengthExpr(
  nameAsParsed: String,
  fnQName: RefQName,
  args: List[Expression],
  resultType: NodeInfo.Kind,
  arg1Type: NodeInfo.Kind,
  arg2Type: NodeInfo.Kind,
  constructor: List[CompiledDPath] => RecipeOp,
) extends LengthExprBase(
    nameAsParsed,
    fnQName,
    args,
    resultType,
    arg1Type,
    arg2Type,
    constructor,
  ) {

  override lazy val leafContentLengthReferencedElements = leafReferencedElements
}

case class ValueLengthExpr(
  nameAsParsed: String,
  fnQName: RefQName,
  args: List[Expression],
  resultType: NodeInfo.Kind,
  arg1Type: NodeInfo.Kind,
  arg2Type: NodeInfo.Kind,
  constructor: List[CompiledDPath] => RecipeOp,
) extends LengthExprBase(
    nameAsParsed,
    fnQName,
    args,
    resultType,
    arg1Type,
    arg2Type,
    constructor,
  ) {

  override lazy val leafValueLengthReferencedElements = leafReferencedElements
}

case class FNThreeArgsExpr(
  nameAsParsed: String,
  fnQName: RefQName,
  args: List[Expression],
  resultType: NodeInfo.Kind,
  arg1Type: NodeInfo.Kind,
  arg2Type: NodeInfo.Kind,
  arg3Type: NodeInfo.Kind,
  constructor: List[CompiledDPath] => RecipeOp,
) extends FunctionCallBase(nameAsParsed, fnQName, args) {

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
    val res = new CompiledDPath(constructor(List(arg1Path, arg2Path, arg3Path)) +: c)
    res
  }
}

case class XSConverterExpr(
  nameAsParsed: String,
  fnQName: RefQName,
  args: List[Expression],
  resultType: NodeInfo.Kind,
) extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val inherentType = resultType

  /*
   * By using the result type as the target for the expression, conversions will
   * do the work of converting into that type. We don't need to put down any
   * additional recipe operator to convert anything.
   */
  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind =
    resultType // NodeInfo.AnyType

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

case class FNArgListExpr(
  nameAsParsed: String,
  fnQName: RefQName,
  args: List[Expression],
  resultType: NodeInfo.Kind,
  argType: NodeInfo.Kind,
  constructor: List[CompiledDPath] => RecipeOp,
) extends FunctionCallBase(nameAsParsed, fnQName, args) {

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

  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind =
    NodeInfo.String

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
    schemaDefinitionUnless(
      args.length == 0,
      "Function %s takes no arguments.",
      fnQName.toPrettyString,
    )
    new CompiledDPath(DFDLOccursIndex +: conversions)
  }

  override lazy val inherentType = NodeInfo.ArrayIndex
  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind =
    Assert.usageError("No subexpressions")
}

case class DFDLTestBitExpr(nameAsParsed: String, fnQName: RefQName, args: List[Expression])
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

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

case class DFDLSetBitsExpr(nameAsParsed: String, fnQName: RefQName, args: List[Expression])
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val inherentType = NodeInfo.UnsignedByte
  override def targetTypeForSubexpression(subexpr: Expression): NodeInfo.Kind =
    NodeInfo.UnsignedByte

  override lazy val compiledDPath = {
    checkArgCount(8)
    new CompiledDPath(DFDLSetBits(args.map { _.compiledDPath }) +: conversions)
  }
}

case class DFDLCheckConstraintsExpr(
  nameAsParsed: String,
  fnQName: RefQName,
  args: List[Expression],
) extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val children = args

  override lazy val compiledDPath = {
    checkArgCount(1)
    val argDPath = args(0).compiledDPath
    val c = conversions
    val res = new CompiledDPath(DFDLCheckConstraints(argDPath) +: c)
    res
  }
  override def targetTypeForSubexpression(subexpr: Expression): NodeInfo.Kind =
    NodeInfo.AnyAtomic

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
  override lazy val compiledDPath =
    expression.compiledDPath // no conversions because we passed on targetType to subexp
}

case class DFDLXTraceExpr(nameAsParsed: String, fnQName: RefQName, args: List[Expression])
  extends FunctionCallBase(nameAsParsed, fnQName, args) {

  lazy val (realArg, msgText) = args match {
    case List(arg0, LiteralExpression(txt: String)) => (arg0, txt)
    case List(arg0, other) =>
      SDE(
        "The second argument to %n must be a string literal, but was %s.",
        nameAsParsed,
        other.text,
      )
    case _ => {
      argCountErr(2)
    }
  }

  override lazy val inherentType: NodeInfo.Kind = realArg.inherentType

  override def targetTypeForSubexpression(subExp: Expression): NodeInfo.Kind = inherentType

  override lazy val compiledDPath =
    new CompiledDPath(DFDLXTrace(realArg.compiledDPath, msgText) +: conversions)
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

case class UserDefinedFunctionCallExpr(
  nameAsParsed: String,
  fnQName: RefQName,
  args: List[Expression],
  argTypes: List[NodeInfo.Kind],
  resultType: NodeInfo.Kind,
  constructor: (String, List[CompiledDPath]) => RecipeOp,
) extends FunctionCallBase(nameAsParsed, fnQName, args) {

  override lazy val inherentType = resultType

  lazy val argToArgType = {
    /*
     * Note that this checkArgCount is necessary as zip will mask any length
     * inconsistencies. Putting the check in $compiledDPath ought to, but doesn't
     * result in an SDE being thrown.
     */
    checkArgCount(argTypes.length)
    (args.zip(argTypes)).toMap
  }

  override def targetTypeForSubexpression(childExpr: Expression): NodeInfo.Kind = {
    argToArgType.get(childExpr) match {
      case Some(tt) => tt
      case None => Assert.invariantFailed("subexpression isn't of the expected type.")
    }
  }

  override lazy val compiledDPath = {
    checkArgCount(argTypes.length)
    val recipes = args.map { _.compiledDPath }
    val res = new CompiledDPath(constructor(nameAsParsed, recipes) +: conversions)
    res
  }
}
