package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.dsom.PrimType
import edu.illinois.ncsa.daffodil.dsom.SimpleTypeGraph
import edu.illinois.ncsa.daffodil.dsom.SimpleTypeNode
import edu.illinois.ncsa.daffodil.dsom.RuntimePrimType
import edu.illinois.ncsa.daffodil.processors.DIArray
import edu.illinois.ncsa.daffodil.processors.DIComplex
import edu.illinois.ncsa.daffodil.exceptions.Assert
import com.ibm.icu.util.DFDLCalendar
import edu.illinois.ncsa.daffodil.dsom.SimpleTypeGraph
import edu.illinois.ncsa.daffodil.dsom.SimpleTypeNode
import edu.illinois.ncsa.daffodil.dsom.RuntimePrimType
import edu.illinois.ncsa.daffodil.dsom.SimpleTypeGraph
import edu.illinois.ncsa.daffodil.dsom.SimpleTypeNode
import edu.illinois.ncsa.daffodil.dsom.RuntimePrimType
import edu.illinois.ncsa.daffodil.dsom.SimpleTypeGraph
import edu.illinois.ncsa.daffodil.dsom.SimpleTypeNode

/**
 * A NodeInfo.Kind describes what kind of result we want from the expression.
 * E.g., a + b we want numbers from both a and b expressions. In /a/b we want
 * a "complex" node from the expression a, and a value from the
 * expression b. But fn:exists(../a/b) we need complex node a so that we can test if b
 * exists. In case of a[b] for b we need a value, but furthermore an array index, so 1..n.
 *
 * Functions motivate some of the options here. e.g., fn:nilled( exp ) is the test
 * for a nilled value. There we want an expression to something that is nillable.
 *
 * This same Kind is also used to describe the inherent value (bottom up) of an
 * expression. So a literal "foo" is of string kind, whereas 5.0 is Numeric
 * <p>
 * The nested objects here allow one to write
 * NodeInfo.Kind (type of any of these enums)
 * NodeInfo.Number.Kind (type of just the number variants)
 * NodeInfo.Value.Kind (type of just the value variants)
 * The enums themselves are just NodeInfo.Decimal (for example)
 */
object NodeInfo extends Enum {
  sealed trait Kind extends EnumValueType {
    def name: String = Misc.getNameFromClass(this)

    def isSubtypeOf(other: NodeInfo.Kind): Boolean = {
      //
      // FIXME this is a temporary workaround so not all the property tests
      // will fail - mostly the properties that take expressions
      // take NonEmptyString, but we don't have a conversion 
      // yet
      //
      if (this.isInstanceOf[NodeInfo.String.Kind] &
        other.isInstanceOf[NodeInfo.String.Kind]) true
      else if (SimpleTypeGraph.isXDerivedFromY(this.name, other.name)) return true
      //      SimpleTypeGraph.canConvertXToY(this.name, other.name)
      false
    }
  }

  // FIXME: duplicates code in DPathUtil.convertTypeString
  // only need one of these case tables. Ideally, PrimType becomes
  // a non-SchemaComponent, and then the distinctino between PrimType and NodeInfo.Kind
  // disappears.
  //
  // FIXME: Also look at SimpleTypeNode, which is yet another graph of simple types
  // That seems isomorphic to NodeInfo.
  //
  def fromPrimType(pt: RuntimePrimType) = {
    pt match {
      case RuntimePrimType.String => NodeInfo.String
      case RuntimePrimType.Int => NodeInfo.Int
      case RuntimePrimType.Byte => NodeInfo.Byte
      case RuntimePrimType.Short => NodeInfo.Short
      case RuntimePrimType.Long => NodeInfo.Long
      case RuntimePrimType.Integer => NodeInfo.Integer
      case RuntimePrimType.Decimal => NodeInfo.Decimal
      case RuntimePrimType.UInt => NodeInfo.UnsignedInt
      case RuntimePrimType.UByte => NodeInfo.UnsignedByte
      case RuntimePrimType.UShort => NodeInfo.UnsignedShort
      case RuntimePrimType.ULong => NodeInfo.UnsignedLong
      case RuntimePrimType.NonNegativeInteger => NodeInfo.NonNegativeInteger
      case RuntimePrimType.Double => NodeInfo.Double
      case RuntimePrimType.Float => NodeInfo.Float
      case RuntimePrimType.HexBinary => NodeInfo.HexBinary
      case RuntimePrimType.Boolean => NodeInfo.Boolean
      case RuntimePrimType.DateTime => NodeInfo.DateTime
      case RuntimePrimType.Date => NodeInfo.Date
      case RuntimePrimType.Time => NodeInfo.Time
    }
  }

  def fromObject(a: Any) = {
    a match {
      case x: String => NodeInfo.String
      case x: Int => NodeInfo.Int
      case x: Byte => NodeInfo.Byte
      case x: Short => NodeInfo.Short
      case x: Long => NodeInfo.Long
      case x: BigInt => NodeInfo.Integer
      case x: BigDecimal => NodeInfo.Decimal
      case x: Double => NodeInfo.Double
      case x: Float => NodeInfo.Float
      case x: Array[Byte] => NodeInfo.HexBinary
      case x: Boolean => NodeInfo.Boolean
      case x: DFDLCalendar => NodeInfo.DateTime
      case _ => Assert.usageError("Unsupported object representation type: %s".format(a))
    }
  }

  def generalize(aExpr: Expression, bExpr: Expression): NodeInfo.Kind = {
    val a = aExpr.inherentType
    val b = bExpr.inherentType
    if (a == b) a
    else if (a.isSubtypeOf(b)) b
    else if (b.isSubtypeOf(a)) a
    else
      (a, b) match {
        case (s: NodeInfo.String.Kind, _) => NodeInfo.String
        case (_, s: NodeInfo.String.Kind) => NodeInfo.String
        case (NodeInfo.Float, NodeInfo.Double) => NodeInfo.Double
        case (NodeInfo.Double, NodeInfo.Float) => NodeInfo.Double
        case (NodeInfo.Decimal, NodeInfo.Double) => NodeInfo.Decimal
        case (NodeInfo.Double, NodeInfo.Decimal) => NodeInfo.Decimal
        case (NodeInfo.Boolean, bt: NodeInfo.Numeric.Kind) => bt
        case (bt: NodeInfo.Numeric.Kind, NodeInfo.Boolean) => bt
        case (it: NodeInfo.Long.Kind, NodeInfo.ArrayIndex) => NodeInfo.ArrayIndex
        case _ => aExpr.SDE("Static type error: expressions '%s' and '%s' have incompatible types %s and %s.", aExpr.text, bExpr.text, a, b)
      }
  }

  protected sealed trait ArrayKind extends NodeInfo.Kind
  case object Array extends SimpleTypeNode(null, Nil) with ArrayKind {
    sealed trait Kind extends ArrayKind
  }

  protected sealed trait AnyTypeKind extends NodeInfo.Kind
  case object AnyType extends SimpleTypeNode(null, List(Nillable)) with AnyTypeKind {
    sealed trait Kind extends AnyTypeKind
  }

  /**
   * This is the return type of the daf:error() function. It's a subtype of
   * everything.
   */
  case object Nothing
    extends SimpleTypeNode(
      AnyType, List(
        Boolean,
        Complex, Nillable, Array, ArrayIndex,
        Double, Float,
        Date, Time, DateTime,
        UnsignedByte, Byte,
        HexBinary,
        String, NonEmptyString))
    with Boolean.Kind with Complex.Kind with Nillable.Kind with Array.Kind
    with ArrayIndex.Kind with Double.Kind with Float.Kind with Date.Kind with Time.Kind with DateTime.Kind with UnsignedByte.Kind with Byte.Kind with HexBinary.Kind with NonEmptyString.Kind

  protected sealed trait ComplexKind extends AnyType.Kind
  case object Complex extends SimpleTypeNode(AnyType, Nil) with ComplexKind {
    type Kind = ComplexKind
  }

  protected sealed trait NillableKind extends AnyType.Kind
  case object Nillable extends SimpleTypeNode(AnyType, List(Complex, AnySimpleType)) with NillableKind {
    type Kind = NillableKind
  }

  protected sealed trait AnySimpleTypeKind extends Nillable.Kind

  case object AnySimpleType extends SimpleTypeNode(Nillable, List(AnyAtomic)) with AnySimpleTypeKind {
    type Kind = AnySimpleTypeKind
  }
  protected sealed trait AnyAtomicKind extends AnySimpleType.Kind
  case object AnyAtomic extends SimpleTypeNode(AnySimpleType, List(String, Numeric, Boolean, Opaque, AnyDateTime)) with AnyAtomicKind {
    type Kind = AnyAtomicKind
  }

  protected sealed trait NumericKind extends AnyAtomic.Kind
  case object Numeric extends SimpleTypeNode(AnyAtomic, List(SignedNumeric, UnsignedNumeric)) with NumericKind {
    type Kind = NumericKind
  }

  protected sealed trait SignedNumericKind extends Numeric.Kind
  case object SignedNumeric extends SimpleTypeNode(Numeric, List(Float, Double, Decimal, SignedInteger)) with SignedNumericKind {
    type Kind = SignedNumericKind
  }

  protected sealed trait UnsignedNumericKind extends Numeric.Kind
  case object UnsignedNumeric extends SimpleTypeNode(Numeric, List(NonNegativeInteger)) with UnsignedNumericKind {
    type Kind = UnsignedNumericKind
  }

  protected sealed trait FloatKind extends SignedNumeric.Kind
  case object Float extends SimpleTypeNode(SignedNumeric, Nil) with FloatKind {
    type Kind = FloatKind
  }

  protected sealed trait DoubleKind extends SignedNumeric.Kind
  case object Double extends SimpleTypeNode(SignedNumeric, Nil) with DoubleKind {
    type Kind = DoubleKind
  }

  protected sealed trait DecimalKind extends SignedNumeric.Kind
  case object Decimal extends SimpleTypeNode(SignedNumeric, List(Integer)) with DecimalKind {
    type Kind = DecimalKind
  }

  protected sealed trait SignedIntegerKind extends SignedNumeric.Kind
  case object SignedInteger extends SimpleTypeNode(SignedNumeric, List(Integer)) with SignedIntegerKind {
    type Kind = SignedIntegerKind
  }

  protected sealed trait IntegerKind extends SignedInteger.Kind
  case object Integer extends SimpleTypeNode(Decimal, List(Long, NonNegativeInteger)) with IntegerKind {
    type Kind = IntegerKind
  }

  protected sealed trait LongKind extends Integer.Kind
  case object Long extends SimpleTypeNode(Integer, List(Int)) with LongKind {
    type Kind = LongKind
  }

  protected sealed trait IntKind extends Long.Kind
  case object Int extends SimpleTypeNode(Long, List(Short)) with IntKind {
    type Kind = IntKind
  }

  protected sealed trait ShortKind extends Int.Kind
  case object Short extends SimpleTypeNode(Int, List(Byte)) with ShortKind {
    type Kind = ShortKind
  }

  protected sealed trait ByteKind extends Short.Kind
  case object Byte extends SimpleTypeNode(Short, Nil) with ByteKind {
    type Kind = ByteKind
  }

  protected sealed trait NonNegativeIntegerKind extends Integer.Kind
  case object NonNegativeInteger extends SimpleTypeNode(Integer, List(UnsignedLong)) with NonNegativeIntegerKind {
    type Kind = NonNegativeIntegerKind
  }
  protected sealed trait UnsignedLongKind extends NonNegativeInteger.Kind
  case object UnsignedLong extends SimpleTypeNode(NonNegativeInteger, List(UnsignedInt)) with UnsignedLongKind {
    type Kind = UnsignedLongKind
  }
  protected sealed trait UnsignedIntKind extends UnsignedLong.Kind
  case object UnsignedInt extends SimpleTypeNode(UnsignedLong, List(UnsignedShort, ArrayIndex)) with UnsignedIntKind {
    type Kind = UnsignedIntKind
  }
  protected sealed trait UnsignedShortKind extends UnsignedInt.Kind
  case object UnsignedShort extends SimpleTypeNode(UnsignedInt, List(UnsignedByte)) with UnsignedShortKind {
    type Kind = UnsignedShortKind
  }
  protected sealed trait UnsignedByteKind extends UnsignedShort.Kind
  case object UnsignedByte extends SimpleTypeNode(UnsignedShort, Nil) with UnsignedByteKind {
    type Kind = UnsignedByteKind
  }

  protected sealed trait ArrayIndexKind extends UnsignedInt.Kind
  case object ArrayIndex extends SimpleTypeNode(UnsignedInt, Nil) with ArrayIndexKind {
    type Kind = ArrayIndexKind
  }

  protected sealed trait StringKind extends AnyAtomic.Kind
  case object String extends SimpleTypeNode(AnyAtomic, List(NonEmptyString)) with StringKind {
    type Kind = StringKind
  }
  protected sealed trait NonEmptyStringKind extends String.Kind
  case object NonEmptyString extends SimpleTypeNode(String, Nil) with NonEmptyStringKind {
    type Kind = NonEmptyStringKind
  }

  protected sealed trait BooleanKind extends AnySimpleType.Kind
  case object Boolean extends SimpleTypeNode(AnyAtomic, Nil) with BooleanKind {
    type Kind = BooleanKind
  }

  protected sealed trait OpaqueKind extends AnyAtomic.Kind
  case object Opaque extends SimpleTypeNode(AnyAtomic, List(HexBinary)) with OpaqueKind {
    type Kind = OpaqueKind
  }
  protected sealed trait HexBinaryKind extends Opaque.Kind
  case object HexBinary extends SimpleTypeNode(Opaque, Nil) with HexBinaryKind {
    type Kind = HexBinaryKind
  }

  protected sealed trait AnyDateTimeKind extends AnyAtomicKind
  case object AnyDateTime extends SimpleTypeNode(AnyAtomic, List(Date, Time, DateTime)) with AnyDateTimeKind {
    type Kind = AnyDateTimeKind
  }
  protected sealed trait DateKind extends AnyDateTimeKind
  case object Date extends SimpleTypeNode(AnyDateTime, Nil) with DateKind {
    type Kind = DateKind
  }
  protected sealed trait DateTimeKind extends AnyDateTimeKind
  case object DateTime extends SimpleTypeNode(AnyDateTime, Nil) with DateTimeKind {
    type Kind = DateTimeKind
  }
  protected sealed trait TimeKind extends AnyDateTimeKind
  case object Time extends SimpleTypeNode(AnyDateTime, Nil) with TimeKind {
    type Kind = TimeKind
  }

  def allTypes =
    List(AnyType, Complex, Nillable, AnySimpleType, AnyAtomic,
      Numeric, SignedNumeric, UnsignedNumeric,
      Float, Double, Decimal,
      SignedInteger, Integer, Long, Int, Short, Byte,
      NonNegativeInteger, UnsignedLong, UnsignedInt, UnsignedShort, UnsignedByte,
      ArrayIndex,
      String, NonEmptyString,
      Boolean,
      Opaque, HexBinary,
      AnyDateTime, Date, DateTime, Time)
}
