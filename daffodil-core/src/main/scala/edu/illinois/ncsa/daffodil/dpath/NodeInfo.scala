package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.processors.DIArray
import edu.illinois.ncsa.daffodil.processors.DIComplex
import edu.illinois.ncsa.daffodil.exceptions.Assert
import com.ibm.icu.util.DFDLCalendar
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.dsom.SimpleTypeBase

abstract class TypeNode(parent: TypeNode, childrenArg: => List[TypeNode]) extends Serializable {
  def name: String

  // Eliminated a var here. Doing functional graph construction now below.
  lazy val children = childrenArg
  lazy val isHead: Boolean = parent == null
  lazy val lcaseName = name.toLowerCase()

  // names in lower case
  lazy val parentList: List[String] = {
    if (isHead) {
      List(this.lcaseName)
    } else {
      lcaseName :: parent.parentList
    }
  }

  def doesParentListContain(typeName: String): Boolean = {
    val list = parentList.filter(n =>
      n.toLowerCase() == typeName.toLowerCase())
    list.size > 0
  }
}

abstract class PrimTypeNode(parent: TypeNode, childrenArg: => List[TypeNode])
  extends TypeNode(parent, childrenArg) with NodeInfo.PrimType

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

  import PrimType._

  // Primitives are not "global" because they don't appear in any schema document
  sealed trait PrimType
    extends NodeInfo.Kind with SimpleTypeBase {

    /**
     * When class name is isomorphic to the type name, compute automatically.
     */
    protected lazy val pname = {
      val cname = Misc.getNameFromClass(this)
      val first = cname(0).toLower
      val rest = cname.substring(1)
      first + rest
    }

    def isError: Boolean = false
    // def context: SchemaComponent = Assert.usageError("PrimType has no context.")
    def primitiveType = this
  }

  private def getTypeNode(name: String) = {
    allTypes.find(stn => stn.lcaseName == name.toLowerCase())
  }

  def isXDerivedFromY(nameX: String, nameY: String): Boolean = {
    if (nameX == nameY) true
    else {
      getTypeNode(nameX) match {
        case Some(stn) => {
          stn.doesParentListContain(nameY)
        }
        case None => false
      }
    }
  }

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
      else if (NodeInfo.isXDerivedFromY(this.name, other.name)) return true
      false
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
  case object Array extends TypeNode(null, Nil) with ArrayKind {
    sealed trait Kind extends ArrayKind
  }

  protected sealed trait AnyTypeKind extends NodeInfo.Kind
  case object AnyType extends TypeNode(null, List(Nillable)) with AnyTypeKind {
    sealed trait Kind extends AnyTypeKind
  }

  /**
   * This is the return type of the daf:error() function. It's a subtype of
   * everything.
   */
  case object Nothing
    extends TypeNode(
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
  case object Complex extends TypeNode(AnyType, Nil) with ComplexKind {
    type Kind = ComplexKind
  }

  protected sealed trait NillableKind extends AnyType.Kind
  case object Nillable extends TypeNode(AnyType, List(Complex, AnySimpleType)) with NillableKind {
    type Kind = NillableKind
  }

  protected sealed trait AnySimpleTypeKind extends Nillable.Kind

  case object AnySimpleType extends TypeNode(Nillable, List(AnyAtomic)) with AnySimpleTypeKind {
    type Kind = AnySimpleTypeKind
  }
  protected sealed trait AnyAtomicKind extends AnySimpleType.Kind
  case object AnyAtomic extends TypeNode(AnySimpleType, List(String, Numeric, Boolean, Opaque, AnyDateTime)) with AnyAtomicKind {
    type Kind = AnyAtomicKind
  }

  protected sealed trait NumericKind extends AnyAtomic.Kind
  case object Numeric extends TypeNode(AnyAtomic, List(SignedNumeric, UnsignedNumeric)) with NumericKind {
    type Kind = NumericKind
  }

  protected sealed trait SignedNumericKind extends Numeric.Kind
  case object SignedNumeric extends TypeNode(Numeric, List(Float, Double, Decimal, SignedInteger)) with SignedNumericKind {
    type Kind = SignedNumericKind
  }

  protected sealed trait UnsignedNumericKind extends Numeric.Kind
  case object UnsignedNumeric extends TypeNode(Numeric, List(NonNegativeInteger)) with UnsignedNumericKind {
    type Kind = UnsignedNumericKind
  }

  protected sealed trait SignedIntegerKind extends SignedNumeric.Kind
  case object SignedInteger extends TypeNode(SignedNumeric, List(Integer)) with SignedIntegerKind {
    type Kind = SignedIntegerKind
  }
  protected sealed trait OpaqueKind extends AnyAtomic.Kind
  case object Opaque extends TypeNode(AnyAtomic, List(HexBinary)) with OpaqueKind {
    type Kind = OpaqueKind
  }
  protected sealed trait NonEmptyStringKind extends String.Kind
  case object NonEmptyString extends TypeNode(String, Nil) with NonEmptyStringKind {
    type Kind = NonEmptyStringKind
  }
  protected sealed trait ArrayIndexKind extends UnsignedInt.Kind
  case object ArrayIndex extends TypeNode(UnsignedInt, Nil) with ArrayIndexKind {
    type Kind = ArrayIndexKind
  }

  protected sealed trait AnyDateTimeKind extends AnyAtomicKind
  case object AnyDateTime extends TypeNode(AnyAtomic, List(Date, Time, DateTime)) with AnyDateTimeKind {
    type Kind = AnyDateTimeKind
  }

  // One might think these can be def, but scala insists on "stable identifier" 
  // where these are used in case matching.
  val String = PrimType.String
  val Int = PrimType.Int
  val Byte = PrimType.Byte
  val Short = PrimType.Short
  val Long = PrimType.Long
  val Integer = PrimType.Integer
  val Decimal = PrimType.Decimal
  val UnsignedInt = PrimType.UnsignedInt
  val UnsignedByte = PrimType.UnsignedByte
  val UnsignedShort = PrimType.UnsignedShort
  val UnsignedLong = PrimType.UnsignedLong
  val NonNegativeInteger = PrimType.NonNegativeInteger
  val Double = PrimType.Double
  val Float = PrimType.Float
  val HexBinary = PrimType.HexBinary
  val Boolean = PrimType.Boolean
  val DateTime = PrimType.DateTime
  val Date = PrimType.Date
  val Time = PrimType.Time

  val allPrims = List(
    String,
    Int,
    Byte,
    Short,
    Long,
    Integer,
    Decimal,
    UnsignedInt,
    UnsignedByte,
    UnsignedShort,
    UnsignedLong,
    NonNegativeInteger,
    Double,
    Float,
    HexBinary,
    Boolean,
    DateTime,
    Date,
    Time)

  object PrimType {

    def fromNameString(name: String): Option[PrimType] = {
      val m: Option[PrimType] = allPrims.find { _.pname.toLowerCase == name.toLowerCase }
      m
    }

    protected sealed trait FloatKind extends SignedNumeric.Kind
    case object Float extends PrimTypeNode(SignedNumeric, Nil) with FloatKind {
      type Kind = FloatKind
    }

    protected sealed trait DoubleKind extends SignedNumeric.Kind
    case object Double extends PrimTypeNode(SignedNumeric, Nil) with DoubleKind {
      type Kind = DoubleKind
    }

    protected sealed trait DecimalKind extends SignedNumeric.Kind
    case object Decimal extends PrimTypeNode(SignedNumeric, List(Integer)) with DecimalKind {
      type Kind = DecimalKind
    }

    protected sealed trait IntegerKind extends SignedInteger.Kind
    case object Integer extends PrimTypeNode(Decimal, List(Long, NonNegativeInteger)) with IntegerKind {
      type Kind = IntegerKind
    }

    protected sealed trait LongKind extends Integer.Kind
    case object Long extends PrimTypeNode(Integer, List(Int)) with LongKind {
      type Kind = LongKind
    }

    protected sealed trait IntKind extends Long.Kind
    case object Int extends PrimTypeNode(Long, List(Short)) with IntKind {
      type Kind = IntKind
    }

    protected sealed trait ShortKind extends Int.Kind
    case object Short extends PrimTypeNode(Int, List(Byte)) with ShortKind {
      type Kind = ShortKind
    }

    protected sealed trait ByteKind extends Short.Kind
    case object Byte extends PrimTypeNode(Short, Nil) with ByteKind {
      type Kind = ByteKind
    }

    protected sealed trait NonNegativeIntegerKind extends Integer.Kind
    case object NonNegativeInteger extends PrimTypeNode(Integer, List(UnsignedLong)) with NonNegativeIntegerKind {
      type Kind = NonNegativeIntegerKind
    }
    protected sealed trait UnsignedLongKind extends NonNegativeInteger.Kind
    case object UnsignedLong extends PrimTypeNode(NonNegativeInteger, List(UnsignedInt)) with UnsignedLongKind {
      type Kind = UnsignedLongKind
    }
    protected sealed trait UnsignedIntKind extends UnsignedLong.Kind
    case object UnsignedInt extends PrimTypeNode(UnsignedLong, List(UnsignedShort, ArrayIndex)) with UnsignedIntKind {
      type Kind = UnsignedIntKind
    }
    protected sealed trait UnsignedShortKind extends UnsignedInt.Kind
    case object UnsignedShort extends PrimTypeNode(UnsignedInt, List(UnsignedByte)) with UnsignedShortKind {
      type Kind = UnsignedShortKind
    }
    protected sealed trait UnsignedByteKind extends UnsignedShort.Kind
    case object UnsignedByte extends PrimTypeNode(UnsignedShort, Nil) with UnsignedByteKind {
      type Kind = UnsignedByteKind
    }

    protected sealed trait StringKind extends AnyAtomic.Kind
    case object String extends PrimTypeNode(AnyAtomic, List(NonEmptyString)) with StringKind {
      type Kind = StringKind
    }

    protected sealed trait BooleanKind extends AnySimpleType.Kind
    case object Boolean extends PrimTypeNode(AnyAtomic, Nil) with BooleanKind {
      type Kind = BooleanKind
    }

    protected sealed trait HexBinaryKind extends Opaque.Kind
    case object HexBinary extends PrimTypeNode(Opaque, Nil) with HexBinaryKind {
      type Kind = HexBinaryKind
    }

    protected sealed trait DateKind extends AnyDateTimeKind
    case object Date extends PrimTypeNode(AnyDateTime, Nil) with DateKind {
      type Kind = DateKind
    }
    protected sealed trait DateTimeKind extends AnyDateTimeKind
    case object DateTime extends PrimTypeNode(AnyDateTime, Nil) with DateTimeKind {
      type Kind = DateTimeKind
    }
    protected sealed trait TimeKind extends AnyDateTimeKind
    case object Time extends PrimTypeNode(AnyDateTime, Nil) with TimeKind {
      type Kind = TimeKind
    }
  }

  import PrimType._
  lazy val allTypes =
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
