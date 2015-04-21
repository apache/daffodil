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

import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.processors.DIArray
import edu.illinois.ncsa.daffodil.processors.DIComplex
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.dsom.SimpleTypeBase
import edu.illinois.ncsa.daffodil.calendar.DFDLCalendar
import edu.illinois.ncsa.daffodil.xml.GlobalQName
import edu.illinois.ncsa.daffodil.xml.QName
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.RefQName
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.processors.TextCalendarConstants

/**
 * We need to have a data structure that lets us represent a type, and
 * its relationship (conversion, subtyping) to other types.
 *
 * This is what TypeNodes are for. They are linked into a graph that
 * can answer questions about how two types are related. It can find the
 * least general supertype, or most general subtype of two types.
 */
sealed abstract class TypeNode(parent: TypeNode, childrenArg: => List[TypeNode]) extends Serializable {
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

  lazy val globalQName: GlobalQName = QName.createGlobal(name, XMLUtils.XSD_NAMESPACE)
}

/*
 * Used to define primitive type objects. We often need to 
 * deal with just the primitive types exclusive of all the abstract 
 * types (like AnyAtomic, or AnyDateTimeType) that surround them. 
 */
sealed abstract class PrimTypeNode(parent: TypeNode, childrenArg: => List[TypeNode])
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
 *
 * Note that you can talk about types using type node objects: E.g., NodeInfo.Number.
 * But you can also use Scala typing to ask whether a particular type object is
 * a subtype of another: e.g.
 * <pre>
 * val x = NodeInfo.String
 * val aa = NodeInfo.AnyAtomic
 * x.isSubTypeOf(aa) // true. Ordinary way to check. Navigates our data structure.
 * x.isInstanceOf[NodeInfo.AnyAtomic.Kind] // true. Uses scala type checking
 * </pre>
 * So each NodeInfo object has a corresponding class (named with .Kind suffix)
 * which is actually a super-type (in Scala sense) of the enums for the types
 * beneath.
 * <p>
 * The primary advantage of the latter is that this is a big bunch of sealed traits/classes,
 * so if you have a match-case analysis by type, scala's compiler can tell you
 * if your match-case exhausts all possibilities and warn you if it does not.
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
    def primitiveType = this

    def fromXMLString(s: String): Any
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
      if (this eq other) true
      else if (this.isInstanceOf[NodeInfo.String.Kind] &&
        other.isInstanceOf[NodeInfo.String.Kind]) true
      else if (NodeInfo.isXDerivedFromY(this.name, other.name)) true
      else false
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

  /**
   * An isolated singleton "type" which is used as a target type for
   * the indexing operation.
   */
  protected sealed trait ArrayKind extends NodeInfo.Kind
  case object Array extends TypeNode(null, Nil) with ArrayKind {
    sealed trait Kind extends ArrayKind
  }

  /**
   * AnyType is the Top of the type lattice. It is the super type of all data
   * types except some special singleton types like ArrayType.
   */
  protected sealed trait AnyTypeKind extends NodeInfo.Kind
  case object AnyType extends TypeNode(null, List(Nillable)) with AnyTypeKind {
    sealed trait Kind extends AnyTypeKind
  }

  /**
   * Nothing is the bottom of the type lattice.
   *
   * It is the return type of the daf:error() function. It's a subtype of
   * every type (except some special singletons like ArrayType).
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

  /**
   * All complex types are represented by this one type object.
   */
  protected sealed trait ComplexKind extends AnyType.Kind
  case object Complex extends TypeNode(AnyType, Nil) with ComplexKind {
    type Kind = ComplexKind
  }

  /**
   * There is nothing corresponding to NillableKind in the DFDL/XML Schema type
   * hierarchy. We have it as a parent of both complex type and simple type since
   * both of them can be nillable.
   */
  protected sealed trait NillableKind extends AnyType.Kind
  case object Nillable extends TypeNode(AnyType, List(Complex, AnySimpleType)) with NillableKind {
    type Kind = NillableKind
  }

  /**
   * For things like fn:exists fn:empty
   */
  protected sealed trait ExistsKind extends AnyType.Kind
  case object Exists extends TypeNode(AnyType, Nil) with NillableKind {
    type Kind = ExistsKind
  }

  /**
   * It might be possible to combine AnySimpleType and AnyAtomic, but both
   * terminologies are used. In DFDL we don't talk of Atomic's much, but
   * lots of XPath and XML Schema materials do, so we have these two types
   * that are very similar really.
   */
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

  /**
   * NonEmptyString is used for the special case where DFDL properties can
   * have expressions that compute their values which are strings, but those
   * strings aren't allowed to be empty strings. Also for properties that simply
   * arent allowed to be empty strings (e.g. padChar).
   */
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

  /**
   * The PrimType objects are a child enum within the overall NodeInfo
   * enum.
   */
  object PrimType {

    def fromRefQName(refQName: RefQName): Option[PrimType] = {
      allPrims.find { prim => refQName.matches(prim.globalQName) }
    }

    def fromNameString(name: String): Option[PrimType] = {
      val m: Option[PrimType] = allPrims.find { _.pname.toLowerCase == name.toLowerCase }
      m
    }

    protected sealed trait FloatKind extends SignedNumeric.Kind
    case object Float extends PrimTypeNode(SignedNumeric, Nil) with FloatKind {
      type Kind = FloatKind
      override def fromXMLString(s: String) = s.toFloat
    }

    protected sealed trait DoubleKind extends SignedNumeric.Kind
    case object Double extends PrimTypeNode(SignedNumeric, Nil) with DoubleKind {
      type Kind = DoubleKind
      override def fromXMLString(s: String) = s.toDouble
    }

    protected sealed trait DecimalKind extends SignedNumeric.Kind
    case object Decimal extends PrimTypeNode(SignedNumeric, List(Integer)) with DecimalKind {
      type Kind = DecimalKind
      override def fromXMLString(s: String) = BigDecimal(s)
    }

    protected sealed trait IntegerKind extends SignedInteger.Kind
    case object Integer extends PrimTypeNode(Decimal, List(Long, NonNegativeInteger)) with IntegerKind {
      type Kind = IntegerKind
      override def fromXMLString(s: String) = BigInt(s)
    }

    protected sealed trait LongKind extends Integer.Kind
    case object Long extends PrimTypeNode(Integer, List(Int)) with LongKind {
      type Kind = LongKind
      override def fromXMLString(s: String) = s.toLong
    }

    protected sealed trait IntKind extends Long.Kind
    case object Int extends PrimTypeNode(Long, List(Short)) with IntKind {
      type Kind = IntKind
      override def fromXMLString(s: String) = s.toInt
    }

    protected sealed trait ShortKind extends Int.Kind
    case object Short extends PrimTypeNode(Int, List(Byte)) with ShortKind {
      type Kind = ShortKind
      override def fromXMLString(s: String) = s.toShort
    }

    protected sealed trait ByteKind extends Short.Kind
    case object Byte extends PrimTypeNode(Short, Nil) with ByteKind {
      type Kind = ByteKind
      override def fromXMLString(s: String) = s.toByte
    }

    protected sealed trait NonNegativeIntegerKind extends Integer.Kind
    case object NonNegativeInteger extends PrimTypeNode(Integer, List(UnsignedLong)) with NonNegativeIntegerKind {
      type Kind = NonNegativeIntegerKind
      override def fromXMLString(s: String) = BigInt(s)
    }

    protected sealed trait UnsignedLongKind extends NonNegativeInteger.Kind
    case object UnsignedLong extends PrimTypeNode(NonNegativeInteger, List(UnsignedInt)) with UnsignedLongKind {
      type Kind = UnsignedLongKind
      val Max = BigInt("18446744073709551615")
      override def fromXMLString(s: String) = BigInt(s)
    }

    protected sealed trait UnsignedIntKind extends UnsignedLong.Kind
    case object UnsignedInt extends PrimTypeNode(UnsignedLong, List(UnsignedShort, ArrayIndex)) with UnsignedIntKind {
      type Kind = UnsignedIntKind
      val Max = 4294967295L
      override def fromXMLString(s: String) = s.toLong
    }

    protected sealed trait UnsignedShortKind extends UnsignedInt.Kind
    case object UnsignedShort extends PrimTypeNode(UnsignedInt, List(UnsignedByte)) with UnsignedShortKind {
      type Kind = UnsignedShortKind
      val Max = 65535
      override def fromXMLString(s: String) = s.toInt
    }

    protected sealed trait UnsignedByteKind extends UnsignedShort.Kind
    case object UnsignedByte extends PrimTypeNode(UnsignedShort, Nil) with UnsignedByteKind {
      type Kind = UnsignedByteKind
      val Max = 255
      override def fromXMLString(s: String) = s.toShort
    }

    protected sealed trait StringKind extends AnyAtomic.Kind
    case object String extends PrimTypeNode(AnyAtomic, List(NonEmptyString)) with StringKind {
      type Kind = StringKind
      override def fromXMLString(s: String) = s
    }

    protected sealed trait BooleanKind extends AnySimpleType.Kind
    case object Boolean extends PrimTypeNode(AnyAtomic, Nil) with BooleanKind {
      type Kind = BooleanKind
      override def fromXMLString(s: String) = s.toBoolean
    }

    protected sealed trait HexBinaryKind extends Opaque.Kind
    case object HexBinary extends PrimTypeNode(Opaque, Nil) with HexBinaryKind {
      type Kind = HexBinaryKind
      override def fromXMLString(s: String): Any = Misc.hex2Bytes(s)
    }

    protected sealed trait DateKind extends AnyDateTimeKind
    case object Date extends PrimTypeNode(AnyDateTime, Nil) with DateKind {
      type Kind = DateKind
      override def fromXMLString(s: String): Any = {
        TextCalendarConstants.tlDateInfosetFormatter.get.parse(s)
      }
    }

    protected sealed trait DateTimeKind extends AnyDateTimeKind
    case object DateTime extends PrimTypeNode(AnyDateTime, Nil) with DateTimeKind {
      type Kind = DateTimeKind
      override def fromXMLString(s: String): Any = {
        TextCalendarConstants.tlDateTimeInfosetFormatter.get.parse(s)
      }
    }

    protected sealed trait TimeKind extends AnyDateTimeKind
    case object Time extends PrimTypeNode(AnyDateTime, Nil) with TimeKind {
      type Kind = TimeKind
      override def fromXMLString(s: String): Any = {
        TextCalendarConstants.tlTimeInfosetFormatter.get.parse(s)
      }
    }
  }

  import PrimType._
  //
  // The below must be lazy vals because of the recursion between this
  // list and the definition of these type objects above.
  //
  private lazy val allAbstractTypes = List(
    AnyType, Nillable, AnySimpleType, AnyAtomic, Exists,
    Numeric, SignedNumeric, UnsignedNumeric, SignedInteger,
    // There is no UnsignedInteger because the concrete type 
    // NonNegativeInteger plays that role.
    Opaque, AnyDateTime, Nothing)
  private lazy val allDFDLTypes = List(
    Float, Double, Decimal, Integer, Long, Int, Short, Byte,
    NonNegativeInteger, UnsignedLong, UnsignedInt, UnsignedShort, UnsignedByte,
    String, Boolean, HexBinary,
    Date, Time, DateTime)

  lazy val allTypes =
    allDFDLTypes ++ List(Complex, ArrayIndex, NonEmptyString) ++ allAbstractTypes

}
