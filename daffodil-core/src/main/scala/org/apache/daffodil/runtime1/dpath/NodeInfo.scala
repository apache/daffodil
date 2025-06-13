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

package org.apache.daffodil.runtime1.dpath

import java.io.Serializable
import java.lang.{ Boolean => JBoolean }
import java.lang.{ Byte => JByte }
import java.lang.{ Double => JDouble }
import java.lang.{ Float => JFloat }
import java.lang.{ Integer => JInt }
import java.lang.{ Long => JLong }
import java.lang.{ Short => JShort }
import java.lang.{ String => JString }
import java.math.{ BigDecimal => JBigDecimal }
import java.math.{ BigInteger => JBigInt }
import java.net.URI
import java.net.URISyntaxException

import org.apache.daffodil.api.DFDLPrimType
import org.apache.daffodil.lib.calendar.DFDLDateConversion
import org.apache.daffodil.lib.calendar.DFDLDateTimeConversion
import org.apache.daffodil.lib.calendar.DFDLTimeConversion
import org.apache.daffodil.lib.util.Delay
import org.apache.daffodil.lib.util.Enum
import org.apache.daffodil.lib.util.MaybeInt
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.util.Numbers.asBigDecimal
import org.apache.daffodil.lib.util.Numbers.asBigInt
import org.apache.daffodil.lib.xml.GlobalQName
import org.apache.daffodil.lib.xml.NoNamespace
import org.apache.daffodil.lib.xml.QName
import org.apache.daffodil.lib.xml.RefQName
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.dsom.walker._
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueBigDecimal
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueBigInt
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueBool
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueByte
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueByteArray
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueDate
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueDateTime
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueDouble
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueFloat
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueInt
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueLong
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueNumber
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueShort
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueTime
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueURI

import com.ibm.icu.util.{ Calendar => ICUCalendar }

/**
 * We need to have a data structure that lets us represent a type, and
 * its relationship (conversion, subtyping) to other types.
 *
 * This is what TypeNodes are for. They are linked into a graph that
 * can answer questions about how two types are related. It can find the
 * least general supertype, or most general subtype of two types.
 *
 * We construct this highly-cyclic graph functionally, using lazy evaluation/Delay
 * tricks to allow us to allocate an object that will eventually point at something
 * that points back at this.
 */
sealed abstract class TypeNode private (
  parentsDelay: Delay[Seq[NodeInfo.Kind]],
  childrenDelay: Delay[Seq[NodeInfo.Kind]]
) extends Serializable
  with NodeInfo.Kind {
  def this(parents: => Seq[NodeInfo.Kind], children: => Seq[NodeInfo.Kind]) =
    this(Delay("TypeNode", parents), Delay("TypeNode", children))

  def this(parentArg: => NodeInfo.Kind) =
    this(
      Delay("TypeNode", Seq(parentArg)),
      Delay("TypeNode", Seq[NodeInfo.Kind](NodeInfo.Nothing))
    )

  final override lazy val parents = parentsDelay.value
  final override lazy val children = childrenDelay.value
}

/*
 * Used to define primitive type objects. We often need to
 * deal with just the primitive types exclusive of all the abstract
 * types (like AnyAtomic, or AnyDateTimeType) that surround them.
 */
sealed abstract class PrimTypeNode(
  val dfdlType: DFDLPrimType,
  parent: => NodeInfo.Kind,
  childrenArg: => Seq[NodeInfo.Kind]
) extends TypeNode(Seq(parent), childrenArg)
  with NodeInfo.PrimType {
  def this(javaType: DFDLPrimType, parent: => NodeInfo.Kind) =
    this(javaType, parent, Seq(NodeInfo.Nothing))

}

class InvalidPrimitiveDataException(msg: String, cause: Throwable = null)
  extends Exception(msg, cause)

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
 * The enums themselves are just PrimType.Decimal (for example)
 *
 * Note that you can talk about types using type node objects: E.g., PrimType.Number.
 * But you can also use Scala typing to ask whether a particular type object is
 * a subtype of another: e.g.
 * <pre>
 * val x = PrimType.String
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

  // Primitives are not "global" because they don't appear in any schema document

  sealed trait PrimType extends PrimTypeKind {

    def dfdlType: DFDLPrimType

    /**
     * When class name is isomorphic to the type name (with initial lower case),
     * compute automatically.
     */
    override lazy val name: String = {
      val cname = super.name
      val first = cname(0).toLower
      val rest = cname.substring(1)
      first +: rest
    }

    def isError: Boolean = false

    private lazy val optPrimType_ = Some(this)
    override def optPrimType: Option[PrimType] = optPrimType_

    def fromXMLString(s: String): DataValuePrimitive

    override def toString: String = name
  }

  sealed trait Kind extends EnumValueType with PrimTypeView {

    def name: String = Misc.getNameFromClass(this)

    def parents: Seq[Kind]
    def children: Seq[Kind]

    /**
     * The prim type corresponding to this type.
     * So if this Kind is derived from a PrimType kind, then
     * this kind should return that prim type object.
     *
     * This exists for all Kinds, but is (currently 2024-02-13)
     * only used on Atomic Kinds, so this base never gets called.
     * That's ok. It's needed on this base Kind anyway.
     *
     * @return None if there is no corresponding prim type.
     */
    def optPrimType: Option[PrimType] = None

    private final lazy val selfAndAllParents: Set[Kind] = parents.flatMap {
      _.selfAndAllParents
    }.toSet ++ parents

    def isSubtypeOf(other: Kind): Boolean =
      if (this eq other) true
      else selfAndAllParents.contains(other)

    def doesParentListContain(typeName: String): Boolean = {
      selfAndAllParents.exists { _.name.equalsIgnoreCase(typeName) }
    }

    private val xsScope = <xs:documentation xmlns:xs={
      XMLUtils.XSD_NAMESPACE.uri.toString
    }/>.scope

    // FIXME: this scope has xs prefix bound, but what if that's not the binding
    // the user has in their DFDL schema? What if they use "xsd" or just "x" or
    // whatever? We really need to display the name of primitive types using the
    // prefix the user defines for the XSD namespace.
    //
    lazy val globalQName: GlobalQName = this match {
      case pt: PrimTypeNode => QName.createGlobal(name, XMLUtils.XSD_NAMESPACE, xsScope)
      case _ => QName.createGlobal(name, NoNamespace, scala.xml.TopScope)
    }
  }

  val ClassString = classOf[java.lang.String]
  val ClassIntBoxed = classOf[java.lang.Integer]
  val ClassIntPrim = classOf[scala.Int]
  val ClassByteBoxed = classOf[java.lang.Byte]
  val ClassBytePrim = classOf[scala.Byte]
  val ClassShortBoxed = classOf[java.lang.Short]
  val ClassShortPrim = classOf[scala.Short]
  val ClassLongBoxed = classOf[java.lang.Long]
  val ClassLongPrim = classOf[scala.Long]
  val ClassJBigInt = classOf[java.math.BigInteger]
  val ClassJBigDecimal = classOf[java.math.BigDecimal]
  val ClassDoubleBoxed = classOf[java.lang.Double]
  val ClassDoublePrim = classOf[scala.Double]
  val ClassFloatBoxed = classOf[java.lang.Float]
  val ClassFloatPrim = classOf[scala.Float]
  val ClassPrimByteArray = classOf[Array[scala.Byte]]
  val ClassBooleanBoxed = classOf[java.lang.Boolean]
  val ClassBooleanPrim = classOf[scala.Boolean]

  /**
   * Used to obtain a corresponding DFDL PrimType object given the
   * class of a Java/Scala object.
   *
   * Note how since Java has no unsigned types that we can never
   * select a DFDL unsigned prim type object here.
   *
   * @param jc the java class
   * @return the corresponding DFDL PrimType object
   */
  def fromClass(jc: Class[_]): Option[PrimType] = {
    val ni = jc match {
      case ClassIntBoxed | ClassIntPrim => Some(NodeInfo.Int)
      case ClassByteBoxed | ClassBytePrim => Some(NodeInfo.Byte)
      case ClassShortBoxed | ClassShortPrim => Some(NodeInfo.Short)
      case ClassLongBoxed | ClassLongPrim => Some(NodeInfo.Long)
      case ClassDoubleBoxed | ClassDoublePrim => Some(NodeInfo.Double)
      case ClassFloatBoxed | ClassFloatPrim => Some(NodeInfo.Float)
      case ClassBooleanBoxed | ClassBooleanPrim => Some(NodeInfo.Boolean)
      case ClassString => Some(NodeInfo.String)
      case ClassJBigInt => Some(NodeInfo.Integer)
      case ClassJBigDecimal => Some(NodeInfo.Decimal)
      case ClassPrimByteArray => Some(NodeInfo.HexBinary)
      case _ => None
    }
    ni
  }

  /**
   * An isolated singleton "type" which is used as a target type for
   * the indexing operation.
   */
  protected sealed trait ArrayKind extends NodeInfo.Kind
  case object Array extends TypeNode(Nil, Nil) with ArrayKind {
    sealed trait Kind extends ArrayKind
  }

  /**
   * AnyType is the Top of the type lattice. It is the super type of all data
   * types except some special singleton types like ArrayType.
   */
  protected sealed trait AnyTypeKind extends NodeInfo.Kind
  case object AnyType
    extends TypeNode(Nil, Seq(AnySimpleType, Complex, Exists))
    with AnyTypeKind {
    sealed trait Kind extends AnyTypeKind
  }

  /**
   * Nothing is the bottom of the type lattice.
   *
   * It is the return type of the dfdlx:error() function. It's a subtype of
   * every type (except some special singletons like ArrayType).
   */
  lazy val Nothing = new TypeNode(
    Seq(
      Boolean,
      Complex,
      Array,
      ArrayIndex,
      Double,
      Float,
      Date,
      Time,
      DateTime,
      UnsignedByte,
      Byte,
      HexBinary,
      AnyURI,
      String,
      NonEmptyString
    ),
    Nil
  ) with Boolean.Kind
    with Complex.Kind
    with Array.Kind
    with ArrayIndex.Kind
    with Double.Kind
    with Float.Kind
    with Date.Kind
    with Time.Kind
    with DateTime.Kind
    with UnsignedByte.Kind
    with Byte.Kind
    with HexBinary.Kind
    with NonEmptyString.Kind
    with AnyURI.Kind

  /**
   * All complex types are represented by this one type object.
   */
  protected sealed trait ComplexKind extends AnyType.Kind
  case object Complex extends TypeNode(AnyType) with ComplexKind {
    type Kind = ComplexKind
  }

  /**
   * For things like fn:exists, fn:empty, dfdl:contentLength
   */
  protected sealed trait ExistsKind extends AnyType.Kind
  case object Exists extends TypeNode(AnyType) with AnyTypeKind {
    type Kind = ExistsKind
  }

  /**
   * It might be possible to combine AnySimpleType and AnyAtomic, but both
   * terminologies are used. In DFDL we don't talk of Atomic's much, but
   * lots of XPath and XML Schema materials do, so we have these two types
   * that are very similar really.
   *
   * There is a type union feature in DFDL, and perhaps the difference between
   * AnyAtomic and AnySimpleType is that AnySimpleType admits XSD unions and list types,
   * where AnyAtomic does not?
   */
  protected sealed trait AnySimpleTypeKind extends AnyType.Kind {
    final def primType: PrimType = optPrimType.get
  }
  case object AnySimpleType
    extends TypeNode(Seq(AnyType), Seq(AnyAtomic))
    with AnySimpleTypeKind {
    type Kind = AnySimpleTypeKind
  }

  protected sealed trait AnyAtomicKind extends AnySimpleType.Kind
  case object AnyAtomic
    extends TypeNode(
      Seq(AnySimpleType),
      Seq(String, Numeric, Boolean, Opaque, AnyDateTime, AnyURI)
    )
    with AnyAtomicKind {
    type Kind = AnyAtomicKind
  }

  protected sealed trait NumericKind extends AnyAtomic.Kind
  case object Numeric
    extends TypeNode(Seq(AnyAtomic), Seq(SignedNumeric, UnsignedNumeric))
    with NumericKind {
    type Kind = NumericKind
  }

  protected sealed trait SignedNumericKind extends Numeric.Kind
  case object SignedNumeric
    extends TypeNode(Seq(Numeric), Seq(Float, Double, Decimal))
    with SignedNumericKind {
    type Kind = SignedNumericKind
  }

  protected sealed trait UnsignedNumericKind extends Numeric.Kind
  case object UnsignedNumeric
    extends TypeNode(Seq(Numeric), Seq(NonNegativeInteger))
    with UnsignedNumericKind {
    type Kind = UnsignedNumericKind
  }

  protected sealed trait OpaqueKind extends AnyAtomic.Kind
  case object Opaque extends TypeNode(Seq(AnyAtomic), Seq(HexBinary)) with OpaqueKind {
    type Kind = OpaqueKind
  }

  /**
   * NonEmptyString is used for the special case where DFDL properties can
   * have expressions that compute their values which are strings, but those
   * strings aren't allowed to be empty strings. Also for properties that simply
   * arent allowed to be empty strings (e.g. padChar).
   */
  protected sealed trait NonEmptyStringKind extends String.Kind {
    override def optPrimType: Option[PrimType] = String.optPrimType
  }
  case object NonEmptyString extends TypeNode(String) with NonEmptyStringKind {
    type Kind = NonEmptyStringKind
  }

  protected sealed trait ArrayIndexKind extends UnsignedInt.Kind
  case object ArrayIndex extends TypeNode(UnsignedInt) with ArrayIndexKind {
    type Kind = ArrayIndexKind
  }

  protected sealed trait AnyDateTimeKind extends AnyAtomicKind
  case object AnyDateTime
    extends TypeNode(Seq(AnyAtomic), Seq(Date, Time, DateTime))
    with AnyDateTimeKind {
    type Kind = AnyDateTimeKind
  }

  // One might think these can be def, but scala insists on "stable identifier"
  // where these are used in case matching.
  lazy val String = PrimType.String
  lazy val Int = PrimType.Int
  lazy val Byte = PrimType.Byte
  lazy val Short = PrimType.Short
  lazy val Long = PrimType.Long
  lazy val Integer = PrimType.Integer
  lazy val Decimal = PrimType.Decimal
  lazy val UnsignedInt = PrimType.UnsignedInt
  lazy val UnsignedByte = PrimType.UnsignedByte
  lazy val UnsignedShort = PrimType.UnsignedShort
  lazy val UnsignedLong = PrimType.UnsignedLong
  lazy val NonNegativeInteger = PrimType.NonNegativeInteger
  lazy val Double = PrimType.Double
  lazy val Float = PrimType.Float
  lazy val HexBinary = PrimType.HexBinary
  lazy val AnyURI = PrimType.AnyURI
  lazy val Boolean = PrimType.Boolean
  lazy val DateTime = PrimType.DateTime
  lazy val Date = PrimType.Date
  lazy val Time = PrimType.Time

  protected sealed trait PrimTypeKind extends AnyAtomic.Kind

  /**
   * The PrimType objects are a child enum within the overall NodeInfo
   * enum.
   */
  object PrimType {
    type Kind = PrimTypeKind

    private lazy val nameToPrimType: Map[String, PrimType] = {
      // I'd like to use an immutable Linked hash map here.
      // So that it's deterministic if it is ever iterated.
      // But Scala (2.12 anyway) has no such thing.
      allDFDLTypes.map { ptn => (ptn.name, ptn) }.toMap
    }

    /**
     * given an initial-lower-case NCName, finds the corresonding PrimType
     * or None if there is no correponding primitive type.
     * @param name initial lower case name, without any namespace prefix, of a DFDL type
     * @return
     */
    def fromNameString(name: String): Option[PrimType] = {
      nameToPrimType.get(name)
    }

    def fromQName(qname: RefQName): Option[PrimType] = {
      if (qname.namespace != XMLUtils.XSD_NAMESPACE) None
      else {
        val optPrimNode = fromNameString(qname.local)
        optPrimNode
      }
    }

    trait PrimNonNumeric { self: AnyAtomic.Kind =>
      protected def fromString(s: String): DataValuePrimitive
      def fromXMLString(s: String): DataValuePrimitive = {
        try {
          fromString(s)
        } catch {
          case iae: IllegalArgumentException =>
            throw new InvalidPrimitiveDataException(
              "Value '%s' is not a valid %s: %s".format(s, this.globalQName, iae.getMessage)
            )
          case uri: URISyntaxException =>
            throw new InvalidPrimitiveDataException(
              "Value '%s' is not a valid %s: %s".format(s, this.globalQName, uri.getMessage)
            )
        }
      }
    }

    trait PrimNumeric { self: Numeric.Kind =>
      def isSigned: Boolean
      def minWidth: MaybeInt
      def maxWidth: MaybeInt
      def isValid(n: Number): Boolean
      protected def fromNumberNoCheck(n: Number): DataValueNumber
      def fromNumber(n: Number): DataValueNumber = {
        if (!isValid(n))
          throw new InvalidPrimitiveDataException(
            "Value '%s' is out of range for type: %s".format(n, this.globalQName)
          )
        val num = fromNumberNoCheck(n)
        num
      }

      protected def fromString(s: String): DataValueNumber
      def fromXMLString(s: String): DataValueNumber = {
        val st = s.trim
        val num =
          try {
            fromString(st)
          } catch {
            case nfe: NumberFormatException =>
              throw new InvalidPrimitiveDataException(
                "Value '%s' is not a valid %s".format(st, this.globalQName)
              )
          }

        if (!isValid(num.getNumber))
          throw new InvalidPrimitiveDataException(
            "Value '%s' is out of range for type: %s".format(st, this.globalQName)
          )

        num
      }

      def isInteger: Boolean
    }

    // this should only be used for integer primitives that can fit inside a
    // long (e.g. long, unsignedInt). Primitives larger than that should
    // implement a custom isValid
    trait PrimNumericInteger extends PrimNumeric { self: Numeric.Kind =>
      val min: Long
      val max: Long
      private lazy val minBD = new JBigDecimal(min)
      private lazy val maxBD = new JBigDecimal(max)

      override def isValid(n: Number): Boolean = n match {
        case bd: JBigDecimal => {
          bd.compareTo(minBD) >= 0 && bd.compareTo(maxBD) <= 0
        }
        case bi: JBigInt => {
          if (bi.bitLength > 63) {
            // check against 63 since bitLength() doesn't count the sign bit
            false
          } else {
            // bit length 63 or less means it can convert to a long without any
            // truncation
            val l = bi.longValue
            l >= min && l <= max
          }
        }
        case d: JDouble if d.isInfinite || d.isNaN => false
        case f: JFloat if f.isInfinite || f.isNaN => false
        case _ => {
          val l = n.longValue
          l >= min && l <= max
        }
      }

      override def isInteger = true
    }

    trait PrimNumericFloat extends PrimNumeric { self: Numeric.Kind =>
      def min: Double
      def max: Double
      def minStr: String
      def maxStr: String
      private lazy val minBD = new JBigDecimal(minStr)
      private lazy val maxBD = new JBigDecimal(maxStr)

      def isValid(n: java.lang.Number): Boolean = n match {
        case bd: JBigDecimal => {
          bd.compareTo(minBD) >= 0 && bd.compareTo(maxBD) <= 0
        }
        case bi: JBigInt => {
          val bd = new JBigDecimal(bi)
          bd.compareTo(minBD) >= 0 && bd.compareTo(maxBD) <= 0
        }
        case _ => {
          val d = n.doubleValue
          (d.isNaN || d.isInfinite) || (d >= min && d <= max)
        }
      }

      override def isInteger = false
    }

    protected sealed trait FloatKind extends SignedNumeric.Kind
    case object Float
      extends PrimTypeNode(DFDLPrimType.Float, SignedNumeric)
      with FloatKind
      with PrimNumericFloat
      with FloatView {
      type Kind = FloatKind
      protected override def fromString(s: String): DataValueFloat = {
        val f: JFloat = XMLUtils.strToFloat(s)
        f
      }
      protected override def fromNumberNoCheck(n: Number): DataValueFloat = n.floatValue
      override val min = -JFloat.MAX_VALUE.doubleValue
      override val max = JFloat.MAX_VALUE.doubleValue
      // we cannot use min/max.toString for minStr/maxStr because min/max are doubles so
      // toString would have a precision different than Float.MaxValue.toString
      override val minStr = "-" + JFloat.MAX_VALUE.toString
      override val maxStr = JFloat.MAX_VALUE.toString
      override val isSigned: Boolean = true
      override val minWidth: MaybeInt = MaybeInt(32)
      override val maxWidth: MaybeInt = MaybeInt(32)
    }

    protected sealed trait DoubleKind extends SignedNumeric.Kind
    case object Double
      extends PrimTypeNode(DFDLPrimType.Double, SignedNumeric)
      with DoubleKind
      with PrimNumericFloat
      with DoubleView {
      type Kind = DoubleKind
      protected override def fromString(s: String): DataValueDouble = {
        val d: JDouble = XMLUtils.strToDouble(s)
        d
      }
      protected override def fromNumberNoCheck(n: Number): DataValueDouble = n.doubleValue
      override val min = -JDouble.MAX_VALUE
      override val max = JDouble.MAX_VALUE
      override val minStr = "-" + JDouble.MAX_VALUE.toString
      override val maxStr = JDouble.MAX_VALUE.toString
      override val isSigned: Boolean = true
      override val minWidth: MaybeInt = MaybeInt(64)
      override val maxWidth: MaybeInt = MaybeInt(64)
    }

    protected sealed trait DecimalKind extends SignedNumeric.Kind
    case object Decimal
      extends PrimTypeNode(DFDLPrimType.Decimal, SignedNumeric, List(Integer))
      with DecimalKind
      with PrimNumeric
      with DecimalView {
      type Kind = DecimalKind
      protected override def fromString(s: String): DataValueBigDecimal = new JBigDecimal(s)
      protected override def fromNumberNoCheck(n: Number): DataValueBigDecimal = asBigDecimal(n)
      override def isValid(n: Number): Boolean = {
        n match {
          case d: JDouble if d.isInfinite || d.isNaN => false
          case f: JFloat if f.isInfinite || f.isNaN => false
          case _ => true
        }
      }
      override val isSigned: Boolean = true
      override val minWidth: MaybeInt = MaybeInt.Nope
      override val maxWidth: MaybeInt = MaybeInt.Nope

      override def isInteger = false
    }

    protected sealed trait IntegerKind extends Decimal.Kind
    case object Integer
      extends PrimTypeNode(DFDLPrimType.Integer, Decimal, List(Long, NonNegativeInteger))
      with IntegerKind
      with PrimNumeric
      with IntegerView {
      type Kind = IntegerKind
      protected override def fromString(s: String): DataValueBigInt = new JBigInt(s)
      protected override def fromNumberNoCheck(n: Number): DataValueBigInt = asBigInt(n)
      override def isValid(n: Number): Boolean = {
        n match {
          case d: JDouble if d.isInfinite || d.isNaN => false
          case f: JFloat if f.isInfinite || f.isNaN => false
          case _ => true
        }
      }
      override val isSigned: Boolean = true
      override val minWidth: MaybeInt = MaybeInt(2)
      override val maxWidth: MaybeInt = MaybeInt.Nope

      override def isInteger = true
    }

    protected sealed trait LongKind extends Integer.Kind
    case object Long
      extends PrimTypeNode(DFDLPrimType.Long, Integer, List(Int))
      with LongKind
      with PrimNumericInteger
      with LongView {
      type Kind = LongKind
      protected override def fromString(s: String): DataValueLong = s.toLong
      protected override def fromNumberNoCheck(n: Number): DataValueLong = n.longValue
      override val min = JLong.MIN_VALUE
      override val max = JLong.MAX_VALUE
      override val isSigned: Boolean = true
      override val minWidth: MaybeInt = MaybeInt(2)
      override val maxWidth: MaybeInt = MaybeInt(64)
    }

    protected sealed trait IntKind extends Long.Kind
    case object Int
      extends PrimTypeNode(DFDLPrimType.Int, Long, List(Short))
      with IntKind
      with PrimNumericInteger
      with IntView {
      type Kind = IntKind
      protected override def fromString(s: String): DataValueInt = s.toInt
      protected override def fromNumberNoCheck(n: Number): DataValueInt = n.intValue
      override val min = JInt.MIN_VALUE.toLong
      override val max = JInt.MAX_VALUE.toLong
      override val isSigned: Boolean = true
      override val minWidth: MaybeInt = MaybeInt(2)
      override val maxWidth: MaybeInt = MaybeInt(32)
    }

    protected sealed trait ShortKind extends Int.Kind
    case object Short
      extends PrimTypeNode(DFDLPrimType.Short, Int, List(Byte))
      with ShortKind
      with PrimNumericInteger
      with ShortView {
      type Kind = ShortKind
      protected override def fromString(s: String): DataValueShort = s.toShort
      protected override def fromNumberNoCheck(n: Number): DataValueShort = n.shortValue
      override val min = JShort.MIN_VALUE.toLong
      override val max = JShort.MAX_VALUE.toLong
      override val isSigned: Boolean = true
      override val minWidth: MaybeInt = MaybeInt(2)
      override val maxWidth: MaybeInt = MaybeInt(16)
    }

    protected sealed trait ByteKind extends Short.Kind
    case object Byte
      extends PrimTypeNode(DFDLPrimType.Byte, Short)
      with ByteKind
      with PrimNumericInteger
      with ByteView {
      type Kind = ByteKind
      protected override def fromString(s: String): DataValueByte = s.toByte
      protected override def fromNumberNoCheck(n: Number): DataValueByte = n.byteValue
      override val min = JByte.MIN_VALUE.toLong
      override val max = JByte.MAX_VALUE.toLong
      override val isSigned: Boolean = true
      override val minWidth: MaybeInt = MaybeInt(2)
      override val maxWidth: MaybeInt = MaybeInt(8)
    }

    protected sealed trait NonNegativeIntegerKind extends Integer.Kind
    case object NonNegativeInteger
      extends PrimTypeNode(DFDLPrimType.NonNegativeInteger, Integer, List(UnsignedLong))
      with NonNegativeIntegerKind
      with PrimNumeric
      with NonNegativeIntegerView {
      type Kind = NonNegativeIntegerKind
      protected override def fromString(s: String): DataValueBigInt = new JBigInt(s)
      protected override def fromNumberNoCheck(n: Number): DataValueBigInt = asBigInt(n)
      def isValid(n: Number): Boolean = n match {
        case bd: JBigDecimal => bd.signum >= 0
        case bi: JBigInt => bi.signum >= 0
        case d: JDouble if d.isInfinite || d.isNaN => false
        case f: JFloat if f.isInfinite || f.isNaN => false
        case _ => n.longValue >= 0
      }
      override val isSigned: Boolean = false
      override val minWidth: MaybeInt = MaybeInt(1)
      override val maxWidth: MaybeInt = MaybeInt.Nope

      override def isInteger = true
    }

    protected sealed trait UnsignedLongKind extends NonNegativeInteger.Kind
    case object UnsignedLong
      extends PrimTypeNode(
        DFDLPrimType.UnsignedLong,
        NonNegativeInteger,
        List(UnsignedInt)
      )
      with UnsignedLongKind
      with PrimNumeric
      with UnsignedLongView {
      type Kind = UnsignedLongKind
      protected override def fromString(s: String): DataValueBigInt = new JBigInt(s)
      protected override def fromNumberNoCheck(n: Number): DataValueBigInt = asBigInt(n)
      def isValid(n: Number): Boolean = n match {
        case bd: JBigDecimal => bd.signum >= 0 && bd.compareTo(maxBD) <= 0
        case bi: JBigInt => bi.signum >= 0 && bi.compareTo(max) <= 0
        case d: JDouble if d.isInfinite || d.isNaN => false
        case f: JFloat if f.isInfinite || f.isNaN => false
        case _ => n.longValue >= 0
      }
      val max = new JBigInt(1, scala.Array.fill(8)(0xff.toByte))
      val maxBD = new JBigDecimal(max)
      override val isSigned: Boolean = false
      override val minWidth: MaybeInt = MaybeInt(1)
      override val maxWidth: MaybeInt = MaybeInt(64)

      override def isInteger = true
    }

    protected sealed trait UnsignedIntKind extends UnsignedLong.Kind
    case object UnsignedInt
      extends PrimTypeNode(
        DFDLPrimType.UnsignedInt,
        UnsignedLong,
        List(UnsignedShort, ArrayIndex)
      )
      with UnsignedIntKind
      with PrimNumericInteger
      with UnsignedIntView {
      type Kind = UnsignedIntKind
      protected override def fromString(s: String): DataValueLong = s.toLong
      protected override def fromNumberNoCheck(n: Number): DataValueLong = n.longValue
      override val min = 0L
      override val max = 0xffffffffL
      override val isSigned: Boolean = false
      override val minWidth: MaybeInt = MaybeInt(1)
      override val maxWidth: MaybeInt = MaybeInt(32)
    }

    protected sealed trait UnsignedShortKind extends UnsignedInt.Kind
    case object UnsignedShort
      extends PrimTypeNode(DFDLPrimType.UnsignedShort, UnsignedInt, List(UnsignedByte))
      with UnsignedShortKind
      with PrimNumericInteger
      with UnsignedShortView {
      type Kind = UnsignedShortKind
      protected override def fromString(s: String): DataValueInt = s.toInt
      protected override def fromNumberNoCheck(n: Number): DataValueInt = n.intValue
      override val min = 0L
      override val max = 0xffffL
      override val isSigned: Boolean = false
      override val minWidth: MaybeInt = MaybeInt(1)
      override val maxWidth: MaybeInt = MaybeInt(16)
    }

    protected sealed trait UnsignedByteKind extends UnsignedShort.Kind
    case object UnsignedByte
      extends PrimTypeNode(DFDLPrimType.UnsignedByte, UnsignedShort)
      with UnsignedByteKind
      with PrimNumericInteger
      with UnsignedByteView {
      type Kind = UnsignedByteKind
      protected override def fromString(s: String): DataValueShort = s.toShort
      protected override def fromNumberNoCheck(n: Number): DataValueShort = n.shortValue
      override val min = 0L
      override val max = 0xffL
      override val isSigned: Boolean = false
      override val minWidth: MaybeInt = MaybeInt(1)
      override val maxWidth: MaybeInt = MaybeInt(8)
    }

    protected sealed trait StringKind extends AnyAtomic.Kind
    case object String
      extends PrimTypeNode(DFDLPrimType.String, AnyAtomic, List(NonEmptyString))
      with StringKind
      with StringView {
      type Kind = StringKind
      override def fromXMLString(s: String) = s
    }

    protected sealed trait BooleanKind extends AnySimpleType.Kind
    case object Boolean
      extends PrimTypeNode(DFDLPrimType.Boolean, AnyAtomic)
      with BooleanKind
      with PrimNonNumeric
      with BooleanView {
      type Kind = BooleanKind
      protected override def fromString(s: String): DataValueBool = {
        s match {
          case "0" | "false" => false
          case "1" | "true" => true
          case _ => throw new IllegalArgumentException("Must be one of 0, 1, true, or false")
        }
      }
    }

    protected sealed trait AnyURIKind extends AnySimpleType.Kind
    case object AnyURI
      extends PrimTypeNode(DFDLPrimType.AnyURI, AnyAtomic)
      with AnyURIKind
      with PrimNonNumeric
      with AnyURIView {
      type Kind = AnyURIKind
      protected override def fromString(s: String): DataValueURI = new URI(s)
    }

    protected sealed trait HexBinaryKind extends Opaque.Kind
    case object HexBinary
      extends PrimTypeNode(DFDLPrimType.HexBinary, Opaque)
      with HexBinaryKind
      with PrimNonNumeric
      with HexBinaryView {
      type Kind = HexBinaryKind
      protected override def fromString(s: String): DataValueByteArray = Misc.hex2Bytes(s)
    }

    protected sealed trait DateKind extends AnyDateTimeKind
    case object Date
      extends PrimTypeNode(DFDLPrimType.Date, AnyDateTime)
      with DateKind
      with PrimNonNumeric
      with DateView {
      type Kind = DateKind
      protected override def fromString(s: String): DataValueDate = {
        DFDLDateConversion.fromXMLString(s)
      }
    }

    protected sealed trait DateTimeKind extends AnyDateTimeKind
    case object DateTime
      extends PrimTypeNode(DFDLPrimType.DateTime, AnyDateTime)
      with DateTimeKind
      with PrimNonNumeric
      with DateTimeView {
      type Kind = DateTimeKind
      protected override def fromString(s: String): DataValueDateTime = {
        DFDLDateTimeConversion.fromXMLString(s)
      }
    }

    protected sealed trait TimeKind extends AnyDateTimeKind
    case object Time
      extends PrimTypeNode(DFDLPrimType.Time, AnyDateTime)
      with TimeKind
      with PrimNonNumeric
      with TimeView {
      type Kind = TimeKind
      protected override def fromString(s: String): DataValueTime = {
        DFDLTimeConversion.fromXMLString(s)
      }
    }

    def toJavaType(dfdlType: DFDLPrimType): Class[_] = {
      dfdlType match {
        case DFDLPrimType.String => classOf[JString]
        case DFDLPrimType.Int => classOf[JInt]
        case DFDLPrimType.Byte => classOf[JByte]
        case DFDLPrimType.Short => classOf[JShort]
        case DFDLPrimType.Long => classOf[JLong]
        case DFDLPrimType.Integer => classOf[JBigInt]
        case DFDLPrimType.Decimal => classOf[JBigDecimal]
        case DFDLPrimType.UnsignedInt => classOf[JLong]
        case DFDLPrimType.UnsignedByte => classOf[JShort]
        case DFDLPrimType.UnsignedShort => classOf[JInt]
        case DFDLPrimType.UnsignedLong => classOf[JBigInt]
        case DFDLPrimType.NonNegativeInteger => classOf[JBigInt]
        case DFDLPrimType.Double => classOf[JDouble]
        case DFDLPrimType.Float => classOf[JFloat]
        case DFDLPrimType.HexBinary => classOf[Array[Byte]]
        case DFDLPrimType.AnyURI => classOf[java.net.URI]
        case DFDLPrimType.Boolean => classOf[JBoolean]
        case DFDLPrimType.DateTime => classOf[ICUCalendar]
        case DFDLPrimType.Date => classOf[ICUCalendar]
        case DFDLPrimType.Time => classOf[ICUCalendar]
      }
    }

    def toJavaTypeString(dfdlType: DFDLPrimType): String = toJavaType(dfdlType).toString
  }

  //
  // The below must be lazy vals because of the recursion between this
  // list and the definition of these type objects above.
  //
  private lazy val allAbstractTypes: Seq[TypeNode] = List(
    AnyType,
    AnySimpleType,
    AnyAtomic,
    AnyDateTime,
    AnyURI,
    Exists,
    Numeric,
    SignedNumeric,
    UnsignedNumeric,
    // There is no UnsignedInteger because the concrete type
    // NonNegativeInteger plays that role.
    Opaque,
    AnyDateTime
  )
  private lazy val allDFDLTypes: Seq[PrimTypeNode] = List(
    Float,
    Double,
    Decimal,
    Integer,
    Long,
    Int,
    Short,
    Byte,
    NonNegativeInteger,
    UnsignedLong,
    UnsignedInt,
    UnsignedShort,
    UnsignedByte,
    String,
    Boolean,
    HexBinary,
    AnyURI,
    Date,
    Time,
    DateTime
  )

  private lazy val allTypes: Seq[TypeNode] =
    allDFDLTypes ++ List(
      Complex,
      Array,
      ArrayIndex,
      NonEmptyString,
      Nothing
    ) ++ allAbstractTypes
}
