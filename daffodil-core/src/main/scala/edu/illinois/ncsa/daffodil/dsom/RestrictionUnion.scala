/* Copyright (c) 2017 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.dsom

import java.math.BigInteger
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import scala.xml.Node
import edu.illinois.ncsa.daffodil.xml.QName
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import com.ibm.icu.text.SimpleDateFormat
import scala.collection.mutable.Queue
import edu.illinois.ncsa.daffodil.exceptions.Assert
import com.ibm.icu.util.GregorianCalendar
import com.ibm.icu.util.TimeZone
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType

/**
 * A schema component for simple type restrictions
 */
final class Restriction(xmlArg: Node, val simpleType: SimpleTypeDefBase)
  extends SchemaComponent(xmlArg, simpleType.schemaDocument)
  with NestingLexicalMixin
  with Facets
  with TypeChecks {

  Assert.invariant(xmlArg.asInstanceOf[scala.xml.Elem].label == "restriction")

  import edu.illinois.ncsa.daffodil.dsom.FacetTypes._

  final lazy val primType: PrimType = {
    optDirectPrimType.getOrElse(optBaseType.get.primType)
  }

  /**
   * Defined if the restriction is derived from a union
   */
  final lazy val optUnion: Option[Union] = {
    optBaseType.flatMap { _.optUnion }.orElse(
      optBaseType.flatMap { _.optRestriction.flatMap { _.optUnion } })
  }

  final lazy val derivationBaseRestrictions: Seq[Restriction] = {
    val obt = optBaseType.toSeq
    val res = obt.flatMap {
      bt =>
        val res = bt.restrictions
        res
    }
    res
  }

  lazy val baseQName = {
    val baseQNameNodeSeq = xml \ "@base"
    val baseQNameString = baseQNameNodeSeq.text
    val tryBaseQName = QName.resolveRef(baseQNameString, xml.scope)
    Assert.invariant(tryBaseQName.isSuccess)
    tryBaseQName.get
  }

  def optBaseDef = optBaseType

  /**
   * Exclusive - restriction either has a baseType or a direct primType.
   */
  private lazy val (optDirectPrimType, optBaseType: Option[GlobalSimpleTypeDef]) = {
    val optPT = schemaSet.getPrimitiveType(baseQName)
    val res =
      if (optPT.isDefined)
        (Some(optPT.get.typeNode), None)
      else {
        val optFactory = schemaSet.getGlobalSimpleTypeDef(baseQName)
        val factory = optFactory.getOrElse(schemaDefinitionError("No type found for base: " + baseQName))
        val bType = factory.forDerivedType(simpleType)
        (None, Some(bType))
      }
    res
  }

  final lazy val localBaseFacets: ElemFacets = {
    val myFacets: Queue[FacetValue] = Queue.empty // val not var - it's a mutable collection
    if (localPatternValue.length > 0) { myFacets.enqueue((Facet.pattern, localPatternValue)) }
    if (localMinLengthValue.length > 0) { myFacets.enqueue((Facet.minLength, localMinLengthValue)) }
    if (localMaxLengthValue.length > 0) { myFacets.enqueue((Facet.maxLength, localMaxLengthValue)) }
    if (localMinInclusiveValue.length > 0) { myFacets.enqueue((Facet.minInclusive, localMinInclusiveValue)) }
    if (localMaxInclusiveValue.length > 0) { myFacets.enqueue((Facet.maxInclusive, localMaxInclusiveValue)) }
    if (localMinExclusiveValue.length > 0) { myFacets.enqueue((Facet.minExclusive, localMinExclusiveValue)) }
    if (localMaxExclusiveValue.length > 0) { myFacets.enqueue((Facet.maxExclusive, localMaxExclusiveValue)) }
    if (localTotalDigitsValue.length > 0) { myFacets.enqueue((Facet.totalDigits, localTotalDigitsValue)) }
    if (localFractionDigitsValue.length > 0) { myFacets.enqueue((Facet.fractionDigits, localFractionDigitsValue)) }
    if (localEnumerationValue.length > 0) { myFacets.enqueue((Facet.enumeration, localEnumerationValue)) }

    val res: ElemFacets = myFacets.toSeq
    res
  }

  final lazy val combinedBaseFacets: Seq[FacetValue] = {
    //    val localF = localBaseFacets
    //    val remoteF = remoteBaseFacets

    val combined: Queue[FacetValue] = Queue.empty

    if (hasEnumeration) {
      val enumVal = getCombinedValueEnum
      combined.enqueue((Facet.enumeration, enumVal))
    }
    if (hasPattern) {
      val lPattern = localBaseFacets.filter { case (f, v) => f == Facet.pattern }
      val rPattern = remoteBaseFacets.filter { case (f, v) => f == Facet.pattern }
      val cPattern = lPattern.union(rPattern)
      cPattern.foreach(x => combined.enqueue(x))
    }
    if (hasMinLength) {
      val cValue = getCombinedValue(Facet.minLength)
      combined.enqueue((Facet.minLength, cValue.toString()))
    }
    if (hasMaxLength) {
      val cValue = getCombinedValue(Facet.maxLength)
      combined.enqueue((Facet.maxLength, cValue.toString()))
    }
    if (hasMaxInclusive) {
      val cValue = getCombinedValue(Facet.maxInclusive)
      combined.enqueue((Facet.maxInclusive, cValue.toString()))
    }
    if (hasMaxExclusive) {
      val cValue = getCombinedValue(Facet.maxExclusive)
      combined.enqueue((Facet.maxExclusive, cValue.toString()))
    }
    if (hasMinInclusive) {
      val cValue = getCombinedValue(Facet.minInclusive)
      combined.enqueue((Facet.minInclusive, cValue.toString()))
    }
    if (hasMinExclusive) {
      val cValue = getCombinedValue(Facet.minExclusive)
      combined.enqueue((Facet.minExclusive, cValue.toString()))
    }
    if (hasTotalDigits) {
      val cValue = getCombinedValue(Facet.totalDigits)
      combined.enqueue((Facet.totalDigits, cValue.toString()))
    }
    if (hasFractionDigits) {
      val cValue = getCombinedValue(Facet.fractionDigits)
      combined.enqueue((Facet.fractionDigits, cValue.toString()))
    }
    combined.toSeq
  }

  final def remoteBaseFacets = LV('remoteBaseFacets) {
    optBaseType match {
      case Some(gstd) => gstd.optRestriction.toSeq.flatMap { _.combinedBaseFacets }
      case None => Nil
    }
  }.value
}

/**
 * A schema component for simple type unions
 */
final class Union(xmlArg: Node, simpleType: SimpleTypeDefBase)
  extends SchemaComponent(xmlArg, simpleType.schemaDocument)
  with NestingLexicalMixin {

  Assert.invariant(xmlArg.asInstanceOf[scala.xml.Elem].label == "union")

  lazy val primType: NodeInfo.PrimType = {
    if (unionMemberTypes.length == 1) {
      // degenerate case of union of 1 thing IS ALLOWED by XSD
      unionMemberTypes.head.primType
    } else {
      Assert.invariant(unionMemberTypes.length > 1)
      val firstMember = unionMemberTypes.head
      val fmpt = firstMember.primType
      val nonMatch = unionMemberTypes.tail.filter { _.primType ne fmpt }
      schemaDefinitionWhen(nonMatch.length > 0,
        "All types in a simple type union must have the same primitive type." +
          "The first type's primitive type '%s' does not match: %s.",
        fmpt.globalQName.toQNameString,
        nonMatch.map { _.primType.globalQName.toQNameString }.mkString(", "))
      fmpt
    }
  }

  private lazy val immediateTypeXMLs = xml \ "simpleType"
  private lazy val immediateTypes = immediateTypeXMLs.map { node =>
    {
      schemaSet.LocalSimpleTypeDefFactory(node, schemaDocument).forElement(simpleType.element)
    }
  }

  private lazy val namedTypeQNameStrings = {
    val attribString = (xml \ "@memberTypes").text
    val strings =
      if (attribString == "") Nil
      else attribString.split("""\s+""").toList
    strings
  }
  private lazy val namedTypeQNames = namedTypeQNameStrings.map { qns => resolveQName(qns) }
  private lazy val namedTypes = namedTypeQNames.map {
    qn => schemaSet.getGlobalSimpleTypeDef(qn).get.forElement(simpleType.element)
  }
  private lazy val directMemberTypes: Seq[SimpleTypeDefBase] = namedTypes ++ immediateTypes

  lazy val unionMemberTypes: Seq[SimpleTypeDefBase] = {
    schemaDefinitionUnless(directMemberTypes.length > 0, "A simpleType union must have 2 or more member types. Only %d were found.", directMemberTypes.length)
    directMemberTypes
  }
}

sealed trait TypeChecks { self: Restriction =>
  protected def dateToBigDecimal(date: String, format: String, dateType: String, context: ThrowsSDE): java.math.BigDecimal = {
    val df = new SimpleDateFormat(format)
    df.setCalendar(new GregorianCalendar())
    df.setTimeZone(TimeZone.GMT_ZONE)
    val bd = try {
      val dt = df.parse(date)
      new java.math.BigDecimal(dt.getTime())
    } catch {
      case s: scala.util.control.ControlThrowable => throw s
      case u: UnsuppressableException => throw u
      case e1: Exception => {
        try {
          // Could already be a BigDecimal
          new java.math.BigDecimal(date)
        } catch {
          case s: scala.util.control.ControlThrowable => throw s
          case u: UnsuppressableException => throw u
          case e2: Exception => context.SDE("Failed to parse (%s) to %s (%s) due to %s (after %s).", date, dateType, format, e2.getMessage(), e1.getMessage())
        }
      }
    }
    bd
  }

  private def convertStringToBigDecimal(value: String, primType: PrimType, context: ThrowsSDE): java.math.BigDecimal = {
    primType match {
      case PrimType.DateTime => dateToBigDecimal(value, "uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxx", PrimType.DateTime.toString(), context)
      case PrimType.Date => dateToBigDecimal(value, "uuuu-MM-ddxxx", PrimType.Date.toString(), context)
      case PrimType.Time => dateToBigDecimal(value, "HH:mm:ss.SSSSSSxxx", PrimType.Time.toString(), context)
      case _ => new java.math.BigDecimal(value)
    }
  }

  def checkRangeReturnsValue(value: String, primType: PrimType, theContext: ThrowsSDE): (Boolean, Option[BigDecimal]) = {
    // EmptyString is only valid for hexBinary and String
    if ((value == null | value.length() == 0)) {
      return primType match {
        case PrimType.HexBinary | PrimType.String => (true, None)
        case _ => (false, None)
      }
    }

    // Don't need to range check String or HexBinary
    // no point attempting a conversion to BigDecimal so
    // return early here.
    primType match {
      case PrimType.String | PrimType.HexBinary => return (true, None)
      case _ => /* Continue on below */
    }

    // Check Boolean, and the Numeric types.
    (value.toLowerCase(), primType) match {
      case ("true", PrimType.Boolean) => (true, Some(BigDecimal(1)))
      case ("false", PrimType.Boolean) => (true, Some(BigDecimal(0)))
      case (x, PrimType.Boolean) => theContext.SDE("%s is not a valid Boolean value. Expected 'true' or 'false'.", x)
      case (_, _) => {
        // Perform conversions once
        val theValue = convertStringToBigDecimal(value, primType, theContext)

        // Here we're just doing range checking for the
        // specified primitive type
        val res: Boolean = primType match {
          case PrimType.Int => isInIntRange(theValue)
          case PrimType.Byte => isInByteRange(theValue)
          case PrimType.Short => isInShortRange(theValue)
          case PrimType.Long => isInLongRange(theValue)
          case PrimType.Integer => true // Unbounded Integer
          case PrimType.UnsignedInt => isInUnsignedIntRange(theValue)
          case PrimType.UnsignedByte => isInUnsignedByteRange(theValue)
          case PrimType.UnsignedShort => isInUnsignedShortRange(theValue)
          case PrimType.UnsignedLong => isInUnsignedLongRange(theValue)
          case PrimType.Double => isInDoubleRange(theValue)
          case PrimType.Float => isInFloatRange(theValue)
          case PrimType.DateTime => true
          case PrimType.Date => true
          case PrimType.Time => true
          case PrimType.Boolean => Assert.impossibleCase // Handled earlier, shouldn't get here
          case PrimType.Decimal => true // Unbounded Decimal
          case PrimType.HexBinary => Assert.impossibleCase // Handled earlier, shouldn't get here
          case PrimType.String => Assert.impossibleCase // Handled earlier, shouldn't get here
          case PrimType.NonNegativeInteger => isInNonNegativeIntegerRange(theValue)
        }
        val isValueWhole = {
          val IsWholeRegex = """^[^.]*(\.0*)?$""".r
          value match {
            case IsWholeRegex(_) => true
            case _ => false
          }
        }
        primType match {
          case PrimType.Int | PrimType.Byte | PrimType.Short | PrimType.Long |
            PrimType.Integer | PrimType.UnsignedInt | PrimType.UnsignedByte | PrimType.UnsignedShort |
            PrimType.UnsignedLong => if (!isValueWhole) theContext.SDE("checkRange - Value (%s) must be a whole number.", value)
          case _ => // OK
        }
        (res, Some(theValue))
      }
    }
  }

  def checkRange(value: String, primType: PrimType, theContext: ThrowsSDE): Boolean = {
    val (boolResult, _) = checkRangeReturnsValue(value, primType, theContext)
    boolResult
  }

  protected def isNumInRange(num: java.math.BigDecimal, min: java.math.BigDecimal,
    max: java.math.BigDecimal): Boolean = {
    val checkMin = num.compareTo(min)
    if (checkMin < 0) { return false } // num less than min
    val checkMax = num.compareTo(max)
    if (checkMax > 0) { return false } // num greater than max
    true
  }
  protected def isInByteRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Byte.MinValue.toLong.toString())
    val max = new java.math.BigDecimal(Byte.MaxValue.toLong.toString())
    isNumInRange(value, min, max)
  }
  protected def isInShortRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Short.MinValue.toLong.toString())
    val max = new java.math.BigDecimal(Short.MaxValue.toLong.toString())
    isNumInRange(value, min, max)
  }
  protected def isInIntRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Int.MinValue.toString())
    val max = new java.math.BigDecimal(Int.MaxValue.toString())
    isNumInRange(value, min, max)
  }
  protected def isInIntegerRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Int.MinValue.toString())
    // Unbounded Integer
    val checkMin = value.compareTo(min)
    if (checkMin < 0) { return false } // num less than min
    true
  }
  protected def isInLongRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Long.MinValue.toString())
    val max = new java.math.BigDecimal(Long.MaxValue.toString())
    isNumInRange(value, min, max)
  }
  protected def isInDoubleRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Double.MinValue.toString())
    val max = new java.math.BigDecimal(Double.MaxValue.toString())
    isNumInRange(value, min, max)
  }
  protected def isInFloatRange(value: java.math.BigDecimal): Boolean = {
    val min = new java.math.BigDecimal(Float.MinValue.toString())
    val max = new java.math.BigDecimal(Float.MaxValue.toString())
    isNumInRange(value, min, max)
  }

  /**
   * Note: all values are in decimal range.
   *
   * See:
   * https://docs.oracle.com/javase/7/docs/api/java/math/BigDecimal.html
   *
   * Regarding MathContext.UNLIMITED:
   *
   * A MathContext object whose settings have the values required for unlimited precision arithmetic. The values of the settings are: precision=0 roundingMode=HALF_UP
   *
   * Where precision is defined as:
   *
   * The precision is the number of digits in the unscaled value.
   *
   * See https://docs.oracle.com/javase/7/docs/api/java/math/BigDecimal.html#precision()
   *
   * See http://stackoverflow.com/questions/35435691/bigdecimal-precision-and-scale
   */
  protected def isInDecimalRange(value: java.math.BigDecimal): Boolean = {
    true
  }
  protected def isInNegativeIntegerRange(value: java.math.BigDecimal, context: ThrowsSDE): Boolean = {
    // TODO: NegativeInteger not supported in DFDL v1.0
    val min = new java.math.BigDecimal(Int.MinValue.toString())
    // val max = new java.math.BigDecimal(Int.MaxValue.toString())
    val isNegative = value.signum == -1
    if (!isNegative) context.SDE("Expected a negative integer for this value.")
    val checkMin = value.compareTo(min)
    if (checkMin < 0) context.SDE("Value (%s) was found to be more negative than allowed by Int.MinValue.", value.intValue())
    true
  }
  protected def isInNonNegativeIntegerRange(value: java.math.BigDecimal): Boolean = {
    // Should be treated as unsigned Integer (unbounded)
    // val min = java.math.BigDecimal.ZERO
    val isNegative = value.signum == -1
    if (isNegative) return false
    true
  }
  protected def isInUnsignedXXXRange(value: java.math.BigDecimal, numBits: Int, typeName: String): Boolean = {
    Assert.usage(numBits <= 64, "isInUnsignedXXXRange: numBits must be <= 64.")
    // val min = java.math.BigDecimal.ZERO
    val max = new java.math.BigDecimal(BigInteger.ONE.shiftLeft(numBits)).subtract(new java.math.BigDecimal(1))
    val isNegative = value.signum == -1
    if (isNegative) return false
    val checkMax = value.compareTo(max)
    if (checkMax > 0) return false
    true
  }
  protected def isInUnsignedLongRange(value: java.math.BigDecimal): Boolean =
    isInUnsignedXXXRange(value, 64, "ulong")

  protected def isInUnsignedIntRange(value: java.math.BigDecimal): Boolean =
    isInUnsignedXXXRange(value, 32, "uint")

  protected def isInUnsignedShortRange(value: java.math.BigDecimal): Boolean =
    isInUnsignedXXXRange(value, 16, "ushort")

  protected def isInUnsignedByteRange(value: java.math.BigDecimal): Boolean =
    isInUnsignedXXXRange(value, 8, "ubyte")
}
