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

package org.apache.daffodil.core.dsom

import java.math.{ BigDecimal => JBigDecimal }
import java.math.{ BigInteger => JBigInt }
import scala.collection.mutable.Queue
import scala.xml.Node

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.exceptions.UnsuppressableException
import org.apache.daffodil.lib.xml.QName
import org.apache.daffodil.lib.xml.RefQName
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType
import org.apache.daffodil.runtime1.dsom.FacetTypes.ElemFacets
import org.apache.daffodil.runtime1.dsom.FacetTypes.FacetValue
import org.apache.daffodil.runtime1.dsom._

import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.GregorianCalendar
import com.ibm.icu.util.TimeZone

object Restriction {
  def apply(xmlArg: Node, simpleTypeDef: SimpleTypeDefBase) = {
    val r = new Restriction(xmlArg, simpleTypeDef)
    r.initialize()
    r
  }
}

/**
 * A schema component for simple type restrictions
 */
final class Restriction private (xmlArg: Node, val simpleTypeDef: SimpleTypeDefBase)
  extends SchemaComponentImpl(xmlArg, simpleTypeDef)
  with Facets
  with NestingLexicalMixin
  with TypeChecks {

  protected override def initialize() = {
    super.initialize()
    optUnion
  }

  Assert.invariant(xmlArg.asInstanceOf[scala.xml.Elem].label == "restriction")

  lazy val primType: PrimType = {
    optDirectPrimType.getOrElse(optBaseTypeDef.get.primType)
  }

  /**
   * Defined if the restriction is derived from a union
   */
  lazy val optUnion: Option[Union] = {
    optBaseTypeDef
      .flatMap { _.optUnion }
      .orElse(optBaseTypeDef.flatMap { _.optRestriction.flatMap { _.optUnion } })
  }

  lazy val derivationBaseRestrictions: Seq[Restriction] = {
    val obt = optBaseTypeDef.toSeq
    val res = obt.flatMap { bt =>
      val res = bt.restrictions
      res
    }
    res
  }

  lazy val baseQNameString: String = {
    val baseQNameNodeSeq = xml \ "@base"
    baseQNameNodeSeq.text
  }

  lazy val baseQName: RefQName = {
    val tryBaseQName =
      QName.resolveRef(
        baseQNameString,
        xml.scope,
        noPrefixNamespace,
        tunable.unqualifiedPathStepPolicy
      )
    schemaDefinitionUnless(
      tryBaseQName.isSuccess,
      "Failed to resolve base property reference for xs:restriction: " + tryBaseQName.failed.get.getMessage
    )
    tryBaseQName.get
  }

  /**
   * Exclusive - restriction either has a baseType or a direct primType.
   */
  lazy val (optDirectPrimType, optBaseTypeDef: Option[GlobalSimpleTypeDef]) = {
    val optPT = PrimType.fromQName(baseQName)
    val res =
      if (optPT.isDefined)
        (optPT, None)
      else {
        val optFactory = schemaSet.getGlobalSimpleTypeDef(baseQName)
        val bType =
          optFactory.getOrElse(schemaDefinitionError("No type found for base: " + baseQName))
        (None, Some(bType))
      }
    res
  }

  lazy val localBaseFacets: ElemFacets = {
    val myFacets: Queue[FacetValue] = Queue.empty // val not var - it's a mutable collection
    if (localPatternValue.length > 0) { myFacets.enqueue((Facet.pattern, localPatternValue)) }
    if (localLengthValue.length > 0) {
      myFacets.enqueue((Facet.length, localLengthValue))
    }
    if (localMinLengthValue.length > 0) {
      myFacets.enqueue((Facet.minLength, localMinLengthValue))
    }
    if (localMaxLengthValue.length > 0) {
      myFacets.enqueue((Facet.maxLength, localMaxLengthValue))
    }
    if (localMinInclusiveValue.length > 0) {
      myFacets.enqueue((Facet.minInclusive, localMinInclusiveValue))
    }
    if (localMaxInclusiveValue.length > 0) {
      myFacets.enqueue((Facet.maxInclusive, localMaxInclusiveValue))
    }
    if (localMinExclusiveValue.length > 0) {
      myFacets.enqueue((Facet.minExclusive, localMinExclusiveValue))
    }
    if (localMaxExclusiveValue.length > 0) {
      myFacets.enqueue((Facet.maxExclusive, localMaxExclusiveValue))
    }
    if (localTotalDigitsValue.length > 0) {
      myFacets.enqueue((Facet.totalDigits, localTotalDigitsValue))
    }
    if (localFractionDigitsValue.length > 0) {
      myFacets.enqueue((Facet.fractionDigits, localFractionDigitsValue))
    }
    if (localEnumerationValue.length > 0) {
      myFacets.enqueue((Facet.enumeration, localEnumerationValue))
    }

    val res: ElemFacets = myFacets.toSeq
    res
  }

  final lazy val combinedBaseFacets: Seq[FacetValue] = {
    val combined: Queue[FacetValue] = Queue.empty

    if (hasEnumeration) {
      val enumVal = getCombinedValueEnum
      combined.enqueue((Facet.enumeration, enumVal))
    }
    if (hasPattern) {
      val lPattern = localBaseFacets.filter { case (f, v) => f == Facet.pattern }
      val rPattern = remoteBaseFacets.filter { case (f, v) => f == Facet.pattern }
      val cPattern = lPattern ++: rPattern
      cPattern.foreach(x => combined.enqueue(x))
    }
    if (hasLength) {
      val cValue = getCombinedValue(Facet.length)
      combined.enqueue((Facet.length, cValue.toString()))
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

  final lazy val remoteBaseFacets = LV(Symbol("remoteBaseFacets")) {
    optBaseTypeDef match {
      case Some(gstd) => gstd.optRestriction.toSeq.flatMap { _.combinedBaseFacets }
      case None => Nil
    }
  }.value

  lazy val enumerations: Seq[EnumerationDef] = {
    val localEnums = (xml \ "enumeration").map(new EnumerationDef(_, simpleTypeDef))
    val enums =
      if (localEnums.isEmpty) {
        val remoteEnums = optBaseTypeDef
          .flatMap(_.optRestriction)
          .map(_.enumerations)
          .getOrElse(Nil)
        remoteEnums
      } else {
        localEnums
      }
    enums
  }

}

object Union {
  def apply(xmlArg: Node, simpleTypeDef: SimpleTypeDefBase) = {
    val u = new Union(xmlArg, simpleTypeDef)
    u.initialize()
    u
  }
}

/**
 * A schema component for simple type unions
 */
final class Union private (val xmlArg: Node, simpleTypeDef: SimpleTypeDefBase)
  extends SchemaComponentImpl(xmlArg, simpleTypeDef)
  with NestingLexicalMixin {
  Assert.invariant(xmlArg.asInstanceOf[scala.xml.Elem].label == "union")

  protected override def initialize() = {
    super.initialize()
    unionMemberTypes
  }

  lazy val primType: NodeInfo.PrimType = {
    if (unionMemberTypes.length == 1) {
      // degenerate case of union of 1 thing IS ALLOWED by XSD
      unionMemberTypes.head.primType
    } else {
      Assert.invariant(unionMemberTypes.length > 1)
      val firstMember = unionMemberTypes.head
      val fmpt = firstMember.primType
      val nonMatch = unionMemberTypes.tail.filter { _.primType ne fmpt }
      schemaDefinitionWhen(
        nonMatch.length > 0,
        "All types in a simple type union must have the same primitive type." +
          "The first type's primitive type '%s' does not match: %s.",
        fmpt.globalQName.toQNameString,
        nonMatch.map { _.primType.globalQName.toQNameString }.mkString(", ")
      )
      fmpt
    }
  }

  private lazy val immediateTypeXMLs = xml \ "simpleType"
  private lazy val immediateTypes: Seq[SimpleTypeDefBase] = immediateTypeXMLs.map { node =>
    {
      LocalSimpleTypeDef(node, schemaDocument)
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
  private lazy val namedTypes: Seq[GlobalSimpleTypeDef] = namedTypeQNames.map { qn =>
    schemaSet.getGlobalSimpleTypeDef(qn).get
  }
  private lazy val directMemberTypes: Seq[SimpleTypeDefBase] = namedTypes ++ immediateTypes

  lazy val unionMemberTypes: Seq[SimpleTypeDefBase] = {
    schemaDefinitionUnless(
      directMemberTypes.length > 0,
      "A simpleType union must have 2 or more member types. Only %d were found.",
      directMemberTypes.length
    )
    directMemberTypes
  }
}

sealed trait TypeChecks { self: Restriction =>
  protected def dateToBigDecimal(
    date: String,
    format: String,
    dateType: String,
    context: ThrowsSDE
  ): JBigDecimal = {
    val df = new SimpleDateFormat(format)
    df.setCalendar(new GregorianCalendar())
    df.setTimeZone(TimeZone.GMT_ZONE)
    val bd =
      try {
        val dt = df.parse(date)
        new JBigDecimal(dt.getTime())
      } catch {
        case s: scala.util.control.ControlThrowable => throw s
        case u: UnsuppressableException => throw u
        case e1: Exception => {
          try {
            // Could already be a BigDecimal
            new JBigDecimal(date)
          } catch {
            case s: scala.util.control.ControlThrowable => throw s
            case u: UnsuppressableException => throw u
            case e2: Exception =>
              context.SDE(
                "Failed to parse (%s) to %s (%s) due to %s (after %s).",
                date,
                dateType,
                format,
                e2.getMessage(),
                e1.getMessage()
              )
          }
        }
      }
    bd
  }

  private def convertStringToBigDecimal(
    value: String,
    primType: PrimType,
    context: ThrowsSDE
  ): JBigDecimal = {
    primType match {
      case PrimType.DateTime =>
        dateToBigDecimal(
          value,
          "uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxx",
          PrimType.DateTime.toString(),
          context
        )
      case PrimType.Date =>
        dateToBigDecimal(value, "uuuu-MM-ddxxx", PrimType.Date.toString(), context)
      case PrimType.Time =>
        dateToBigDecimal(value, "HH:mm:ss.SSSSSSxxx", PrimType.Time.toString(), context)
      case _ => new JBigDecimal(value)
    }
  }

  def checkRangeReturnsValue(
    value: String,
    primType: PrimType,
    theContext: ThrowsSDE
  ): (Boolean, Option[JBigDecimal]) = {
    // EmptyString is only valid for hexBinary and String
    if ((value == null | value.length() == 0)) {
      return primType match {
        case PrimType.HexBinary | PrimType.String => (true, None)
        case _ => (false, None)
      }
    }

    // Don't need to range check String or HexBinary or blobs.
    // no point attempting a conversion to BigDecimal so
    // return early here.
    primType match {
      case PrimType.String | PrimType.HexBinary | PrimType.AnyURI => return (true, None)
      case _ => /* Continue on below */
    }

    // Check Boolean, and the Numeric types.
    (value.toLowerCase(), primType) match {
      case ("true", PrimType.Boolean) => (true, Some(JBigDecimal.ONE))
      case ("false", PrimType.Boolean) => (true, Some(JBigDecimal.ZERO))
      case (x, PrimType.Boolean) =>
        theContext.SDE("%s is not a valid Boolean value. Expected 'true' or 'false'.", x)
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
          case PrimType.HexBinary =>
            Assert.impossibleCase // Handled earlier, shouldn't get here
          case PrimType.String => Assert.impossibleCase // Handled earlier, shouldn't get here
          case PrimType.AnyURI => Assert.impossibleCase // Handled earlier, shouldn't get here
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
              PrimType.Integer | PrimType.UnsignedInt | PrimType.UnsignedByte |
              PrimType.UnsignedShort | PrimType.UnsignedLong =>
            if (!isValueWhole)
              theContext.SDE("checkRange - Value (%s) must be a whole number.", value)
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

  protected def isNumInRange(num: JBigDecimal, min: JBigDecimal, max: JBigDecimal): Boolean = {
    val checkMin = num.compareTo(min)
    if (checkMin < 0) { return false } // num less than min
    val checkMax = num.compareTo(max)
    if (checkMax > 0) { return false } // num greater than max
    true
  }
  protected def isInByteRange(value: JBigDecimal): Boolean = {
    val min = new JBigDecimal(Byte.MinValue.toLong.toString())
    val max = new JBigDecimal(Byte.MaxValue.toLong.toString())
    isNumInRange(value, min, max)
  }
  protected def isInShortRange(value: JBigDecimal): Boolean = {
    val min = new JBigDecimal(Short.MinValue.toLong.toString())
    val max = new JBigDecimal(Short.MaxValue.toLong.toString())
    isNumInRange(value, min, max)
  }
  protected def isInIntRange(value: JBigDecimal): Boolean = {
    val min = new JBigDecimal(Int.MinValue.toString())
    val max = new JBigDecimal(Int.MaxValue.toString())
    isNumInRange(value, min, max)
  }
  protected def isInIntegerRange(value: JBigDecimal): Boolean = {
    val min = new JBigDecimal(Int.MinValue.toString())
    // Unbounded Integer
    val checkMin = value.compareTo(min)
    if (checkMin < 0) { return false } // num less than min
    true
  }
  protected def isInLongRange(value: JBigDecimal): Boolean = {
    val min = new JBigDecimal(Long.MinValue.toString())
    val max = new JBigDecimal(Long.MaxValue.toString())
    isNumInRange(value, min, max)
  }
  protected def isInDoubleRange(value: JBigDecimal): Boolean = {
    val min = new JBigDecimal(Double.MinValue.toString())
    val max = new JBigDecimal(Double.MaxValue.toString())
    isNumInRange(value, min, max)
  }
  protected def isInFloatRange(value: JBigDecimal): Boolean = {
    val min = new JBigDecimal(Float.MinValue.toString())
    val max = new JBigDecimal(Float.MaxValue.toString())
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
  protected def isInDecimalRange(value: JBigDecimal): Boolean = {
    true
  }
  protected def isInNegativeIntegerRange(value: JBigDecimal, context: ThrowsSDE): Boolean = {
    // TODO: NegativeInteger not supported in DFDL v1.0
    val min = new JBigDecimal(Int.MinValue.toString())
    val isNegative = value.signum == -1
    if (!isNegative) context.SDE("Expected a negative integer for this value.")
    val checkMin = value.compareTo(min)
    if (checkMin < 0)
      context.SDE(
        "Value (%s) was found to be more negative than allowed by Int.MinValue.",
        value.intValue()
      )
    true
  }
  protected def isInNonNegativeIntegerRange(value: JBigDecimal): Boolean = {
    // Should be treated as unsigned Integer (unbounded)
    val isNegative = value.signum == -1
    if (isNegative) return false
    true
  }
  protected def isInUnsignedXXXRange(
    value: JBigDecimal,
    numBits: Int,
    typeName: String
  ): Boolean = {
    Assert.usage(numBits <= 64, "isInUnsignedXXXRange: numBits must be <= 64.")
    val max = new JBigDecimal(JBigInt.ONE.shiftLeft(numBits)).subtract(new JBigDecimal(1))
    val isNegative = value.signum == -1
    if (isNegative) return false
    val checkMax = value.compareTo(max)
    if (checkMax > 0) return false
    true
  }
  protected def isInUnsignedLongRange(value: JBigDecimal): Boolean =
    isInUnsignedXXXRange(value, 64, "ulong")

  protected def isInUnsignedIntRange(value: JBigDecimal): Boolean =
    isInUnsignedXXXRange(value, 32, "uint")

  protected def isInUnsignedShortRange(value: JBigDecimal): Boolean =
    isInUnsignedXXXRange(value, 16, "ushort")

  protected def isInUnsignedByteRange(value: JBigDecimal): Boolean =
    isInUnsignedXXXRange(value, 8, "ubyte")
}
