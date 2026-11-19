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

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInt
import scala.collection.mutable.Queue
import scala.xml.Node

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.xml.QName
import org.apache.daffodil.lib.xml.RefQName
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType
import org.apache.daffodil.runtime1.dsom.*

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

  private def checkValueSpaceFacetRange(
    localFacet: java.math.BigDecimal,
    facetType: Facet.Type
  ): Unit = {
    // Necessary for min/max Inclusive/Exclusive Facets
    facetType match {
      case Facet.maxExclusive | Facet.maxInclusive | Facet.minExclusive | Facet.minInclusive |
          Facet.enumeration => {
        // Here we're just doing range checking for the
        // specified primitive type
        primType match {
          case PrimType.Int => {
            if (!isInIntRange(localFacet)) {
              SDE(
                "%s facet value (%s) was found to be outside of Int range.",
                facetType,
                localFacet
              )
            }
          }
          case PrimType.Byte => {
            if (!isInByteRange(localFacet)) {
              SDE(
                "%s facet value (%s) was found to be outside of Byte range.",
                facetType,
                localFacet
              )
            }
          }
          case PrimType.Short => {
            if (!isInShortRange(localFacet)) {
              SDE(
                "%s facet value (%s) was found to be outside of Short range.",
                facetType,
                localFacet
              )
            }
          }
          case PrimType.Long => {
            if (!isInLongRange(localFacet)) {
              SDE(
                "%s facet value (%s) was found to be outside of Long range.",
                facetType,
                localFacet
              )
            }
          }
          case PrimType.Integer => {
            // Unbounded integer
            if (!isInIntegerRange(localFacet)) {
              SDE(
                "%s facet value (%s) was found to be outside of Integer range.",
                facetType,
                localFacet
              )
            }
          }
          case PrimType.UnsignedInt => {
            if (!isInUnsignedIntRange(localFacet)) {
              SDE(
                "%s facet value (%s) was found to be outside of unsigned int range.",
                facetType,
                localFacet
              )
            }
          }
          case PrimType.UnsignedByte => {
            if (!isInUnsignedByteRange(localFacet)) {
              SDE(
                "%s facet value (%s) was found to be outside of unsigned byte range.",
                facetType,
                localFacet
              )
            }
          }
          case PrimType.UnsignedShort => {
            if (!isInUnsignedShortRange(localFacet)) {
              SDE(
                "%s facet value (%s) was found to be outside of unsigned short range.",
                facetType,
                localFacet
              )
            }
          }
          case PrimType.UnsignedLong => {
            if (!isInUnsignedLongRange(localFacet)) {
              SDE(
                "%s facet value (%s) was found to be outside of unsigned long range.",
                facetType,
                localFacet
              )
            }
          }
          case PrimType.Double => {
            if (!isInDoubleRange(localFacet)) {
              SDE(
                "%s facet value (%s) was found to be outside of Double range.",
                facetType,
                localFacet
              )
            }
          }
          case PrimType.Float => {
            if (!isInFloatRange(localFacet)) {
              SDE(
                "%s facet value (%s) was found to be outside of Float range.",
                facetType,
                localFacet
              )
            }
          }
          case PrimType.NonNegativeInteger => {
            // Unsigned Unbounded Integer
            if (!isInNonNegativeIntegerRange(localFacet)) {
              SDE(
                "%s facet value (%s) was found to be outside of NonNegativeInteger range.",
                facetType,
                localFacet
              )
            }
          }
          case PrimType.Decimal => {
            /* Nothing to do here */
          }
          case PrimType.DateTime => {
            /* Nothing to do here */
          }
          case PrimType.Date => {
            /* Nothing to do here */
          }
          case PrimType.Time => {
            /* Nothing to do here */
          }
          case PrimType.Boolean => notYetImplemented("checkValueSpaceFacetRange - Boolean")
          case PrimType.HexBinary => {
            /* Nothing to do here */
          }
          case PrimType.String => {
            /* Nothing to do here */
          }
          case _ =>
            Assert.usageError(
              "checkValueSpaceFacetRange - Unrecognized primitive type: " + primType.name
            )
        }
      }
      case _ => {
        /* Nothing to do */
      }
    }
  }

  protected[dsom] override def initialize() = {
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
    // val not var - it's a mutable collection
    val myFacets: Queue[FacetOrdered] = Queue.empty[FacetOrdered]

    if (localPatternValue.nonEmpty)
      myFacets.enqueue(new FacetPattern(localPatternValue))
    if (localLengthValue.nonEmpty)
      myFacets.enqueue(new FacetLength(localLengthValue, this))
    if (localMinLengthValue.nonEmpty)
      myFacets.enqueue(new FacetMinLength(localMinLengthValue, this))
    if (localMaxLengthValue.nonEmpty)
      myFacets.enqueue(new FacetMaxLength(localMaxLengthValue, this))
    if (localMinInclusiveValue.nonEmpty) {
      val facet = new FacetMinInclusive(localMinInclusiveValue, primType, this)
      checkValueSpaceFacetRange(facet.bigDecimalValue, Facet.minInclusive)
      myFacets.enqueue(facet)
    }
    if (localMaxInclusiveValue.nonEmpty) {
      val facet = new FacetMaxInclusive(localMaxInclusiveValue, primType, this)
      checkValueSpaceFacetRange(facet.bigDecimalValue, Facet.maxInclusive)
      myFacets.enqueue(facet)
    }
    if (localMinExclusiveValue.nonEmpty) {
      val facet = new FacetMinExclusive(localMinExclusiveValue, primType, this)
      checkValueSpaceFacetRange(facet.bigDecimalValue, Facet.minExclusive)
      myFacets.enqueue(facet)
    }
    if (localMaxExclusiveValue.nonEmpty) {
      val facet = new FacetMaxExclusive(localMaxExclusiveValue, primType, this)
      checkValueSpaceFacetRange(facet.bigDecimalValue, Facet.maxExclusive)
      myFacets.enqueue(facet)
    }
    if (localTotalDigitsValue.nonEmpty)
      myFacets.enqueue(new FacetTotalDigits(localTotalDigitsValue, this))
    if (localFractionDigitsValue.nonEmpty)
      myFacets.enqueue(new FacetFractionDigits(localFractionDigitsValue, this))
    if (localEnumerationValue.nonEmpty)
      myFacets.enqueue(new FacetEnumeration(localEnumerationValue))

    myFacets.toSeq
  }

  final lazy val combinedBaseFacets: ElemFacets = {
    val combined: Queue[FacetOrdered] = Queue.empty

    if (hasEnumeration) {
      val lValue = localBaseFacets.collectFirst { case f: FacetEnumeration => f }
      val rValue = remoteBaseFacets.collectFirst { case f: FacetEnumeration => f }
      // validate subset if both exist
      for (l <- lValue; r <- rValue) {
        l.xmlValue.foreach(e => {
          if (r.xmlValue.nonEmpty && !r.xmlValue.contains(e))
            SDE("Local enumerations must be a subset of base enumerations.")
        })
      }
      // local wins if present, otherwise remote
      // hasEnumeration guarantees at least one is defined
      combined.enqueue(lValue.orElse(rValue).getOrElse(Assert.impossibleCase))
    }
    // Patterns within a type are OR'd, collect all
    // per http://www.xfront.com/XML-Schema-library/papers/Algorithm-for-Merging-a-simpleType-Dependency-Chain.pdf
    if (hasPattern) {
      val lPattern = localBaseFacets.collect { case f: FacetPattern => f }
      val rPattern = remoteBaseFacets.collect { case f: FacetPattern => f }
      (lPattern ++: rPattern).foreach(combined.enqueue)
    }
    if (hasLength) {
      val facets =
        localBaseFacets.collect { case f: FacetLength => f } ++
          remoteBaseFacets.collect { case f: FacetLength => f }
      val narrowed = facets.reduce[FacetOrdered]((l, r) => l.narrow(r))
      combined.enqueue(narrowed)
    }
    if (hasMinLength) {
      val facets =
        localBaseFacets.collect { case f: FacetMinLength => f } ++
          remoteBaseFacets.collect { case f: FacetMinLength => f }
      val narrowed = facets.reduce[FacetOrdered]((l, r) => l.narrow(r))
      combined.enqueue(narrowed)
    }
    if (hasMaxLength) {
      val facets =
        localBaseFacets.collect { case f: FacetMaxLength => f } ++
          remoteBaseFacets.collect { case f: FacetMaxLength => f }
      val narrowed = facets.reduce[FacetOrdered]((l, r) => l.narrow(r))
      combined.enqueue(narrowed)
    }
    if (hasMinInclusive) {
      val facets =
        localBaseFacets.collect { case f: FacetMinInclusive => f } ++
          remoteBaseFacets.collect { case f: FacetMinInclusive => f }
      val narrowed = facets.reduce[FacetOrdered]((l, r) => l.narrow(r))
      combined.enqueue(narrowed)
    }
    if (hasMaxInclusive) {
      val facets =
        localBaseFacets.collect { case f: FacetMaxInclusive => f } ++
          remoteBaseFacets.collect { case f: FacetMaxInclusive => f }
      val narrowed = facets.reduce[FacetOrdered]((l, r) => l.narrow(r))
      combined.enqueue(narrowed)
    }
    if (hasMinExclusive) {
      val facets =
        localBaseFacets.collect { case f: FacetMinExclusive => f } ++
          remoteBaseFacets.collect { case f: FacetMinExclusive => f }
      val narrowed = facets.reduce[FacetOrdered]((l, r) => l.narrow(r))
      combined.enqueue(narrowed)
    }
    if (hasMaxExclusive) {
      val facets =
        localBaseFacets.collect { case f: FacetMaxExclusive => f } ++
          remoteBaseFacets.collect { case f: FacetMaxExclusive => f }
      val narrowed = facets.reduce[FacetOrdered]((l, r) => l.narrow(r))
      combined.enqueue(narrowed)
    }
    if (hasTotalDigits) {
      val facets =
        localBaseFacets.collect { case f: FacetTotalDigits => f } ++
          remoteBaseFacets.collect { case f: FacetTotalDigits => f }
      val narrowed = facets.reduce[FacetOrdered]((l, r) => l.narrow(r))
      combined.enqueue(narrowed)
    }
    if (hasFractionDigits) {
      val facets =
        localBaseFacets.collect { case f: FacetFractionDigits => f } ++
          remoteBaseFacets.collect { case f: FacetFractionDigits => f }
      val narrowed = facets.reduce[FacetOrdered]((l, r) => l.narrow(r))
      combined.enqueue(narrowed)
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

  protected[dsom] override def initialize() = {
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
