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

package org.apache.daffodil.runtime1.dsom

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInt
import scala.util.matching.Regex

import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.util.Enum
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.dpath.InvalidPrimitiveDataException
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType

object Facet extends Enum {
  sealed trait Type extends EnumValueType
  case object enumeration extends Type
  case object fractionDigits extends Type
  case object length extends Type
  case object maxExclusive extends Type
  case object maxInclusive extends Type
  case object maxLength extends Type
  case object minExclusive extends Type
  case object minInclusive extends Type
  case object minLength extends Type
  case object pattern extends Type
  case object totalDigits extends Type
  case object whiteSpace extends Type
}

/**
 * Base sealed trait for compiled XSD facet constraints.
 *
 * Each subclass represents a specific XSD facet, carrying both the original
 * XML string value and, where applicable, a converted representation used for
 * runtime validation.
 *
 * Previously facets were stored as `(Facet.Type, String)` tuples, which
 * are serializable by default. Typed facet objects now require an explicit
 * Serializable declaration otherwise a java.io.NotSerializableException
 * will be thrown during serialization.
 */
sealed trait FacetOrdered extends Serializable {
  def xmlValue: String

  /**
   * Validates this local facet against the remote base facet.
   * Throws SDE if this facet does not correctly narrow the base.
   * Returns this facet if valid.
   *
   * The default implementation is a no-op for facets where narrowing does not apply
   * (for example, pattern and enumeration).
   */
  def narrow(remote: FacetOrdered): FacetOrdered = this
}

type ElemFacets = Seq[FacetOrdered]

/**
 * Base abstract class for integer facets. Parses [[xmlValue]] to [[JBigInt]]
 * once, validating sign via [[isNonNegative]] — non-negative facets reject
 * negative values, positive facets reject zero and negative values.
 */
abstract class FacetInteger(val xmlValue: String, val context: ThrowsSDE) extends FacetOrdered {
  def facetName: String
  def isNonNegative: Boolean

  lazy val bigIntValue: JBigInt = {
    val localFacet =
      try { new JBigInt(xmlValue) }
      catch {
        case e: IllegalArgumentException =>
          context.SDE("invalid %s facet restriction: %s", facetName, e.getMessage)
      }
    if (isNonNegative && localFacet.signum() == -1)
      context.SDE("The %s facet must be a non-negative integer.", facetName)
    else if (!isNonNegative && localFacet.signum() != 1)
      context.SDE("The %s facet must be a positive integer.", facetName)
    localFacet
  }
}

/**
 * Base abstract class for non-negative integer facets (length, minLength,
 * maxLength, fractionDigits). Uses [[BigInteger]] for comparison.
 */
abstract class FacetNonNegativeInteger(xmlValue: String, context: ThrowsSDE)
  extends FacetInteger(xmlValue, context) {
  final val isNonNegative = true

  protected def isInvalidNarrowing(remoteValue: JBigInt): Boolean
  protected def narrowingRelation: String

  override final def narrow(remote: FacetOrdered): FacetOrdered = {
    val remoteFacet = remote.asInstanceOf[FacetNonNegativeInteger].bigIntValue
    if (isInvalidNarrowing(remoteFacet))
      context.SDE(
        "SimpleTypes: The local %s (%s) was %s the base %s (%s)",
        facetName,
        bigIntValue,
        narrowingRelation,
        facetName,
        remoteFacet
      )
    this
  }
}

/**
 * Base abstract class for value-space facets (minInclusive, maxInclusive,
 * minExclusive, maxExclusive). Uses [[JBigDecimal]] for comparison.
 *
 * Range validation against the primitive type's value space is
 * performed separately prior to storage in localBaseFacets.
 */
abstract class FacetValueSpace(
  val xmlValue: String,
  val primType: PrimType,
  val context: ThrowsSDE
) extends FacetOrdered {

  def facetName: String
  protected def isInvalidNarrowing(remoteValue: JBigDecimal): Boolean
  protected def narrowingRelation: String

  lazy val bigDecimalValue: JBigDecimal = {
    try {
      primType match {
        case PrimType.DateTime | PrimType.Date | PrimType.Time =>
          try {
            primType.fromXMLString(xmlValue).getCalendar.toJBigDecimal
          } catch {
            case e: InvalidPrimitiveDataException =>
              context.SDE(
                "Failed to parse %s facet value (%s) to %s.",
                facetName,
                xmlValue,
                primType.toString()
              )
          }
        case _ => new JBigDecimal(xmlValue)
      }
    } catch {
      case e: IllegalArgumentException =>
        context.SDE("invalid %s facet restriction: %s", facetName, e.getMessage)
    }
  }

  override final def narrow(remote: FacetOrdered): FacetOrdered = {
    val remoteFacet = remote.asInstanceOf[FacetValueSpace].bigDecimalValue
    if (isInvalidNarrowing(remoteFacet))
      context.SDE(
        "SimpleTypes: The local %s (%s) was %s the base %s (%s)",
        facetName,
        bigDecimalValue,
        narrowingRelation,
        facetName,
        remoteFacet
      )
    this
  }
}

/**
 * Facet for xs:totalDigits. Narrowing requires local <= base.
 * Inlines positive integer validation — value must be strictly
 * greater than zero.
 */
final class FacetTotalDigits(xmlValue: String, context: ThrowsSDE)
  extends FacetInteger(xmlValue, context) {
  val facetName = "totalDigits"
  val isNonNegative = false

  override def narrow(remote: FacetOrdered): FacetOrdered = {
    val remoteFacet = remote.asInstanceOf[FacetTotalDigits].bigIntValue
    if (bigIntValue.compareTo(remoteFacet) > 0)
      context.SDE(
        "SimpleTypes: The local %s (%s) was greater than the base %s (%s)",
        facetName,
        bigIntValue,
        facetName,
        remoteFacet
      )
    this
  }
}

/** Facet for xs:enumeration. */
final class FacetEnumeration(val xmlValue: String) extends FacetOrdered

/**
 * Facet for xs:pattern.
 *
 * The DFDL Infoset can contain strings which hold characters
 * that are not allowed in XML at all.
 *
 * In order to talk about these characters in a XSD pattern facet
 * we use a remapping of such characters into the Unicode
 * Private Use Area, so as to have XML-legal characters.
 *
 * See the section titled "XML Illegal Characters" on this web page:
 * https://daffodil.apache.org/infoset/
 *
 * Before processing a regex of these characters in Daffodil's pattern facet
 * validation, we must remap these PUA characters back to the originally
 * intended code points, since that's what the Infoset strings will contain.
 *
 * Consider the character code 0xB. This is illegal in XML v1.0 documents.
 * A DFDL Schema is an XML Schema, which is an XML document; hence, we cannot
 * use the character with code 0xB directly, nor can we use an XML numeric
 * character entity like &#xB; for it. The character is simply disallowed in
 * XML, including DFDL schemas. Hence, we mention 0xB by using a remapping of it
 * to the PUA area character 0xE00B, which we express by &#xE00B;
 * Hence a pattern regex like "[&#xE00B;&#x20;0-9a-zA-Z]" will match
 * the character with char code 0xB (remapped from E00B), as well as spaces,
 * and alphanumeric characters.
 *
 * The XSD numeric character entity &#xE000; can be used to match ASCII NUL
 * (char code 0).
 *
 * This remapping is for pattern facets, which are inside a DFDL schema,
 * and so will not contain CR characters, since XML reading will convert those
 * to LF. To discuss CR in this pattern we can't use `&#x0d;` syntax because that
 * turns into a CR which gets turned into a LF. Plus the pattern value is
 * an XML attribute, the value of which gets its whitespace collapsed, all
 * line-ending chars converted to spaces, and adjacent spaces collapsed to one.
 *
 * So a pattern facet must use `\r` and '\n' to describe line-endings within the pattern.
 * And in general one must be careful about whitespace.
 */
final class FacetPattern(val xmlValue: String) extends FacetOrdered {
  lazy val regex: Regex = XMLUtils.remapPUAToXMLIllegalCharacters(xmlValue).r
}

/** Facet for xs:length. Narrowing requires local == base exactly. */
final class FacetLength(xmlValue: String, context: ThrowsSDE)
  extends FacetNonNegativeInteger(xmlValue, context) {
  val facetName = "length"
  protected def isInvalidNarrowing(remote: JBigInt): Boolean =
    bigIntValue.compareTo(remote) != 0
  protected def narrowingRelation = "not equal to"
}

/** Facet for xs:minLength. Narrowing requires local >= base. */
final class FacetMinLength(xmlValue: String, context: ThrowsSDE)
  extends FacetNonNegativeInteger(xmlValue, context) {
  val facetName = "minLength"
  protected def isInvalidNarrowing(remote: JBigInt): Boolean =
    bigIntValue.compareTo(remote) < 0
  protected def narrowingRelation = "less than"
}

/** Facet for xs:maxLength. Narrowing requires local <= base. */
final class FacetMaxLength(xmlValue: String, context: ThrowsSDE)
  extends FacetNonNegativeInteger(xmlValue, context) {
  val facetName = "maxLength"
  protected def isInvalidNarrowing(remote: JBigInt): Boolean =
    bigIntValue.compareTo(remote) > 0
  protected def narrowingRelation = "greater than"
}

/** Facet for xs:fractionDigits. Narrowing requires local <= base. */
final class FacetFractionDigits(xmlValue: String, context: ThrowsSDE)
  extends FacetNonNegativeInteger(xmlValue, context) {
  val facetName = "fractionDigits"
  protected def isInvalidNarrowing(remote: JBigInt): Boolean =
    bigIntValue.compareTo(remote) > 0
  protected def narrowingRelation = "greater than"
}

/** Facet for xs:minInclusive. Narrowing requires local >= base. */
final class FacetMinInclusive(xmlValue: String, primType: PrimType, context: ThrowsSDE)
  extends FacetValueSpace(xmlValue, primType, context) {
  val facetName = "minInclusive"
  protected def isInvalidNarrowing(remote: JBigDecimal): Boolean =
    bigDecimalValue.compareTo(remote) < 0
  protected def narrowingRelation = "less than"
}

/** Facet for xs:maxInclusive. Narrowing requires local <= base. */
final class FacetMaxInclusive(xmlValue: String, primType: PrimType, context: ThrowsSDE)
  extends FacetValueSpace(xmlValue, primType, context) {
  val facetName = "maxInclusive"
  protected def isInvalidNarrowing(remote: JBigDecimal): Boolean =
    bigDecimalValue.compareTo(remote) > 0
  protected def narrowingRelation = "greater than"
}

/** Facet for xs:minExclusive. Narrowing requires local >= base. */
final class FacetMinExclusive(xmlValue: String, primType: PrimType, context: ThrowsSDE)
  extends FacetValueSpace(xmlValue, primType, context) {
  val facetName = "minExclusive"
  protected def isInvalidNarrowing(remote: JBigDecimal): Boolean =
    bigDecimalValue.compareTo(remote) < 0
  protected def narrowingRelation = "less than"
}

/** Facet for xs:maxExclusive. Narrowing requires local <= base. */
final class FacetMaxExclusive(xmlValue: String, primType: PrimType, context: ThrowsSDE)
  extends FacetValueSpace(xmlValue, primType, context) {
  val facetName = "maxExclusive"
  protected def isInvalidNarrowing(remote: JBigDecimal): Boolean =
    bigDecimalValue.compareTo(remote) > 0
  protected def narrowingRelation = "greater than"
}
