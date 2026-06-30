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

import scala.xml.Node

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.iapi.InvalidRestrictionPolicy
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType
import org.apache.daffodil.runtime1.dsom.*

trait Facets { self: Restriction =>

  private def retrieveFacetValueFromRestrictionBase(
    xml: Node,
    facetName: Facet.Type
  ): String = {
    val res = xml \\ "restriction" \ facetName.toString() \ "@value"
    if (res.length > 0) res.head.text else ""
  }

  private def retrieveFacetValuesFromRestrictionBase(
    xml: Node,
    facetName: Facet.Type
  ): Seq[String] = {
    val res = xml \\ "restriction" \ facetName.toString() \\ "@value"
    val ret = if (res.length > 0) res.map(n => n.text).toList else List.empty
    ret
  }
  private def enumeration(xml: Node): Seq[String] = {
    retrieveFacetValuesFromRestrictionBase(xml, Facet.enumeration)
  }
  private def fractionDigits(xml: Node): String = {
    retrieveFacetValueFromRestrictionBase(xml, Facet.fractionDigits)
  }
  private def length(xml: Node): String = {
    retrieveFacetValueFromRestrictionBase(xml, Facet.length)
  }
  private def maxExclusive(xml: Node): String = {
    retrieveFacetValueFromRestrictionBase(xml, Facet.maxExclusive)
  }
  private def maxInclusive(xml: Node): String = {
    retrieveFacetValueFromRestrictionBase(xml, Facet.maxInclusive)
  }
  private def maxLength(xml: Node): String = {
    retrieveFacetValueFromRestrictionBase(xml, Facet.maxLength)
  }
  private def minExclusive(xml: Node): String = {
    retrieveFacetValueFromRestrictionBase(xml, Facet.minExclusive)
  }
  private def minInclusive(xml: Node): String = {
    retrieveFacetValueFromRestrictionBase(xml, Facet.minInclusive)
  }
  private def minLength(xml: Node): String = {
    retrieveFacetValueFromRestrictionBase(xml, Facet.minLength)
  }
  private def pattern(xml: Node): Seq[String] = {
    // Patterns are OR'd locally, AND'd remotely
    val res = retrieveFacetValuesFromRestrictionBase(xml, Facet.pattern).map(p => p)
    res
  }
  private def totalDigits(xml: Node): String = {
    retrieveFacetValueFromRestrictionBase(xml, Facet.totalDigits)
  }
  private def whitespace(xml: Node): String = {
    retrieveFacetValueFromRestrictionBase(xml, Facet.whiteSpace)
  }

  final lazy val localPatternValue: String = {
    // Patterns within a type are OR'd
    // per http://www.xfront.com/XML-Schema-library/papers/Algorithm-for-Merging-a-simpleType-Dependency-Chain.pdf
    //
    // Assumed to be valid RegEx
    val patterns = pattern(xml)
    patterns.mkString("|")
  }
  final lazy val localMinInclusiveValue: String = minInclusive(xml)
  final lazy val localMaxInclusiveValue: String = maxInclusive(xml)
  final lazy val localMinExclusiveValue: String = minExclusive(xml)
  final lazy val localMaxExclusiveValue: String = maxExclusive(xml)
  final lazy val localLengthValue: String = length(xml)
  final lazy val localMinLengthValue: String = {
    val ml = minLength(xml)
    // Xerces checks for the case where length and min/maxLength are used together,
    // so we won't get to this code in those cases unless Xerces validation is turned off
    Assert.usage(
      ml.isEmpty || localLengthValue.isEmpty,
      "Facets length and minLength cannot be specified together"
    )
    ml
  }
  final lazy val localMaxLengthValue: String = {
    val ml = maxLength(xml)
    // Xerces checks for the case where length and min/maxLength are used together,
    // so we won't get to this code in those cases unless Xerces validation is turned off
    Assert.usage(
      ml.isEmpty || localLengthValue.isEmpty,
      "Facets length and maxLength cannot be specified together"
    )
    ml
  }
  final lazy val localTotalDigitsValue: String = totalDigits(xml)
  final lazy val localFractionDigitsValue: String = fractionDigits(xml)
  final lazy val localEnumerationValue: String = {
    // Enumerations are OR'd
    // May be empty string
    // Must be unique
    val enumerations = enumeration(xml)
    val distinctEnums = enumerations.distinct
    if (enumerations.size != distinctEnums.size) SDE("Enumerations must be unique!")
    // Not a regular expression, but we plan to use it as one
    // so we must escape characters that can be interpreted as RegEx
    enumerations.map(s => escapeForRegex(s)).mkString("|")
  }
  final lazy val localWhitespaceValue: String = {
    whitespace(xml)
    SDE("whitespaceValue is not implemented for DFDL v1.0 schemas but reserved for future use.")
  }

  private def escapeForRegex(s: String): String = {
    val sb = new StringBuilder
    s.foreach(c => {
      c match {
        case '[' => sb.append("\\[")
        case '\\' => sb.append("\\\\")
        case '^' => sb.append("\\^")
        case '$' => sb.append("\\$")
        case '.' => sb.append("\\.")
        case '|' => sb.append("\\|")
        case '?' => sb.append("\\?")
        case '*' => sb.append("\\*")
        case '+' => sb.append("\\+")
        case '(' => sb.append("\\(")
        case ')' => sb.append("\\)")
        case '{' => sb.append("\\{")
        case '}' => sb.append("\\}")
        case x => sb.append(x)
      }
    })
    sb.toString()
  }

  final lazy val hasEnumeration: Boolean =
    localEnumerationValue.nonEmpty || remoteBaseFacets.collectFirst {
      case _: FacetEnumeration =>
    }.isDefined
  final lazy val hasPattern: Boolean =
    localPatternValue.nonEmpty || remoteBaseFacets.collectFirst { case _: FacetPattern =>
    }.isDefined
  final lazy val hasLength: Boolean =
    localLengthValue.nonEmpty || remoteBaseFacets.collectFirst { case _: FacetLength =>
    }.isDefined
  final lazy val hasMinLength: Boolean =
    localMinLengthValue.nonEmpty || remoteBaseFacets.collectFirst { case _: FacetMinLength =>
    }.isDefined
  final lazy val hasMaxLength: Boolean =
    localMaxLengthValue.nonEmpty || remoteBaseFacets.collectFirst { case _: FacetMaxLength =>
    }.isDefined
  final lazy val hasMinInclusive: Boolean =
    localMinInclusiveValue.nonEmpty || remoteBaseFacets.collectFirst {
      case _: FacetMinInclusive =>
    }.isDefined
  final lazy val hasMaxInclusive: Boolean =
    localMaxInclusiveValue.nonEmpty || remoteBaseFacets.collectFirst {
      case _: FacetMaxInclusive =>
    }.isDefined
  final lazy val hasMinExclusive: Boolean =
    localMinExclusiveValue.nonEmpty || remoteBaseFacets.collectFirst {
      case _: FacetMinExclusive =>
    }.isDefined
  final lazy val hasMaxExclusive: Boolean =
    localMaxExclusiveValue.nonEmpty || remoteBaseFacets.collectFirst {
      case _: FacetMaxExclusive =>
    }.isDefined
  final lazy val hasTotalDigits: Boolean =
    localTotalDigitsValue.nonEmpty || remoteBaseFacets.collectFirst {
      case _: FacetTotalDigits =>
    }.isDefined
  final lazy val hasFractionDigits: Boolean =
    localFractionDigitsValue.nonEmpty || remoteBaseFacets.collectFirst {
      case _: FacetFractionDigits =>
    }.isDefined

  final lazy val patternValues: Seq[FacetPattern] = {
    val valuesRemapped: Seq[FacetPattern] = combinedBaseFacets.collect { case f: FacetPattern =>
      f
    }
    (primType, tunable.invalidRestrictionPolicy) match {
      case (PrimType.String, _) =>
        valuesRemapped
      case (_, InvalidRestrictionPolicy.Validate) =>
        valuesRemapped
      case (_, InvalidRestrictionPolicy.Error) =>
        SDE(
          "Pattern restriction is only allowed to be applied to string and types derived from string."
        )
      case (_, InvalidRestrictionPolicy.Ignore) =>
        Nil
    }
  }

  final lazy val enumerationValues: Option[String] =
    combinedBaseFacets.collectFirst { case f: FacetEnumeration => f }.map(_.xmlValue)
  final lazy val lengthValue: java.math.BigDecimal =
    combinedBaseFacets
      .collectFirst { case f: FacetLength => f }
      .map(f => new java.math.BigDecimal(f.bigIntValue))
      .getOrElse(SDE("The length facet was not found."))
  final lazy val minLengthValue: java.math.BigDecimal =
    combinedBaseFacets
      .collectFirst { case f: FacetMinLength => f }
      .map(f => new java.math.BigDecimal(f.bigIntValue))
      .getOrElse(SDE("The minLength facet was not found."))
  final lazy val maxLengthValue: java.math.BigDecimal =
    combinedBaseFacets
      .collectFirst { case f: FacetMaxLength => f }
      .map(f => new java.math.BigDecimal(f.bigIntValue))
      .getOrElse(SDE("The maxLength facet was not found."))
  final lazy val minInclusiveValue: java.math.BigDecimal =
    combinedBaseFacets
      .collectFirst { case f: FacetMinInclusive => f }
      .map(_.bigDecimalValue)
      .getOrElse(SDE("The minInclusive facet was not found."))
  final lazy val maxInclusiveValue: java.math.BigDecimal =
    combinedBaseFacets
      .collectFirst { case f: FacetMaxInclusive => f }
      .map(_.bigDecimalValue)
      .getOrElse(SDE("The maxInclusive facet was not found."))
  final lazy val minExclusiveValue: java.math.BigDecimal =
    combinedBaseFacets
      .collectFirst { case f: FacetMinExclusive => f }
      .map(_.bigDecimalValue)
      .getOrElse(SDE("The minExclusive facet was not found."))
  final lazy val maxExclusiveValue: java.math.BigDecimal =
    combinedBaseFacets
      .collectFirst { case f: FacetMaxExclusive => f }
      .map(_.bigDecimalValue)
      .getOrElse(SDE("The maxExclusive facet was not found."))
  final lazy val totalDigitsValue: java.math.BigDecimal =
    combinedBaseFacets
      .collectFirst { case f: FacetTotalDigits => f }
      .map(f => new java.math.BigDecimal(f.bigIntValue))
      .getOrElse(SDE("The totalDigits facet was not found."))
  final lazy val fractionDigitsValue: java.math.BigDecimal =
    combinedBaseFacets
      .collectFirst { case f: FacetFractionDigits => f }
      .map(f => new java.math.BigDecimal(f.bigIntValue))
      .getOrElse(SDE("The fractionDigits facet was not found."))
}
