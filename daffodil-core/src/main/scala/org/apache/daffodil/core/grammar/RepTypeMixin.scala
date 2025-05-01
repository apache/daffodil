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

package org.apache.daffodil.core.grammar

import scala.xml.Elem
import scala.xml.Null
import scala.xml.UnprefixedAttribute

import org.apache.daffodil.core.dsom.ElementBase
import org.apache.daffodil.core.dsom.GlobalSimpleTypeDef
import org.apache.daffodil.core.dsom.RepTypeQuasiElementDecl
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.dpath.ComparisonOps
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.dpath.NumberCompareOp
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueNumber
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueString

trait RepTypeMixin { self: ElementBase =>

  lazy val hasRepType: Boolean = LV(Symbol("hasRepType")) {
    val hasRT = isSimpleType && findPropertyOption("repType").isDefined
    schemaDefinitionWhen(
      hasRT && isOutputValueCalc,
      "dfdl:outputValueCalc and dfdlx:repType cannot be defined on the same element"
    )
    hasRT
  }.value

  private lazy val repTypeGSTD: GlobalSimpleTypeDef = LV(Symbol("repTypeGSTD")) {
    // throws an SDE if the simple type def is not found or if it is not a simple type (e.g. a
    // primitive type)
    val gstd = schemaSet.getGlobalSimpleTypeDef(repType).getOrElse {
      errMissingGlobalReferenceNoPrim(
        repType,
        "dfdlx:repType",
        "global simpleType definition"
      )
    }
    schemaDefinitionUnless(
      gstd.primType.isInstanceOf[NodeInfo.Integer.Kind],
      "dfdlx:repType (%s) must resolve to a global simple type definition derived from xs:integer, but was %s",
      repType,
      gstd.primType.globalQName
    )
    gstd
  }.value

  lazy val repTypeElementDecl: RepTypeQuasiElementDecl = LV(Symbol("repTypeElementDecl")) {
    // this quasi element must use the in-scope namespaces from where the repType property was
    // defined, which isn't necessarily the same as the in-scope namespaces on this element,
    // since the repType property could be defined on a simpleType in another file with
    // completely different namesapce prefixes. This information is available on the Found
    // class, but the generated property code does not make that available for repType, so we
    // must manually do a lookup here.
    val repTypeNamespaces = findProperty("repType").location.namespaces
    val xmlElem = Elem(
      null,
      "QuasiElementForRepType",
      new UnprefixedAttribute("type", repType.toString, Null),
      repTypeNamespaces,
      true
    )
    RepTypeQuasiElementDecl(xmlElem, repTypeGSTD)
  }.value

  lazy val optRepTypeElementDecl: Option[RepTypeQuasiElementDecl] =
    LV(Symbol("optRepTypeElementDecl")) {
      if (hasRepType) Some(repTypeElementDecl) else None
    }.value

  lazy val (
    repTypeCompareLT: NumberCompareOp,
    repTypeCompareLE: NumberCompareOp
  ) = LV(Symbol("repTypeComparers")) {
    Assert.invariant(
      repTypeElementDecl.primType.isSubtypeOf(NodeInfo.Integer),
      "repType should only be an integer type"
    )
    val comparisonOps = ComparisonOps.forType(repTypeElementDecl.primType)
    (comparisonOps.lt, comparisonOps.le)
  }.value

  lazy val (
    repTypeParseValuesMap: Map[DataValueNumber, DataValueString],
    repTypeParseRangesMap: Seq[(DataValueNumber, DataValueNumber, DataValueString)],
    repTypeUnparseMap: Map[DataValueString, DataValueNumber]
  ) = LV(Symbol("repTypeMaps")) {

    schemaDefinitionUnless(
      primType.isInstanceOf[NodeInfo.String.Kind],
      "Type must derive from xs:string when dfdlx:repType (%s) is defined: %s",
      repType,
      primType.globalQName
    )

    schemaDefinitionWhen(
      typeDef.optRestriction.isEmpty,
      "A restriction is required when dfdlx:repType (%s) is defined",
      repType
    )

    val restriction = typeDef.optRestriction.get

    schemaDefinitionWhen(
      restriction.enumerations.isEmpty,
      "There must be at least one enumeration defined when dfdlx:repType (%s) is defined",
      repType
    )

    val lt = repTypeCompareLT

    // This is Seq of attributes extracted from each enumeration, where each item in the
    // sequence is a 3-tuple containing the cooked repValues, repValueRanges, and enum value
    // from the associated enum
    val enumAttributes = restriction.enumerations.map { enumVal =>
      enumVal.schemaDefinitionWhen(
        enumVal.repValuesRaw.isEmpty && enumVal.repValueRangesRaw.isEmpty,
        "All enumerations must define dfdlx:repValues and/or dfdlx:repValueRanges when dfdlx:repType (%s) is defined",
        repType
      )

      val repValues: Seq[DataValueNumber] =
        enumVal.repValuesRaw
          .map(repTypeElementDecl.primType.fromXMLString(_).asInstanceOf[DataValueNumber])
      val repValueRanges: Seq[(DataValueNumber, DataValueNumber)] =
        enumVal.repValueRangesRaw
          .map(repTypeElementDecl.primType.fromXMLString(_).asInstanceOf[DataValueNumber])
          .sliding(2, 2)
          .map { case Seq(low, high) =>
            enumVal.schemaDefinitionUnless(
              lt.operate(low, high).getBoolean,
              "dfdlx:repValueRanges low value (%s) must be less than high value (%s)",
              low.value,
              high.value
            )
            (low, high)
          }
          .toSeq
      val enumValue = enumVal.enumValueCooked.asInstanceOf[DataValueString]
      (repValues, repValueRanges, enumValue)
    }

    // combine all rep values and ranges into a single seq of low/high tuples, sort them by
    // their low value, and look for overlaps
    enumAttributes
      .flatMap { case (repValues, repRanges, _) => repValues.map(v => (v, v)) ++ repRanges }
      .sortWith { case ((l1, _), (l2, _)) => lt.operate(l1, l2).getBoolean }
      .reduce[(DataValueNumber, DataValueNumber)] { case ((l1, h1), (l2, h2)) =>
        restriction.schemaDefinitionUnless(
          lt.operate(h1, l2).getBoolean,
          "Overlapping dfdlx:%s (%s) and dfdlx:%s (%s) found",
          if (lt.operate(l1, h1).getBoolean) "repValueRanges" else "repValues",
          if (lt.operate(l1, h1).getBoolean) l1.value.toString + " " + h1.value.toString
          else l1.value.toString,
          if (lt.operate(l2, h2).getBoolean) "repValueRanges" else "repValues",
          if (lt.operate(l2, h2).getBoolean) l2.value.toString + " " + h2.value.toString
          else l2.value.toString
        )
        (l2, h2)
      }

    // generate the data structures for fast mapping of rep values to/from logical values
    val parseValuesMap: Map[DataValueNumber, DataValueString] = enumAttributes
      .filterNot(_._1.isEmpty)
      .flatMap { case (repValues, _, enumValue) =>
        repValues.map { repValue => (repValue, enumValue) }
      }
      .toMap
    val parseRangesMap: Seq[(DataValueNumber, DataValueNumber, DataValueString)] =
      enumAttributes
        .filterNot(_._2.isEmpty)
        .flatMap { case (_, repRanges, enumValue) =>
          repRanges.map { case (low, high) => (low, high, enumValue) }
        }
    val unparseMap: Map[DataValueString, DataValueNumber] = enumAttributes.map {
      case (repValues, repRanges, enumValue) =>
        val canonicalRep = repValues.headOption
          .orElse(repRanges.headOption.map(_._1))
          .getOrElse(
            // $COVERAGE-OFF$
            Assert.invariantFailed("Must have at least one of repValues or repValuesRanges")
            // $COVERAGE-ON$
          )
        (enumValue, canonicalRep)
    }.toMap
    (parseValuesMap, parseRangesMap, unparseMap)
  }.value

}
