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
import scala.util.matching.Regex

import org.apache.daffodil.lib.util.Enum

object Facet extends Enum {
  sealed abstract trait Type extends EnumValueType
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

  private val lengthFacets: Set[Facet.Type] = Set(
    Facet.length,
    Facet.minLength,
    Facet.maxLength,
    Facet.totalDigits,
    Facet.fractionDigits
  )

  /**
   * Returns true if the facet is a "length-family" facet: length, minLength,
   * maxLength, totalDigits, or fractionDigits.
   *
   * These facets are always non-negative integer constraints, independent of
   * the element's primitive type, so their values are parsed as plain integers
   * rather than in the type's value space (which matters for date/time types,
   * whose value-space facets parse as calendars).
   *
   * @param facetType the facet type to classify
   * @return true if the facet is a length-family facet
   */
  def isLengthFamilyFacet(facetType: Facet.Type): Boolean =
    lengthFacets.contains(facetType)
}

object FacetTypes {
  type Values = String
  type ValuesR = Regex
  type FacetValue = (Facet.Type, Values)
  type FacetValueR = (Facet.Type, ValuesR)
  type ElemFacets = Seq[FacetValue]
  type ElemFacetsR = Seq[FacetValueR]
}
