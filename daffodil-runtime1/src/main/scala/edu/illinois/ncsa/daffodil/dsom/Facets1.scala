package edu.illinois.ncsa.daffodil.dsom
import edu.illinois.ncsa.daffodil.util.Enum
import scala.util.matching.Regex

object Facet extends Enum {
  sealed abstract trait Type extends EnumValueType
  case object enumeration extends Type
  case object fractionDigits extends Type
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

object FacetTypes {
  // These were defined to make life simpler
  // TODO: Should we modify these to also include the name of the simpleType?
  type Values = String
  type ValuesR = Regex
  type FacetValue = (Facet.Type, Values)
  type FacetValueR = (Facet.Type, ValuesR)
  type ElemFacets = Seq[FacetValue]
  type ElemFacetsR = Seq[FacetValueR]
}
