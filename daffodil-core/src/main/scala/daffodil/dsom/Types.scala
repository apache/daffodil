package daffodil.dsom

import scala.xml._
import daffodil.exceptions._
import daffodil.grammar._
import daffodil.xml._
import scala.collection.mutable.Queue
import scala.util.matching.Regex
import daffodil.util.TestUtils

/////////////////////////////////////////////////////////////////
// Type System
/////////////////////////////////////////////////////////////////

trait TypeBase // (xmlArg : Node, context : SchemaComponent) 
  extends SharedPropertyLists // extends SchemaComponent(xmlArg)

trait SimpleTypeBase
  extends TypeBase
  with DiagnosticsProviding {

  def context: SchemaComponent
  def primitiveType: PrimitiveType

}

trait Facets {
  import Facet._
  def retrieveFacetValueFromRestrictionBase(xml: Node, facetName: Facet): String = {
    val res = xml \\ "restriction" \ facetName.toString() \ "@value"
    if (res.length > 0) res.head.text else ""
  }
  def retrieveFacetValuesFromRestrictionBase(xml: Node, facetName: Facet): Seq[String] = {
    val res = xml \\ "restriction" \ facetName.toString() \\ "@value"
    if (res.length > 0) res.map(n => n.text).toList else List.empty
  }
  def enumeration(xml: Node): Seq[String] = { retrieveFacetValuesFromRestrictionBase(xml, Facet.enumeration) }
  def fractionDigits(xml: Node): String = { retrieveFacetValueFromRestrictionBase(xml, Facet.fractionDigits) }
  def maxExclusive(xml: Node): String = { retrieveFacetValueFromRestrictionBase(xml, Facet.maxExclusive) }
  def maxInclusive(xml: Node): String = { retrieveFacetValueFromRestrictionBase(xml, Facet.maxInclusive) }
  def maxLength(xml: Node): String = { retrieveFacetValueFromRestrictionBase(xml, Facet.maxLength) }
  def minExclusive(xml: Node): String = { retrieveFacetValueFromRestrictionBase(xml, Facet.minExclusive) }
  def minInclusive(xml: Node): String = { retrieveFacetValueFromRestrictionBase(xml, Facet.minInclusive) }
  def minLength(xml: Node): String = { retrieveFacetValueFromRestrictionBase(xml, Facet.minLength) }
  def pattern(xml: Node): Seq[String] = {
    // Patterns are OR'd locally, AND'd remotely
    retrieveFacetValuesFromRestrictionBase(xml, Facet.pattern).map(p => p)
  }
  def totalDigits(xml: Node): String = { retrieveFacetValueFromRestrictionBase(xml, Facet.totalDigits) }
  def whitespace(xml: Node): String = { retrieveFacetValueFromRestrictionBase(xml, Facet.whiteSpace) }
}

object Facet extends Enumeration {
  type Facet = Value
  val enumeration, fractionDigits, maxExclusive, maxInclusive, maxLength, minExclusive, minInclusive, minLength, pattern, totalDigits, whiteSpace = Value
}

object FacetTypes {
  // These were defined to make life simpler
  // TODO: Should we modify these to also include the name of the simpleType?
  type Values = Seq[String]
  type ValuesR = Seq[Regex]
  type FacetValue = (Facet.Facet, Values)
  type FacetValueR = (Facet.Facet, ValuesR)
  type ElemFacets = Seq[FacetValue]
  type ElemFacetsR = Seq[FacetValueR]
}

abstract class SimpleTypeDefBase(xmlArg: Node, val parent: SchemaComponent)
  extends AnnotatedSchemaComponent(xmlArg)
  with SimpleTypeBase
  with DFDLStatementMixin
  with Facets {

  import daffodil.dsom.FacetTypes._

  def emptyFormatFactory = new DFDLSimpleType(newDFDLAnnotationXML("simpleType"), this)

  def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSimpleType]

  def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:simpleType>{ contents @ _* }</dfdl:simpleType> => new DFDLSimpleType(node, this)
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  lazy val localAndFormatRefProperties: Map[String, String] = {
    this.formatAnnotation.getFormatPropertiesNonDefault()
  }

  lazy val localProperties = {
    this.formatAnnotation.combinedLocalProperties
  }

  lazy val formatRefProperties = {
    this.formatAnnotation.formatRefProperties
  }

  lazy val combinedSimpleTypeAndBaseProperties = {
    schemaDefinition(overlappingLocalProperties.size == 0,
      "Overlap detected between the local SimpleType ("
        + this.detailName + ") properties and its base.")

    val props = this.localAndFormatRefProperties ++ this.simpleTypeBaseProperties
    props
  }

  // Returns name of base class in the form of
  // ex:myType
  //
  lazy val restrictionBase: String = {
    val rsb = xml \\ "restriction" \ "@base"
    rsb.head.text
  }

  // Returns pattern value of base class
  // 11/1/2012
  lazy val localPatternValue: Seq[String] = pattern(xml)

  // Returns minInclusive value of base class
  // 11/1/2012
  lazy val localMinInclusiveValue: String = minInclusive(xml)

  // Returns maxInclusive value of base class
  // 11/1/2012
  lazy val localMaxInclusiveValue: String = maxInclusive(xml)

  // Returns minExclusive value of base class
  // 11/1/2012
  lazy val localMinExclusiveValue: String = minExclusive(xml)

  // Returns maxExclusive value of base class
  // 11/1/2012
  lazy val localMaxExclusiveValue: String = maxExclusive(xml)

  // Returns minLength value of base class
  // 11/1/2012
  lazy val localMinLengthValue: String = minLength(xml)

  // Returns maxLength value of base class
  // 11/1/2012
  lazy val localMaxLengthValue: String = maxLength(xml)

  // Returns totalDigits value of base class
  // 11/1/2012
  lazy val localTotalDigitsValue: String = totalDigits(xml)

  // Returns fractionDigitsValue value of base class
  // 11/1/2012
  lazy val localFractionDigitsValue: String = fractionDigits(xml)

  // Returns enumeration values of base class
  // 11/1/2012
  lazy val localEnumerationValues: Seq[String] = enumeration(xml)

  // Returns whitespace value of base class
  // 11/1/2012
  lazy val localWhitespaceValue: String = {
    whitespace(xml)
    Assert.notYetImplemented("whitespaceValue is not implemented for DFDL v1.0 schemas but reserved for future use.")
  }

  lazy val myPrimitiveType = {
    val (nsURI, localName) = baseTypeQName
    if (nsURI == XMLUtils.XSD_NAMESPACE
      ||
      nsURI == XMLUtils.DFDL_SUBSET_NAMESPACE) { // tolerate use of this subset.
      // XSD namespace
      val prim = schemaDocument.schema.schemaSet.getPrimitiveType(localName)
      schemaDefinition(prim != None,
        "Type " + localName + " is not an XSD primitive type.")
      prim
    } else None
  }

  lazy val myBaseTypeFactory = {
    Assert.invariant(restrictionBase.length() != 0)
    val (nsURI, localName) = baseTypeQName
    Assert.invariant(myPrimitiveType == None)
    val factory = schemaDocument.schema.schemaSet.getGlobalSimpleTypeDef(nsURI, localName)
    factory
  }

  /**
   * Follows all indirections to get you the ultimate primitive
   * built-in simple type that must underlie all simple types
   * eventually.
   */
  lazy val primitiveType = {
    myBaseType.primitiveType
  }

  lazy val baseTypeQName = XMLUtils.QName(xml, restrictionBase, schemaDocument)

  lazy val myBaseType: SimpleTypeBase = {
    myPrimitiveType match {
      case Some(pt) => pt
      case None => {
        val bt = myBaseTypeFactory.map { _.forDerivedType(this) }
        bt match {
          case None => schemaDefinitionError("No type found for base: " + baseTypeQName)
          case Some(bt) => bt
        }
      }
    }
  }

  lazy val myBaseTypeList = List(myBaseType)

  lazy val diagnosticChildren = annotationObjs ++ myBaseTypeList

  lazy val simpleTypeBaseProperties: Map[String, String] = {

    val baseProps = {
      myBaseDef match {
        case Some(st) => st.localAndFormatRefProperties
        case None => Map.empty[String, String]
      }
    }
    baseProps
  }

  lazy val myBaseDef = myBaseType match {
    case st: SimpleTypeDefBase => Some(st)
    case _ => None
  }

  lazy val overlappingLocalProperties = {
    val localAndFormatRef = localAndFormatRefProperties.map { x => x._1 }.toSet
    val baseProps = simpleTypeBaseProperties.map { x => x._1 }.toSet
    val intersect = localAndFormatRef.intersect(baseProps)
    intersect
  }

  lazy val hasOverlap: Boolean = {
    if (overlappingLocalProperties.size > 0) {
      true
    } else {
      false
    }
  }

  override lazy val allNonDefaultProperties = {
    schemaDefinition(!hasOverlap, "Overlap detected between simpleType (" + this.detailName + ") and its base.")

    val theLocalUnion = this.combinedSimpleTypeAndBaseProperties
    theLocalUnion
  }
  import Facet._
  private val facetList: List[Facet] = List(Facet.enumeration, Facet.fractionDigits,
    Facet.maxExclusive, Facet.maxInclusive, Facet.maxLength, Facet.minExclusive,
    Facet.minInclusive, Facet.minLength, Facet.pattern, Facet.totalDigits, Facet.whiteSpace)

  lazy val patternValues: Seq[ElemFacetsR] = {
    val remotePatternValues = combinedBaseFacets.filter { s => s.filter { case (f, _) => f == Facet.pattern }.size > 0 }
    if (remotePatternValues.size > 0) {
      val res: Seq[ElemFacetsR] = remotePatternValues.map(s => s.map { case (f, v) => (f, v.map(r => r.r)) })
      res
    } else Seq.empty
  }
  lazy val localBaseFacets: ElemFacets = {
    var myFacets: Queue[FacetValue] = Queue.empty
    if (localPatternValue.length > 0) { myFacets.enqueue((Facet.pattern, localPatternValue)) }
    val res: ElemFacets = myFacets.toSeq
    res
  }

  lazy val combinedBaseFacets: Seq[ElemFacets] = {
    //localBaseFacets.union(remoteBaseFacets)
    // Here we really want a Set of Set[(Facet, Any)]
    val local: Seq[ElemFacets] = Seq(localBaseFacets)
    val combined: Seq[ElemFacets] = local.union(remoteBaseFacets)
    combined
  }

  lazy val remoteBaseFacets = remoteBaseFacets_.value
  private lazy val remoteBaseFacets_ = LV {
    val (ns, localPart) = this.baseTypeQName
    val ss = schema.schemaSet
    val prim = ss.getPrimitiveType(localPart)
    if (prim != None) Seq.empty[ElemFacets]
    else {
      val gstd = ss.getGlobalSimpleTypeDef(ns, localPart)
      val res = gstd match {
        case Some(gstdFactory) => Some(gstdFactory.forRoot())
        case None => schemaDefinitionError("Error while fetching facets.  No type definition found for %s", restrictionBase)
      }
      res.get.combinedBaseFacets
    }
  }

  /**
   * Combine our statements with those of our base def (if there is one)
   *
   * The order is important here. I.e., we FIRST put in each list those from our base. Then our own local ones.
   */
  lazy val statements: Seq[DFDLStatement] = myBaseDef.map { _.statements }.getOrElse(Nil) ++ localStatements
  lazy val newVariableInstanceStatements: Seq[DFDLNewVariableInstance] =
    myBaseDef.map { _.newVariableInstanceStatements }.getOrElse(Nil) ++ localNewVariableInstanceStatements
  lazy val (discriminatorStatements, assertStatements) = checkDiscriminatorsAssertsDisjoint(combinedDiscrims, combinedAsserts)
  private lazy val combinedAsserts: Seq[DFDLAssert] = myBaseDef.map { _.assertStatements }.getOrElse(Nil) ++ localAssertStatements
  private lazy val combinedDiscrims: Seq[DFDLDiscriminator] = myBaseDef.map { _.discriminatorStatements }.getOrElse(Nil) ++ localDiscriminatorStatements

  lazy val setVariableStatements: Seq[DFDLSetVariable] = {
    val combinedSvs = myBaseDef.map { _.setVariableStatements }.getOrElse(Nil) ++ localSetVariableStatements
    checkDistinctVariableNames(combinedSvs)
  }

}

class LocalSimpleTypeDef(xmlArg: Node, parent: ElementBase)
  extends SimpleTypeDefBase(xmlArg, parent)
  with LocalComponentMixin {

  lazy val detailName = "inside " + parent.detailName

  lazy val baseName = (xml \ "restriction" \ "@base").text
  lazy val baseType = {
    val res = if (baseName == "") None
    else Assert.notYetImplemented() // should go find the global simple type here
  }

  lazy val prettyName = "simpleType" // Of(" + parent.name + ")"
}

/**
 * We need a schema document and such for our primitives
 * so that our invariant, that *everything* has a schema document, schema, and schema set
 * holds true.
 */
object Fakes {
  lazy val sch = TestUtils.dfdlTestSchema(
    <dfdl:format ref="tns:daffodilTest1"/>,
    <xs:element name="fake" type="xs:string" dfdl:lengthKind="delimited"/>)
  lazy val xsd_sset = new SchemaSet(sch, "http://example.com", "fake")
  lazy val xsd_schema = xsd_sset.getSchema("http://example.com").get
  lazy val xsd_sd = xsd_schema.schemaDocuments(0)
  lazy val fakeElem = xsd_sd.getGlobalElementDecl("fake").get.forRoot()
}

//TBD: are Primitives "global", or do they just have names like globals do?
class PrimitiveType(name_ : String)
  extends SchemaComponent(<primitive/>)
  with SimpleTypeBase // use fake schema document
  with NamedMixin {

  val primitiveType = this

  override def toString = "PrimitiveType(" + prettyName + ")"

  override lazy val name = name_
  override lazy val prettyName = name_
  override lazy val scPath = ""
  lazy val diagnosticChildren = Nil

  // override val xml = Assert.invariantFailed("Primitives don't have xml definitions.")

  override lazy val schemaDocument = Fakes.xsd_sd

  lazy val localAndFormatRefProperties = Map.empty[String, String]

}

/**
 * The factory is sharable even though the global object it creates cannot
 * be shared.
 *
 * Call forElement(element) and supply the element referring
 * to the global type, then you get back an instance that is one-to-one with the
 * element.
 *
 * This then allows attributes of the type to refer to the element in deciding things.
 * I.e., the context is clear and kept separate for each place a global type is used.
 */

class GlobalSimpleTypeDefFactory(val xml: Node, schemaDocument: SchemaDocument)
  extends NamedMixin {
  def forRoot() = new GlobalSimpleTypeDef(xml, schemaDocument, None)

  /**
   * Create a private instance for this element's use.
   */
  def forElement(element: ElementBase) = new GlobalSimpleTypeDef(xml, schemaDocument, Some(element))
  def forDerivedType(derivedType: SimpleTypeDefBase) = new GlobalSimpleTypeDef(xml, schemaDocument, None)
}
/**
 * The instance type for global simple type definitions.
 */

class GlobalSimpleTypeDef(xmlArg: Node, schemaDocumentArg: SchemaDocument, val element: Option[AnnotatedMixin])
  extends SimpleTypeDefBase(xmlArg, schemaDocumentArg) with NamedMixin
  with GlobalComponentMixin {

  def schemaDocument = schemaDocumentArg

}

abstract class ComplexTypeBase(xmlArg: Node, val parent: SchemaComponent)
  extends SchemaComponent(xmlArg)
  with TypeBase
  with ComplexTypeBaseGrammarMixin {
  def element: ElementBase

  lazy val <complexType>{ xmlChildren @ _* }</complexType> = xml

  lazy val Seq(modelGroup) = smg.value
  lazy val smg = LV {
    xmlChildren.flatMap { GroupFactory(_, this, 1) }
  }

  // provides needed polymorphism across unannotated complex types, and
  // the annotated objects.
  lazy val localAndFormatRefProperties: Map[String, String] = {
    Map.empty[String, String]
  }

  lazy val diagnosticChildren = List(modelGroup)
}

class GlobalComplexTypeDefFactory(val xml: Node, schemaDocument: SchemaDocument)
  extends NamedMixin {
  def forElement(element: ElementBase) = new GlobalComplexTypeDef(xml, schemaDocument, element)
}

class GlobalComplexTypeDef(xmlArg: Node, schemaDocumentArg: SchemaDocument, val element: ElementBase)
  extends ComplexTypeBase(xmlArg, schemaDocumentArg)
  with GlobalComponentMixin {
  def schemaDocument = schemaDocumentArg

}

class LocalComplexTypeDef(xmlArg: Node, val element: ElementBase)
  extends ComplexTypeBase(xmlArg, element)
  with LocalComponentMixin {

  lazy val prettyName = "complexType" // Of(" + element.name + ")"
}
