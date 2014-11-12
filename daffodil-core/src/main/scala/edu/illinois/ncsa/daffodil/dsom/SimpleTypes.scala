package edu.illinois.ncsa.daffodil.dsom

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import scala.xml.Node
import scala.collection.mutable.Queue
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType

abstract class SimpleTypeDefBase(xmlArg: Node, parent: SchemaComponent)
  extends AnnotatedSchemaComponent(xmlArg, parent)
  with SimpleTypeBase
  with DFDLStatementMixin
  with Facets
  with OverlapCheckMixin {

  def element: ElementBase

  override def encodingInfo = element.encodingInfo

  requiredEvaluations(myBaseTypeList)

  lazy val bases: Seq[SimpleTypeDefBase] =
    myBaseDef match {
      case None => Nil
      case Some(st: SimpleTypeDefBase) => st +: st.bases
      case _ => Nil
    }

  lazy val sTypeNonDefault: Seq[ChainPropProvider] = bases.map { _.nonDefaultFormatChain }
  lazy val sTypeDefault: Seq[ChainPropProvider] = bases.map { _.defaultFormatChain }

  // want a QueueSet i.e., fifo order if iterated, but duplicates
  // kept out of the set. Will simulate by calling distinct.
  lazy val nonDefaultPropertySources = nonDefaultPropertySources_.value
  private val nonDefaultPropertySources_ = LV('nonDefaultPropertySources) {
    val seq = (this.nonDefaultFormatChain +: sTypeNonDefault).distinct
    checkNonOverlap(seq)
    seq
  }

  lazy val defaultPropertySources = defaultPropertySources_.value
  private val defaultPropertySources_ = LV('defaultPropertySources) {
    val seq = (this.defaultFormatChain +: sTypeDefault).distinct
    seq
  }

  import edu.illinois.ncsa.daffodil.dsom.FacetTypes._

  def emptyFormatFactory = new DFDLSimpleType(newDFDLAnnotationXML("simpleType"), this)

  def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLSimpleType]

  def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:simpleType>{ contents @ _* }</dfdl:simpleType> => new DFDLSimpleType(node, this)
      case _ => annotationFactoryForDFDLStatement(node, this)
    }
  }

  // Returns name of base class in the form of QName
  //
  lazy val restrictionBase: String = {
    val rsb = xml \\ "restriction" \ "@base"
    if (rsb.length != 1) {
      context.SDE("Restriction base was not found.")
    }
    rsb.head.text
  }

  lazy val optPrimType: Option[PrimType] = {
    val (nsURI, localName) = baseTypeQName
    if (nsURI == XMLUtils.XSD_NAMESPACE) {
      // XSD namespace
      val prim = schemaDocument.schemaSet.getPrimType(nsURI, localName)
      schemaDefinitionUnless(prim != None,
        "Type {%s}%s is not an XSD primitive type.", nsURI, localName)
      prim
    } else None
  }

  lazy val myBaseDef = myBaseType match {
    case st: SimpleTypeDefBase => Some(st)
    case _ => None
  }

  lazy val myBaseTypeFactory = {
    Assert.invariant(restrictionBase.length() != 0)
    val (nsURI, localName) = baseTypeQName
    Assert.invariant(optPrimType == None)
    val factory = schemaDocument.schemaSet.getGlobalSimpleTypeDef(nsURI, localName)
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

  lazy val baseTypeQName = XMLUtils.QName(this.namespaces, restrictionBase, schemaDocument)

  lazy val myBaseType: SimpleTypeBase = {
    optPrimType match {
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

  lazy val localBaseFacets: ElemFacets = {
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

  lazy val combinedBaseFacets: Seq[FacetValue] = {
    val localF = localBaseFacets
    val remoteF = remoteBaseFacets

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

  lazy val remoteBaseFacets = remoteBaseFacets_.value
  private val remoteBaseFacets_ = LV('remoteBaseFacets) {
    myBaseType match {
      case gstd: GlobalSimpleTypeDef => gstd.combinedBaseFacets
      case prim: PrimType => Nil
      case _ => Assert.impossible()
    }
  }

  /**
   * Combine our statements with those of our base def (if there is one)
   *
   * The order is important here. I.e., we FIRST put in each list those from our base. Then our own local ones.
   */
  lazy val statements: Seq[DFDLStatement] = myBaseDef.map { _.statements }.getOrElse(Nil) ++ localStatements
  // TODO: refactor into shared code for combining all the annotations in the resolved set of annotations 
  // for a particular annotation point, checking that there is only one format annotation, that 
  // asserts and discriminators are properly excluding each-other, etc.
  // Code should be sharable for many kinds of annotation points, perhaps specialized for groups, complex type
  // elements, and simple type elements.
  //
  // See JIRA issue DFDL-481
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

  override lazy val element = parent

  lazy val baseName = (xml \ "restriction" \ "@base").text
  lazy val baseType = {
    val res = if (baseName == "") None
    else notYetImplemented("local simpleType with base attribute.") // should go find the global simple type here
  }

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

class GlobalSimpleTypeDefFactory(xmlArg: Node, schemaDocumentArg: SchemaDocument)
  extends SchemaComponent(xmlArg, schemaDocumentArg) with NamedMixin {
  // def forRoot() = new GlobalSimpleTypeDef(xml, schemaDocument, None)

  /**
   * Create a private instance for this element's use.
   */
  def forElement(element: ElementBase) = new GlobalSimpleTypeDef(None, xml, schemaDocument, Some(element))
  def forDerivedType(derivedType: SimpleTypeDefBase) = new GlobalSimpleTypeDef(Some(derivedType), xml, schemaDocument, None)

}
/**
 * The instance type for global simple type definitions.
 */

class GlobalSimpleTypeDef(derivedType: Option[SimpleTypeDefBase], xmlArg: Node, schemaDocumentArg: SchemaDocument, val referringElement: Option[ElementBase])
  extends SimpleTypeDefBase(xmlArg, schemaDocumentArg)
  with GlobalComponentMixin {

  override lazy val referringComponent: Option[SchemaComponent] =
    (derivedType, referringElement) match {
      case (Some(dt), None) => derivedType
      case (None, Some(elem)) => referringElement
      case _ => Assert.impossible("SimpleType must either have a derivedType or an element. Not both.")
    }

  override lazy val element: ElementBase = referringComponent match {
    case Some(dt: SimpleTypeDefBase) => dt.element
    case Some(e: ElementBase) => e
    case _ => Assert.invariantFailed("unexpected referringComponent")
  }

  override def prettyName = "simpleType." + name

}
