/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.dsom.FacetTypes
import scala.xml.NamespaceBinding
import scala.xml.PrefixedAttribute
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.dsom.DPathElementCompileInfo
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.Representation
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.processors.unparsers.NextElementResolver
import edu.illinois.ncsa.daffodil.util.TransientParam
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.YesNo
import edu.illinois.ncsa.daffodil.dsom.PropertyLookupResult

trait HasSlotIndexInParent {
  def slotIndexInParent: Int
}

/**
 * Singleton. If found as the default value, means to use nil as
 * the default value instead of an actual value.
 */
object UseNilForDefault

class ElementRuntimeData(
  /**
   * These transient by-name args are part of how we hook these objects into a
   * parent-child tree without having to use an assignment to a var. Note that
   * all transient elements must be added to the preSerialization method below
   * to allow parser serialization/deserialization to work.
   */
  @TransientParam parentArg: => Option[ElementRuntimeData],
  @TransientParam parentTermArg: => Maybe[TermRuntimeData],
  @TransientParam childrenArg: => Seq[ElementRuntimeData],
  @TransientParam variableMapArg: => VariableMap,
  @TransientParam nextElementResolverArg: => NextElementResolver,
  @TransientParam childElementResolverArg: => NextElementResolver,
  @TransientParam encInfoArg: => EncodingRuntimeData,
  @TransientParam dpathElementCompileInfoArg: => DPathElementCompileInfo,
  @TransientParam schemaFileLocationArg: => SchemaFileLocation,
  @TransientParam prettyNameArg: => String,
  @TransientParam pathArg: => String,
  @TransientParam namespacesArg: => NamespaceBinding,
  @TransientParam minimizedScopeArg: => NamespaceBinding,
  @TransientParam uniqueScopeArg: => NamespaceBinding,
  @TransientParam defaultBitOrderArg: => BitOrder,
  @TransientParam optPrimTypeArg: => Option[PrimType],
  @TransientParam targetNamespaceArg: => NS,
  @TransientParam thisElementsNamespaceArg: => NS,
  @TransientParam patternValuesArg: => Option[Seq[FacetTypes.FacetValueR]],
  @TransientParam enumerationValuesArg: => Option[String],
  @TransientParam minLengthArg: => Option[java.math.BigDecimal],
  @TransientParam maxLengthArg: => Option[java.math.BigDecimal],
  @TransientParam minInclusiveArg: => Option[java.math.BigDecimal],
  @TransientParam maxInclusiveArg: => Option[java.math.BigDecimal],
  @TransientParam minExclusiveArg: => Option[java.math.BigDecimal],
  @TransientParam maxExclusiveArg: => Option[java.math.BigDecimal],
  @TransientParam totalDigitsArg: => Option[java.math.BigDecimal],
  @TransientParam fractionDigitsArg: => Option[java.math.BigDecimal],
  @TransientParam minOccursArg: => Option[Int],
  @TransientParam maxOccursArg: => Option[Int],
  @TransientParam nameArg: => String,
  @TransientParam targetNamespacePrefixArg: => String,
  @TransientParam thisElementsNamespacePrefixArg: => String,
  @TransientParam isHiddenArg: => Boolean,
  @TransientParam nChildSlotsArg: => Int,
  @TransientParam slotIndexInParentArg: => Int,
  @TransientParam isNillableArg: => Boolean,
  @TransientParam isArrayArg: => Boolean, // can have more than 1 occurrence
  @TransientParam isOptionalArg: => Boolean, // can have only 0 or 1 occurrence
  @TransientParam isRequiredArg: => Boolean, // must have at least 1 occurrence
  /**
   * This is the properly qualified name for recognizing this
   * element.
   *
   * This takes into account xs:schema's elementFormDefault attribute.
   * If 'qualified' then there will be a namespace component.
   * If 'unqualified' the the namespace component will be No_Namespace.
   */
  @TransientParam namedQNameArg: => NamedQName,
  @TransientParam isRepresentedArg: => Boolean,
  @TransientParam couldHaveTextArg: => Boolean,
  @TransientParam alignmentValueInBitsArg: => Int,
  @TransientParam hasNoSkipRegionsArg: => Boolean,
  @TransientParam impliedRepresentationArg: => Representation,
  @TransientParam optIgnoreCaseArg: => Option[YesNo],
  @TransientParam optDefaultValueArg: => Option[AnyRef],
  //
  // Unparser-specific arguments
  //
  /**
   * pass true for this if the corresponding infoset element is never
   * accessed by way of expressions. Enables the element to be dropped
   * from the infoset immediately after unparsing is complete.
   */
  @TransientParam notReferencedByExpressionsArg: => Boolean,
  @TransientParam fillByteValueArg: => Int,
  @TransientParam optTruncateSpecifiedLengthStringArg: => Option[Boolean],
  @TransientParam outputValueCalcExprArg: => Option[CompiledExpression[AnyRef]])
  extends TermRuntimeData(parentArg, parentTermArg, encInfoArg, dpathElementCompileInfoArg, isRepresentedArg, couldHaveTextArg, alignmentValueInBitsArg, hasNoSkipRegionsArg,
    fillByteValueArg, defaultBitOrderArg, optIgnoreCaseArg)
  with HasSlotIndexInParent {

  lazy val parent = parentArg
  lazy val parentTerm = parentTermArg
  lazy val children = childrenArg
  lazy val variableMap = variableMapArg
  lazy val nextElementResolver = nextElementResolverArg
  lazy val childElementResolver = childElementResolverArg
  lazy val encInfo = encInfoArg
  lazy val dpathElementCompileInfo = dpathElementCompileInfoArg
  lazy val schemaFileLocation = schemaFileLocationArg
  lazy val prettyName = prettyNameArg
  lazy val path = pathArg
  lazy val namespaces = namespacesArg
  lazy val minimizedScope = minimizedScopeArg
  lazy val uniqueScope = uniqueScopeArg
  lazy val optPrimType = optPrimTypeArg
  lazy val targetNamespace = targetNamespaceArg
  lazy val thisElementsNamespace = thisElementsNamespaceArg
  lazy val patternValues = patternValuesArg
  lazy val enumerationValues = enumerationValuesArg
  lazy val minLength = minLengthArg
  lazy val maxLength = maxLengthArg
  lazy val minInclusive = minInclusiveArg
  lazy val maxInclusive = maxInclusiveArg
  lazy val minExclusive = minExclusiveArg
  lazy val maxExclusive = maxExclusiveArg
  lazy val totalDigits = totalDigitsArg
  lazy val fractionDigits = fractionDigitsArg
  lazy val minOccurs = minOccursArg
  lazy val maxOccurs = maxOccursArg
  lazy val name = nameArg
  lazy val targetNamespacePrefix = targetNamespacePrefixArg
  lazy val thisElementsNamespacePrefix = thisElementsNamespacePrefixArg
  lazy val isHidden = isHiddenArg
  lazy val nChildSlots = nChildSlotsArg
  lazy val slotIndexInParent = slotIndexInParentArg
  lazy val isNillable = isNillableArg
  lazy val isArray = isArrayArg
  lazy val isOptional = isOptionalArg
  lazy val isRequired = isRequiredArg
  lazy val namedQName = namedQNameArg
  lazy val impliedRepresentation = impliedRepresentationArg
  lazy val optDefaultValue = optDefaultValueArg
  lazy val notReferencedByExpressions = notReferencedByExpressionsArg
  lazy val optTruncateSpecifiedLengthString = optTruncateSpecifiedLengthStringArg
  lazy val outputValueCalcExpr = outputValueCalcExprArg

  override def preSerialization: Unit = {
    super.preSerialization
    parent
    parentTerm
    children
    variableMap
    nextElementResolver
    childElementResolver
    encInfo
    dpathElementCompileInfo
    schemaFileLocation
    prettyName
    path
    namespaces
    minimizedScope
    uniqueScope
    optPrimType
    targetNamespace
    thisElementsNamespace
    patternValues
    enumerationValues
    minLength
    maxLength
    minInclusive
    maxInclusive
    minExclusive
    maxExclusive
    totalDigits
    fractionDigits
    minOccurs
    maxOccurs
    name
    targetNamespacePrefix
    thisElementsNamespacePrefix
    isHidden
    nChildSlots
    slotIndexInParent
    isNillable
    isArray
    isOptional
    isRequired
    namedQName
    impliedRepresentation
    optDefaultValue
    notReferencedByExpressions
    optTruncateSpecifiedLengthString
    outputValueCalcExpr
  }

  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

  lazy val rootERD: ElementRuntimeData = parent.map { _.rootERD }.getOrElse(this)

  final def childERDs = children

  def isSimpleType = optPrimType.isDefined

  def schemaURIStringsForFullValidation = schemaURIStringsForFullValidation1.distinct
  private def schemaURIStringsForFullValidation1: Seq[String] = (schemaFileLocation.uriString +:
    childERDs.flatMap { _.schemaURIStringsForFullValidation1 })

  def isComplexType = !isSimpleType

  // note: these nil xml things are constant chunks of XML.
  val nilledXML: Maybe[scala.xml.Elem] = {
    if (!isNillable) Nope
    else One(scala.xml.Elem(thisElementsNamespacePrefix, name, XMLUtils.xmlNilAttribute, minimizedScope, true))
  }

}
