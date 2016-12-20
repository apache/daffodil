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

import scala.xml.NamespaceBinding
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.dsom.DPathCompileInfo
import edu.illinois.ncsa.daffodil.dsom.ImplementsThrowsSDE
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.dsom.ConstantExpression
import edu.illinois.ncsa.daffodil.exceptions.HasSchemaFileLocation
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.YesNo
import edu.illinois.ncsa.daffodil.util.PreSerialization
import edu.illinois.ncsa.daffodil.util.TransientParam
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.xml.GlobalQName
import edu.illinois.ncsa.daffodil.xml.RefQName
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.dsom.FacetTypes
import scala.xml.NamespaceBinding
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.dsom.DPathElementCompileInfo
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.Representation
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.processors.unparsers.NextElementResolver
import edu.illinois.ncsa.daffodil.util.TransientParam
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.YesNo

/*
 * NOTE: Any time you add a member to one of these objects, you must modify at least 3 places.
 *
 * 1) the argument list
 * 2) the lazy vals
 * 3) the pre-serialization method
 *
 * If however that member is passed to the constructor of a parent class, then steps 2 and 3
 * are only needed at the top of the class hierarchy. The other classes can just pass the argument
 * to the parent constructor.
 */

sealed trait RuntimeData
  extends ImplementsThrowsSDE
  with HasSchemaFileLocation {
  val schemaFileLocation: SchemaFileLocation
  val diagnosticDebugName: String
  val path: String
  val namespaces: NamespaceBinding

  def immediateEnclosingElementRuntimeData: Option[ElementRuntimeData]
  def immediateEnclosingTermRuntimeData: Maybe[TermRuntimeData]
  def variableMap: VariableMap
  override def toString = diagnosticDebugName

}

object TermRuntimeData {

  private var nextID = 0

  def generateTermID: Int = synchronized {
    val n = nextID
    nextID += 1
    n
  }

}

sealed abstract class TermRuntimeData(
  /**
   * These transient by-name args are part of how we
   * hook these objects into a parent-child tree without
   * having to use an assignment to a var.
   */
  @TransientParam immediateEnclosingElementRuntimeDataArg: => Option[ElementRuntimeData],
  @TransientParam immedEnclosingTermRuntimeDataArg: => Maybe[TermRuntimeData],
  @TransientParam encodingInfoArg: => EncodingRuntimeData,
  @TransientParam dpathCompileInfoArg: => DPathCompileInfo,
  @TransientParam isRepresentedArg: => Boolean,
  @TransientParam couldHaveTextArg: => Boolean,
  @TransientParam alignmentValueInBitsArg: => Int,
  @TransientParam hasNoSkipRegionsArg: => Boolean,
  @TransientParam defaultBitOrderArg: => BitOrder,
  @TransientParam optIgnoreCaseArg: => Option[YesNo])
  extends RuntimeData
  with Serializable
  with PreSerialization {

  private val termID = TermRuntimeData.generateTermID

  final override def hashCode(): Int = termID

  final override def equals(other: Any) = other match {
    case ref: AnyRef => this eq ref
    case _ => false
  }

  lazy val immediateEnclosingElementRuntimeData = immediateEnclosingElementRuntimeDataArg
  lazy val immediateEnclosingTermRuntimeData = immedEnclosingTermRuntimeDataArg
  lazy val encodingInfo = encodingInfoArg
  lazy val dpathCompileInfo = dpathCompileInfoArg
  lazy val isRepresented = isRepresentedArg
  lazy val couldHaveText = couldHaveTextArg
  lazy val alignmentValueInBits = alignmentValueInBitsArg
  lazy val hasNoSkipRegions = hasNoSkipRegionsArg
  lazy val defaultBitOrder = defaultBitOrderArg
  lazy val optIgnoreCase = optIgnoreCaseArg

  override def preSerialization: Unit = {
    super.preSerialization
    immediateEnclosingElementRuntimeData
    immediateEnclosingTermRuntimeData
    encodingInfo
    dpathCompileInfo
    isRepresented
    couldHaveText
    alignmentValueInBits
    hasNoSkipRegions
    defaultBitOrder
    optIgnoreCase
  }
  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)
}

sealed class NonTermRuntimeData(
  /**
   * These transient by-name args are part of how we
   * hook these objects into a parent-child tree without
   * having to use an assignment to a var.
   */
  @TransientParam variableMapArg: => VariableMap,
  @TransientParam schemaFileLocationArg: => SchemaFileLocation,
  @TransientParam diagnosticDebugNameArg: => String,
  @TransientParam pathArg: => String,
  @TransientParam namespacesArg: => NamespaceBinding,
  @TransientParam immediateEnclosingElementRuntimeDataArg: => Option[ElementRuntimeData],
  @TransientParam immedEnclosingTermRuntimeDataArg: => Maybe[TermRuntimeData])
  extends RuntimeData
  with PreSerialization {

  override lazy val variableMap = variableMapArg
  lazy val schemaFileLocation = schemaFileLocationArg
  lazy val diagnosticDebugName = diagnosticDebugNameArg
  lazy val path = pathArg
  lazy val namespaces = namespacesArg
  lazy val immediateEnclosingElementRuntimeData = immediateEnclosingElementRuntimeDataArg
  lazy val immediateEnclosingTermRuntimeData = immedEnclosingTermRuntimeDataArg

  override def preSerialization: Unit = {
    super.preSerialization
    variableMap
    schemaFileLocation
    diagnosticDebugName
    path
    namespaces
    immediateEnclosingElementRuntimeData
    immediateEnclosingTermRuntimeData
  }
  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

}

trait HasSlotIndexInParent {
  def slotIndexInParent: Int
}

/**
 * Singleton. If found as the default value, means to use nil as
 * the default value instead of an actual value.
 */
object UseNilForDefault

/*
 * These objects have become too big. Most processors don't need most of this stuff.
 *
 * The objective should be to take things OUT of this structure and pass directly to the
 * constructor of the parser/unparser.
 *
 * These objects are for things that are generally heavily used everywhere like information for
 * providing diagnostics.
 */

final class ElementRuntimeData(
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
  @TransientParam diagnosticDebugNameArg: => String,
  @TransientParam pathArg: => String,
  @TransientParam namespacesArg: => NamespaceBinding,
  @TransientParam minimizedScopeArg: => NamespaceBinding,
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
  @TransientParam optTruncateSpecifiedLengthStringArg: => Option[Boolean],
  @TransientParam outputValueCalcExprArg: => Option[CompiledExpression[AnyRef]])
  extends TermRuntimeData(parentArg, parentTermArg, encInfoArg, dpathElementCompileInfoArg, isRepresentedArg, couldHaveTextArg, alignmentValueInBitsArg, hasNoSkipRegionsArg,
    defaultBitOrderArg, optIgnoreCaseArg)
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
  lazy val diagnosticDebugName = diagnosticDebugNameArg
  lazy val path = pathArg
  lazy val namespaces = namespacesArg
  lazy val minimizedScope = minimizedScopeArg
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

  def isReferencedByExpressions = !notReferencedByExpressions

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
    diagnosticDebugName
    path
    namespaces
    minimizedScope
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

  def prefixedName = {
    if (thisElementsNamespacePrefix != null) {
      thisElementsNamespacePrefix + ":" + name
    } else {
      name
    }
  }

}

sealed abstract class ModelGroupRuntimeData(
  /**
   * These transient by-name args are part of how we
   * hook these objects into a parent-child tree without
   * having to use an assignment to a var.
   */
  @TransientParam variableMapArg: => VariableMap,
  @TransientParam encInfoArg: => EncodingRuntimeData,
  @TransientParam schemaFileLocationArg: => SchemaFileLocation,
  @TransientParam ciArg: => DPathCompileInfo,
  @TransientParam diagnosticDebugNameArg: => String,
  @TransientParam pathArg: => String,
  @TransientParam namespacesArg: => NamespaceBinding,
  @TransientParam defaultBitOrderArg: => BitOrder,
  @TransientParam groupMembersArg: => Seq[TermRuntimeData],
  @TransientParam erdArg: ElementRuntimeData,
  @TransientParam trdArg: => TermRuntimeData,
  @TransientParam isRepresentedArg: => Boolean,
  @TransientParam couldHaveTextArg: => Boolean,
  @TransientParam alignmentValueInBitsArg: => Int,
  @TransientParam hasNoSkipRegionsArg: => Boolean,
  @TransientParam optIgnoreCaseArg: => Option[YesNo])
  extends TermRuntimeData(Some(erdArg),
    Maybe(trdArg),
    encInfoArg, ciArg, isRepresentedArg, couldHaveTextArg, alignmentValueInBitsArg, hasNoSkipRegionsArg,
    defaultBitOrderArg, optIgnoreCaseArg) {

  lazy val variableMap = variableMapArg
  lazy val encInfo = encInfoArg
  lazy val schemaFileLocation = schemaFileLocationArg
  lazy val ci = ciArg
  lazy val diagnosticDebugName = diagnosticDebugNameArg
  lazy val path = pathArg
  lazy val namespaces = namespacesArg
  lazy val groupMembers = groupMembersArg
  lazy val erd = erdArg
  lazy val trd = trdArg

  override def preSerialization: Unit = {
    super.preSerialization
    variableMap
    encInfo
    schemaFileLocation
    ci
    diagnosticDebugName
    path
    namespaces
    groupMembers
    erd
    trd
  }
  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)
}

final class SequenceRuntimeData(
  /**
   * These transient by-name args are part of how we
   * hook these objects into a parent-child tree without
   * having to use an assignment to a var.
   */
  @TransientParam variableMapArg: => VariableMap,
  @TransientParam encInfoArg: => EncodingRuntimeData,
  @TransientParam schemaFileLocationArg: => SchemaFileLocation,
  @TransientParam ciArg: => DPathCompileInfo,
  @TransientParam diagnosticDebugNameArg: => String,
  @TransientParam pathArg: => String,
  @TransientParam namespacesArg: => NamespaceBinding,
  @TransientParam defaultBitOrderArg: => BitOrder,
  @TransientParam groupMembersArg: => Seq[TermRuntimeData],
  @TransientParam erdArg: ElementRuntimeData,
  @TransientParam trdArg: => TermRuntimeData,
  @TransientParam isRepresentedArg: => Boolean,
  @TransientParam couldHaveTextArg: => Boolean,
  @TransientParam alignmentValueInBitsArg: => Int,
  @TransientParam hasNoSkipRegionsArg: => Boolean,
  @TransientParam optIgnoreCaseArg: => Option[YesNo])
  extends ModelGroupRuntimeData(variableMapArg, encInfoArg, schemaFileLocationArg, ciArg, diagnosticDebugNameArg, pathArg, namespacesArg, defaultBitOrderArg, groupMembersArg,
    erdArg, trdArg, isRepresentedArg, couldHaveTextArg, alignmentValueInBitsArg, hasNoSkipRegionsArg, optIgnoreCaseArg)

final class ChoiceRuntimeData(
  /**
   * These transient by-name args are part of how we
   * hook these objects into a parent-child tree without
   * having to use an assignment to a var.
   */
  @TransientParam variableMapArg: => VariableMap,
  @TransientParam encInfoArg: => EncodingRuntimeData,
  @TransientParam schemaFileLocationArg: => SchemaFileLocation,
  @TransientParam ciArg: => DPathCompileInfo,
  @TransientParam diagnosticDebugNameArg: => String,
  @TransientParam pathArg: => String,
  @TransientParam namespacesArg: => NamespaceBinding,
  @TransientParam defaultBitOrderArg: => BitOrder,
  @TransientParam groupMembersArg: => Seq[TermRuntimeData],
  @TransientParam erdArg: ElementRuntimeData,
  @TransientParam trdArg: => TermRuntimeData,
  @TransientParam isRepresentedArg: => Boolean,
  @TransientParam couldHaveTextArg: => Boolean,
  @TransientParam alignmentValueInBitsArg: => Int,
  @TransientParam hasNoSkipRegionsArg: => Boolean,
  @TransientParam optIgnoreCaseArg: => Option[YesNo])
  extends ModelGroupRuntimeData(variableMapArg, encInfoArg, schemaFileLocationArg, ciArg, diagnosticDebugNameArg, pathArg, namespacesArg, defaultBitOrderArg, groupMembersArg,
    erdArg, trdArg, isRepresentedArg, couldHaveTextArg, alignmentValueInBitsArg, hasNoSkipRegionsArg, optIgnoreCaseArg)

final class VariableRuntimeData(
  @TransientParam schemaFileLocationArg: => SchemaFileLocation,
  @TransientParam diagnosticDebugNameArg: => String,
  @TransientParam pathArg: => String,
  @TransientParam namespacesArg: => NamespaceBinding,
  @TransientParam externalArg: => Boolean,
  @TransientParam maybeDefaultValueExprArg: => Maybe[CompiledExpression[AnyRef]],
  @TransientParam typeRefArg: => RefQName,
  @TransientParam globalQNameArg: => GlobalQName,
  @TransientParam primTypeArg: => NodeInfo.PrimType)
  extends NonTermRuntimeData(
    null, // no variable map
    schemaFileLocationArg,
    diagnosticDebugNameArg,
    pathArg,
    namespacesArg,
    None,
    None)
  with Serializable {

  lazy val external = externalArg
  lazy val maybeDefaultValueExpr = maybeDefaultValueExprArg
  lazy val typeRef = typeRefArg
  lazy val globalQName = globalQNameArg
  lazy val primType = primTypeArg

  override def preSerialization: Unit = {
    super.preSerialization
    external
    maybeDefaultValueExpr
    typeRef
    globalQName
    primType
  }

  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

  private val state =
    if (!maybeDefaultValueExpr.isDefined) VariableUndefined
    else VariableDefined

  private val maybeValue: Maybe[AnyRef] =
    if (maybeDefaultValueExpr.isEmpty) Nope
    else {
      val defaultValueExpr = maybeDefaultValueExpr.get
      defaultValueExpr match {
        case constExpr: ConstantExpression[_] => One(constExpr.constant.asInstanceOf[AnyRef])
        case _ => Nope
      }
    }

  def newVariableInstance: Variable = Variable(state, maybeValue, this, maybeDefaultValueExpr)

}
