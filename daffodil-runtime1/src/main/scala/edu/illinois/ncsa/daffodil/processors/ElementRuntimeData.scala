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

trait HasSlotIndexInParent {
  def slotIndexInParent: Int
}

class ElementRuntimeData(
  /**
   * These transient by-name args are part of how we
   * hook these objects into a parent-child tree without
   * having to use an assignment to a var.
   */
  @transient parentArg: => Option[ElementRuntimeData],
  @transient childrenArg: => Seq[ElementRuntimeData],
  @transient variableMapArg: => VariableMap,
  @transient nextElementResolverArg: => NextElementResolver,
  @transient childElementResolverArg: => NextElementResolver,
  encInfo: EncodingRuntimeData,
  val dpathElementCompileInfo: DPathElementCompileInfo,
  override val schemaFileLocation: SchemaFileLocation,
  override val prettyName: String,
  override val path: String,
  override val namespaces: NamespaceBinding,
  val minimizedScope: NamespaceBinding,
  override val defaultBitOrder: BitOrder,
  val optPrimType: Option[PrimType],
  val targetNamespace: NS,
  val thisElementsNamespace: NS,
  val patternValues: Option[Seq[FacetTypes.FacetValueR]],
  val enumerationValues: Option[String],
  val minLength: Option[java.math.BigDecimal],
  val maxLength: Option[java.math.BigDecimal],
  val minInclusive: Option[java.math.BigDecimal],
  val maxInclusive: Option[java.math.BigDecimal],
  val minExclusive: Option[java.math.BigDecimal],
  val maxExclusive: Option[java.math.BigDecimal],
  val totalDigits: Option[java.math.BigDecimal],
  val fractionDigits: Option[java.math.BigDecimal],
  val minOccurs: Option[Int],
  val maxOccurs: Option[Int],
  val name: String,
  val targetNamespacePrefix: String,
  val thisElementsNamespacePrefix: String,
  val isHidden: Boolean,
  val nChildSlots: Int,
  override val slotIndexInParent: Int,
  val isNillable: Boolean,
  val defaultValue: Option[Any],
  val isArray: Boolean, // can have more than 1 occurrence
  val isOptional: Boolean, // can have only 0 or 1 occurrence
  val isRequired: Boolean, // must have at least 1 occurrence
  /**
   * This is the properly qualified name for recognizing this
   * element.
   *
   * This takes into account xs:schema's elementFormDefault attribute.
   * If 'qualified' then there will be a namespace component.
   * If 'unqualified' the the namespace component will be No_Namespace.
   */
  val namedQName: NamedQName,
  isRepresented: Boolean,
  couldHaveText: Boolean,
  alignmentValueInBits: Int,
  hasNoSkipRegions: Boolean,
  val impliedRepresentation: Representation,
  //
  // Unparser-specific arguments
  //
  /**
   * pass true for this if the corresponding infoset element is never
   * accessed by way of expressions. Enables the element to be dropped
   * from the infoset immediately after unparsing is complete.
   */
  val notReferencedByExpressions: Boolean,
  fillByteValue: Int,
  val optTruncateSpecifiedLengthString: Option[Boolean])
  extends TermRuntimeData(parentArg, encInfo, dpathElementCompileInfo, isRepresented, couldHaveText, alignmentValueInBits, hasNoSkipRegions, fillByteValue)
  with HasSlotIndexInParent {

  lazy val children = childrenArg
  lazy val parent = parentArg
  override lazy val variableMap = variableMapArg
  lazy val nextElementResolver = nextElementResolverArg
  lazy val childElementResolver = childElementResolverArg

  override def preSerialization: Unit = {
    super.preSerialization
    children
    parent
    variableMap
  }

  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

  final def childERDs = children

  def isSimpleType = optPrimType.isDefined

  def schemaURIStringsForFullValidation = schemaURIStringsForFullValidation1.distinct
  private def schemaURIStringsForFullValidation1: Seq[String] = (schemaFileLocation.uriString +:
    childERDs.flatMap { _.schemaURIStringsForFullValidation1 })

  def isDefaultable = defaultValue.isDefined

  def isComplexType = !isSimpleType

  // note: these nil xml things are constant chunks of XML.  
  val nilledXML: Maybe[scala.xml.Elem] = {
    if (!isNillable) Nope
    else One(scala.xml.Elem(thisElementsNamespacePrefix, name, XMLUtils.xmlNilAttribute, minimizedScope, true))
  }

}

