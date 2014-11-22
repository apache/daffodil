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
import edu.illinois.ncsa.daffodil.dsom.EncodingMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.Representation
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType

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
  val dpathElementCompileInfo: DPathElementCompileInfo,
  override val schemaFileLocation: SchemaFileLocation,
  override val prettyName: String,
  override val path: String,
  override val namespaces: NamespaceBinding,
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
  val isArray: Boolean,
  val isOptional: Boolean,
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
  val impliedRepresentation: Representation)
  extends TermRuntimeData(parentArg, dpathElementCompileInfo, isRepresented, couldHaveText, alignmentValueInBits, hasNoSkipRegions)
  with HasSlotIndexInParent {

  lazy val children = childrenArg
  lazy val parent = parentArg
  override lazy val variableMap = variableMapArg

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

  def schemaFileNames: Seq[String] = (schemaFileLocation.fileName +: childERDs.flatMap { _.schemaFileNames }).distinct

  def isDefaultable = defaultValue.isDefined

  def isComplexType = !isSimpleType

  // note: these nil xml things are constant chunks of XML.  
  val nilledXML: Maybe[scala.xml.Elem] = {
    if (!isNillable) Nope
    else One(scala.xml.Elem(thisElementsNamespacePrefix, name, XMLUtils.xmlNilAttribute, namespaces, true))
  }

}

