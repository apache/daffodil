package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.dsom.PrimitiveType
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
import edu.illinois.ncsa.daffodil.dsom.RuntimePrimType
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.dsom.EncodingMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width

trait HasSlotIndexInParent {
  def slotIndexInParent: Int
}

class ElementRuntimeData(
  override val lineAttribute: Option[String],
  override val columnAttribute: Option[String],
  override val fileAttribute: Option[String],
  override val prettyName: String,
  override val path: String,
  override val namespaces: NamespaceBinding,
  override val defaultBitOrder: BitOrder,
  override val optPrimType: Option[RuntimePrimType],
  val targetNamespace: NS,
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
  val isHidden: Boolean,
  val nChildSlots: Int,
  override val slotIndexInParent: Int,
  val isNillable: Boolean,
  val defaultValue: Option[Any],
  @transient childrenArg: => Seq[ElementRuntimeData],
  override val isArray: Boolean,
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
  override val variableMap: VariableMap,
  @transient parentArg: => Option[ElementRuntimeData],
  encoding: CompiledExpression,
  optionUTF16Width: Option[UTF16Width],
  isScannable: Boolean,
  defaultEncodingErrorPolicy: EncodingErrorPolicy)
  extends TermRuntimeData(encoding, optionUTF16Width, isScannable, defaultEncodingErrorPolicy, parentArg)
  with EncodingMixin
  with DPathElementCompileInfo
  with HasSlotIndexInParent {

  lazy val children = childrenArg
  lazy val parent = parentArg

  final def childERDs = children
  @transient final override lazy val elementChildrenCompileInfo = children.asInstanceOf[Seq[DPathElementCompileInfo]]

  def isSimpleType = optPrimType.isDefined

  final lazy val elementRuntimeData = this

  /**
   * This parent pointer has to be done imperatively because otherwise we
   * end up in a stack-overflow where the parent ElementRuntimeData has
   * childERDs, but those can't be created without the parent.
   */
  //  private var _parentERD: Option[ElementRuntimeData] = null
  //  override def parentERD = {
  //    Assert.usage(_parentERD != null, "must be initialized before use.")
  //    _parentERD
  //  }
  //  def init() {
  //    childERDs.foreach { _._parentERD = Some(this) }
  //  }
  //
  //  def isRootERD = _parentERD == None

  def isRootElement = parent == None

  private var _diagnostics: List[Diagnostic] = Nil

  def getDiagnostics = _diagnostics

  override def warn(th: Diagnostic) {
    if (isRootElement) _diagnostics :+= th
    else rootElement.warn(th)
  }

  override def error(th: Diagnostic) {
    if (isRootElement) _diagnostics :+= th
    else rootElement.error(th)
  }

  def schemaFileNames: Seq[String] = (fileAttribute.toList ++ childERDs.flatMap { _.schemaFileNames }).distinct

  def isDefaultable = defaultValue.isDefined

  def isComplexType = !isSimpleType

  // note: these nil values are constants. They could be cached. 
  val nilledXML: Maybe[scala.xml.Elem] = {
    if (!isNillable) Nope
    else One(scala.xml.Elem(targetNamespacePrefix, name, XMLUtils.xmlNilAttribute, namespaces, true))
  }

}

