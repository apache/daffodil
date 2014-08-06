package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.dsom.PrimitiveType
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.dsom.FacetTypes

class ElementRuntimeData(
  override val lineAttribute: Option[String],
  override val columnAttribute: Option[String],
  override val fileAttribute: Option[String],
  override val prettyName: String,
  override val path: String,
  override val namespaces: Seq[org.jdom2.Namespace],
  override val defaultBitOrder: BitOrder,
  val primType: Option[PrimitiveType],
  val isSimpleType: Boolean,
  val knownEncodingWidthInBits: Option[Int],
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
  val isHidden: Boolean)

  extends TermRuntimeData {

  var schemaComponentID: Int = -1

  @deprecated("don't call this. The object itself IS a runtimeData", "2014-08-07")
  def runtimeData: RuntimeData = this

}
