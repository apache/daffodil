package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocatable
import edu.illinois.ncsa.daffodil.dsom.ImplementsThrowsSDE
import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionError
import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionWarning
import edu.illinois.ncsa.daffodil.api.Diagnostic
import scala.xml.NamespaceBinding
import edu.illinois.ncsa.daffodil.dsom.DPathCompileInfo
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.xml.GlobalQName
import edu.illinois.ncsa.daffodil.dsom.PrimType
import edu.illinois.ncsa.daffodil.dsom.RuntimePrimType
import edu.illinois.ncsa.daffodil.dsom.DPathElementCompileInfo
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width

trait RuntimeData extends SchemaFileLocatable
  with ImplementsThrowsSDE
  with DPathCompileInfo
  with Serializable {
  val lineAttribute: Option[String]
  val columnAttribute: Option[String]
  val fileAttribute: Option[String]
  val prettyName: String
  val path: String
  val namespaces: NamespaceBinding

  def immediateEnclosingRuntimeData: Option[RuntimeData]

  final override def immediateEnclosingCompileInfo = immediateEnclosingRuntimeData

  def variableMap: VariableMap

  override val fileName = fileAttribute.getOrElse("<no file>")

  override def toString = prettyName

  override def schemaComponent = this // needed by ImplementsThrowsSDE

  override def error(sde: Diagnostic): Unit
  override def warn(sdw: Diagnostic): Unit

}

abstract class TermRuntimeData(
  val encoding: CompiledExpression,
  val optionUTF16Width: Option[UTF16Width],
  val isScannable: Boolean,
  val defaultEncodingErrorPolicy: EncodingErrorPolicy,
  @transient immedEnclosingRD: => Option[RuntimeData])
  extends RuntimeData
  with Serializable {

  lazy val immediateEnclosingRuntimeData = immedEnclosingRD
  val defaultBitOrder: BitOrder
  lazy val utf16Width = optionUTF16Width.get
}

class NonTermRuntimeData(
  override val lineAttribute: Option[String],
  override val columnAttribute: Option[String],
  override val fileAttribute: Option[String],
  override val prettyName: String,
  override val path: String,
  override val namespaces: NamespaceBinding,
  override val immediateEnclosingRuntimeData: Option[RuntimeData],
  override val variableMap: VariableMap)
  extends RuntimeData {

  private def erd = this.enclosingElementCompileInfo

  override def warn(th: Diagnostic) = erd.get.warn(th)
  override def error(th: Diagnostic) = erd.get.error(th)

  override def elementChildrenCompileInfo: Seq[DPathElementCompileInfo] = Assert.invariantFailed("asked for element children of non-element, non-model-group: " + this.prettyName)

}

class ModelGroupRuntimeData(
  override val lineAttribute: Option[String],
  override val columnAttribute: Option[String],
  override val fileAttribute: Option[String],
  override val prettyName: String,
  override val path: String,
  override val namespaces: NamespaceBinding,
  override val defaultBitOrder: BitOrder,
  val groupMembers: Seq[RuntimeData],
  val erd: ElementRuntimeData,
  override val variableMap: VariableMap,
  override val elementChildrenCompileInfo: Seq[DPathElementCompileInfo],
  encoding: CompiledExpression,
  optUTF16Width: Option[UTF16Width],
  isScannable: Boolean,
  defaultEncodingErrorPolicy: EncodingErrorPolicy)
  extends TermRuntimeData(encoding, optUTF16Width, isScannable, defaultEncodingErrorPolicy, Some(erd)) {

  override def warn(th: Diagnostic) = erd.warn(th)
  override def error(th: Diagnostic) = erd.error(th)
}

class VariableRuntimeData(
  override val lineAttribute: Option[String],
  override val columnAttribute: Option[String],
  override val fileAttribute: Option[String],
  override val prettyName: String,
  override val path: String,
  override val namespaces: NamespaceBinding,
  val external: Boolean,
  val defaultValue: Option[String],
  @deprecated("use globalQName", "2014-09-18") val extName: String,
  @deprecated("use globalQName", "2014-09-18") val extType: String,
  val globalQName: GlobalQName,
  val primType: RuntimePrimType)
  extends NonTermRuntimeData(
    lineAttribute,
    columnAttribute,
    fileAttribute,
    prettyName,
    path,
    namespaces,
    None,
    null) {

  override def elementChildrenCompileInfo: Seq[DPathElementCompileInfo] = Assert.invariantFailed("asked for element children of non-element, non-model-group: " + this.prettyName)
}
