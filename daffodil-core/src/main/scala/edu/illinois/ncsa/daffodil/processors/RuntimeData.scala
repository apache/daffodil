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
import edu.illinois.ncsa.daffodil.util.PreSerialization
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation
import edu.illinois.ncsa.daffodil.exceptions.HasSchemaFileLocation

trait RuntimeData
  extends ImplementsThrowsSDE
  with HasSchemaFileLocation {
  val schemaFileLocation: SchemaFileLocation
  val prettyName: String
  val path: String
  val namespaces: NamespaceBinding

  def immediateEnclosingRuntimeData: Option[RuntimeData]
  def variableMap: VariableMap
  override def toString = prettyName

}

abstract class TermRuntimeData(
  @transient immedEnclosingRD: => Option[RuntimeData],
  val dpathCompileInfo: DPathCompileInfo,
  val isRepresented: Boolean,
  val couldHaveText: Boolean,
  val alignmentValueInBits: Int,
  val hasNoSkipRegions: Boolean)
  extends RuntimeData
  with Serializable
  with PreSerialization {

  lazy val immediateEnclosingRuntimeData = immedEnclosingRD

  val defaultBitOrder: BitOrder

  override def preSerialization: Unit = {
    super.preSerialization
    immediateEnclosingRuntimeData
  }
  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)
}

class NonTermRuntimeData(
  @transient variableMapArg: => VariableMap,
  override val schemaFileLocation: SchemaFileLocation,
  override val prettyName: String,
  override val path: String,
  override val namespaces: NamespaceBinding,
  override val immediateEnclosingRuntimeData: Option[RuntimeData])
  extends RuntimeData
  with PreSerialization {

  override lazy val variableMap = variableMapArg

  override def preSerialization: Unit = {
    super.preSerialization
    variableMap
  }
  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

  // override def elementChildrenCompileInfo: Seq[DPathElementCompileInfo] = Assert.invariantFailed("asked for element children of non-element, non-model-group: " + this.prettyName)

}

class ModelGroupRuntimeData(
  @transient variableMapArg: => VariableMap,
  // val elementChildrenCompileInfo: Seq[DPathElementCompileInfo],
  override val schemaFileLocation: SchemaFileLocation,
  ci: DPathCompileInfo,
  override val prettyName: String,
  override val path: String,
  override val namespaces: NamespaceBinding,
  override val defaultBitOrder: BitOrder,
  val groupMembers: Seq[RuntimeData],
  val erd: ElementRuntimeData,
  isRepresented: Boolean,
  couldHaveText: Boolean,
  alignmentValueInBits: Int,
  hasNoSkipRegions: Boolean)
  extends TermRuntimeData(Some(erd), ci, isRepresented, couldHaveText, alignmentValueInBits, hasNoSkipRegions) {

  override lazy val variableMap = variableMapArg

  override def preSerialization: Unit = {
    super.preSerialization
    variableMap
  }
  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

}

class VariableRuntimeData(
  sfl: SchemaFileLocation,
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
    null, // no variable map
    sfl,
    prettyName,
    path,
    namespaces,
    None)
  with Serializable {

  // override def elementChildrenCompileInfo: Seq[DPathElementCompileInfo] = Assert.invariantFailed("asked for element children of non-element, non-model-group: " + this.prettyName)
}
