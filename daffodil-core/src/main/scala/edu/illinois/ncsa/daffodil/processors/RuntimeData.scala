package edu.illinois.ncsa.daffodil.processors
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocatable
import edu.illinois.ncsa.daffodil.dsom.ImplementsThrowsSDE
import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionError
import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionWarning
import edu.illinois.ncsa.daffodil.api.Diagnostic

trait RuntimeData extends SchemaFileLocatable
  with ImplementsThrowsSDE {
  val lineAttribute: Option[String]
  val columnAttribute: Option[String]
  val fileAttribute: Option[String]
  val prettyName: String
  val path: String
  val namespaces: Seq[org.jdom2.Namespace]

  override val fileName = fileAttribute.getOrElse("<no file>")

  override def toString = prettyName

  override def schemaComponent = this // needed by ImplementsThrowsSDE

  override def error(sde: Diagnostic) = toss(sde)
  override def warn(sdw: Diagnostic) = toss(sdw)
}

abstract class TermRuntimeData
  extends RuntimeData {
  val defaultBitOrder: BitOrder
}

class NonTermRuntimeData(
  override val lineAttribute: Option[String],
  override val columnAttribute: Option[String],
  override val fileAttribute: Option[String],
  override val prettyName: String,
  override val path: String,
  override val namespaces: Seq[org.jdom2.Namespace])
  extends RuntimeData

class ModelGroupRuntimeData(
  override val lineAttribute: Option[String],
  override val columnAttribute: Option[String],
  override val fileAttribute: Option[String],
  override val prettyName: String,
  override val path: String,
  override val namespaces: Seq[org.jdom2.Namespace],
  override val defaultBitOrder: BitOrder,
  val groupMembers: Seq[RuntimeData])
  extends TermRuntimeData

