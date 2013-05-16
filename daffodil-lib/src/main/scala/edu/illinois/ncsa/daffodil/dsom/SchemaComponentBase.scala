package edu.illinois.ncsa.daffodil.dsom
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.OOLAGHost
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.xml.GetAttributesMixin
import edu.illinois.ncsa.daffodil.api.LocationInSchemaFile
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocatable
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.util.Logging

abstract class SchemaComponentBase(xmlArg: scala.xml.Node, parent: SchemaComponentBase)
  extends OOLAGHost(parent)
  with ThrowsSDE
  with GetAttributesMixin {

  val contextLocatable = parent

  val xml = xmlArg
  val aaa_xml = xml // for debugging, so we don't have to scroll down.

  def schemaComponent = this
  /**
   * override in derived class to narrow the result type.
   *
   * Concrete here only for unit tests that create instances.
   */
  def enclosingComponent =
    if (parent != null) Some(parent) else None

  def isHidden = false

}