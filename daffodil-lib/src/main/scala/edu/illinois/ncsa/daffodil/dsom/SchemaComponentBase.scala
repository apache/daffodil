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
  with GetAttributesMixin
  with SchemaFileLocatable
  with LocationInSchemaFile //FIXME: rename: LocationInSchemaFile to LocationInSchemaFile 
  // (to avoid confusion with the XSD attribute named schemaLocation)
  // Also, a SchemaComponent isn't a LocationInSchemaFile. Rather, it has one or more
  // schema locations (more than one because schema component have a lexical 
  // span over many lines, and can enclose other components.
  {

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