package edu.illinois.ncsa.daffodil.dsom
import scala.xml.Node
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.xml.GetAttributesMixin
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE

/**
 * Element references and Group References use this.
 */
trait HasRef
  extends GetAttributesMixin {
  // TODO: Consolidate this and the xsdRef attributes that do QName stuff
  //From GroupRef.
  private lazy val xsdRef = getAttributeRequired("ref")
  lazy val ref = xsdRef
}

trait ResolvesQNames
  extends ThrowsSDE {
  def xml: Node

  /**
   * If prefix of name is unmapped, SDE, otherwise break into NS and local part.
   */
  def resolveQName(name: String): (NS, String) = {
    val pair @ (ns, localName) = XMLUtils.getQName(name, xml)
    schemaDefinition(ns != null, "In QName '%s', the prefix was not defined.", name)
    pair
  }

  /**
   * Just chop off the prefix
   */
  def removePrefix(prefixedValue: String): String = {
    prefixedValue.split(":").last
  }
}