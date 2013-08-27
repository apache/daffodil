package edu.illinois.ncsa.daffodil.externalvars

import edu.illinois.ncsa.daffodil.xml.XMLUtils
import scala.xml.Node
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.xml.NoNamespace

class Binding(val varName: String, namespace: Option[NS], val varValue: String) {

  private lazy val theNsString = namespace match {
    case Some(ns) => "{" + ns + "}"
    case None => ""
  }
  override def toString() = {
    "<binding name='" + theNsString + varName + "'>" + varValue + "</binding>"
  }

  // A specified Namespace is one that is Some(NS(URI)) or Some(NoNamespace)
  // None simply means that Daffodil will need to try to figure it out.
  lazy val hasNamespaceSpecified: Boolean = namespace.isDefined

  // namespace == None => varName
  // namespace == Some(ns) => {ns}varName
  //  Where ns can be an actual URI or NoNamespace.
  lazy val extName = namespace match {
    case None => varName
    case Some(ns) => XMLUtils.expandedQName(ns, varName)
  }

  override def hashCode = extName.hashCode()
  override def equals(o: Any): Boolean = {
    o match {
      case that: Binding => this.extName == that.extName
      case _ => false
    }
  }
}

/**
 * This object is for cases when external variable bindings
 * are passed in via the Command Line Interface.
 */
object Binding {

  def apply(name: String, value: String): Binding = {
    val (ns, variableName) = XMLUtils.getQName(name)
    new Binding(variableName, ns, value)
  }

  def apply(node: Node): Binding = {
    val name = (node \ "@name").head.text
    val (nameNS, varNameNoPrefix) = XMLUtils.getQName(name, node)

    // TODO: Remove once we fix the above getQName method to return Option[NS]
    val theNameNS = nameNS match {
      case null => None
      case _ => Some(nameNS)
    }

    val value = node.text
    new Binding(varNameNoPrefix, theNameNS, value)
  }

  def apply(name: String, namespace: Option[NS], value: String): Binding = {
    new Binding(name, namespace, value)
  }

}
