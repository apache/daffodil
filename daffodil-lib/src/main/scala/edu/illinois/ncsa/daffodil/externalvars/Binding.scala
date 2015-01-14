package edu.illinois.ncsa.daffodil.externalvars

import edu.illinois.ncsa.daffodil.xml.XMLUtils
import scala.xml.Node
import edu.illinois.ncsa.daffodil.xml._

class Binding(val varQName: RefQName, val varValue: String) {

  override def toString() = {
    "<binding name='" + varQName + "'>" + varValue + "</binding>"
  }

  def hasNamespaceSpecified: Boolean = !varQName.namespace.isUnspecified

  override def hashCode = varQName.hashCode

  override def equals(o: Any): Boolean = {
    o match {
      case that: Binding => this.varQName == that.varQName
      case _ => false
    }
  }

  def globalQName = QName.createGlobal(varQName.local, varQName.namespace)
}

/**
 * This object is for cases when external variable bindings
 * are passed in via the Command Line Interface.
 */
object Binding {

  /**
   * extSyntax is {uri}ncName, or {}ncName, or ncName
   */
  def apply(extSyntax: String, value: String): Binding = {
    val maybeRefQName = QName.refQNameFromExtendedSyntax(extSyntax)
    maybeRefQName match {
      case Right(refQName) => new Binding(refQName, value)
      case Left(th) => throw th
    }
  }

  def apply(node: Node): Binding = {
    val name = (node \ "@name").head.text
    val refQName = QName.resolveRef(name, node.scope).get
    val value = node.text
    new Binding(refQName, value)
  }

  def apply(name: String, namespace: Option[NS], value: String): Binding = {
    new Binding(RefQName(None, name, namespace.getOrElse(UnspecifiedNamespace)), value)
  }

}
