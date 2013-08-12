package edu.illinois.ncsa.daffodil.externalvars

import edu.illinois.ncsa.daffodil.xml.XMLUtils
import scala.xml.Node
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.xml.NoNamespace

class Binding(node: Node) {
  val name = (node \ "@name").head.text

  val (nameNS, varName) = XMLUtils.getQName(name, node)

  val varValue = node.text

  lazy val extName = XMLUtils.expandedQName(nameNS, varName)

  override def toString() = {
    "<binding name='{" + nameNS + "}" + varName + "'>" + varValue + "</binding>"
  }

  override def hashCode = extName.hashCode()
  override def equals(o: Any): Boolean = {
    o match {
      case that: Binding => this.extName == that.extName
      case _ => false
    }
  }
}

object Binding {
  val NSFormatNotEmpty = """\{([^\{\}]+)\}(.+)""".r
  val NSFormatEmpty = """\{\}(.+)""".r

  /**
   * Specialized getQName function for handling
   * manually specified variables via the CLI.
   *
   * Variables will be of the format:
   *
   * 1. {nsURI}varName=value
   * 2. {}varName=value
   * 3. varName=value
   */
  private def getQName(name: String) = {
    name match {
      case NSFormatNotEmpty(nsURI, varName) => (NS(nsURI), varName)
      case NSFormatEmpty(varName) => (NoNamespace, varName)
      case _ => (NoNamespace, name)
    }
  }
  def apply(name: String, value: String): Binding = {
    val (ns, variableName) = getQName(name)
    new Binding(<bind name={ variableName }>{ value }</bind>) {
      override val nameNS = ns
      override val varName = variableName
      override lazy val extName = XMLUtils.expandedQName(ns, variableName)
    }
  }
}
