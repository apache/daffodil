package edu.illinois.ncsa.daffodil.xml

import scala.xml._
import edu.illinois.ncsa.daffodil.exceptions.Assert

object JDOMUtils {

  val xsiNS = org.jdom2.Namespace.getNamespace("xsi", XMLUtils.XSI_NAMESPACE.toString)

  import scala.language.reflectiveCalls // allows structural type - the def toXML below.

  /**
   * JDOM is no longer our infoset, but we will still want to convert
   * into it for Java users who want JDOM.
   */
  def Infoset2JDOM(node: { def toXML: scala.xml.NodeSeq }) = {
    val scalaXML = node.toXML
    elem2Element(scalaXML)
  }

  def elem2Element(nodes: scala.xml.NodeSeq): Seq[org.jdom2.Element] = nodes.map { elem => elem2Element(elem) }

  def elem2Element(node: scala.xml.Node): org.jdom2.Element = {
    val jdomNode = new org.jdom2.Element(node.label, node.prefix, node.namespace)
    var Elem(_, _, _, nsBinding: NamespaceBinding, _*) = node.asInstanceOf[scala.xml.Elem]

    XMLUtils.namespaceBindings(nsBinding).foreach { ns =>
      {
        val prefix = ns.prefix
        if (prefix != null & prefix != ""
          && jdomNode.getNamespace(prefix) == null)
          jdomNode.addNamespaceDeclaration(org.jdom2.Namespace.getNamespace(ns.prefix, ns.uri))
      }
    }

    val attribsList = if (node.attributes == null) Null else node.attributes

    val attribs = attribsList.map { (attribute: MetaData) =>
      {
        val attrNS = attribute.getNamespace(node)
        val name = attribute.key
        val value = attribute.value.text
        val prefixedKey = attribute.prefixedKey
        val prefix = if (prefixedKey.contains(":")) prefixedKey.split(":")(0) else ""
        val ns = (prefix, attrNS) match {
          //
          // to make our test cases less cluttered and more compact visually, we're 
          // going to specifically allow for an attribute named xsi:nil where xsi prefix
          // is NOT defined.
          //
          case ("xsi", null) | ("xsi", "") => xsiNS
          case (_, null) | (_, "") => {
            Assert.invariantFailed("attribute with prefix '%s', but no associated namespace".format(prefix))
          }
          case ("", uri) => org.jdom2.Namespace.getNamespace(uri)
          case (pre, uri) => org.jdom2.Namespace.getNamespace(pre, uri)
        }

        if (attribute.isPrefixed && attrNS != "") {
          //          println("THE ATTRIBUTE IS: " + name)
          //          println("THE NAMESPACE SHOULD BE: " + attrNS)
          //          println("IT ACTUALLY IS:" + Namespace.getNamespace(name, attrNS))

          // jdomNode setAttribute (name, value, ns)
          new org.jdom2.Attribute(name, value, ns)
        } else
          // jdomNode setAttribute (name, value)
          new org.jdom2.Attribute(name, value)
      }
    }
    attribs.foreach { attrib => jdomNode.setAttribute(attrib) }

    for (child <- node.child) {
      child.label match {
        case "#PCDATA" => jdomNode.addContent(child.toString)
        case "#CDATA" => jdomNode.addContent(new org.jdom2.CDATA(child.toString))
        case "#REM" =>
        case _ => jdomNode.addContent(elem2Element(child))
      }
    }
    jdomNode
  }

  def isNil(e: org.jdom2.Element) = {
    val nilAttr = e.getAttribute("nil", xsiNS)
    val res =
      if (nilAttr == null) false
      else nilAttr.getValue() == "true"
    res
  }
}