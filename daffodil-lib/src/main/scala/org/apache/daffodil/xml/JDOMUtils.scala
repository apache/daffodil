/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.xml

import scala.xml._
import org.apache.daffodil.exceptions.Assert

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
    val Elem(_, _, _, nsBinding: NamespaceBinding, _*) = node.asInstanceOf[scala.xml.Elem]

    XMLUtils.namespaceBindings(nsBinding).foreach { ns =>
      {
        val prefix = ns.prefix
        if (prefix != null && prefix != ""
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
          new org.jdom2.Attribute(name, value, ns)
        } else
          new org.jdom2.Attribute(name, value)
      }
    }
    attribs.foreach { attrib => jdomNode.setAttribute(attrib) }

    for (child <- node.child) {
      child.label match {
        case "#PI" => // drop ProcInstrs
        // Note that for PCDATA and CDATA, we want to use child.text. If
        // instead we used child.toString, then we will get the escaped string
        // (e.g. apersands willbe &amp;). The problem is that when we provide
        // these escape strings to jdom, jdom will escape them too, so &amp;
        // would become &amp;amp;, effectively double escaping strings. By
        // using child.text, we get the unescaped value, which jdom is then
        // free to escape.
        case "#PCDATA" => jdomNode.addContent(child.text)
        case "#CDATA" => jdomNode.addContent(new org.jdom2.CDATA(child.text))
        case "#REM" => // leave out comments
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
