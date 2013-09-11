package edu.illinois.ncsa.daffodil.processors

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.SchemaComponentRegistry
import edu.illinois.ncsa.daffodil.processors.xpath.XPathUtil.CompiledExpressionFactory
import javax.xml.namespace.QName
import edu.illinois.ncsa.daffodil.processors.xpath.XPathUtil
import javax.xml.xpath.XPathConstants._
import edu.illinois.ncsa.daffodil.Implicits._

/**
 * Our abstraction layer over JDOM for the DFDL Infoset
 */

object Infoset {

  type SchemaComponentIDObject = String

  def addComponent(e: ElementBase): SchemaComponentIDObject = {
    val idString = e.schemaSet.schemaComponentRegistry.addComponent(e)
    idString
  }

  def newElement(e: ElementBase, isHidden: Boolean) = {
    Assert.usage(e.name != "")
    val jdomE =
      new org.jdom2.Element(e.name, e.targetNamespacePrefix, e.targetNamespace.toStringOrNullIfNoNS)
    //
    // Note: you can't save the attribute and reuse it because in JDOM
    // attributes have parent pointers. So this creates one and points it back at the jdomE
    // holding it.
    jdomE.setAttribute("context", e.schemaComponentID, XMLUtils.INT_NS_OBJECT)
    jdomE.setAttribute("defaulted", "false", XMLUtils.INT_NS_OBJECT)
    if (isHidden)
      jdomE.setAttribute("hidden", "true", XMLUtils.INT_NS_OBJECT)
    val res = new InfosetElement(jdomE)
    res
  }

  def newDocument() = {
    val res = new InfosetDocument(new org.jdom2.Document())
    res
  }

  def apply(rootNode: scala.xml.Node): InfosetElement = {
    val jElt = XMLUtils.elem2Element(rootNode)
    val jDoc_ignored = new org.jdom2.Document(jElt) // must have or jdom expressions won't work.
    val item = new InfosetElement(jElt)
    item
  }

  type ElementState = Int
  type Namespace = org.jdom2.Namespace

}

class InfosetElement(private val elt: org.jdom2.Element) extends InfosetItem {

  Assert.usage(elt != null)

  val jdomElt = Some(elt)

  def isNil: Boolean = XMLUtils.isNil(elt)
  def makeNil(): Unit = { elt.setAttribute(XMLUtils.nilAttribute()) }

  def setDataValue(s: String): Unit = {
    elt.setContent(new org.jdom2.Text(XMLUtils.remapXMLIllegalCharactersToPUA(s)))
  }

  def addElement(e: InfosetElement) = elt.addContent(e.elt)
  
  def setDefaulted() = {
    elt.setAttribute("defaulted","true", XMLUtils.INT_NS_OBJECT)
  }
  
  def isDefaulted: Boolean = {
    // dafint:defaulted='true'
    val res = elt.getAttribute("defaulted", XMLUtils.INT_NS_OBJECT).getBooleanValue()
    res
  }

  /**
   * Our JDOM-based infoset is not a functional-programming thing, we side-effect it, so
   * we have to be able to undo those side-effects when we backtrack.
   */
  def removeContent(c: InfosetElement): Unit = {
    elt.removeContent(c.elt)
  }

  def toXML: scala.xml.Node = {
    XMLUtils.element2Elem(elt)
  }

  /**
   * Retrieve the schema component that gave rise to this infoset
   * item.
   */
  def schemaComponent(pstate: PState): ElementBase = {
    val currentElement = elt
    val attr = currentElement.getAttributeValue("context", XMLUtils.INT_NS_OBJECT)
    val context = attr match {
      case null => {
        Assert.invariantFailed("No schema component context attribute on Infoset element")
      }
      case uuid => {
        val scr = pstate.schemaComponentRegistry
        // We have a uuid, retrieve the schema component
        scr.getComponentByID(uuid) match {
          case Some(e) => e
          case None => {
            Assert.invariantFailed("Schema context component was not found in lookup table")
          }
        }
      }
    }
    context
  }

  def schemaComponentUUID(pstate: PState): String = {
    val currentElement = elt
    val attr = currentElement.getAttributeValue("context", XMLUtils.INT_NS_OBJECT)
    val result = attr match {
      case null => Assert.invariantFailed("No schema component context attribute on Infoset element.")
      case uuid => uuid
    }
    result
  }

  def parent = {
    val par = elt.getParent()
    val res = par match {
      case e: org.jdom2.Element => new InfosetElement(e)
      case d: org.jdom2.Document => new InfosetDocument(d)
      case _ => null
    }
    res
  }

  def namespace = elt.getNamespace()
  def name = elt.getName()

  def dataValue = elt.getText()

  def getChild(childName: String) = new InfosetElement(elt.getChild(childName))
  def getChild(cName: String, ns: org.jdom2.Namespace) = new InfosetElement(elt.getChild(cName, ns))

  def captureState(): Infoset.ElementState = {
    elt.getContentSize()
  }

  def restoreState(st: Infoset.ElementState): Unit = {
    val previousContentSize: Int = st
    val currentContentSize = elt.getContentSize()
    for (i <- (previousContentSize until currentContentSize).reverse) {
      elt.removeContent(i)
    }
  }

  def evalExpression(
    expressionForErrorMsg: String,
    compiledExprFactory: CompiledExpressionFactory,
    variables: VariableMap,
    targetType: QName = NODE) = {
    val contextNode = elt.asInstanceOf[org.jdom2.Parent]
    val res = XPathUtil.evalExpression(expressionForErrorMsg, compiledExprFactory, variables, contextNode, targetType)
    res
  }

}

class InfosetDocument(val jDoc: org.jdom2.Document) extends InfosetItem {

  val jdomElt = {
    if (jDoc.hasRootElement()) Some(jDoc.getRootElement())
    else None
  }

  def toXML: scala.xml.Node = {
    if (jDoc.hasRootElement()) XMLUtils.element2Elem(jDoc.getRootElement())
    else <dafint:document xmlns:dafint={ XMLUtils.INT_NS }/>
  }

  var rootElement: Option[InfosetElement] =
    if (jDoc.hasRootElement()) Some(new InfosetElement(jDoc.getRootElement))
    else None

  def getRootElement() = {
    rootElement
  }

  def addElement(e: InfosetElement) = {
    rootElement = Some(e)
    // Content of the document is the root element.
    jDoc.setRootElement(e.jdomElt.get)
    // jDoc.addContent(e.jdomElt.get)
  }

}

sealed abstract class InfosetItem {
  override def toString(): String = jdomElt.getOrElse("InfosetItem(None)").toString

  def jdomElt: Option[org.jdom2.Element]

  def addElement(e: InfosetElement): Unit

  def toXML: scala.xml.Node

  def toBriefXML = {
    XMLUtils.removeAttributes(toXML)
  }

}
