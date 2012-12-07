package daffodil.processors
import daffodil.xml.XMLUtils
import daffodil.dsom.ElementBase
import daffodil.exceptions.Assert
import daffodil.dsom.SchemaComponentRegistry
import daffodil.processors.xpath.XPathUtil.CompiledExpressionFactory
import javax.xml.namespace.QName
import daffodil.processors.xpath.XPathUtil
import javax.xml.xpath.XPathConstants._

/**
 * Our abstraction layer over JDOM for the DFDL Infoset
 */

object Infoset {

  type SchemaComponentIDObject = String

  def addComponent(e: ElementBase): SchemaComponentIDObject = {
    val idString = SchemaComponentRegistry.addComponent(e)
    idString
  }

  def newElement(e: ElementBase) = {
    Assert.usage(e.name != "")
    val jdomE = new org.jdom.Element(e.name, e.targetNamespacePrefix, e.targetNamespace)
    //
    // Note: you can't save the attribute and reuse it because in JDOM
    // attributes have parent pointers. So this creates one and points it back at the jdomE
    // holding it.
    jdomE.setAttribute("context", e.schemaComponentID, XMLUtils.INT_NS_OBJECT)
    val res = new InfosetElement(jdomE)
    res
  }

  def newDocument() = {
    val res = new InfosetDocument(new org.jdom.Document())
    res
  }

  def apply(rootNode: scala.xml.Node): InfosetElement = {
    val jElt = XMLUtils.elem2Element(rootNode)
    val jDoc_ignored = new org.jdom.Document(jElt) // must have or jdom expressions won't work.
    val item = new InfosetElement(jElt)
    item
  }

  type ElementState = Int
  type Namespace = org.jdom.Namespace

}

class InfosetElement(private val elt: org.jdom.Element) extends InfosetItem {

  Assert.usage(elt != null)

  val jdomElt = Some(elt)

  def isNil: Boolean = XMLUtils.isNil(elt)
  def makeNil(): Unit = { elt.setAttribute(XMLUtils.nilAttribute()) }

  def setDataValue(s: String): Unit = {
    elt.setContent(new org.jdom.Text(XMLUtils.remapXMLIllegalCharactersToPUA(s)))
  }

  def addElement(e: InfosetElement) = elt.addContent(e.elt)

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
        // We have a uuid, retrieve the schema component
        SchemaComponentRegistry.getComponentByID(uuid) match {
          case Some(e) => e
          case None => {
            Assert.invariantFailed("Schema context component was not found in lookup table")
          }
        }
      }
    }
    context
  }

  def parent = {
    val par = elt.getParent()
    val res = par match {
      case e: org.jdom.Element => new InfosetElement(e)
      case d: org.jdom.Document => new InfosetDocument(d)
    }
    res
  }

  def namespace = elt.getNamespace()
  def name = elt.getName()

  def dataValue = elt.getText()

  def getChild(childName: String) = new InfosetElement(elt.getChild(childName))
  def getChild(cName: String, ns: org.jdom.Namespace) = new InfosetElement(elt.getChild(cName, ns))

  def captureState(): Infoset.ElementState = {
    elt.getContentSize()
  }

  def restoreState(st: Infoset.ElementState): Unit = {
    val previousContentSize: Int = st
    val currentContentSize = elt.getContentSize()
    for (i <- previousContentSize until currentContentSize) {
      elt.removeContent(i)
    }
  }

  def evalExpression(
    expressionForErrorMsg: String,
    compiledExprFactory: CompiledExpressionFactory,
    variables: VariableMap,
    targetType: QName = NODE) = {
    val contextNode = elt.asInstanceOf[org.jdom.Parent]
    val res = XPathUtil.evalExpression(expressionForErrorMsg, compiledExprFactory, variables, contextNode, targetType)
    res
  }

}

class InfosetDocument(private val jDoc: org.jdom.Document) extends InfosetItem {

  val jdomElt = None

  def toXML: scala.xml.Node = {
    if (jDoc.hasRootElement()) XMLUtils.element2Elem(jDoc.getRootElement())
    else <dafint:document xmlns:dafint={ XMLUtils.INT_NS }/>
  }

  def addElement(e: InfosetElement) = {
    // Content of the document is the root element.
    jDoc.setRootElement(e.jdomElt.get)
    // jDoc.addContent(e.jdomElt.get)
  }

}

abstract class InfosetItem {

  protected def jdomElt: Option[org.jdom.Element]

  def addElement(e: InfosetElement): Unit

  def toXML: scala.xml.Node

  def toBriefXML = {
    XMLUtils.removeAttributes(toXML)
  }

}
