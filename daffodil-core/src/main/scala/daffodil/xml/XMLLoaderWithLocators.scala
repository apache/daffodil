package daffodil.xml

/**
 * Adapted from a question/answer on the Stack Overflow web site.
 *
 * See http://stackoverflow.com/questions/4446137/how-to-track-the-source-line-location-of-an-xml-element
 */

import org.xml.sax.Locator
import scala.xml.parsing.NoBindingFactoryAdapter
import scala.xml._
import java.io.File
import java.io.FileDescriptor
import java.net.URL
import daffodil.exceptions.Assert

class Adapter extends parsing.NoBindingFactoryAdapter {

  // having trouble with scala compiler...wierdness
  // let's go with explicit setter to a private local var
  var fileName: String = ""

  var saxLocator: org.xml.sax.Locator = _

  // Get location
  override def setDocumentLocator(locator: org.xml.sax.Locator) {
    //    println("setDocumentLocator line=%s col=%s sysID=%s, pubID=%s".format(
    //      locator.getLineNumber, locator.getColumnNumber,
    //      locator.getSystemId, locator.getPublicId))
    this.saxLocator = locator
    super.setDocumentLocator(locator)
  }

  case class Locator(line: Int, col: Int, sysID: String, pubID: String)

  // Without a trick, locators will always provide the end position of an element
  // and we want the start position.
  // With this trick, the line and column will be of the ending ">" of the
  // starting element tag.

  // startElement saves locator information on stack
  val locatorStack = new scala.collection.mutable.Stack[Locator]
  // endElement pops it off into here
  var elementStartLocator: Locator = _

  // create node then uses it.
  override def createNode(pre: String, label: String, attrs: MetaData, scope: NamespaceBinding, children: List[Node]): Elem = {

    // If we're the xs:schema node, then append attribute for _file_ as well.
    val nsURI = scope.getURI(pre)
    val isXSSchemaNode = (label == "schema" && nsURI != null &&
      (nsURI == XMLUtils.XSD_NAMESPACE ||
        nsURI == XMLUtils.DFDL_SUBSET_NAMESPACE))
    val isTDMLTestSuiteNode = (label == "testSuite" && nsURI != null &&
      nsURI == XMLUtils.TDML_NAMESPACE)
    val isFileRootNode = isXSSchemaNode || isTDMLTestSuiteNode

    // augment the scope with the dafint namespace binding but only
    // for root nodes (to avoid clutter with the long xmlns:dafint="big uri")
    // and only if it isn't already there.
    val scopeWithDafInt =
      if (scope.getPrefix(XMLUtils.INT_NS) == null
        && isFileRootNode)
        NamespaceBinding(XMLUtils.INT_PREFIX, XMLUtils.INT_NS, scope)
      else scope

    val haveFileName = isFileRootNode && fileName != ""

    val asIs = super.createNode(pre, label, attrs, scopeWithDafInt, children)

    val alreadyHasFile = attrs.get(XMLUtils.INT_NS, scopeWithDafInt, XMLUtils.FILE_ATTRIBUTE_NAME) != None

    // If there is already a _line_ attribute, then we're reloading something
    // that was probably converted back into a string and written out. 
    // The original line numbers are therefore the ones wanted, not any new
    // line numbers, so we don't displace any line numbers that already existed.

    val alreadyHasLine = attrs.get(XMLUtils.INT_NS, scopeWithDafInt, XMLUtils.LINE_ATTRIBUTE_NAME) != None
    val alreadyHasCol = attrs.get(XMLUtils.INT_NS, scopeWithDafInt, XMLUtils.COLUMN_ATTRIBUTE_NAME) != None
    Assert.invariant(alreadyHasLine == alreadyHasCol)

    val lineAttr =
      if (alreadyHasLine) Null
      else Attribute(XMLUtils.INT_PREFIX, XMLUtils.LINE_ATTRIBUTE_NAME, Text(elementStartLocator.line.toString), Null)
    val colAttr =
      if (alreadyHasCol) Null
      else Attribute(XMLUtils.INT_PREFIX, XMLUtils.COLUMN_ATTRIBUTE_NAME, Text(elementStartLocator.col.toString), Null)
    val fileAttr =
      if (alreadyHasFile || !haveFileName) Null
      else {
        Attribute(XMLUtils.INT_PREFIX, XMLUtils.FILE_ATTRIBUTE_NAME, Text(fileName), Null)
      }

    // % operator creates a new element with updated attributes
    val res = asIs % lineAttr % colAttr % fileAttr
    //        % (if (hasSysID) Attribute("_sysID_", Text(elementStartLocator.sysID), Null) else Null)
    //        % (if (hasPubID) Attribute("_pubID_", Text(elementStartLocator.pubID), Null) else Null)
    res
  }

  override def startElement(uri: String, _localName: String, qname: String, attributes: org.xml.sax.Attributes): Unit = {
    // println("startElement")
    val loc = Locator(saxLocator.getLineNumber, saxLocator.getColumnNumber, saxLocator.getSystemId, saxLocator.getPublicId)
    locatorStack.push(loc)
    super.startElement(uri, _localName, qname, attributes)
  }

  override def endElement(uri: String, _localName: String, qname: String): Unit = {
    // println("endElement")
    elementStartLocator = locatorStack.pop
    super.endElement(uri, _localName, qname)
  }

}

object XMLLoaderWithLocator extends factory.XMLLoader[Elem] {

  override val adapter = new Adapter

  // capture the file name
  //
  override def loadFile(f: File) = {
    adapter.fileName = f.getAbsolutePath()
    super.loadFile(f)
  }

  override def loadFile(name: String) = {
    adapter.fileName = name
    super.loadFile(name)
  }

  override def load(url: URL) = {
    adapter.fileName = url.toURI.toASCIIString
    val res = super.load(url)
    res
  }

}
