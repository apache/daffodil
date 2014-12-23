package edu.illinois.ncsa.daffodil.xml

import scala.xml._
import scala.util.parsing.input.Position
import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.net.URI

/**
 * Common logic for dealing with adding file/line/col attributes to XML
 * while loading.
 *
 * Shared by both Xerces (used for validation), and the ConstructingLoader
 * (used to actually construct the scala XML nodes).
 */
class FileLineColInfo(pre: String, label: String,
  scope: NamespaceBinding, attrs: MetaData,
  line: String,
  col: String,
  uriString: String) {

  // If we're the xs:schema node, then append the attribute for _file_ as well.

  private val nsURI = NS(scope.getURI(pre))
  private val isXSSchemaNode = (label == "schema" && nsURI == XMLUtils.XSD_NAMESPACE)
  private val isTDMLTestSuiteNode = (label == "testSuite" && nsURI == XMLUtils.TDML_NAMESPACE)
  private val isFileRootNode = isXSSchemaNode || isTDMLTestSuiteNode

  //
  // If this parser is at all sane in node construction, then it creates
  // nodes passing in the scope of the parent enclosing element. If that 
  // has the dafint binding in it already, we don't add it again.
  // 
  private lazy val scopeWithDafInt = {
    val intPre = scope.getPrefix(XMLUtils.INT_NS)
    if (intPre == null)
      NamespaceBinding(XMLUtils.INT_PREFIX, XMLUtils.INT_NS, scope)
    else {
      Assert.usage(intPre == XMLUtils.INT_PREFIX) // can't deal with some other binding for dafint.
      scope
    }
  }

  private val haveFileName = isFileRootNode && uriString != ""

  val lineAttr = attrs.get(XMLUtils.INT_NS, scopeWithDafInt, XMLUtils.LINE_ATTRIBUTE_NAME).map { _.text }
  val colAttr = attrs.get(XMLUtils.INT_NS, scopeWithDafInt, XMLUtils.COLUMN_ATTRIBUTE_NAME).map { _.text }
  val fileAttr = attrs.get(XMLUtils.INT_NS, scopeWithDafInt, XMLUtils.FILE_ATTRIBUTE_NAME).map { _.text }

  val alreadyHasLocation = lineAttr.isDefined || colAttr.isDefined

  val newScope =
    if (alreadyHasLocation) scope
    else scopeWithDafInt

  private def makeLineAttr() =
    Attribute(XMLUtils.INT_PREFIX, XMLUtils.LINE_ATTRIBUTE_NAME, Text(line), Null)

  private def makeColAttr() =
    Attribute(XMLUtils.INT_PREFIX, XMLUtils.COLUMN_ATTRIBUTE_NAME, Text(col), Null)

  private def makeFileAttr() = {
    Attribute(XMLUtils.INT_PREFIX, XMLUtils.FILE_ATTRIBUTE_NAME, Text(uriString), Null)
  }

  val newLineAttr =
    if (!alreadyHasLocation) makeLineAttr() else Null

  val newColAttr =
    if (!alreadyHasLocation) makeColAttr() else Null

  val newFileAttr =
    if (!haveFileName) Null
    else if (!alreadyHasLocation) makeFileAttr() else Null
}