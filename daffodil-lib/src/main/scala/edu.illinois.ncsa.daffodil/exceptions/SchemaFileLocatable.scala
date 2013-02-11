package edu.illinois.ncsa.daffodil.exceptions
import scala.xml.Node
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import java.net.URL

trait SchemaFileLocatable {
  def xml: Node

  lazy val lineNumber = xml.attribute(XMLUtils.INT_NS, XMLUtils.LINE_ATTRIBUTE_NAME) match {
    case Some(seqNodes) => Some(seqNodes.toString)
    case None => None
  }

  lazy val lineDescription = lineNumber match {
    case Some(num) => " line " + num
    case None => ""
  }

  lazy val columnNumber = xml.attribute(XMLUtils.INT_NS, XMLUtils.COLUMN_ATTRIBUTE_NAME) match {
    case Some(seqNodes) => Some(seqNodes.toString)
    case None => None
  }

  lazy val columnDescription = columnNumber match {
    case Some(num) => " column " + num
    case None => ""
  }

  lazy val fileDescription = " in " + fileName

  lazy val locationDescription = {
    val showInfo = lineDescription != "" || fileDescription != ""
    val info = lineDescription + columnDescription + fileDescription
    val txt = if (showInfo) "Location" + info else ""
    txt
  }

  /**
   *
   * override if you don't have a fileName attribute appended
   * but are in a context where some enclosing construct does
   * normally only a root node would have a file attribute.
   *
   * implement as
   * @example {{{
   *     lazy val fileName = fileNameFromAttribute()
   * }}}
   * or delegate like
   * @example {{{
   *     lazy val fileName = schemaDocument.fileName
   * }}}
   */
  def fileName: URL

  def fileNameFromAttribute() = {
    xml.attribute(XMLUtils.INT_NS, XMLUtils.FILE_ATTRIBUTE_NAME) match {
      case Some(seqNodes) => Some(new URL(seqNodes.toString))
      case None => None
    }

  }

}

