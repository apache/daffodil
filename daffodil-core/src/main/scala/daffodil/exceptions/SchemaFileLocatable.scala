package daffodil.exceptions
import scala.xml.Node
import daffodil.xml.XMLUtils

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

  lazy val fileDescription = fileName match {
    case Some(num) => " in " + num
    case None => ""
  }

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
   * <p>
   * implement as
   * <pre>
   *     lazy val fileName = fileNameFromAttribute()
   * </pre>
   * or delegate like
   * <pre>
   *     lazy val fileName = schemaDocument.fileName
   * </pre>
   */
  def fileName: Option[String]

  def fileNameFromAttribute() = {
    xml.attribute(XMLUtils.INT_NS, XMLUtils.FILE_ATTRIBUTE_NAME) match {
      case Some(seqNodes) => Some(seqNodes.toString)
      case None => None
    }

  }

}

