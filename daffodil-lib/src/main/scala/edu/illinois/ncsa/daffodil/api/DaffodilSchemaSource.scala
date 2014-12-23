package edu.illinois.ncsa.daffodil.api
import org.xml.sax.InputSource
import java.net.URI
import scala.xml.Node
import java.io.FileInputStream
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import org.apache.commons.io.input.XmlStreamReader

/**
 * Our abstraction of the source of a schema.
 *
 * Because we make multiple passes over the schema (for validation)
 * we have to cache the schema if it is given to us in any sort of transient form
 * like as an input stream (stdin for example).
 *
 * Can also be used for other XML artifacts such as TDML files.
 */
sealed trait DaffodilSchemaSource {

  /**
   * Use to get a org.xml.sax.InputSource for use by Xerces
   */
  def newInputSource(): InputSource

  /**
   * Use to get the URI that can be used to load the xml.
   */
  def uriForLoading: URI
}

case class URISchemaSource(fileOrResource: URI) extends DaffodilSchemaSource {
  override def newInputSource() = {
    val is = new InputSource(fileOrResource.toURL.openStream())
    is.setSystemId(fileOrResource.toString)
    is
  }
  override def uriForLoading = fileOrResource
}

/**
 * For stdin, or other anonymous pipe-like source of schema.
 */
case class InputStreamSchemaSource(is: java.io.InputStream, blameName: String, extension: String) extends DaffodilSchemaSource {
  lazy val tempSchemaFile = XMLUtils.convertInputStreamToTempFile(is, blameName, extension)
  lazy val tempURI = tempSchemaFile.toURI
  lazy val csName = {
    val xmlStream = new XmlStreamReader(tempSchemaFile)
    val csName = xmlStream.getEncoding()
    xmlStream.close()
    csName
  }
  override def newInputSource() = {
    val inSrc = new InputSource(tempURI.toString)
    inSrc.setEncoding(csName)
    // 
    inSrc.setSystemId(blameName)
    inSrc
  }
  override def uriForLoading = tempURI
}

protected sealed abstract class NodeSchemaSourceBase(node: Node, nameHint: String) extends DaffodilSchemaSource {
  lazy val tempSchemaFile = XMLUtils.convertNodeToTempFile(node, nameHint)
  lazy val tempURI = tempSchemaFile.toURI
  def blameName: String
  override def newInputSource() = {
    val is = new FileInputStream(tempSchemaFile)
    val inSrc = new InputSource(is)
    inSrc.setSystemId(blameName)
    inSrc
  }
  override def uriForLoading = tempURI
}

case class UnitTestSchemaSource(node: Node, nameHint: String)
  extends NodeSchemaSourceBase(node, nameHint) {
  override val blameName =
    if (nameHint != "") "unittest:" + nameHint
    else tempURI.toString
}

/**
 * Used by TDML runner for embedded schemas- the schema node is constructed out of the TDML file
 * which in order to be able to validate repeatedly and such, is written to a temp file.
 */
case class EmbeddedSchemaSource(node: Node, nameHint: String)
  extends NodeSchemaSourceBase(node, nameHint) {
  override val blameName = tempURI.toString
}
