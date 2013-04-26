package edu.illinois.ncsa.daffodil.dsom
import org.xml.sax.SAXParseException
import edu.illinois.ncsa.daffodil.xml.DaffodilXMLLoader
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.xml.NS
import java.net.URL
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import java.io.File
import edu.illinois.ncsa.daffodil.dsom.IIUtils._
import edu.illinois.ncsa.daffodil.xml.DaffodilCatalogResolver
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.HasIsError
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG

/**
 * represents one schema document file
 *
 * manages loading of it, and keeping track of validation errors
 */
class DFDLSchemaFile(val sset: SchemaSet,
                     sourceOfSchema: => Any, // fileName, URL, or a scala.xml.Node
                     val iiParent: IIBase,
                     seenBeforeArg: IIMap)
  extends SchemaComponent(<file/>, sset)
  with org.xml.sax.ErrorHandler {

  requiredEvaluations(isValid)

  lazy val seenBefore = seenBeforeArg

  //  /**
  //   * Delegate back to the include or import that references us.
  //   * 
  //   * This is the schema document we are contained in, not the one
  //   * we are referring to.
  //   */
  // lazy val schemaDocument = Assert.invariantFailed("schemaDocument called on schemaFile. You wan't to call schemaDocument on the include/import object.")
  override lazy val schemaDocument = {
    // the one containing the reference to the file
    // Not the schema document in this file (that one is iiSchemaDocument).    
    val res = iiParent.schemaDocument
    // the schemaDocument in this file is called iiSchemaDocument,
    // but you may only be interested in its XML characteristics (namespace 
    // for example), in which case you want iiXMLSchemaDocument
    res
  }

  override lazy val prettyName = url.toString

  lazy val diagnosticChildren = Nil // no recursive descent. We just want the loader's validation errors.

  lazy val fakeURL = new File("tempFile.xsd").toURI.toURL

  lazy val url = sourceOfSchema match {
    case fn: String => new File(fn).toURI.toURL
    case url: URL => url
    case n: scala.xml.Node => fakeURL
    case _ => Assert.usageError("sourceOfSchema must be a fileName string, a URL or a schema node")
  }

  override lazy val enclosingComponent = None

  var validationDiagnostics_ : Seq[Diagnostic] = Nil

  def validationDiagnostics = validationDiagnostics_

  def isValid: Boolean = {
    node // demanding this forces the load to happen
    val ld = validationDiagnostics
    // warnings won't stop things. 
    // TODO: options to control when validation warnings
    // should be escalated to errors.
    val res = !ld.exists { d =>
      {
        val isE = d.isError
        isE
      }
    }
    res
  }

  def warning(exception: SAXParseException) = {
    val sdw = new SchemaDefinitionWarning(this, "Warning loading schema", exception)
    warn(sdw)
    validationDiagnostics_ :+= sdw
  }

  def error(exception: SAXParseException) = {
    val sde = new SchemaDefinitionError(this, "Error loading schema", exception)
    // println(sde)
    error(sde)
    validationDiagnostics_ :+= sde
  }

  def fatalError(exception: SAXParseException) = {
    val sde = new SchemaDefinitionError(this, "Fatal error loading schema", exception)
    // error(sde) // will get picked up when parser throws after this returns
    validationDiagnostics_ :+= sde
    // parser throws out of fatalErrors.
  }

  lazy val loader = new DaffodilXMLLoader(this)
  lazy val resolver = DaffodilCatalogResolver.resolver

  lazy val loadedURL = loadedURL_.value
  private val loadedURL_ = LV('loadedURL) {
    val node = loader.load(url)
    node
  }

  lazy val node =
    sourceOfSchema match {
      case schemaNode: scala.xml.Node => schemaNode
      case _ => loadedURL
    }

  //  lazy val seenBeforePlusThis = {
  //    val res = if (ii.notSeenThisBefore) {
  //      seenBefore + ii.mapTuple
  //    } else seenBefore
  //    res
  //  }

  lazy val iiXMLSchemaDocument = iiXMLSchemaDocument_.value
  val iiXMLSchemaDocument_ = LV('iiXMLSchemaDocument) {
    val res = loadXMLSchemaDocument(seenBefore, Some(this))
    res
  }

  lazy val iiSchemaDocument = iiSchemaDocument_.value
  private val iiSchemaDocument_ = LV('iiSchemaDocument) {
    val res = new SchemaDocument(iiXMLSchemaDocument)
    res
  }

  def loadXMLSchemaDocument(before: IIMap, sf: Option[DFDLSchemaFile]) = {
    val sd = node match {
      case <schema>{ _* }</schema> if (NS(node.namespace) == XMLUtils.xsdURI) => {
        // top level is a schema. 

        val sd = new XMLSchemaDocument(node, sset, Some(iiParent), sf, before)
        sd
      }
      case _ => schemaDefinitionError("The file %s did not contain a schema element as the document element. Found %s in namespace %s.", url, node.label, node.namespace)
    }
    sd
  }

  lazy val seenAfter: IIMap = seenAfter_.value
  private val seenAfter_ = LV('seenAfter) {
    val res = OOLAG.keepGoing(seenBefore) {
      val aft = iiXMLSchemaDocument.seenAfter
      aft
    }
    res
  }
}
