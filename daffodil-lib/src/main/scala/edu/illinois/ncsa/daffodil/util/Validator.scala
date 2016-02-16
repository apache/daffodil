/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.util

//
// Copyright (C) 2011, 2012 by Michael J. Beckerle, All rights Reserved.
// Permission is granted to use this software for any purpose so long as
// this copyright is preserved in both the source and binary forms, and
// in any documentation provided with the software.
//

import javax.xml.transform.stream.StreamSource
import javax.xml.XMLConstants
import javax.xml.validation.SchemaFactory
import javax.xml.parsers.SAXParser
import javax.xml.parsers.SAXParserFactory
import javax.xml.validation.Schema
import javax.xml.validation.ValidatorHandler
import org.xml.sax.XMLReader
import scala.xml.parsing.NoBindingFactoryAdapter
import scala.xml._
import java.io.Reader
import java.io.File
import java.io.FileReader
import java.io.InputStreamReader
import java.io.FileInputStream
import java.io.StringReader
import java.net.URI
import org.w3c.dom.ls.LSResourceResolver
import org.jdom2.Document
import org.jdom2.output.Format
import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.Attributes
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException

// Leaving this in temporarily, but commented out.
// We may want to use this for extra validation passes in the TDML Runner
// to do a validation pass on the TDML expected Infoset w.r.t. the model and to
// do a validation pass on the actual result w.r.t. the model as an XML document.
//
object Validator extends NoBindingFactoryAdapter {

  val xr = parser.getXMLReader()
  val sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)

  def makeParser(schema: Option[Schema] = None): SAXParser = {
    var parser: SAXParser = null
    try {
      val f = SAXParserFactory.newInstance()
      schema match {
        case Some(s) => f.setSchema(s)
        case _ => // Do Nothing
      }
      f.setNamespaceAware(true)
      f.setFeature("http://xml.org/sax/features/namespace-prefixes", true)
      //
      // Leaving these lines in, to document that they don't work if enabled.
      //
      //      f.setFeature("http://xml.org/sax/features/external-general-entities", false)
      //      f.setFeature("http://xml.org/sax/features/external-parameter-entities", false)
      //
      //      // Issue DFDL-76 in Jira - just adding these two lines does check more stuff, but it seems to
      //      // cause all sorts of havoc with not finding various schemas, etc.
      //      // Commented out for now pending more thorough investigation of how to fix this issue.
      //
      //      f.setFeature("http://apache.org/xml/features/validation/schema", true)
      //      f.setFeature("http://apache.org/xml/features/validation/schema-full-checking", true)
      //

      f.setFeature("http://xml.org/sax/features/validation", true)
      f.setFeature("http://apache.org/xml/features/validation/dynamic", true)
      f.setFeature("http://apache.org/xml/features/validation/schema", true)
      f.setFeature("http://apache.org/xml/features/validation/schema-full-checking", true)
      //f.setValidating(true)
      parser = f.newSAXParser()
    } catch {
      case s: scala.util.control.ControlThrowable => throw s
      case u: UnsuppressableException => throw u
      case e: Exception =>
        Console.err.println("error: Unable to instantiate parser")
        throw e
    }
    return parser
  }

  def validateXMLFiles(schemaFile: File, documentFile: File): Elem = {
    val xml = validateXML(new StreamSource(schemaFile), new InputSource(documentFile.toURI().toASCIIString()))
    return xml
  }

  def validateXMLStream(schemaResource: URI, documentReader: Reader, documentSystemId: String = "") = {
    val schemaSource = new StreamSource(schemaResource.toASCIIString())
    val document = new InputSource(documentReader)
    if (documentSystemId != "") document.setSystemId(documentSystemId)
    validateXML(schemaSource, document)
  }

  def validateXMLSources(schemaFileNames: Seq[String], document: Node): Unit = {
    val schemaSources: Seq[javax.xml.transform.Source] = schemaFileNames.map { fn =>
      {
        val uri = new URI(fn)
        val f = new File(uri)
        if (!f.exists()) {
          // we don't have the schema file, so we can't validate against it.
          throw ValidationException("Unable to validate. File not found: " + f)
        }
        val stream = new StreamSource(f)
        stream
      }
    }
    val documentSource = new InputSource(new StringReader(document.toString()))
    val schema = sf.newSchema(schemaSources.toArray)

    val parser = makeParser(Some(schema))

    val dh = new ContentHandler() //new DefaultHandler()
    parser.parse(documentSource, dh)
  }

  def validateXML(schemaSource: StreamSource, documentSource: InputSource) = {
    val schema = sf.newSchema(schemaSource)
    val parser = makeParser()
    val xr = parser.getXMLReader()
    val vh = schema.newValidatorHandler()
    vh.setContentHandler(this)
    xr.setContentHandler(vh)
    scopeStack.push(TopScope)
    xr.parse(documentSource)
    scopeStack.pop
    rootElem.asInstanceOf[Elem]
  }

  /**
   * Convenient for unit tests
   * @param schemaNode
   * @param documentNode
   * @return
   */
  def validateXMLNodes(schemaNode: Node, documentNode: NodeSeq): Elem = {
    // serialize the scala document XML node back to a string because
    // java library wants to read the document from an InputSource.
    val documentSource = new InputSource(new StringReader(documentNode.toString()))
    val schemaSource = new StreamSource(new StringReader(schemaNode.toString()))
    return validateXML(schemaSource, documentSource)
  }

  /**
   * Retrieve a schema that is part of the daffodil-lib.
   */
  // Note: for a resource, a path begining with "/" means classPath root relative.
  def dfdlSchemaFileName(): String = "/xsd/XMLSchema.xsd"

}

case class ValidationException(msg: String) extends Exception(msg) {

}

class ContentHandler extends DefaultHandler {
  private var element: String = ""

  override def startElement(uri: String, localName: String, qName: String, attributes: Attributes): Unit = {
    if (localName != null && !localName.isEmpty) element = localName
    else element = qName
  }
  override def warning(exception: SAXParseException): Unit = {
    val msg: java.lang.String = element + ": " + exception.getMessage
    throw new ValidationException(msg)
  }
  override def error(exception: SAXParseException): Unit = {
    val msg: java.lang.String = element + ": " + exception.getMessage
    throw new ValidationException(msg)
  }
  override def fatalError(exception: SAXParseException): Unit = {
    val msg: java.lang.String = element + ": " + exception.getMessage
    throw new ValidationException(msg)
  }
  def getElement(): String = element
}
