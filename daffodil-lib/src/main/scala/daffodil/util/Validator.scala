package daffodil.util

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

object Validator extends NoBindingFactoryAdapter {

  val xr = parser.getXMLReader()
  val sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
  sf.setResourceResolver(new LSResourceResolver {
    def resolveResource(
      type_ : String, namespaceURI: String,
      publicId: String, systemId: String, baseURI: String) = {
      //println(String.format("rr: %s %s %s %s %s", type_, namespaceURI, publicId, systemId, baseURI))
      null
    }
  });

  def makeParser(): SAXParser = {
    var parser: SAXParser = null
    try {
      val f = SAXParserFactory.newInstance()
      f.setNamespaceAware(true)
      f.setFeature("http://xml.org/sax/features/namespace-prefixes", true)
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
      parser = f.newSAXParser()
    } catch {
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

  def validateXML(
    schemaSource: StreamSource,
    documentSource: InputSource) = {
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
  // Note: for a resournce, a path begining with "/" means classPath root relative.
  def dfdlSchemaFileName(): String = "/xsd/DFDLSubsetOfXMLSchema_v1_036.xsd"

}