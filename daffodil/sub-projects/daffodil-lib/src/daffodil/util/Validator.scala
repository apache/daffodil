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

//import com.dataiti.utility.ResourceResolver._

import scala.xml.parsing.NoBindingFactoryAdapter 
import scala.xml._
import java.io.Reader
import java.io.File
import java.io.FileReader
import java.io.InputStreamReader
import java.io.FileInputStream
import java.io.StringReader

  
object Validator extends NoBindingFactoryAdapter {

  val xr = parser.getXMLReader()
  val sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI) 
  sf.setResourceResolver(new ResourceResolver());
  
  def makeParser() : SAXParser = {
    var parser : SAXParser = null 
    try {
      val f = SAXParserFactory.newInstance()
      f.setNamespaceAware(true)
      f.setFeature("http://xml.org/sax/features/namespace-prefixes", true)
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
      case e: Exception => Console.err.println("error: Unable to instantiate parser")
      throw e
    }
    return parser
  }
  
  def validateXMLFiles(schemaFileName : String, documentFileName : String) : Elem = {
    val schemaReader = new FileReader(new File(schemaFileName))
    val documentReader = new FileReader(new File(documentFileName))
    val xml = validateXMLStream(schemaReader, documentReader)
    return xml
  }
  
  def validateXMLStrings(schemaString : String, documentString : String) :Elem = {
    val schemaReader = new StringReader(schemaString)
    val documentReader = new StringReader(documentString)
    val xml = validateXMLStream(schemaReader, documentReader)
    return xml
  }
  
  def validateXMLStream(schemaReader: Reader, documentReader : Reader): Elem = {
    val schemaSource = new StreamSource(schemaReader)
    val schema = sf.newSchema(schemaSource)
    val parser = makeParser()
    val xr = parser.getXMLReader()
    val vh = schema.newValidatorHandler()
    vh.setContentHandler(this)
    xr.setContentHandler(vh)
    scopeStack.push(TopScope)
    val document = new InputSource(documentReader)
    xr.parse(document)
    scopeStack.pop
    return rootElem.asInstanceOf[Elem]
  }
     
  /**
   * Convenient for unit tests
   * @param schemaNode
   * @param documentNode
   * @return
   */
  def validateXMLNodes(schemaNode : Node, documentNode : NodeSeq) : Elem = {
    // serialize the scala document XML node back to a string because
    // java library wants to read the document from an InputSource.
    val documentString = documentNode.toString()
    val schemaString = schemaNode.toString()
    return validateXMLStrings(schemaString, documentString)
  }
    
  /**
   * Retrieve a schema that is part of the daffodil-lib. 
   * (In its jar or file tree.)
   */
  //TODO: all this location of schema stuff should be replaced with an XML Catalog
  def daffodilLibSchema(fn : String) = {
    Misc.getResourceOrFileStream(fn)
  }
  
  def dfdlSchemaFileName () : String = "../daffodil-lib/src/xsd/DFDLSubsetOfXMLSchema_v1_036.xsd" 
    // TODO: find this file no matter where the application is called from, and regardless of 
    // xsi:schemaLocation stuff in the dfdl schema. This file should be a resource 
    // stored in the jar file for the DFDL processor.
  
}