package daffodil.dsom

import scala.xml._
import scala.xml.parsing._
import daffodil.exceptions._
import daffodil.schema.annotation.props.gen._
import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.collection.JavaConversions._

import daffodil.util.Validator
import daffodil.util.Misc

import java.io.InputStreamReader
import java.io.StringReader

object DsomCompiler extends App {
  
//  def rip(schema: String) = {
//    val parser = new XSOMParser()
//    val apf = new DomAnnotationParserFactory()
//    parser.setAnnotationParser(apf)
//
//    val instream = new ByteArrayInputStream(schema.getBytes());
//
//    parser.parse(instream)
//
//    val sset = parser.getResult()
//    val sds = parser.getDocuments()
//    (sds, sset)
//  }

  /**
   * For unit tests
   */
  def compile(xml: Node) = {
    validateDFDLSchema(xml)
    new SchemaSet(List(xml))
  }
  
  /**
   * validate a DFDL schema.
   * 
   * This validates the XML Schema language subset that DFDL uses, and also all the annotations
   * hung off of it.
   */
  def validateDFDLSchema(doc : Node) = {
    // TODO: should this do something other than throw an exception on a validation error?
    //
    // Users will write DFDL Schemas, using the xs or xsd prefix (usually) bound to the XML Schema namespace,
    // and the dfdl prefix (usually) bound to the DFDL namespace.
    //
    // However, we don't want to validate using the XML Schema for XML Schema (which would be the usual interpretation
    // of validating an XML Schema), instead we want to use the schema for the DFDL Subset of XML Schema.
    //
    // So, the hack here, is we're going to textually substitute the URIs, so that the validator doesn't have to be 
    // modified to do this switch, and we don't have to lie in the DFDL Subset schema, and claim it is realizing the
    // XML Schema URI.
    //
    // However, we should consider whether there is a better way to do this involving either (a) lying and having the
    // DFDL Subset Schema pretend it is the XSD schema, or we can play some catalog tricks perhaps.
    //
    // Also, the way this whole thing finds the necessary schemas is a bit daft. It should look in the jar or files,
    // but it should be using an XML Catalog.
    //
    val docstring = doc.toString()
    val xmlnsURI = "http://www.w3.org/2001/XMLSchema" ;
    val xsdSubsetURI = "http://www.ogf.org/dfdl/dfdl-1.0/XMLSchemaSubset" ;
    val docReplaced = docstring.replaceAll(xmlnsURI, xsdSubsetURI)
    val docReader = new StringReader(docReplaced)
    val schemaInputStream = Validator.daffodilLibSchema(Validator.dfdlSchemaFileName())
    val schemaReader = new InputStreamReader(schemaInputStream)
    val res = Validator.validateXMLStream(schemaReader, docReader)
    res
  }
  
}
