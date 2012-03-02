package daffodil.dsom

import java.io.ByteArrayInputStream
import java.io.InputStreamReader
import java.io.StringReader

import scala.xml.Node
import scala.xml.XML

import daffodil.api.DFDL
import daffodil.exceptions.Assert
import daffodil.util.Validator
import daffodil.xml.XMLUtil

class Compiler extends DFDL.Compiler {
  var root: String = ""
  var rootNamespace: String = ""
  var debugMode = false

  def setDistinguishedRootNode(name: String, namespace: String = ""): Unit = {
    root = name
    rootNamespace = namespace
  }

  def setExternalDFDLVariable(name: String, namespace: String, value: String): Unit = {
    Assert.notYetImplemented()
  }

  def setDebugging(flag: Boolean) {
    debugMode = flag
  }

  /*
   * for unit testing of front end
   */
  private[dsom] def frontEnd(xml: Node) = {
    Compiler.validateDFDLSchema(xml)
    val sset = new SchemaSet(List(xml))
    // That's what we have for now....
    sset
  }

  def commonCompile(xml: Node) = {
    val elts = (xml \ "element")
    Assert.usage(elts.length != 0, "No top level element declarations found.")

    // we must have a root node specified, because it is not unique.
    if (root == "") {
      // default to first element decl.
      val firstElt = elts(0)
      val firstEltName = (firstElt \ "@name").text.toString
      root = firstEltName
      rootNamespace = firstElt.namespace
    }

    lazy val hasMatch = elts.exists { elt =>
      {
        val eltName = (elt \ "@name").text.toString
        val eltNS = elt.namespace
        val nameMatches = root == eltName
        // Tolerate namespace ""
        val namespaceMatches = (rootNamespace == "" || rootNamespace == eltNS)
        val res = nameMatches && namespaceMatches
        res
      }
    }
    Assert.usage(root != "" && hasMatch, "The root node named " + root + " was not found.")

    //
    // New front end (validates etc. We want to exercise it.)
    //
    frontEnd(xml)
  }

  def reload(fileNameOfSavedParser: String) = {
    val sp = daffodil.parser.SchemaParser.readParser(fileNameOfSavedParser)
    backEnd(sp, Assert.notYetImplemented())
  }

  def compile(schemaFileName: String): DFDL.ProcessorFactory = {
    val schemaNode = XML.load(schemaFileName)
    compileSchema(schemaNode)
  }

  def compile(xml: Node): DFDL.ProcessorFactory = compileSchema(xml)

  private def compileSchema(xml: Node): DFDL.ProcessorFactory = {
    val sset = commonCompile(xml)

    //
    // older front end, and back-end....
    //
    val sp = new daffodil.parser.SchemaParser
    sp.parse(new ByteArrayInputStream(xml.toString.getBytes())) // parse the schema that is.

    backEnd(sp, sset)
  }

  def backEnd(sp: daffodil.parser.SchemaParser, sset: SchemaSet) = {
    new DFDL.ProcessorFactory {

      lazy val schemaSet = sset
      def onPath(xpath: String): DFDL.DataProcessor =
        new DFDL.DataProcessor {

          def save(fileName: String): Unit = {
            daffodil.parser.SchemaParser.writeParser(sp, fileName)
          }
          
          def parse(input: DFDL.Input): scala.xml.Node = {
            val inStream = java.nio.channels.Channels.newInputStream(input)
            val bufferedInStream = new java.io.BufferedInputStream(inStream)
            sp.setDebugging(debugMode)
            val jdomElt = sp.eval(bufferedInStream, root)
            val node = XMLUtil.element2Elem(jdomElt)
            node
          }

          def unparse(output: DFDL.Output, node: scala.xml.Node): Unit = {
            sp.setDebugging(debugMode)
            val jdomElem = XMLUtil.elem2Element(node)
            val jdomDoc = new org.jdom.Document(jdomElem)
            val data = sp.unparse(jdomDoc, root)
            // it would be nicer to stream this, avoid a copy, etc.
            // But that requires us to change the signature of sp.unparse.
            output.write(data)
            output
          }
        }

    }
  }

}

object Compiler {

  def apply() = new Compiler()

  /**
   * validate a DFDL schema.
   *
   * This validates the XML Schema language subset that DFDL uses, and also all the annotations
   * hung off of it.
   */
  def validateDFDLSchema(doc: Node) = {
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
    val xmlnsURI = "http://www.w3.org/2001/XMLSchema";
    val xsdSubsetURI = "http://www.ogf.org/dfdl/dfdl-1.0/XMLSchemaSubset";
    val docReplaced = docstring.replaceAll(xmlnsURI, xsdSubsetURI)
    val docReader = new StringReader(docReplaced)
    val schemaInputStream = Validator.daffodilLibSchema(Validator.dfdlSchemaFileName())
    val schemaReader = new InputStreamReader(schemaInputStream)
    val res = Validator.validateXMLStream(schemaReader, docReader)
    res
  }

  def stringToReadableByteChannel(s: String) = {
    val bytes = s.getBytes()
    val inputStream = new ByteArrayInputStream(bytes);
    val rbc = java.nio.channels.Channels.newChannel(inputStream);
    rbc
  }

  def fileToReadableByteChannel(file: java.io.File) = {
    val inputStream = new java.io.FileInputStream(file)
    val rbc = java.nio.channels.Channels.newChannel(inputStream);
    rbc
  }

  def testString(testSchema: Node, data: String) = {
    val compiler = Compiler()
    val pf = compiler.compile(testSchema)
    val p = pf.onPath("/")
    val d = Compiler.stringToReadableByteChannel(data)
    val actual = p.parse(d)
    actual
  }

  def testFile(testSchema: Node, fileName: String) = {
    val compiler = Compiler()
    val pf = compiler.compile(testSchema)
    val p = pf.onPath("/")
    val d = Compiler.fileToReadableByteChannel(new java.io.File(fileName))
    val actual = p.parse(d)
    actual
  }
}

  // Example of using XSOM
  //
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
