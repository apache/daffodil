package daffodil.tdml

import java.io.File
import scala.Array.canBuildFrom
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.Utility
import scala.xml.XML
import org.scalatest.junit.JUnit3Suite
import daffodil.Implicits.using
import daffodil.dsom.Compiler
import daffodil.xml.XMLUtils
import daffodil.util._
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import junit.framework.Assert.fail
import daffodil.util.Misc._
import java.io.FileInputStream
import java.io.FileNotFoundException
import org.xml.sax.InputSource
import java.io.StringReader
import javax.xml.transform.stream.StreamSource
import java.net.URL
import java.net.URI

/**
 * Parses and runs tests expressed in IBM's contributed tdml "Test Data Markup Language"
 */

//
// TODO: plug in Daffodil API to run the tests.
//
// TODO: validate the TDML with the schema for TDML. (Temporarily just do this in eclipse using its validator)
//
// TODO: validate the infoset XML (expected result) against the DFDL Schema, that is using it as an XML Schema
// for the infoset. This would prevent errors where the infoset instance and the schema drift apart under maintenance.
//
// TODO: validate the actual result against the DFDL Schema using it as an XML Schema. 
//
// TODO: add ability to embed the schema directly in the TDML file for a 100% self-contained test case. Note that
// the schemas should not be inside the individual test cases, but instead should be separate top-level structures 
// referenced from the test cases.
//
// TODO: Unparser variant. Inverts the whole thing by starting from the infoset, and constructing a document.
// 

/**
 * TDML test suite runner
 *
 * Keep this independent of Daffodil, so that it can be used to run tests against other DFDL implementations as well.
 * E.g., it should only need an API specified as a collection of Scala traits, and some simple way to inject
 * dependency on one factory to create processors.
 */

class DFDLTestSuite(ts : Node, tdmlFile : File, tsInputSource : InputSource) {
  
  def this(tdmlFile : File) = this(XML.loadFile(tdmlFile), tdmlFile, new InputSource(tdmlFile.toURI().toASCIIString()))
  def this(tsNode : Node) = this(tsNode, null, new InputSource(new StringReader(tsNode.toString)))
  def this(tsURL : URL) = this(XML.load(tsURL), null, new InputSource(tsURL.toURI().toASCIIString()))

  //
  // we immediately validate the incoming test suite document
  // against its schema. We're depending on Validator to find all the 
  // included schemas such as that for embedded defineSchema named schema nodes.
  // 
  val tdmlXSDResourcePath = "/xsd/tdml.xsd"

  val tdmlSchemaResource = Misc.getRequiredResource(tdmlXSDResourcePath)
  assert(Validator.validateXML(
      new StreamSource(tdmlSchemaResource.toURI().toASCIIString()),
      tsInputSource) != null)
 
  lazy val parserTestCases = (ts \ "parserTestCase").map { node => ParserTestCase(node, this) }
  lazy val suiteName = (ts \ "@suiteName").text
  lazy val suiteID = (ts \ "@ID").text
  lazy val description = (ts \ "@description").text
  lazy val embeddedSchemas = (ts \ "defineSchema").map { node => DefinedSchema(node, this) }

  def runAllTests(schema : Option[Node] = None) {
    parserTestCases.map { _.run(schema) }
  }

  def runOneTest(testName : String, schema : Option[Node] = None) {
    val testCase = parserTestCases.find(_.name == testName)
    testCase match {
      case None => throw new Exception("test " + testName + " was not found.")
      case Some(tc) => {
        tc.run(schema)
      }
    }
  }

  /**
   * Try a few possibilities to find the model/schema file.
   *
   * IBM's suites have funny model paths in them. We don't have that file structure,
   * so we look for the schema/model files in the working directory, and in the same
   * directory as the tdml file, and some other variations.
   */
  def findModelFile(fileName : String) : File = {
    val firstTry = new File(fileName)
    if (firstTry.exists()) return firstTry
    // see if it can be found relative to the tdml test file, like next to it.
    val sysId = tsInputSource.getSystemId()
    if (sysId != null) {
      val sysFile = new File(new URI(sysId))
      if (sysFile.exists()) {
        // the system Id of the tdml file was a file.
        val sysPath = sysFile.getParent()
        val modelFileName = sysPath + File.separator + fileName
        println("Model file name is: " + modelFileName)
        val modelFile = new File(modelFileName)
        if (modelFile.exists()) return modelFile
      }
    }
    // try ignoring the directory part
    val parts = fileName.split("/")
    if (parts.length > 1) {
      val filePart = parts.last
      val secondTry = findModelFile(filePart) // recursively
      if (secondTry.exists()) return secondTry;
    }
    throw new FileNotFoundException("Unable to find model file " + fileName + ".")
  }

  def findModel(modelName : String) : Node = {
    // schemas defined with defineSchema take priority as names.
    val es = embeddedSchemas.find { defSch => defSch.name == modelName }
    es match {
      case Some(defschema) => defschema.xsdSchema
      case None => {
        val file = findModelFile(modelName)
        val schema = XML.loadFile(file)
        schema
      }
    }
  }

}

case class ParserTestCase(ptc : NodeSeq, val parent : DFDLTestSuite) {
  lazy val Seq(document) = (ptc \ "document").map { node => new Document(node, this) }
  lazy val Seq(infoset) = (ptc \ "infoset").map { node => new Infoset(node, this) }
  lazy val name = (ptc \ "@name").text
  lazy val ptcID = (ptc \ "@ID").text
  lazy val id = name + (if (ptcID != "") "(" + ptcID + ")" else "")
  lazy val root = (ptc \ "@root").text
  lazy val model = (ptc \ "@model").text
  lazy val description = (ptc \ "@description").text
  lazy val unsupported = (ptc \ "@unsupported").text match {
    case "true" => true
    case "false" => false
    case _ => false
  }

  def findModel(modelName : String) : Node = {
    if (modelName == "") {
      suppliedSchema match {
        case None => throw new Exception("No model.")
        case Some(s) => return s
      }
    } else
      parent.findModel(modelName)
  }

  var suppliedSchema : Option[Node] = None

  def run(schema : Option[Node] = None) = {
    suppliedSchema = schema
    val sch = schema match {
      case Some(sch) => {
        if (model != "") throw new Exception("You supplied a model attribute, and a schema argument. Can't have both.")
        sch
      }
      case None => {
        if (model == "") throw new Exception("No model was found.")
        val schemaNode = findModel(model)
        schemaNode
      }
    }
    val compiler = Compiler()
    compiler.setDistinguishedRootNode(root)
    val parser = compiler.compile(sch).onPath("/")
    val data = document.input
    val actual = parser.parse(data)
    val trimmed = Utility.trim(actual.result)
    if (!actual.canProceed()) {
      val diags = actual.getDiagnostics().map(_.getMessage()).foldLeft("")(_ + "\n" + _)
      throw new Exception(diags) // if you just assertTrue(actual.canProceed), and it fails, you get NOTHING useful.
      fail()
    }
    //
    // Attributes on the XML like xsi:type and also namespaces (I think) are 
    // making things fail these comparisons, so we strip all attributes off (since DFDL doesn't 
    // use attributes at all)
    // 
    val actualNoAttrs = XMLUtils.removeAttributes(trimmed)
    // 
    // Would be great to validate the actuals against the DFDL schema, used as
    // an XML schema on the returned infoset XML.
    // Getting this to work is a bigger issue. What with stripping of attributes
    // etc.
    // 
    // TODO: Fix so we can validate here.
    //
    // assert(Validator.validateXMLNodes(sch, actualNoAttrs) != null)
    val expected = infoset.contents

    if (expected != actualNoAttrs) {
      throw new Exception("Comparison failed. Expected: " + expected + " but got " + actualNoAttrs)
      fail()
    }
    // if we get here, the test passed. If we don't get here then some exception was
    // thrown either during the run of the test or during the comparison.
    System.err.println("Test " + id + " passed.")
  }
}

case class DefinedSchema(xml : Node, parent : DFDLTestSuite) {
  lazy val xsdSchema = (xml \ "schema")(0)
  lazy val name = (xml \ "@name").text.toString
}

sealed abstract class DocumentContentType
case object Text extends DocumentContentType
case object Byte extends DocumentContentType
// TODO: add a Bits type so one can do 0110 1101 0010 0000 and so forth.
// TODO: add capability to specify character set encoding into which text is to be converted (all UTF-8 currently)

case class Document(d : NodeSeq, parent : ParserTestCase) {
  lazy val realDocumentParts = (d \ "documentPart").map { node => new DocumentPart(node, this) }
  lazy val documentParts = realDocumentParts match {
    case Seq() => {
      val docPart = new DocumentPart(<documentPart type="text">{ d.text }</documentPart>, this)
      List(docPart)
    }
    case _ => realDocumentParts
  }
  lazy val documentBytes = documentParts.map { _.convertedContent }.flatten

  /**
   * this 'input' is the kind our parser's parse method expects.
   */
  lazy val input = {
    val bytes = documentBytes.toArray
    val inputStream = new java.io.ByteArrayInputStream(bytes);
    val rbc = java.nio.channels.Channels.newChannel(inputStream);
    rbc
  }

}

case class DocumentPart(part : Node, parent : Document) {
  lazy val partContentType = (part \ "@type").toString match {
    case "text" => Text
    case "byte" => Byte
  }
  lazy val partRawContent = part.child.text
  lazy val convertedContent : Seq[Byte] = partContentType match {
    case Text => partRawContent.getBytes
    case Byte => hexContentToBytes
  }

  lazy val hexContentToBytes = hex2Bytes(hexDigits)

  val validHexDigits = "0123456789abcdefABCDEF"

  // Note: anything that is not a valid hex digit is simply skipped
  // TODO: we should check for whitespace and other characters we want to allow, and verify them.
  // TODO: Or better, validate this in the XML Schema for tdml via a pattern facet
  // TODO: Consider whether to support a comment syntax. When showing data examples this may be useful.
  //
  lazy val hexDigits = partRawContent.flatMap { ch => if (validHexDigits.contains(ch)) List(ch) else Nil }

}

case class Infoset(i : NodeSeq, parent : ParserTestCase) {
  lazy val Seq(dfdlInfoset) = (i \ "dfdlInfoset").map { node => new DFDLInfoset(Utility.trim(node), this) }
  lazy val contents = dfdlInfoset.contents
}

case class DFDLInfoset(di : Node, parent : Infoset) {
  lazy val Seq(contents) = {
    val c = di.child(0)
    val expected = Utility.trim(c) // must be exactly one root element in here.
    val expectedNoAttrs = XMLUtils.removeAttributes(expected)
    //
    // Let's validate the expected content against the schema
    // Just to be sure they don't drift.
    //
    val ptc = parent.parent
    val schemaNode = ptc.findModel(ptc.model)
    //
    // This is causing trouble, with the stripped attributes, etc.
    // TODO: Fix so we can validate these expected results against
    // the DFDL schema used as a XSD for the expected infoset XML.
    //
    // assert(Validator.validateXMLNodes(schemaNode, expectedNoAttrs) != null)
    expectedNoAttrs
  }
}

