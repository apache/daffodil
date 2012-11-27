package daffodil.tdml

import java.io.File
import scala.Array.canBuildFrom
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml._
import scala.util.matching.Regex
import scala.util.matching.Regex.Match
import org.scalatest.junit.JUnitSuite
import daffodil.Implicits.using
import daffodil.compiler.Compiler
import daffodil.xml.XMLUtils
import daffodil.util._
import daffodil.api._
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
import daffodil.exceptions.Assert
import java.nio.ByteBuffer
import java.nio.charset.CharsetEncoder
import com.ibm.icu.charset.CharsetICU
import java.nio.CharBuffer
import java.io.InputStream
import daffodil.processors.GeneralParseFailure

/**
 * Parses and runs tests expressed in IBM's contributed tdml "Test Data Markup Language"
 */

//
// TODO: validate the infoset XML (expected result) against the DFDL Schema, that is using it as an XML Schema
// for the infoset. This would prevent errors where the infoset instance and the schema drift apart under maintenance.
//
// TODO: validate the actual result against the DFDL Schema using it as an XML Schema. 
//
/**
 * TDML test suite runner
 *
 * Keep this independent of Daffodil, so that it can be used to run tests against other DFDL implementations as well.
 * E.g., it should only need an API specified as a collection of Scala traits, and some simple way to inject
 * dependency on one factory to create processors.
 */

class DFDLTestSuite(ts: Node, tdmlFile: File, tsInputSource: InputSource)
  extends Logging {

  var checkAllTopLevel: Boolean = false
  def setCheckAllTopLevel(flag: Boolean) {
    checkAllTopLevel = flag
  }

  def this(tdmlFile: File) = this(XML.loadFile(tdmlFile), tdmlFile, new InputSource(tdmlFile.toURI().toASCIIString()))
  def this(tsNode: Node) = this(tsNode, null, new InputSource(new StringReader(tsNode.toString)))
  def this(tsURL: URL) = this(XML.load(tsURL), null, new InputSource(tsURL.toURI().toASCIIString()))

  //
  // we immediately validate the incoming test suite document
  // against its schema. We're depending on Validator to find all the 
  // included schemas such as that for embedded defineSchema named schema nodes.
  // 
  val tdmlXSDResourcePath = "/xsd/tdml.xsd"

  val tdmlSchemaResource = Misc.getRequiredResource(tdmlXSDResourcePath)

  lazy val isTDMLFileValid = {
    val validatedXML = Validator.validateXML(
      new StreamSource(tdmlSchemaResource.toURI().toASCIIString()),
      tsInputSource)
    val status = validatedXML != null
    status
  }

  val parserTestCases = (ts \ "parserTestCase").map { node => ParserTestCase(node, this) }
  //
  // Note: IBM started this TDML file format. They call an unparser test a "serializer" test.
  // We will use their TDML file names, but in the code here, we call it an UnparserTestCase
  //
  val unparserTestCases = (ts \ "serializerTestCase").map { node => UnparserTestCase(node, this) }
  val testCases: Seq[TestCase] = parserTestCases ++
    unparserTestCases
  val suiteName = (ts \ "@suiteName").text
  val suiteID = (ts \ "@ID").text
  val description = (ts \ "@description").text
  val embeddedSchemas = (ts \ "defineSchema").map { node => DefinedSchema(node, this) }

  private val embeddedSchemaGroups = embeddedSchemas.groupBy { _.name }

  embeddedSchemaGroups.foreach {
    case (name, Seq(sch)) => // ok
    case (name, seq) =>
      Assert.usageError("More than one definition for embedded schema " + name)
  }

  def runAllTests(schema: Option[Node] = None) {
    if (isTDMLFileValid)
      testCases.map { _.run(schema) }
    else {
      log(Error("TDML file %s is not valid.", tsInputSource.getSystemId))
    }
  }

  def runOneTest(testName: String, schema: Option[Node] = None) {
    if (isTDMLFileValid)
      runOneTestNoTDMLValidation(testName, schema)
    else {
      log(Error("TDML file %s is not valid.", tsInputSource.getSystemId))
    }
  }

  /**
   * Use to bypass validation of the TDML document itself.
   *
   * This is used for testing whether one can detect validation errors
   * in the DFDL schema.
   *
   * Without this, you can't get to the validation errors, because it
   * rejects the TDML file itself.
   */
  def runOneTestNoTDMLValidation(testName: String, schema: Option[Node] = None) {
    val testCase = testCases.find(_.name == testName)
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
  def findModelFile(fileName: String): File = {
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
        log(Debug("Model file name is: %s", modelFileName))
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

  def findModel(modelName: String): Node = {
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

abstract class TestCase(ptc: NodeSeq, val parent: DFDLTestSuite)
  extends Logging {

  def toOpt[T](n: Seq[T]) = {
    n match {
      case Seq() => None
      case Seq(a) => Some(a)
      // ok for it to error if there is more than one in sequence.
    }
  }

  val document = toOpt(ptc \ "document").map { node => new Document(node, this) }
  val infoset = toOpt(ptc \ "infoset").map { node => new Infoset(node, this) }
  val errors = toOpt(ptc \ "errors").map { node => new ExpectedErrors(node, this) }
  val warnings = toOpt(ptc \ "warnings").map { node => new ExpectedWarnings(node, this) }

  val name = (ptc \ "@name").text
  val ptcID = (ptc \ "@ID").text
  val id = name + (if (ptcID != "") "(" + ptcID + ")" else "")
  val root = (ptc \ "@root").text
  val model = (ptc \ "@model").text
  val description = (ptc \ "@description").text
  val unsupported = (ptc \ "@unsupported").text match {
    case "true" => true
    case "false" => false
    case _ => false
  }

  def findModel(modelName: String): Node = {
    if (modelName == "") {
      suppliedSchema match {
        case None => throw new Exception("No model.")
        case Some(s) => return s
      }
    } else
      parent.findModel(modelName)
  }

  var suppliedSchema: Option[Node] = None

  protected def runProcessor(processor: DFDL.ProcessorFactory,
                             data: Option[DFDL.Input],
                             nBits: Option[Long],
                             infoset: Option[Infoset],
                             errors: Option[ExpectedErrors],
                             warnings: Option[ExpectedWarnings]): Unit

  def run(schema: Option[Node] = None) {
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
    compiler.setCheckAllTopLevel(parent.checkAllTopLevel)
    val pf = compiler.compile(sch)
    val data = document.map { _.data }
    val nBits = document.map { _.nBits }

    runProcessor(pf, data, nBits, infoset, errors, warnings)
    // if we get here, the test passed. If we don't get here then some exception was
    // thrown either during the run of the test or during the comparison.
    // log(Debug("Test %s passed.", id))
  }

  def verifyAllDiagnosticsFound(actual: WithDiagnostics, expectedDiags: Option[ErrorWarningBase]) = {
    val actualDiags = actual.getDiagnostics
    if (actualDiags.length == 0) {
      throw new Exception("""No diagnostic objects found.""")
    } else {
      actualDiags.foreach { ad => log(Error(ad.toString)) }
    }
    val actualDiagMsgs = actualDiags.map { _.toString }
    val expectedDiagMsgs = expectedDiags.map { _.messages }.getOrElse(Nil)
    // must find each expected warning message within some actual warning message.
    expectedDiagMsgs.foreach {
      expected =>
        {
          val wasFound = actualDiagMsgs.exists {
            actual => actual.contains(expected)
          }
          if (!wasFound) {
            throw new Exception("""Did not find diagnostic message """" +
              expected + """" in any of the actual diagnostic messages: """ + "\n" +
              actualDiagMsgs.mkString("\n"))
          }
        }
    }
  }

}

case class ParserTestCase(ptc: NodeSeq, parentArg: DFDLTestSuite)
  extends TestCase(ptc, parentArg) {

  def runProcessor(pf: DFDL.ProcessorFactory,
                   data: Option[DFDL.Input],
                   lengthLimitInBits: Option[Long],
                   optInfoset: Option[Infoset],
                   optErrors: Option[ExpectedErrors],
                   warnings: Option[ExpectedWarnings]) = {

    val nBits = lengthLimitInBits.get
    val dataToParse = data.get
    (optInfoset, optErrors) match {
      case (Some(infoset), None) => runParseExpectSuccess(pf, dataToParse, nBits, infoset, warnings)
      case (None, Some(errors)) => runParseExpectErrors(pf, dataToParse, nBits, errors, warnings)
      case _ => throw new Exception("Invariant broken. Should be Some None, or None Some only.")
    }

  }

  def verifyParseInfoset(actual: DFDL.ParseResult, infoset: Infoset) {
    val trimmed = Utility.trim(actual.result)
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
    //

    // Something about the way XML is constructed is different between our jdom-converted 
    // results and the ones created by scala directly parsing the TDML test files.
    //
    // This has something to do with values being lists of text nodes and entities
    // and not just simple strings. I.e., if you write: <foo>a&#x5E74;</foo>, that's not
    // an element with a string as its value. It's an element with several text nodes as
    // its values.
    //
    // so we run the expected stuff through the same converters that were used to
    // convert the actual.
    val expected = XMLUtils.element2Elem(XMLUtils.elem2Element(infoset.contents))
    // infoset.contents already has attributes removed.

    if (expected != actualNoAttrs) {
      val diffs = XMLUtils.computeDiff(expected, actualNoAttrs)
      if (diffs.length > 0) {
        //throw new Exception("Comparison failed. Expected: " + expected + " but got " + actualNoAttrs)
        throw new Exception("""
Comparison failed.
Expected 
          %s
Actual 
          %s
Differences were (path, expected, actual):
 %s""".format(
          expected.toString, actualNoAttrs.toString, diffs.map { _.toString }.mkString("\n")))
      }
    }
  }

  def runParseExpectErrors(pf: DFDL.ProcessorFactory,
                           dataToParse: DFDL.Input,
                           lengthLimitInBits: Long,
                           errors: ExpectedErrors,
                           warnings: Option[ExpectedWarnings]) {

    val objectToDiagnose =
      if (pf.isError) pf
      else {
        val processor = pf.onPath("/")
        if (processor.isError) {
          val diags = processor.getDiagnostics.map(_.getMessage).mkString("\n")
          throw new Exception(diags)
        }
        val actual = processor.parse(dataToParse, lengthLimitInBits)

        val loc: DataLocation = actual.resultState.currentLocation

        if (actual.canProceed) {
          if (!loc.isAtEnd) {
            actual.addDiagnostic(new GeneralParseFailure("Left over data: " + loc.toString))
            actual
          } else {
            // We did not get an error!!
            // val diags = actual.getDiagnostics().map(_.getMessage()).foldLeft("")(_ + "\n" + _)
            throw new Exception("Expected error. Didn't get one. Actual result was " + actual.result) // if you just assertTrue(actual.canProceed), and it fails, you get NOTHING useful.
          }
        } else actual
      }

    // check for any test-specified errors
    verifyAllDiagnosticsFound(objectToDiagnose, Some(errors))

    // TODO Implement Warnings
    // check for any test-specified warnings
    // verifyAllDiagnosticsFound(objectToDiagnose, warnings)

  }

  def runParseExpectSuccess(pf: DFDL.ProcessorFactory,
                            dataToParse: DFDL.Input,
                            lengthLimitInBits: Long,
                            infoset: Infoset,
                            warnings: Option[ExpectedWarnings]) {

    if (pf.isError) {
      val diags = pf.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(diags)
    } else {
      val processor = pf.onPath("/")
      if (processor.isError) {
        val diags = processor.getDiagnostics.map(_.getMessage).mkString("\n")
        throw new Exception(diags)
      }
      val actual = processor.parse(dataToParse, lengthLimitInBits)

      if (!actual.canProceed) {
        // Means there was an error, not just warnings.
        val diags = actual.getDiagnostics.map(_.getMessage).mkString("\n")
        throw new Exception(diags) // if you just assertTrue(objectToDiagnose.canProceed), and it fails, you get NOTHING useful.
      }

      val loc: DataLocation = actual.resultState.currentLocation
      val leftOverException = if (!loc.isAtEnd) {
        val leftOverMsg = "Left over data: " + loc.toString
        println(leftOverMsg)
        Some(new Exception(leftOverMsg))
      } else None

      verifyParseInfoset(actual, infoset)

      leftOverException.map { throw _ } // if we get here, throw the left over data exception.

      // TODO: Implement Warnings
      // check for any test-specified warnings
      // verifyAllDiagnosticsFound(actual, warnings)

      // if we get here, the test passed. If we don't get here then some exception was
      // thrown either during the run of the test or during the comparison.
    }
  }
}

case class UnparserTestCase(ptc: NodeSeq, parentArg: DFDLTestSuite)
  extends TestCase(ptc, parentArg) {

  def runProcessor(pf: DFDL.ProcessorFactory,
                   optData: Option[DFDL.Input],
                   optNBits: Option[Long],
                   optInfoset: Option[Infoset],
                   optErrors: Option[ExpectedErrors],
                   warnings: Option[ExpectedWarnings]) = {

    val infoset = optInfoset.get

    (optData, optErrors) match {
      case (Some(data), None) => runUnparserExpectSuccess(pf, data, infoset, warnings)
      case (_, Some(errors)) => runUnparserExpectErrors(pf, optData, infoset, errors, warnings)
      case _ => throw new Exception("Invariant broken. Should be Some None, or None Some only.")
    }

  }

  def verifyData(data: DFDL.Input, outStream: java.io.ByteArrayOutputStream) {
    val actualBytes = outStream.toByteArray

    val inbuf = java.nio.ByteBuffer.allocate(1024 * 1024) // TODO: allow override? Detect overrun?
    val readCount = data.read(inbuf)
    data.close()
    if (readCount == -1) {
      // example data was of size 0 (could not read anything). We're not supposed to get any actual data.
      if (actualBytes.length > 0) {
        throw new Exception("Unexpected data was created.")
      }
      return // we're done. Nothing equals nothing.
    }

    Assert.invariant(readCount == inbuf.position())

    // compare expected data to what was output.
    val expectedBytes = inbuf.array().toList.slice(0, readCount)
    if (actualBytes.length != readCount) {
      throw new Exception("output data length " + actualBytes.length + " for " + actualBytes.toList +
        " doesn't match expected value " + readCount + " for " + expectedBytes)
    }

    val pairs = expectedBytes zip actualBytes zip Stream.from(1)
    pairs.foreach {
      case ((expected, actual), index) =>
        if (expected != actual) {
          val msg = "Unparsed data differs at byte %d. Expected 0x%02x. Actual was 0x%02x.".format(index, expected, actual)
          throw new Exception(msg)
        }
    }
  }

  def runUnparserExpectSuccess(pf: DFDL.ProcessorFactory,
                               data: DFDL.Input,
                               infoset: Infoset,
                               warnings: Option[ExpectedWarnings]) {

    val outStream = new java.io.ByteArrayOutputStream()
    val output = java.nio.channels.Channels.newChannel(outStream)
    val node = infoset.contents
    if (pf.isError) {
      val diags = pf.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(diags)
    }
    val processor = pf.onPath("/")
    if (processor.isError) {
      val diags = processor.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(diags)
    }
    val actual = processor.unparse(output, node)
    output.close()

    verifyData(data, outStream)

    // TODO: Implement Warnings - check for any test-specified warnings
    // verifyAllDiagnosticsFound(actual, warnings)

  }

  def runUnparserExpectErrors(pf: DFDL.ProcessorFactory,
                              optData: Option[DFDL.Input],
                              infoset: Infoset,
                              errors: ExpectedErrors,
                              warnings: Option[ExpectedWarnings]) {

    val outStream = new java.io.ByteArrayOutputStream()
    val output = java.nio.channels.Channels.newChannel(outStream)
    val node = infoset.contents
    if (pf.isError) {
      // check for any test-specified errors
      verifyAllDiagnosticsFound(pf, Some(errors))

      // check for any test-specified warnings
      verifyAllDiagnosticsFound(pf, warnings)
    }
    val processor = pf.onPath("/")
    if (processor.isError) {
      val diags = processor.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(diags)
    }
    val actual = processor.unparse(output, node)
    output.close()
    val actualBytes = outStream.toByteArray()

    // Verify that some partial output has shown up in the bytes.
    optData.map { data => verifyData(data, outStream) }

    // check for any test-specified errors
    verifyAllDiagnosticsFound(actual, Some(errors))

    // check for any test-specified warnings
    verifyAllDiagnosticsFound(actual, warnings)

  }

}

case class DefinedSchema(xml: Node, parent: DFDLTestSuite) {
  val name = (xml \ "@name").text.toString

  val defineFormats = (xml \ "defineFormat")
  val defaultFormats = (xml \ "format")
  val defineVariables = (xml \ "defineVariable")
  val defineEscapeSchemes = (xml \ "defineEscapeScheme")

  val globalElementDecls = (xml \ "element")
  val globalSimpleTypeDefs = (xml \ "simpleType")
  val globalComplexTypeDefs = (xml \ "complexType")
  val globalGroupDefs = (xml \ "group")

  val dfdlTopLevels = defineFormats ++ defaultFormats ++ defineVariables ++ defineEscapeSchemes
  val xsdTopLevels = globalElementDecls ++ globalSimpleTypeDefs ++
    globalComplexTypeDefs ++ globalGroupDefs
  val xsdSchema = TestUtils.dfdlTestSchema(dfdlTopLevels, xsdTopLevels)
}

sealed abstract class DocumentContentType
case object Text extends DocumentContentType
case object Byte extends DocumentContentType
case object Bits extends DocumentContentType
// TODO: add capability to specify character set encoding into which text is to be converted (all UTF-8 currently)

case class Document(d: NodeSeq, parent: TestCase) {

  val Seq(<document>{ children @ _* }</document>) = d

  val actualDocumentPartElementChildren = children.toList.flatMap {
    child =>
      child match {
        case <documentPart>{ _* }</documentPart> => List(new DocumentPart(child, this))
        case _ => Nil
      }
  }

  // check that document element either contains text content directly with no other documentPart children, 
  // or it contains ONLY documentPart children (and whitespace around them).
  //
  if (actualDocumentPartElementChildren.length > 0) {
    children.foreach { child =>
      child match {
        case <documentPart>{ _* }</documentPart> => // ok
        case scala.xml.Text(s) if (s.matches("""\s+""")) => // whitespace text nodes ok
        case x => Assert.usageError("Illegal TDML data document content '" + x + "'")
      }
    }
  }

  val documentParts =
    if (actualDocumentPartElementChildren.length > 0) actualDocumentPartElementChildren
    else List(new DocumentPart(<documentPart type="text">{ children }</documentPart>, this))

  /**
   * Due to alignment, and bits-granularity issues, everything is lowered into
   * bits first, and then concatenated, and then converted back into bytes
   */
  val documentBits = documentParts.map { _.contentAsBits }.mkString

  val nBits: Long = documentBits.length
  val nFragBits = (nBits % 8).toInt
  val nAddOnBits = if (nFragBits == 0) 0 else 8 - nFragBits
  val addOnBits = (1 to nAddOnBits) collect { case _ => "0" } mkString
  val documentBitsFullBytes = documentBits + addOnBits

  val documentBytes = bits2Bytes(documentBitsFullBytes)

  /**
   * this 'data' is the kind our parser's parse method expects.
   */
  lazy val data = {
    val bytes = documentBytes.toArray
    val inputStream = new java.io.ByteArrayInputStream(bytes);
    val rbc = java.nio.channels.Channels.newChannel(inputStream);
    rbc.asInstanceOf[DFDL.Input]
  }

}

case class DocumentPart(part: Node, parent: Document) {
  val validHexDigits = "0123456789abcdefABCDEF"
  val validBinaryDigits = "01"

  lazy val partContentType = (part \ "@type").toString match {
    case "text" => Text
    case "byte" => Byte
    case "bits" => Bits
    case _ => Assert.invariantFailed("invalid content type.")
  }
  lazy val encoder = CharsetICU.forNameICU("UTF-8").newEncoder()
  lazy val partRawContent = part.child.text

  lazy val contentAsBits = {
    val res = partContentType match {
      case Text => textContentAsBits
      case Byte => hexContentAsBits
      case Bits => bitDigits
    }
    res
  }

  lazy val textContentToBytes = {
    // replace DFDL character entities
    val regex = new Regex("%(%|([^;]*;))")
    val replacedRawContent = regex.replaceAllIn(partRawContent,
      m => convertDFDLCharEntity(m.group(0)) match {
        case "$" => "\\$" // the dollar sign charater means smething special
        // in Java's Matcher::AppendReplacement (which is used by
        // regex.replaceAllIn), so we need to escape it
        case converted => converted
      })

    val bytes = replacedRawContent.getBytes("UTF-8") //must specify charset name (JIRA DFDL-257)
    bytes
  }

  lazy val textContentAsBits = bytes2Bits(textContentToBytes)

  /**
   * Convert a character entity to its unicode equivalent. Returns the empty
   * string if not a valid DFDL entity
   */
  def convertDFDLCharEntity(s: String): String = {
    val HexCodePoint = "%#x([0-9a-fA-F]+);".r
    val DecCodePoint = "%#([0-9]+);".r
    val converted = s match {
      case "%%" => "%" //special case, percent escapes percent
      case HexCodePoint(hex) => {
        try {
          val i = Integer.parseInt(hex, 16)
          new String(Character.toChars(i))
        } catch {
          case e => ""
        }
      }
      case DecCodePoint(dec) => {
        try {
          val i = Integer.parseInt(dec, 10)
          new String(Character.toChars(i))
        } catch {
          case e => ""
        }
      }
      case "%NUL;" => "\u0000"
      case "%SOH;" => "\u0001"
      case "%STX;" => "\u0002"
      case "%ETX;" => "\u0003"
      case "%EOT;" => "\u0004"
      case "%ENQ;" => "\u0005"
      case "%ACK;" => "\u0006"
      case "%BEL;" => "\u0007"
      case "%BS;" => "\u0008"
      case "%HT;" => "\u0009"
      case "%LF;" => "\u000A"
      case "%VT;" => "\u000B"
      case "%FF;" => "\u000C"
      case "%CR;" => "\u000D"
      case "%SO;" => "\u000E"
      case "%SI;" => "\u000F"
      case "%DLE;" => "\u0010"
      case "%DC1;" => "\u0011"
      case "%DC2;" => "\u0012"
      case "%DC3;" => "\u0013"
      case "%DC4;" => "\u0014"
      case "%NAK;" => "\u0015"
      case "%SYN;" => "\u0016"
      case "%ETB;" => "\u0017"
      case "%CAN;" => "\u0018"
      case "%EM;" => "\u0019"
      case "%SUB;" => "\u001A"
      case "%ESC;" => "\u001B"
      case "%FS;" => "\u001C"
      case "%GS;" => "\u001D"
      case "%RS;" => "\u001E"
      case "%US;" => "\u001F"
      case "%SP;" => "\u0020"
      case "%DEL;" => "\u007F"
      case "%NBSP;" => "\u00A0"
      case "%NEL;" => "\u0085"
      case "%LS;" => "\u2028"
      case _ => ""
    }
    converted
  }

  lazy val hexContentAsBits = hex2Bits(hexDigits)

  // Note: anything that is not a valid hex digit (or binary digit for binary) is simply skipped
  // TODO: we should check for whitespace and other characters we want to allow, and verify them.
  // TODO: Or better, validate this in the XML Schema for tdml via a pattern facet
  // TODO: Consider whether to support a comment syntax. When showing data examples this may be useful.
  //
  lazy val hexDigits = partRawContent.flatMap { ch => if (validHexDigits.contains(ch)) List(ch) else Nil }

  lazy val bitContentToBytes = bits2Bytes(bitDigits).toList

  lazy val bitDigits = partRawContent.flatMap {
    ch =>
      {
        if (validBinaryDigits.contains(ch))
          List(ch)
        else Nil
      }
  }
}

case class Infoset(i: NodeSeq, parent: TestCase) {
  lazy val Seq(dfdlInfoset) = (i \ "dfdlInfoset").map { node => new DFDLInfoset(Utility.trim(node), this) }
  lazy val contents = dfdlInfoset.contents
}

case class DFDLInfoset(di: Node, parent: Infoset) {
  lazy val Seq(contents) = {
    val c = di.child(0)
    val expected = Utility.trim(c) // must be exactly one root element in here.
    val expectedNoAttrs = XMLUtils.removeAttributes(expected)
    //
    // Let's validate the expected content against the schema
    // Just to be sure they don't drift.
    //
    //    val ptc = parent.parent
    //    val schemaNode = ptc.findModel(ptc.model)
    //
    // This is causing trouble, with the stripped attributes, etc.
    // TODO: Fix so we can validate these expected results against
    // the DFDL schema used as a XSD for the expected infoset XML.
    //
    // assert(Validator.validateXMLNodes(schemaNode, expectedNoAttrs) != null)
    expectedNoAttrs
  }
}

abstract class ErrorWarningBase(n: NodeSeq, parent: TestCase) {
  lazy val matchAttrib = (n \ "@match").text
  protected def diagnosticNodes: Seq[Node]
  lazy val messages = diagnosticNodes.map { _.text }
}

case class ExpectedErrors(node: NodeSeq, parent: TestCase)
  extends ErrorWarningBase(node, parent) {

  val diagnosticNodes = node \\ "error"

}

case class ExpectedWarnings(node: NodeSeq, parent: TestCase)
  extends ErrorWarningBase(node, parent) {

  val diagnosticNodes = node \\ "warning"

}

