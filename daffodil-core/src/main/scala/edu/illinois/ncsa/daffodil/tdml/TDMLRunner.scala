package edu.illinois.ncsa.daffodil.tdml

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import java.io.File
import scala.Array.canBuildFrom
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml._
import scala.util.matching.Regex
import scala.util.matching.Regex.Match
import org.scalatest.junit.JUnitSuite
import edu.illinois.ncsa.daffodil.Implicits.using
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.api._
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import junit.framework.Assert.fail
import edu.illinois.ncsa.daffodil.util.Misc._
import java.io.FileInputStream
import java.io.FileNotFoundException
import org.xml.sax.InputSource
import java.io.StringReader
import javax.xml.transform.stream.StreamSource
import java.net.URL
import java.net.URI
import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.nio.ByteBuffer
import java.nio.charset.CharsetEncoder
import com.ibm.icu.charset.CharsetICU
import java.nio.CharBuffer
import java.io.InputStream
import edu.illinois.ncsa.daffodil.processors.GeneralParseFailure
import edu.illinois.ncsa.daffodil.dsom.EntityReplacer
import edu.illinois.ncsa.daffodil.xml.DaffodilXMLLoader
import edu.illinois.ncsa.daffodil.processors.IteratorInputStream
import edu.illinois.ncsa.daffodil.processors.DFDLCharCounter

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
 *
 *
 * Use the validateTDMLFile arg to bypass validation of the TDML document itself.
 *
 * This is used for testing whether one can detect validation errors
 * in the DFDL schema.
 *
 * Without this, you can't get to the validation errors, because it
 * rejects the TDML file itself.
 */

class DFDLTestSuite(aNodeFileOrURL: Any, validateTDMLFile: Boolean = true)
  extends Logging {

  val errorHandler = new org.xml.sax.ErrorHandler {
    def warning(exception: SAXParseException) = {
      loadingExceptions == exception +: loadingExceptions
      System.err.println("TDMLRunner Warning: " + exception.getMessage())
    }

    def error(exception: SAXParseException) = {
      loadingExceptions = exception :: loadingExceptions
      System.err.println("TDMLRunner Error: " + exception.getMessage())
      isLoadingError = true
    }
    def fatalError(exception: SAXParseException) = {
      loadingExceptions == exception +: loadingExceptions
      System.err.println("TDMLRunner Fatal Error: " + exception.getMessage())
      isLoadingError = true
    }
  }

  var isLoadingError: Boolean = false

  var loadingExceptions: List[Exception] = Nil

  def getLoadingDiagnosticMessages() = {
    val msgs = loadingExceptions.map { _.toString() }.mkString(" ")
    msgs
  }

  /**
   * our loader here accumulates load-time errors here on the
   * test suite object.
   */
  val loader = new DaffodilXMLLoader(errorHandler)
  loader.setValidation(validateTDMLFile)

  val (ts, tdmlFile, tsInputSource) = {
    val tuple = aNodeFileOrURL match {
      case tsNode: Node => {
        val tempFileName = XMLUtils.convertNodeToTempFile(tsNode)
        val newNode = loader.loadFile(tempFileName)
        val tempFile = new File(tempFileName)
        (newNode, null, new InputSource(tempFile.toURI().toASCIIString()))
      }
      case tdmlFile: File => {
        log(Debug("loading TDML file: %s", tdmlFile))
        val res = (loader.loadFile(tdmlFile), tdmlFile, new InputSource(tdmlFile.toURI().toASCIIString()))
        log(Debug("done loading TDML file: %s", tdmlFile))
        res
      }
      case tsURL: URL => {
        val res = (loader.load(tsURL), null, new InputSource(tsURL.toURI().toASCIIString()))
        res
      }
      case _ => Assert.usageError("not a Node, File, or URL")
    }
    tuple
  }

  lazy val isTDMLFileValid = !this.isLoadingError

  var checkAllTopLevel: Boolean = false
  def setCheckAllTopLevel(flag: Boolean) {
    checkAllTopLevel = flag
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
    if (isTDMLFileValid) {
      val testCase = testCases.find(_.name == testName)
      testCase match {
        case None => throw new Exception("test " + testName + " was not found.")
        case Some(tc) => {
          tc.run(schema)
        }
      }
    } else {
      log(Error("TDML file %s is not valid.", tsInputSource.getSystemId))
      val msgs = this.loadingExceptions.map { _.toString }.mkString(" ")
      throw new Exception(msgs)
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
        val schema = {
          val res = (new DaffodilXMLLoader(errorHandler)).loadFile(file)
          res
        }
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
    compiler.setDistinguishedRootNode(root, null)
    compiler.setCheckAllTopLevel(parent.checkAllTopLevel)
    val pf = compiler.compile(sch)
    val data = document.map { _.data }
    val nBits = document.map { _.nBits }

    runProcessor(pf, data, nBits, infoset, errors, warnings)
    println("Bytes processed: " + IteratorInputStream.getAndResetCalls)
    println("Characters processed: " + DFDLCharCounter.getAndResetCount)
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
            actual => actual.toLowerCase.contains(expected.toLowerCase)
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
    // and that our internal Daffodil XML Catalog has a special treatment of the
    // mapping of the XML Schema URI.
    // etc.
    // 
    // TODO: Fix so we can validate here.
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
        if (processor.isError) processor
        else {
          val actual = processor.parse(dataToParse, lengthLimitInBits)
          if (actual.isError) actual
          else {
            val loc: DataLocation = actual.resultState.currentLocation
            if (!loc.isAtEnd) {
              actual.addDiagnostic(new GeneralParseFailure("Left over data: " + loc.toString))
              actual
            } else {
              // We did not get an error!!
              // val diags = actual.getDiagnostics().map(_.getMessage()).foldLeft("")(_ + "\n" + _)
              throw new Exception("Expected error. Didn't get one. Actual result was " + actual.briefResult) // if you just assertTrue(actual.canProceed), and it fails, you get NOTHING useful.
            }
          }
        }
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

    val isError = pf.isError
    val diags = pf.getDiagnostics.map(_.getMessage).mkString("\n")
    if (pf.isError) {
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
  val fileName = parent.ts.attribute(XMLUtils.INT_NS, XMLUtils.FILE_ATTRIBUTE_NAME) match {
    case Some(seqNodes) => seqNodes.toString
    case None => ""
  }
  val xsdSchema = TestUtils.dfdlTestSchema(dfdlTopLevels, xsdTopLevels, fileName)
}

sealed abstract class DocumentContentType
case object ContentTypeText extends DocumentContentType
case object ContentTypeByte extends DocumentContentType
case object ContentTypeBits extends DocumentContentType
case object ContentTypeFile extends DocumentContentType
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

  lazy val replaceDFDLEntities: Boolean = {
    val res = (part \ "@replaceDFDLEntities")
    if (res.length == 0) { true }
    else { res(0).toString().toBoolean }
  }
  lazy val partContentType = (part \ "@type").toString match {
    case "text" => ContentTypeText
    case "byte" => ContentTypeByte
    case "bits" => ContentTypeBits
    case "file" => ContentTypeFile
    case _ => Assert.invariantFailed("invalid content type.")
  }
  lazy val encoder = CharsetICU.forNameICU("UTF-8").newEncoder()
  lazy val partRawContent = part.child.text

  lazy val contentAsBits = {
    val res = partContentType match {
      case ContentTypeText => textContentAsBits
      case ContentTypeByte => hexContentAsBits
      case ContentTypeBits => bitDigits
      case ContentTypeFile => fileContentAsBits
    }
    res
  }

  lazy val textContentWithoutEntities = {
    if (replaceDFDLEntities) {
      EntityReplacer.replaceAll(partRawContent)
    } else partRawContent
  }

  lazy val textContentToBytes = {
    // Fails here if we use getBytes("UTF-8") because that uses the utf-8 encoder,
    // and that will fail on things like unpaired surrogate characters that we allow
    // in our data and our infoset.
    // So instead we must do our own UTF-8-like encoding of the data
    // so that we can put in codepoints we want. 
    val bytes = utf8LikeEncode(textContentWithoutEntities)
    // val bytes = replacedRawContent.getBytes("UTF-8") //must specify charset name (JIRA DFDL-257)
    bytes.toArray
  }

  def byteList(args: Int*) = args.map { _.toByte }

  def utf8LikeEncode(s: String): Seq[Byte] = {
    // 
    // Scala/Java strings represent characters above 0xFFFF as a surrogate pair
    // of two codepoints. 
    //
    // We want to handle both properly match surrogate pairs, and isolated surrogate characters.
    // That means if we see an isolated low (second) surrogate character, we have to know 
    // whether it was preceded by a high surrogate or not.
    // 
    // For every 16-bit code point, do do this right we need to potentially also see the previous
    // or next codepoint.
    //
    val bytes = XMLUtils.walkUnicodeString(s)(utf8LikeEncoding).flatten
    // val bytes = tuples.flatMap { case ((prevcp, cp), nextcp) => utf8LikeEncoding(prevcp, cp, nextcp) }
    bytes
  }

  /**
   * Encode in the style of utf-8 (see wikipedia article on utf-8)
   *
   * Variation is that we accept some things that a conventional utf-8 encoder
   * rejects. Examples are illegal codepoints such as isolated Unicode surrogates
   * (not making up a surrogate pair).
   *
   * We also assume we're being handed surrogate pairs for any of the
   * 4-byte character representations.
   *
   */

  def utf8LikeEncoding(prev: Char, c: Char, next: Char): Seq[Byte] = {
    // handles 16-bit codepoints only
    Assert.usage(prev <= 0xFFFF)
    Assert.usage(c <= 0xFFFF)
    Assert.usage(next <= 0xFFFF)

    val i = c.toInt
    val byte1 = ((i >> 8) & 0xFF)
    val byte2 = (i & 0xFF)

    def threeByteEncode() = {
      val low6 = byte2 & 0x3F
      val mid6 = ((byte1 & 0x0F) << 2) | (byte2 >> 6)
      val high4 = byte1 >> 4
      byteList(high4 | 0xE0, mid6 | 0x80, low6 | 0x80)
    }

    /**
     * create 4-byte utf-8 encoding from surrogate pair found
     * in a scala string.
     */
    def fourByteEncode(leadingSurrogate: Char, trailingSurrogate: Char) = {
      val h = leadingSurrogate.toInt // aka 'h for high surrogate'
      val l = trailingSurrogate.toInt // aka 'l for low surrogate'
      val cp = 0x10000 + ((h - 0xD800) * 0x400) + (l - 0xDC00)
      val byte1 = (cp >> 24) & 0xFF
      val byte2 = (cp >> 16) & 0xFF
      val byte3 = (cp >> 8) & 0xFF
      val byte4 = cp & 0xFF
      val low6 = byte4 & 0x3F
      val midlow6 = ((byte3 & 0x0F) << 2) | (byte4 >> 6)
      val midhig6 = ((byte2 & 0x03) << 4) | byte3 >> 4
      val high3 = byte2 >> 2
      byteList(high3 | 0xF0, midhig6 | 0x80, midlow6 | 0x80, low6 | 0x80)
    }

    val res = i match {
      case _ if (i <= 0x7F) => byteList(byte2)
      case _ if (i <= 0x7FF) => {
        val low6 = byte2 & 0x3F
        val high5 = ((byte1 & 0x07) << 2) | (byte2 >> 6)
        byteList(high5 | 0xC0, low6 | 0x80)
      }
      case _ if (XMLUtils.isLeadingSurrogate(c)) => {
        // High (initial) Surrogate character case.
        if (XMLUtils.isTrailingSurrogate(next)) {
          // Next codepoint is a low surrogate.
          // We need to create a 4-byte representation from the
          // two surrogate characters.
          fourByteEncode(c, next)
        } else {
          // isolated high surrogate codepoint case.
          threeByteEncode()
        }
      }
      case _ if (XMLUtils.isTrailingSurrogate(c)) => {
        // Low (subsequent) Surrogate character case.
        if (XMLUtils.isLeadingSurrogate(prev)) {
          // Previous codepoint was a high surrogate. 
          // This codepoint was handled as part of converting the
          // surrogate pair.
          // so we output no bytes at all.
          List()
        } else {
          // Isolated low-surrogate codepoint case.
          threeByteEncode()
        }

      }
      case _ if (i <= 0xFFFF) => {
        threeByteEncode()
      }

      case _ => Assert.invariantFailed("char code out of range.")
    }
    res
  }

  lazy val textContentAsBits = bytes2Bits(textContentToBytes)

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

  lazy val fileContentAsBits = {
    val file = new File(Misc.getRequiredResource(partRawContent).toURI)
    val fis = new FileInputStream(file)
    val fileBytes = Stream.continually(fis.read()).takeWhile(_ != -1).map(_.toByte).toArray
    bytes2Bits(fileBytes)
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

