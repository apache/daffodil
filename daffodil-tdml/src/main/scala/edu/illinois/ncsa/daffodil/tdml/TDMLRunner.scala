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
import edu.illinois.ncsa.daffodil.processors.IterableReadableByteChannel
import edu.illinois.ncsa.daffodil.Tak._
import java.net.URLDecoder
import scala.xml.parsing.ConstructingParser
import edu.illinois.ncsa.daffodil.Tak

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

  /**
   * Detects the encoding of the File for us.
   */
  def determineEncoding(theFile: File): String = {
    val encH = scala.xml.include.sax.EncodingHeuristics
    val is = new java.io.FileInputStream(theFile)
    val bis = new java.io.BufferedInputStream(is)
    val enc = encH.readEncodingFromStream(bis)
    enc
  }

  val (ts, tdmlFile, tsInputSource) = {

    /**
     * We have to use the ConstructingParser in order for Scala to preserve
     * CDATA elements in the dfdl:infoset of the TDML tests.  However,
     * ConstructingParser does not detect encoding and neither does scala.io.Source.
     * We have to use EncodingHeuristics to detect this encoding for us.
     *
     * The other thing to note here is that we still need to perform some sort
     * of validation against the TDML.  We do this by leaving the original
     * loader.loadFile(file) call here.
     */
    val tuple = aNodeFileOrURL match {
      case tsNode: Node => {
        val tempFile = XMLUtils.convertNodeToTempFile(tsNode)
        val enc = determineEncoding(tempFile)
        val input = scala.io.Source.fromURI(tempFile.toURI)(enc)
        val origNode = loader.loadFile(tempFile) // used for validation only
        val newNode = ConstructingParser.fromSource(input, true).document.docElem
        (newNode, null, new InputSource(tempFile.toURI().toASCIIString()))
      }
      case tdmlFile: File => {
        log(LogLevel.Debug, "loading TDML file: %s", tdmlFile)
        val enc = determineEncoding(tdmlFile)
        val input = scala.io.Source.fromURI(tdmlFile.toURI)(enc)
        val origNode = loader.loadFile(tdmlFile) // used for validation only
        val someNode = ConstructingParser.fromSource(input, true).document.docElem
        val res = (someNode, tdmlFile, new InputSource(tdmlFile.toURI().toASCIIString()))
        log(LogLevel.Debug, "done loading TDML file: %s", tdmlFile)
        res
      }
      case tsURI: URI => {
        val f = new File(tsURI)
        val enc = determineEncoding(f)
        val input = scala.io.Source.fromURI(tsURI)(enc)
        val origNode = loader.load(tsURI) // used for validation only
        val someNode = ConstructingParser.fromSource(input, true).document.docElem
        val res = (someNode, null, new InputSource(tsURI.toASCIIString()))
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

  def runPerfTest(testName: String, schema: Option[Node] = None) {
    var bytesProcessed: Long = 0
    var charsProcessed: Long = 0
    Tak.calibrate
    val ns = Timer.getTimeNS(testName, {
      val (by, ch) = runOneTestWithDataVolumes(testName, schema)
      bytesProcessed = by
      charsProcessed = ch
    })
    val takeonsThisRun = ns / Tak.takeons
    val bpns = ((bytesProcessed * 1.0) / ns)
    val kbps = bpns * 1000000
    val callsPerByte = 1 / (Tak.takeons * bpns)
    println("\nKB/sec = " + kbps)
    println("tak call equivalents per byte (takeons/byte) =  " + callsPerByte)
  }

  def runOneTest(testName: String, schema: Option[Node] = None) {
    runOneTestWithDataVolumes(testName, schema)
  }

  def runOneTestWithDataVolumes(testName: String, schema: Option[Node] = None): (Long, Long) = {
    if (isTDMLFileValid) {
      val testCase = testCases.find(_.name == testName)
      testCase match {
        case None => throw new Exception("test " + testName + " was not found.")
        case Some(tc) => {
          return tc.run(schema)
        }
      }
    } else {
      log(Error("TDML file %s is not valid.", tsInputSource.getSystemId))
      val msgs = this.loadingExceptions.map { _.toString }.mkString(" ")
      throw new Exception(msgs)
    }
  }

  /**
   * Try a few possibilities to find the model/schema/tdml resources
   *
   * IBM's suites have funny model paths in them. We don't have that file structure,
   * so we look for the schema/model/tdml resources in the working directory, and in the same
   * directory as the tdml file, and some other variations.
   */
  def findTDMLResource(fileName: String): Option[File] = {
    // try it as is. Maybe it will be in the cwd or relative to that or absolute
    val firstTry = new File(fileName)
    if (firstTry.exists()) return Some(firstTry)
    // see if it can be found relative to the tdml test file, like next to it.
    val sysId = tsInputSource.getSystemId()
    if (sysId != null) {
      val sysFile = new File(URI.create(sysId))
      if (sysFile.exists()) {
        // the system Id of the tdml file was a file.
        val sysPath = sysFile.getParent()
        val resourceFileName = sysPath + File.separator + fileName
        log(LogLevel.Debug, "TDML resource name is: %s", resourceFileName)
        val resourceFile = new File(resourceFileName)
        if (resourceFile.exists()) return Some(resourceFile)
      }
    }
    // try as a classpath resource (allows eclipse to find it)
    // ( also will look in the jar - which is bad. Have to avoid that.)
    val (maybeRes, _) = Misc.getResourceOption(fileName)
    maybeRes.foreach { uri =>
      // could be that we found the file, could be that we
      // got a match in the jar. This should tell them apart.
      if (uri.toURL().getProtocol == "file") {
        val resolvedName = uri.getPath()
        val resFile = new File(resolvedName)
        if (resFile.exists()) return Some(resFile)
      }
    }
    // try ignoring the directory part
    val parts = fileName.split("/")
    if (parts.length > 1) { // if there is one
      val filePart = parts.last
      val secondTry = findTDMLResource(filePart) // recursively
      return secondTry
    }
    None
  }

  def findEmbeddedSchema(modelName: String): Option[Node] = {
    // schemas defined with defineSchema take priority as names.
    val es = embeddedSchemas.find { defSch => defSch.name == modelName }
    es match {
      case Some(defschema) => Some(defschema.xsdSchema)
      case None => None
    }
  }

  def findSchemaFileName(modelName: String) = findTDMLResource(modelName)

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

  var suppliedSchema: Option[Node] = None

  protected def runProcessor(processor: DFDL.ProcessorFactory,
    data: Option[DFDL.Input],
    nBits: Option[Long],
    infoset: Option[Infoset],
    errors: Option[ExpectedErrors],
    warnings: Option[ExpectedWarnings]): Unit

  def run(schema: Option[Node] = None): (Long, Long) = {
    suppliedSchema = schema
    val sch = schema match {
      case Some(node) => {
        if (model != "") throw new Exception("You supplied a model attribute, and a schema argument. Can't have both.")
        node
      }
      case None => {
        if (model == "") throw new Exception("No model was specified.") // validation of the TDML should prevent writing this.
        val schemaNode = parent.findEmbeddedSchema(model)
        val schemaFileName = parent.findSchemaFileName(model)
        val schemaNodeOrFileName = (schemaNode, schemaFileName) match {
          case (None, None) => throw new Exception("Model '" + model + "' was not passed, found embedded in the TDML file, nor as a schema file.")
          case (Some(_), Some(_)) => throw new Exception("Model '" + model + "' is ambiguous. There is an embedded model with that name, AND a file with that name.")
          case (Some(node), None) => node
          case (None, Some(fn)) => fn
        }
        schemaNodeOrFileName
      }
    }
    val compiler = Compiler()
    compiler.setDistinguishedRootNode(root, null)
    compiler.setCheckAllTopLevel(parent.checkAllTopLevel)
    val pf = sch match {
      case node: Node => compiler.compile(node)
      case theFile: File => compiler.compile(theFile)
      case _ => Assert.invariantFailed("can only be Node or File") //Assert.invariantFailed("can only be Node or String")
    }
    val data = document.map { _.data }
    val nBits = document.map { _.nBits }

    runProcessor(pf, data, nBits, infoset, errors, warnings)
    val bytesProcessed = IterableReadableByteChannel.getAndResetCalls
    val charsProcessed = DFDLCharCounter.getAndResetCount
    println("Bytes processed: " + bytesProcessed)
    println("Characters processed: " + charsProcessed)
    (bytesProcessed, charsProcessed)
    // if we get here, the test passed. If we don't get here then some exception was
    // thrown either during the run of the test or during the comparison.
    // log(LogLevel.Debug, "Test %s passed.", id))
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
    val expected = XMLUtils.element2ElemTDML(XMLUtils.elem2ElementTDML(infoset.contents)) //val expected = XMLUtils.element2Elem(XMLUtils.elem2Element(infoset.contents))
    // infoset.contents already has attributes removed.
    val trimmedExpected = Utility.trim(expected)

    if (trimmedExpected != actualNoAttrs) {
      val diffs = XMLUtils.computeDiff(trimmedExpected, actualNoAttrs)
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
          trimmedExpected.toString, actualNoAttrs.toString, diffs.map { _.toString }.mkString("\n")))
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
  val globalIncludes = (xml \ "include")
  val globalImports = (xml \ "import")

  val dfdlTopLevels = defineFormats ++ defaultFormats ++ defineVariables ++ defineEscapeSchemes
  val xsdTopLevels = globalImports ++ globalIncludes ++ globalElementDecls ++ globalSimpleTypeDefs ++
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
        case scala.xml.Comment(_) => // ok
        case x => Assert.usageError("Illegal TDML data document content '" + x + "'")
      }
    }
  }

  val documentParts =
    if (actualDocumentPartElementChildren.length > 0) actualDocumentPartElementChildren
    else List(new DocumentPart(<documentPart type="text">{ children }</documentPart>, this))

  /**
   * When data is coming from the TDML file as small test data, then
   * Due to alignment, and bits-granularity issues, everything is lowered into
   * bits first, and then concatenated, and then converted back into bytes
   *
   * These are all lazy val, since if data is coming from a file these aren't
   * needed at all.
   */
  lazy val documentBits = documentParts.map { _.contentAsBits }.mkString
  lazy val nBits: Long =
    if (isDPFile) -1
    else documentBits.length
  lazy val nFragBits = (nBits % 8).toInt
  lazy val nAddOnBits = if (nFragBits == 0) 0 else 8 - nFragBits
  lazy val addOnBits = (1 to nAddOnBits).collect { case _ => "0" }.mkString
  lazy val documentBitsFullBytes = documentBits + addOnBits
  lazy val documentBytes = {
    Assert.usage(!isDPFile, "Cannot call documentBytes if documentPart type is file.")
    bits2Bytes(documentBitsFullBytes)
  }

  /**
   * data coming from a file?
   */
  val isDPFile = {
    val res = documentParts.length > 0 &&
      documentParts(0).partContentType == ContentTypeFile
    if (res) {
      Assert.usage(documentParts.length == 1, "There can be only one documentPart of type file, and it must be the only documentPart.")
    }
    res
  }

  /**
   * this 'data' is the kind our parser's parse method expects.
   */
  lazy val data = {
    if (isDPFile) {
      // direct I/O to the file. No 'bits' lowering involved. 
      val dp = documentParts(0)
      val input = dp.fileDataInput
      input
    } else {
      // assemble the input from the various pieces, having lowered
      // everything to bits.
      val bytes = documentBytes.toArray
      val inputStream = new java.io.ByteArrayInputStream(bytes);
      val rbc = java.nio.channels.Channels.newChannel(inputStream);
      rbc.asInstanceOf[DFDL.Input]
    }
  }

}

case class DocumentPart(part: Node, parent: Document) {
  val validHexDigits = "0123456789abcdefABCDEF"
  val validBinaryDigits = "01"

  lazy val replaceDFDLEntities: Boolean = {
    val res = (part \ "@replaceDFDLEntities")
    if (res.length == 0) { false }
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
      case ContentTypeFile =>
        Assert.invariantFailed("shouldn't do contentAsBits for file documentPart type")
    }
    res
  }

  lazy val textContentWithoutEntities = {
    if (replaceDFDLEntities) {
      try { EntityReplacer.replaceAll(partRawContent) }
      catch { case (e: Exception) => Assert.abort(e.getMessage()) }
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

  lazy val fileDataInput = {
    val maybeFile = parent.parent.parent.findTDMLResource(partRawContent)
    val file = maybeFile.getOrElse(throw new FileNotFoundException("TDMLRunner: data file '" + partRawContent + "' was not found"))
    val fis = new FileInputStream(file)
    val rbc = fis.getChannel()
    rbc.asInstanceOf[DFDL.Input]
  }
}

case class Infoset(i: NodeSeq, parent: TestCase) {
  lazy val Seq(dfdlInfoset) = (i \ "dfdlInfoset").map { node => new DFDLInfoset(Utility.trim(node), this) }
  lazy val contents = dfdlInfoset.contents
}

case class DFDLInfoset(di: Node, parent: Infoset) {
  lazy val Seq(contents) = {
    Assert.usage(di.child.size == 1, "dfdlInfoset element must contain a single root element")

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
