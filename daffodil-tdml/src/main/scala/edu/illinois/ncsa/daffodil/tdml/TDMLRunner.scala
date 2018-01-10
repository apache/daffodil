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

package edu.illinois.ncsa.daffodil.tdml

import java.io.File
import java.io.FileNotFoundException
import java.io.StringWriter
import java.io.StringReader
import java.net.URI
import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.SAXParseException
import edu.illinois.ncsa.daffodil.Tak
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.api.WithDiagnostics
import edu.illinois.ncsa.daffodil.api.DaffodilSchemaSource
import edu.illinois.ncsa.daffodil.api.UnitTestSchemaSource
import edu.illinois.ncsa.daffodil.api.URISchemaSource
import edu.illinois.ncsa.daffodil.api.EmbeddedSchemaSource
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.configuration.ConfigurationLoader
import edu.illinois.ncsa.daffodil.dsom.ValidationError
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.externalvars.Binding
import edu.illinois.ncsa.daffodil.externalvars.ExternalVariablesLoader
import edu.illinois.ncsa.daffodil.processors.parsers.GeneralParseFailure
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.Misc.bits2Bytes
import edu.illinois.ncsa.daffodil.util.Misc.hex2Bits
import edu.illinois.ncsa.daffodil.util.SchemaUtils
import edu.illinois.ncsa.daffodil.util.Timer
import edu.illinois.ncsa.daffodil.xml.DaffodilXMLLoader
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.processors.charset.CharsetUtils
import edu.illinois.ncsa.daffodil.processors.DataProcessor
import edu.illinois.ncsa.daffodil.debugger._
import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.channels.Channels
import java.nio.charset.CoderResult
import java.io.ByteArrayInputStream
import edu.illinois.ncsa.daffodil.io.NonByteSizeCharsetEncoder
import scala.language.postfixOps
import java.nio.file.Paths
import java.nio.file.Files
import edu.illinois.ncsa.daffodil.equality._
import org.apache.commons.io.IOUtils
import edu.illinois.ncsa.daffodil.processors.HasSetDebugger
import edu.illinois.ncsa.daffodil.processors.UnparseResult
import edu.illinois.ncsa.daffodil.cookers.EntityReplacer
import edu.illinois.ncsa.daffodil.configuration.ConfigurationLoader
import edu.illinois.ncsa.daffodil.dsom.ExpressionCompilers
import edu.illinois.ncsa.daffodil.io.NonByteSizeCharset
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.infoset._
import edu.illinois.ncsa.daffodil.util.MaybeBoolean
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.api.DaffodilTunables

/**
 * Parses and runs tests expressed in IBM's contributed tdml "Test Data Markup Language"
 */

private[tdml] object DFDLTestSuite {
  type CompileResult = Either[Seq[Diagnostic], (Seq[Diagnostic], DFDL.DataProcessor)]
}
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
 *
 * defaultRoundTripDefault if true the round trip default for the test suite will be
 * taken from this value if it is not specified on the testSuite itself.
 */

class DFDLTestSuite private[tdml] (
  val __nl: Null, // this extra arg allows us to make this primary constructor package private so we can deprecate the one generally used.
  aNodeFileOrURL: Any,
  validateTDMLFile: Boolean,
  val validateDFDLSchemas: Boolean,
  val compileAllTopLevel: Boolean,
  val defaultRoundTripDefault: Boolean,
  val defaultValidationDefault: String)
  extends Logging with HasSetDebugger {

  // Uncomment to force conversion of all test suites to use Runner(...) instead.
  // That avoids creating the test suites repeatedly, but also leaks memory unless
  // you have an @AfterClass shutdown method in the object that calls runner.reset() at end.
  //
  // @deprecated("2016-12-30", "Use Runner(...) instead.")
  def this(aNodeFileOrURL: Any,
    validateTDMLFile: Boolean = true,
    validateDFDLSchemas: Boolean = true,
    compileAllTopLevel: Boolean = false,
    defaultRoundTripDefault: Boolean = Runner.defaultRoundTripDefaultDefault,
    defaultValidationDefault: String = Runner.defaultValidationDefaultDefault) =
    this(null, aNodeFileOrURL, validateTDMLFile, validateDFDLSchemas, compileAllTopLevel, defaultRoundTripDefault, defaultValidationDefault)

  if (!aNodeFileOrURL.isInstanceOf[scala.xml.Node])
    System.err.println("Creating DFDL Test Suite for " + aNodeFileOrURL)
  val TMP_DIR = System.getProperty("java.io.tmpdir", ".")

  aNodeFileOrURL match {
    case _: URI => // ok
    case _: File => // ok
    case _: scala.xml.Node => // ok
    case x => Assert.usageError("argument was not a scala.xmlNode, File, or URI: " + x)
  }

  val errorHandler = new org.xml.sax.ErrorHandler {
    def warning(exception: SAXParseException) = {
      loadingExceptions == exception +: loadingExceptions
      // System.err.println("TDMLRunner Warning: " + exception.getMessage())
    }

    def error(exception: SAXParseException) = {
      loadingExceptions = exception :: loadingExceptions
      // System.err.println("TDMLRunner Error: " + exception.getMessage())
      isLoadingError = true
    }
    def fatalError(exception: SAXParseException) = {
      loadingExceptions == exception +: loadingExceptions
      // System.err.println("TDMLRunner Fatal Error: " + exception.getMessage())
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

  val (ts, tsURI) = aNodeFileOrURL match {
    case tsNode: Node => {
      //
      // We were passed a literal schema node. This is for unit testing
      // purposes.
      //
      val tmpDir = new File(TMP_DIR, "daffodil")
      tmpDir.mkdirs()

      val src = new UnitTestSchemaSource(tsNode, "", Some(tmpDir))
      loader.load(src)
      //
      (tsNode, src.uriForLoading)
    }
    case tdmlFile: File => {
      log(LogLevel.Info, "loading TDML file: %s", tdmlFile)
      val uri = tdmlFile.toURI()
      val newNode = loader.load(new URISchemaSource(uri))
      val res = (newNode, uri)
      log(LogLevel.Debug, "done loading TDML file: %s", tdmlFile)
      res
    }
    case uri: URI => {
      val newNode = loader.load(new URISchemaSource(uri))
      val res = (newNode, uri)
      res
    }
    case _ => Assert.usageError("not a Node, File, or URL")
  } // end match

  lazy val isTDMLFileValid = !this.isLoadingError

  var checkAllTopLevel: Boolean = compileAllTopLevel
  def setCheckAllTopLevel(flag: Boolean) {
    checkAllTopLevel = flag
  }

  val parserTestCases = (ts \ "parserTestCase").map { node => ParserTestCase(node, this) }
  //
  // Note: IBM started this TDML file format. They call an unparser test a "serializer" test.
  // We call it an UnparserTestCase
  //
  val unparserTestCases = (ts \ "unparserTestCase").map { node => UnparserTestCase(node, this) }
  val testCases: Seq[TestCase] = parserTestCases ++
    unparserTestCases
  val duplicateTestCases = testCases.groupBy { _.name }.filter { case (name, seq) => seq.length > 1 }
  if (duplicateTestCases.size > 0) {
    duplicateTestCases.foreach {
      case (name, _) =>
        System.err.println("TDML Runner: More than one test case for name '%s'.".format(name))
    }
  }
  val testCaseMap = testCases.map { tc => (tc.name -> tc) }.toMap
  val suiteName = (ts \ "@suiteName").text
  val suiteID = (ts \ "@ID").text
  val description = (ts \ "@description").text
  val defaultRoundTrip = {
    val str = (ts \ "@defaultRoundTrip").text
    if (str == "") defaultRoundTripDefault else str.toBoolean
  }
  val defaultValidation = {
    val str = (ts \ "@defaultValidation").text
    if (str == "") defaultValidationDefault else str
  }
  val defaultConfig = {
    val str = (ts \ "@defaultConfig").text
    str
  }
  val embeddedSchemasRaw = (ts \ "defineSchema").map { node => DefinedSchema(node, this) }
  val embeddedConfigs = (ts \ "defineConfig").map { node => DefinedConfig(node, this) }

  val embeddedSchemas = {
    val embeddedSchemaGroups = embeddedSchemasRaw.groupBy { _.name }
    embeddedSchemaGroups.foreach {
      case (name, Seq(sch)) => // ok
      case (name, seq) =>
        // TDML XML schema has uniqueness check for this. Hence, this is just an Assert here
        // since it means that the validation of the TDML file didn't catch the duplicate name.
        Assert.usageError("More than one definition for embedded schema " + name)
    }
    embeddedSchemasRaw
  }

  def runAllTests(schema: Option[Node] = None) {
    if (isTDMLFileValid)
      testCases.map { _.run(schema) }
    else {
      log(LogLevel.Error, "TDML file %s is not valid.", tsURI)
    }
  }

  def runPerfTest(testName: String, schema: Option[Node] = None) {
    var bytesProcessed: Long = 0
    Tak.calibrate
    val ns = Timer.getTimeNS(testName, {
      val nBytes = runOneTestWithDataVolumes(testName, schema)
      bytesProcessed = nBytes
    })
    // val takeonsThisRun = ns / Tak.takeons
    val bpns = ((bytesProcessed * 1.0) / ns)
    val kbps = bpns * 1000000
    val callsPerByte = 1 / (Tak.takeons * bpns)
    println("\nKB/sec = " + kbps)
    println("tak call equivalents per byte (takeons/byte) =  " + callsPerByte)
  }

  private lazy val builtInTracer = new InteractiveDebugger(new TraceDebuggerRunner, ExpressionCompilers)

  final var optDebugger: Option[Debugger] = None

  def trace = {
    setDebugging(true)
    this // allows chaining like runner.trace.runOneTest(....)
  }

  def setDebugging(b: Boolean) {
    if (b == true)
      if (optDebugger.isEmpty) optDebugger = Some(builtInTracer)
      else
        optDebugger = None
  }

  def setDebugger(d: Debugger) {
    optDebugger = Some(d)
  }

  def runOneTest(testName: String, schema: Option[Node] = None, leakCheck: Boolean = false) {
    if (leakCheck) {
      System.gc()
      Thread.sleep(1) // needed to give tools like jvisualvm ability to "grab on" quickly
    }
    runOneTestWithDataVolumes(testName, schema)
  }

  /**
   * Returns number of bytes processed.
   */
  def runOneTestWithDataVolumes(testName: String, schema: Option[Node] = None): Long = {
    if (isTDMLFileValid) {
      val testCase = testCases.find(_.name == testName) // Map.get(testName)
      testCase match {
        case None => throw new TDMLException("test " + testName + " was not found.")
        case Some(tc) => {
          return tc.run(schema)
        }
      }
    } else {
      log(LogLevel.Error, "TDML file %s is not valid.", tsURI)
      val msgs = this.loadingExceptions.map { _.toString }.mkString("\n")
      throw new TDMLException(msgs)
    }
  }

  /**
   * Try a few possibilities to find the model/schema/tdml resources
   *
   * IBM's suites have funny model paths in them. We don't have that file structure,
   * so we look for the schema/model/tdml resources in the working directory, and in the same
   * directory as the tdml file, and some other variations.
   */
  def findTDMLResource(resName: String): Option[URI] = {
    val resPath = Paths.get(resName)
    val resolvedURI =
      if (Files.exists(resPath)) Some(resPath.toFile().toURI())
      else Misc.getResourceRelativeOption(resName, Some(tsURI))
    val res = resolvedURI.orElse {
      // try ignoring the directory part
      val parts = resName.split("/")
      if (parts.length > 1) { // if there is one
        val filePart = parts.last
        val secondTry = findTDMLResource(filePart) // recursively
        secondTry
      } else {
        None
      }
    }
    res
  }

  def findEmbeddedSchema(modelName: String): Option[DefinedSchema] = {
    // schemas defined with defineSchema take priority as names.
    val es = embeddedSchemas.find { defSch => defSch.name == modelName }
    es
  }

  def findSchemaFileName(modelName: String) = findTDMLResource(modelName)

  def findEmbeddedConfig(configName: String): Option[DefinedConfig] = {
    val ecfg = embeddedConfigs.find { defCfg => defCfg.name == configName }
    ecfg match {
      case Some(defConfig) => Some(defConfig)
      case None => None
    }
  }

  def findConfigFileName(configName: String) = findTDMLResource(configName)

}

abstract class TestCase(testCaseXML: NodeSeq, val parent: DFDLTestSuite)
  extends Logging {

  lazy val defaultRoundTrip: Boolean = parent.defaultRoundTrip
  lazy val defaultValidation: String = parent.defaultValidation

  /**
   * This doesn't fetch a serialized processor, it runs whatever the processor is
   * through a serialize then unserialize path to get a processor as if
   * it were being fetched from a file.
   */
  private def generateProcessor(pf: DFDL.ProcessorFactory, useSerializedProcessor: Boolean): DFDLTestSuite.CompileResult = {
    val p = pf.onPath("/")
    val diags = p.getDiagnostics
    if (p.isError) Left(diags)
    else {
      val dp =
        if (useSerializedProcessor) {
          val os = new java.io.ByteArrayOutputStream()
          val output = Channels.newChannel(os)
          p.save(output)

          val is = new java.io.ByteArrayInputStream(os.toByteArray)
          val input = Channels.newChannel(is)
          val compiler_ = Compiler()
          compiler_.reload(input)
        } else p

      Right((diags, dp))
    }
  }

  private def compileProcessor(schemaSource: DaffodilSchemaSource, useSerializedProcessor: Boolean): DFDLTestSuite.CompileResult = {
    val pf = compiler.compileSource(schemaSource)
    val diags = pf.getDiagnostics
    if (pf.isError) {
      Left(diags) // throw new TDMLException(diags)
    } else {
      val res = this.generateProcessor(pf, useSerializedProcessor)
      res
    }
  }

  final protected def getProcessor(schemaSource: DaffodilSchemaSource, useSerializedProcessor: Boolean): DFDLTestSuite.CompileResult = {
    val res: DFDLTestSuite.CompileResult = schemaSource match {
      case uss: URISchemaSource if parent.checkAllTopLevel =#= true =>
        SchemaDataProcessorCache.compileAndCache(uss, useSerializedProcessor) {
          compileProcessor(uss, useSerializedProcessor)
        }
      case _ => {
        compileProcessor(schemaSource, useSerializedProcessor)
      }
    }
    res
  }

  lazy val document = (testCaseXML \ "document").headOption.map { node => new Document(node, this) }
  lazy val optExpectedOrInputInfoset = (testCaseXML \ "infoset").headOption.map { node => new Infoset(node, this) }
  lazy val errors = (testCaseXML \ "errors").headOption.map { node => new ExpectedErrors(node, this) }
  lazy val warnings = (testCaseXML \ "warnings").headOption.map { node => new ExpectedWarnings(node, this) }
  lazy val validationErrors = (testCaseXML \ "validationErrors").headOption.map { node => new ExpectedValidationErrors(node, this) }

  val name = (testCaseXML \ "@name").text
  lazy val tcID = (testCaseXML \ "@ID").text
  lazy val id = name + (if (tcID != "") "(" + tcID + ")" else "")
  lazy val root = (testCaseXML \ "@root").text
  lazy val model = (testCaseXML \ "@model").text
  lazy val config = (testCaseXML \ "@config").text
  lazy val tcRoundTrip = (testCaseXML \ "@roundTrip").text
  lazy val roundTrip: Boolean =
    if (tcRoundTrip == "") defaultRoundTrip else tcRoundTrip.toBoolean
  lazy val description = (testCaseXML \ "@description").text
  lazy val unsupported = (testCaseXML \ "@unsupported").text match {
    case "true" => true
    case "false" => false
    case _ => false
  }
  lazy val validationMode: ValidationMode.Type = (testCaseXML \ "@validation").text match {
    case "on" => ValidationMode.Full
    case "limited" => ValidationMode.Limited
    case "off" => ValidationMode.Off
    case "" => defaultValidation match {
      case "on" => ValidationMode.Full
      case "limited" => ValidationMode.Limited
      case "off" => ValidationMode.Off
      case other => Assert.invariantFailed("unrecognized default validation enum string: " + other)
    }
    case other => Assert.invariantFailed("unrecognized validation enum string: " + other)
  }
  lazy val shouldValidate = validationMode != ValidationMode.Off
  lazy val expectsValidationError = if (validationErrors.isDefined) validationErrors.get.hasDiagnostics else false

  protected def runProcessor(schemaSource: DaffodilSchemaSource,
    expectedData: Option[DFDL.Input],
    nBits: Option[Long],
    errors: Option[ExpectedErrors],
    warnings: Option[ExpectedWarnings],
    validationErrors: Option[ExpectedValidationErrors],
    validationMode: ValidationMode.Type,
    roundTrip: Boolean,
    tracer: Option[Debugger]): Unit

  private def retrieveBindings(cfg: DefinedConfig, tunable: DaffodilTunables): Seq[Binding] = {
    val bindings: Seq[Binding] = cfg.externalVariableBindings match {
      case None => Seq.empty
      case Some(bindingsNode) => ExternalVariablesLoader.getVariables(bindingsNode, tunable)
    }
    bindings
  }

  private def retrieveTunables(cfg: DefinedConfig): Map[String, String] = {
    val configFileTunables: Map[String, String] = cfg.tunables match {
      case None => Map.empty
      case Some(tunableNode) => {
        tunableNode.child.map { n => (n.label, n.text) }.toMap
      }
    }

    configFileTunables
  }

  // Provide ability to override existing (default) tunables
  private def retrieveTunablesCombined(existingTunables: Map[String, String], cfg: DefinedConfig) = {
    val configFileTunables: Map[String, String] = retrieveTunables(cfg)

    // Note, ++ on Maps replaces any key/value pair from the left with that on the
    // right, so key/value pairs defined in tunables overrule those defiend in
    // the config file
    val combined = existingTunables ++ configFileTunables
    combined
  }

  def getSuppliedSchema(schemaArg: Option[Node]): DaffodilSchemaSource = {
    val embeddedSchema = parent.findEmbeddedSchema(model)
    val schemaURI = parent.findSchemaFileName(model)
    val suppliedSchema = (schemaArg, embeddedSchema, schemaURI) match {
      case (None, None, None) => throw new TDMLException("Model '" + model + "' was not passed, found embedded in the TDML file, nor as a schema file.")
      case (None, Some(_), Some(_)) => throw new TDMLException("Model '" + model + "' is ambiguous. There is an embedded model with that name, AND a file with that name.")
      case (Some(node), _, _) => {
        // unit test case. There is no URI/file location
        if (model != "") throw new TDMLException("You supplied a model attribute, and a schema argument. Can't have both.")
        // note that in this case, since a node was passed in, this node has no file/line/col information on it
        // so error messages will end up being about some temp file.
        UnitTestSchemaSource(node, name)
      }
      case (None, Some(defSchema), None) => {
        Assert.invariant(model != "") // validation of the TDML should prevent this
        EmbeddedSchemaSource(defSchema.xsdSchema, defSchema.name)
      }
      case (None, None, Some(uri)) => {
        //
        // In this case, we have a real TDML file (or resource) to open
        URISchemaSource(uri)
      }
    } // end match
    suppliedSchema
  }

  protected val compiler = Compiler(parent.validateDFDLSchemas)

  private def configFromName(cfgName: String, attrName: String): Option[DefinedConfig] = {
    Assert.usage(cfgName != "")
    Assert.usage(attrName != "")
    val cfgNode = parent.findEmbeddedConfig(cfgName)
    val cfgFileName = parent.findConfigFileName(cfgName)
    val optDefinedConfig: Option[DefinedConfig] = (cfgName, cfgNode, cfgFileName) match {
      case ("", None, None) => None
      case (name, Some(x), None) if name != "" => Some(x)
      case (name, None, Some(uri)) if name != "" => {
        // Read file, convert to definedConfig
        val node = ConfigurationLoader.getConfiguration(parent.loader, uri)
        val definedConfig = DefinedConfig(node, parent)
        Some(definedConfig)
      }
      case (name, None, None) if name != "" =>
        throw new TDMLException("The " + attrName + " '" + cfgName + "' was not found either as a embedded config, nor as a file.")
      case (name, Some(_), Some(_)) if name != "" =>
        throw new TDMLException("The " + attrName + " '" + cfgName + "' is ambiguous. There is an embedded config with that name, AND a file with that name.")
    }
    optDefinedConfig
  }

  /**
   * Returns number of bytes processed.
   */
  def run(schemaArg: Option[Node] = None): Long = {
    val suppliedSchema = getSuppliedSchema(schemaArg)

    val cfg: Option[DefinedConfig] = {
      (config, parent.defaultConfig) match {
        case ("", "") => None
        case (configName, "") => configFromName(configName, "config")
        case ("", defaultConfigName) => configFromName(defaultConfigName, "defaultConfig")
        case (configName, defaultConfigName) => {
          // check defaultConfigName for errors, but we'll use the configName
          configFromName(defaultConfigName, "defaultConfig")
          configFromName(configName, "config")
        }
      }
    }

    val defaultTunables: Map[String, String] = cfg match {
      case None => Map.empty
      case Some(definedConfig) => retrieveTunables(definedConfig)
    }

    val tunables: Map[String, String] = cfg match {
      case None => defaultTunables
      case Some(embeddedConfig) => retrieveTunablesCombined(defaultTunables, embeddedConfig)
    }

    val tunableObj = DaffodilTunables(tunables)

    compiler.setTunables(tunables)

    val externalVarBindings: Seq[Binding] = cfg match {
      case None => Seq.empty
      case Some(definedConfig) => retrieveBindings(definedConfig, tunableObj)
    }

    compiler.setDistinguishedRootNode(root, null)
    compiler.setCheckAllTopLevel(parent.checkAllTopLevel)
    compiler.setExternalDFDLVariables(externalVarBindings)

    val optInputOrExpectedData = document.map { _.data }
    val nBits: Option[Long] = document.map { _.nBits }

    runProcessor(suppliedSchema, optInputOrExpectedData, nBits, errors, warnings, validationErrors, validationMode,
      roundTrip, parent.optDebugger)

    // return number of bytes processed.
    nBits.getOrElse(0L) / 8
  }

  def setupDebugOrTrace(processor: DFDL.DataProcessor) {
    parent.optDebugger.foreach { d =>
      val p = processor.asInstanceOf[DataProcessor]
      p.setDebugger(d)
      p.setDebugging(true)
    }
  }
}

case class ParserTestCase(ptc: NodeSeq, parentArg: DFDLTestSuite)
  extends TestCase(ptc, parentArg) {

  lazy val optExpectedInfoset = this.optExpectedOrInputInfoset

  override def runProcessor(schemaSource: DaffodilSchemaSource,
    optDataToParse: Option[DFDL.Input],
    optLengthLimitInBits: Option[Long],
    optErrors: Option[ExpectedErrors],
    optWarnings: Option[ExpectedWarnings],
    optValidationErrors: Option[ExpectedValidationErrors],
    validationMode: ValidationMode.Type,
    roundTrip: Boolean,
    tracer: Option[Debugger]) = {

    val useSerializedProcessor =
      if (validationMode == ValidationMode.Full) false
      else if (optWarnings.isDefined) false
      else true
    val nBits = optLengthLimitInBits.get
    val dataToParse = optDataToParse.get

    val processor = getProcessor(schemaSource, useSerializedProcessor)
    processor.right.foreach {
      case (warnings, proc) =>
        //
        // Print out the warnings
        // (JIRA DFDL-1583 is implementation of expected warnings checking.)
        //
        warnings.foreach { System.err.println(_) }

        setupDebugOrTrace(proc)
    }

    (optExpectedInfoset, optErrors) match {
      case (Some(infoset), None) => {
        processor.left.foreach { diags => throw new TDMLException(diags) }
        processor.right.foreach {
          case (warnings, processor) =>
            runParseExpectSuccess(processor, dataToParse, nBits, optWarnings, optValidationErrors, validationMode, roundTrip)
        }
      }

      case (None, Some(errors)) => {
        processor.left.foreach { diags =>
          VerifyTestCase.verifyAllDiagnosticsFound(diags, Some(errors))
        }
        processor.right.foreach {
          case (warnings, processor) =>
            runParseExpectErrors(processor, dataToParse, nBits, errors, optWarnings, optValidationErrors, validationMode)
        }
      }

      case _ => Assert.invariantFailed("Should be Some None, or None Some only.")
    }
  }

  def runParseExpectErrors(processor: DFDL.DataProcessor,
    dataToParse: DFDL.Input,
    lengthLimitInBits: Long,
    errors: ExpectedErrors,
    optWarnings: Option[ExpectedWarnings],
    optValidationErrors: Option[ExpectedValidationErrors],
    validationMode: ValidationMode.Type) {
    lazy val sw = new StringWriter()
    val (diagnostics, isError: Boolean) = {
      if (processor.isError) (processor.getDiagnostics, true)
      else {

        val out = new XMLTextInfosetOutputter(sw)
        val actual = processor.parse(dataToParse, out, lengthLimitInBits)
        val isErr: Boolean =
          if (actual.isError) true
          else {
            //
            // didn't get an error.
            // If we're not at the end of data, synthesize an error for left-over-data
            //
            val loc: DataLocation = actual.resultState.currentLocation

            if (!loc.isAtEnd) {
              val leftOverMsg = "Left over data. Consumed %s bit(s) with %s bit(s) remaining.".format(loc.bitPos1b - 1, lengthLimitInBits - (loc.bitPos1b - 1))
              actual.addDiagnostic(new GeneralParseFailure(leftOverMsg))
              true
            } else {
              false
            }
          }

        val diagnostics = processor.getDiagnostics ++ actual.getDiagnostics
        (diagnostics, isErr)
      }
    }
    // check for any test-specified warnings
    VerifyTestCase.verifyAllDiagnosticsFound(diagnostics, optWarnings)
    if (isError) {
      // good we expected an error
      // check for any test-specified errors
      VerifyTestCase.verifyAllDiagnosticsFound(diagnostics, Some(errors))
    } else {
      throw new TDMLException("Expected error. Didn't get one. Actual result was\n" + sw.toString)
    }
  }

  def runParseExpectSuccess(processor: DFDL.DataProcessor,
    dataToParse: DFDL.Input,
    lengthLimitInBits: Long,
    warnings: Option[ExpectedWarnings],
    validationErrors: Option[ExpectedValidationErrors],
    validationMode: ValidationMode.Type,
    roundTripArg: Boolean) {

    val roundTrip = roundTripArg // change to true to force all parse tests to round trip (to see which fail to round trip)

    if (processor.isError) {
      val diagObjs = processor.getDiagnostics
      if (diagObjs.length == 1) throw diagObjs(0)
      throw new TDMLException(diagObjs)
    }
    processor.setValidationMode(validationMode)

    var testPass = 1
    var stillTesting = true
    var testData = IOUtils.toByteArray(Channels.newInputStream(dataToParse))
    var testDataLength = lengthLimitInBits
    val testInfoset = optExpectedInfoset.get

    while (stillTesting) {
      val outputter = new TDMLInfosetOutputter()
      val actual = processor.parse(Channels.newChannel(new ByteArrayInputStream(testData)), outputter, testDataLength)

      if (actual.isProcessingError) {
        // Means there was an error, not just warnings.
        val diagObjs = actual.getDiagnostics
        if (diagObjs.length == 1) throw new TDMLException(diagObjs(0))
        val diags = actual.getDiagnostics.map(_.getMessage()).mkString("\n")
        throw new TDMLException(diags)
      }

      val loc: DataLocation = actual.resultState.currentLocation

      val leftOverException = if (!loc.isAtEnd) {
        val leftOverMsg = "Left over data. Consumed %s bit(s) with %s bit(s) remaining.".format(loc.bitPos1b - 1, lengthLimitInBits - (loc.bitPos1b - 1))
        Some(new TDMLException(leftOverMsg))
      } else None

      val resultXmlNode = outputter.getResult
      VerifyTestCase.verifyParserTestData(resultXmlNode, testInfoset)

      (shouldValidate, expectsValidationError) match {
        case (true, true) => {
          VerifyTestCase.verifyAllDiagnosticsFound(actual.getDiagnostics, validationErrors) // verify all validation errors were found
          Assert.invariant(actual.isValidationError)
        }
        case (true, false) => {
          VerifyTestCase.verifyNoValidationErrorsFound(actual) // Verify no validation errors from parser
          Assert.invariant(!actual.isValidationError)
        }
        case (false, true) => throw new TDMLException("Test case invalid. Validation is off but the test expects an error.")
        case (false, false) => // Nothing to do here.
      }

      leftOverException.map { throw _ } // if we get here, throw the left over data exception.

      val allDiags = processor.getDiagnostics ++ actual.getDiagnostics
      VerifyTestCase.verifyAllDiagnosticsFound(allDiags, warnings)

      // if we get here, the test passed. If we don't get here then some exception was
      // thrown either during the run of the test or during the comparison.

      if (roundTrip && testPass < 2) {
        val outStream = new java.io.ByteArrayOutputStream()
        val output = java.nio.channels.Channels.newChannel(outStream)

        val inputter = outputter.toInfosetInputter()
        val unparseResult = processor.unparse(inputter, output).asInstanceOf[UnparseResult]
        if (unparseResult.isProcessingError) {
          val diagObjs = processor.getDiagnostics ++ unparseResult.resultState.diagnostics
          if (diagObjs.length == 1) throw diagObjs(0)
          throw new TDMLException(diagObjs)
        }
        output.close()

        try {
          VerifyTestCase.verifyUnparserTestData(Channels.newChannel(new ByteArrayInputStream(testData)), outStream)
          //
          // if verification passes, we're done
          stillTesting = false
        } catch {
          case e: TDMLException => {
            // if verification threw (failed)
            // consider this:
            //    infoset1 = parse(data1)
            //    data2 = unparse(infoset1)
            // if comparing expected output to data2 failed, then we want to try again:
            //    infoset2 = parse(data2)
            //    data3 = unparse(infoset2)
            // if comparing data2 to data 3 fails, then the test fails
            //
            // As an example of why this is needed.
            // If the input data is 64 bits all 1s, parsing that as a double produces a NaN as
            // do many bit patterns. Unparsing this produces the cannonical NaN (which is *not* all 1s)
            // However, parsing this cannnonical NaN bit pattern produces a NaN again, and unparsing
            // it again produces the same cannoical NaN bits. So this second comparison will succeed.
            //
            // Any time a parse+unparse is a many to 1 mapping, a parser test case isn't necessarily
            // going to invert into an unparser test.
            //
            if (testPass == 1) {
              // Try again
              testData = outStream.toByteArray
              testDataLength = unparseResult.resultState.bitPos0b
              val fullBytesNeeded = (testDataLength + 7) / 8
              if (testData.length != fullBytesNeeded) {
                throw new TDMLException("Unparse result data was was %d bytes, but the result length (%d bits) requires %d bytes.".format(testData.length, testDataLength, fullBytesNeeded))
              }
            } else
              throw e
          }
        }

        testPass += 1
      } else stillTesting = false
    }
  }
}

case class UnparserTestCase(ptc: NodeSeq, parentArg: DFDLTestSuite)
  extends TestCase(ptc, parentArg) {

  lazy val inputInfoset = this.optExpectedOrInputInfoset.get

  def runProcessor(schemaSource: DaffodilSchemaSource,
    optExpectedData: Option[DFDL.Input],
    optNBits: Option[Long],
    optErrors: Option[ExpectedErrors],
    optWarnings: Option[ExpectedWarnings],
    optValidationErrors: Option[ExpectedValidationErrors],
    validationMode: ValidationMode.Type,
    roundTrip: Boolean,
    tracer: Option[Debugger]) = {

    val useSerializedProcessor = if (validationMode == ValidationMode.Full) false else true
    val processor = getProcessor(schemaSource, useSerializedProcessor)
    processor.right.foreach {
      case (warnings, proc) =>
        setupDebugOrTrace(proc)
    }

    (optExpectedData, optErrors) match {
      case (Some(expectedData), None) => {
        processor.left.foreach { diags => throw new TDMLException(diags) }
        processor.right.foreach {
          case (warnings, processor) =>
            runUnparserExpectSuccess(processor, expectedData, optWarnings, roundTrip)
        }
      }

      case (_, Some(errors)) => {
        processor.left.foreach { diags =>
          VerifyTestCase.verifyAllDiagnosticsFound(diags, Some(errors))
          // check warnings even if there are errors expected.
          VerifyTestCase.verifyAllDiagnosticsFound(diags, optWarnings)
        }
        processor.right.foreach {
          case (warnings, processor) =>
            runUnparserExpectErrors(processor, optExpectedData, errors, optWarnings)
        }
      }
      case _ => Assert.impossibleCase()
    }

  }

  def runUnparserExpectSuccess(processor: DFDL.DataProcessor,
    expectedData: DFDL.Input,
    optWarnings: Option[ExpectedWarnings],
    roundTrip: Boolean) {

    val outStream = new java.io.ByteArrayOutputStream()
    val output = java.nio.channels.Channels.newChannel(outStream)
    val infosetXML = inputInfoset.dfdlInfoset.rawContents

    val inputter = new ScalaXMLInfosetInputter(infosetXML)
    val actual = processor.unparse(inputter, output).asInstanceOf[UnparseResult]
    output.close()
    if (actual.isError)
      throw new TDMLException(actual.getDiagnostics)

    //
    // Test that we are getting the number of full bytes needed.
    val testData = outStream.toByteArray
    val testDataLength = actual.resultState.dataOutputStream.maybeAbsBitPos0b.get
    val fullBytesNeeded = (testDataLength + 7) / 8
    if (testData.length != fullBytesNeeded) {
      throw new TDMLException("Unparse result data was was %d bytes, but the result length (%d bits) requires %d bytes.".format(testData.length, testDataLength, fullBytesNeeded))
    }

    if (actual.isScannable) {
      // all textual in one encoding, so we can do display of results
      // in terms of text so the user can see what is going on.
      VerifyTestCase.verifyTextData(expectedData, outStream, actual.encodingName)
    } else {
      // data is not all textual, or in mixture of encodings
      // So while we can still use the encoding as a heuristic,
      // we will need to treat as Hex bytes as well.
      VerifyTestCase.verifyBinaryOrMixedData(expectedData, outStream)
    }
    val allDiags = actual.getDiagnostics ++ processor.getDiagnostics
    VerifyTestCase.verifyAllDiagnosticsFound(allDiags, warnings)

    if (roundTrip) {
      val out = new ScalaXMLInfosetOutputter()
      val parseActual = processor.parse(Channels.newChannel(new ByteArrayInputStream(outStream.toByteArray)), out, testDataLength)

      if (parseActual.isProcessingError) {
        // Means there was an error, not just warnings.
        val diagObjs = parseActual.getDiagnostics
        if (diagObjs.length == 1) throw diagObjs(0)
        val diags = parseActual.getDiagnostics.map(_.getMessage()).mkString("\n")
        throw new TDMLException(diags)
      }
      val loc: DataLocation = parseActual.resultState.currentLocation

      val leftOverException = if (!loc.isAtEnd) {
        val leftOverMsg = "Left over data. Consumed %s bit(s) with %s bit(s) remaining.".format(loc.bitPos1b - 1, testDataLength - (loc.bitPos1b - 1))
        println(leftOverMsg)
        Some(new TDMLException(leftOverMsg))
      } else None

      val xmlNode = out.getResult
      VerifyTestCase.verifyParserTestData(xmlNode, inputInfoset)
      VerifyTestCase.verifyAllDiagnosticsFound(actual.getDiagnostics, warnings)

      (shouldValidate, expectsValidationError) match {
        case (true, true) => {
          VerifyTestCase.verifyAllDiagnosticsFound(actual.getDiagnostics, validationErrors) // verify all validation errors were found
          Assert.invariant(actual.isValidationError)
        }
        case (true, false) => {
          VerifyTestCase.verifyNoValidationErrorsFound(actual) // Verify no validation errors from parser
          Assert.invariant(!actual.isValidationError)
        }
        case (false, true) => throw new TDMLException("Test case invalid. Validation is off but the test expects an error.")
        case (false, false) => // Nothing to do here.
      }

      leftOverException.map { throw _ } // if we get here, throw the left over data exception.
    }
  }

  def runUnparserExpectErrors(processor: DFDL.DataProcessor,
    optExpectedData: Option[DFDL.Input],
    errors: ExpectedErrors,
    optWarnings: Option[ExpectedWarnings]) {

    val diagnostics = {
      if (processor.isError) processor.getDiagnostics
      else {
        val outStream = new java.io.ByteArrayOutputStream()
        val output = java.nio.channels.Channels.newChannel(outStream)
        val infosetXML = inputInfoset.dfdlInfoset.rawContents
        val inputter = new ScalaXMLInfosetInputter(infosetXML)
        val actual = processor.unparse(inputter, output)
        output.close()

        // Verify that some partial output has shown up in the bytes.
        val dataErrors = {
          optExpectedData.flatMap { data =>
            try {
              if (actual.isScannable) {
                // all textual in one encoding, so we can do display of results
                // in terms of text so the user can see what is going on.
                VerifyTestCase.verifyTextData(data, outStream, actual.encodingName)
              } else {
                // data is not all textual, or in mixture of encodings
                // So while we can still use the encoding as a heuristic,
                // we will need to treat as Hex bytes as well.
                VerifyTestCase.verifyBinaryOrMixedData(data, outStream)
              }
              None
            } catch {
              //
              // verifyData throws TDMLExceptions if the data doesn't match
              // In order for negative tests to look for these errors
              // we need to capture them and treat like regular diagnostics.
              //
              case x: TDMLException =>
                Some(x)
            }
          }
        }
        processor.getDiagnostics ++ actual.getDiagnostics ++ dataErrors
      }
    }

    // check for any test-specified errors or warnings
    VerifyTestCase.verifyAllDiagnosticsFound(diagnostics, Some(errors))
    VerifyTestCase.verifyAllDiagnosticsFound(diagnostics, optWarnings)
  }
}

object VerifyTestCase {
  def verifyParserTestData(actual: Node, infoset: Infoset) {
    //
    // Attributes on the XML like xsi:type and also namespaces (I think) are
    // making things fail these comparisons, so we strip all attributes off (since DFDL doesn't
    // use attributes at all)
    //
    val actualNoAttrs = XMLUtils.removeAttributes(actual)
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

    // Something about the way XML is constructed is different between our infoset
    // results and the ones created by scala directly parsing the TDML test files.
    //
    // This has something to do with values being lists of text nodes and entities
    // and not just simple strings. I.e., if you write: <foo>a&#x5E74;</foo>, that's not
    // an element with a string as its value. It's an element with several text nodes as
    // its values.
    //
    // so we run the expected stuff through the same converters that were used to
    // convert the actual.
    // val expected = XMLUtils.element2ElemTDML(XMLUtils.elem2ElementTDML(infoset.contents)) //val expected = XMLUtils.element2Elem(XMLUtils.elem2Element(infoset.contents))
    val expected = infoset.contents
    // infoset.contents already has attributes removed.
    // however, we call removeAttributes anyway because of the way it collapses
    // multiple strings within a text node.
    val expectedNoAttrs = XMLUtils.removeAttributes(XMLUtils.convertPCDataToText(expected))

    XMLUtils.compareAndReport(expectedNoAttrs, actualNoAttrs)
  }

  def verifyUnparserTestData(expectedData: DFDL.Input, actualOutStream: java.io.ByteArrayOutputStream) {
    val actualBytes = actualOutStream.toByteArray

    val expectedDataStream = Channels.newInputStream(expectedData)
    val expectedBytes = IOUtils.toByteArray(expectedDataStream)
    // example data was of size 0 (could not read anything). We're not supposed to get any actual data.
    if (expectedBytes.length == 0 && actualBytes.length > 0) {
      throw new TDMLException("Unexpected data was created.")
    }

    val readCount = expectedBytes.length

    if (actualBytes.length != readCount) {
      throw new TDMLException("output data length " + actualBytes.length + " for " + actualBytes.toList +
        " doesn't match expected value " + readCount + " for " + expectedBytes)
    }

    val pairs = expectedBytes zip actualBytes zip Stream.from(1)
    pairs.foreach {
      case ((expected, actual), index) =>
        if (expected != actual) {
          val msg = "Unparsed data differs at byte %d. Expected 0x%02x. Actual was 0x%02x.".format(index, expected, actual)
          throw new TDMLException(msg)
        }
    }
  }

  def verifyAllDiagnosticsFound(actualDiags: Seq[Throwable], expectedDiags: Option[ErrorWarningBase]) = {

    val actualDiagMsgs = actualDiags.map { _.toString }
    val expectedDiagMsgs = expectedDiags.map { _.messages }.getOrElse(Nil)

    if (expectedDiags.isDefined && actualDiags.length == 0) {
      throw new TDMLException(""""Diagnostic message(s) were expected but not found."""" +
        "\n" + """Expected: """ + expectedDiagMsgs.mkString("\n") +
        (if (actualDiagMsgs.length == 0)
          "\n No diagnostic messages were issued."
        else
          "\n The actual diagnostics messages were: " + actualDiagMsgs.mkString("\n")))
    }

    // must find each expected warning message within some actual warning message.
    expectedDiagMsgs.foreach {
      expected =>
        {
          val wasFound = actualDiagMsgs.exists {
            actual => actual.toLowerCase.contains(expected.toLowerCase)
          }
          if (!wasFound) {
            throw new TDMLException("""Did not find diagnostic message """" +
              expected + """" in any of the actual diagnostic messages: """ + "\n" +
              actualDiagMsgs.mkString("\n"))
          }
        }
    }
  }

  def verifyNoValidationErrorsFound(actual: WithDiagnostics) = {
    val actualDiags = actual.getDiagnostics.filter(d => d.isInstanceOf[ValidationError])
    if (actualDiags.length != 0) {
      val actualDiagMsgs = actualDiags.map { _.toString }
      throw new TDMLException("Validation errors found where none were expected by the test case.\n" +
        actualDiagMsgs.mkString("\n"))
    }
  }

  def verifyTextData(expectedData: DFDL.Input, actualOutStream: java.io.ByteArrayOutputStream, encodingName: String) {
    // Getting this decoder and decoding the bytes to text this way is
    // necessary, as opposed to toString(encodingName), because it is possible
    // that encodingName is a custom DFDL specific decoder (e.g. 7-bit ASCII)
    // that Java does not know about.
    val decoder = CharsetUtils.getCharset(encodingName).newDecoder()
    val actualBytes = actualOutStream.toByteArray
    val expectedBytes = IOUtils.toByteArray(Channels.newInputStream(expectedData))
    val actualText = decoder.decode(ByteBuffer.wrap(actualBytes)).toString
    val expectedText = decoder.decode(ByteBuffer.wrap(expectedBytes)).toString
    expectedData.close()
    if (expectedText.length == 0) {
      // example data was of size 0 (could not read anything). We're not supposed to get any actual data.
      if (actualText.length > 0) {
        throw new TDMLException("Unexpected data '%s' was created.".format(actualText))
      }
      return // we're done. Nothing equals nothing.
    }

    // compare expected data to what was output.
    val maxTextCharsToShow = 100
    def trimToMax(str: String) = {
      if (str.length <= maxTextCharsToShow) str
      else str.substring(0, maxTextCharsToShow) + "..."
    }
    if (actualText.length != expectedText.length) {
      val actualCharsToShow = if (actualText.length == 0) "" else " for '" + trimToMax(actualText) + "'"
      val expectedCharsToShow = if (expectedText.length == 0) "" else " for '" + trimToMax(expectedText) + "'"
      throw new TDMLException("output data length " + actualText.length + actualCharsToShow +
        " doesn't match expected length " + expectedText.length + expectedCharsToShow)
    }

    val pairs = expectedText.toSeq zip actualText.toSeq zip Stream.from(1)
    pairs.foreach {
      case ((expected, actual), index) =>
        if (expected != actual) {
          val msg = "Unparsed data differs at character %d. Expected '%s'. Actual was '%s'".format(index, expected, actual)
          throw new TDMLException(msg)
        }
    }
  }

  private val cs8859 = java.nio.charset.Charset.forName("iso-8859-1")

  def verifyBinaryOrMixedData(expectedData: DFDL.Input, actualOutStream: java.io.ByteArrayOutputStream) {
    val actualBytes = actualOutStream.toByteArray
    lazy val actual8859String = cs8859.newDecoder().decode(ByteBuffer.wrap(actualBytes)).toString()
    lazy val displayableActual = Misc.remapControlsAndLineEndingsToVisibleGlyphs(actual8859String)

    val expectedBytes = IOUtils.toByteArray(Channels.newInputStream(expectedData))
    lazy val expected8859String = cs8859.newDecoder().decode(ByteBuffer.wrap(expectedBytes)).toString()
    lazy val displayableExpected = Misc.remapControlsAndLineEndingsToVisibleGlyphs(expected8859String)

    lazy val expectedAndActualDisplayStrings = "\n" +
      "Excected data (as iso8859-1): " + displayableExpected + "\n" +
      "Actual data                 : " + displayableActual

    val readCount = expectedBytes.length
    expectedData.close()
    if (readCount == 0) {
      // example data was of size 0 (could not read anything). We're not supposed to get any actual data.
      if (actualBytes.length > 0) {
        throw new TDMLException("Unexpected data was created: '" + displayableActual + "'")
      }
      return // we're done. Nothing equals nothing.
    }

    // compare expected data to what was output.
    if (actualBytes.length != readCount) {
      val bytesToShow = if (actualBytes.length == 0) "" else " for " + Misc.bytes2Hex(actualBytes)
      throw new TDMLException("output data length " + actualBytes.length + bytesToShow +
        " doesn't match expected length " + readCount + " for " + Misc.bytes2Hex(expectedBytes) +
        expectedAndActualDisplayStrings)
    }

    val pairs = expectedBytes zip actualBytes zip Stream.from(1)
    pairs.foreach {
      case ((expected, actual), index) =>
        if (expected != actual) {
          val msg = "Unparsed data differs at byte %d. Expected 0x%02x. Actual was 0x%02x.".format(index, expected, actual) +
            expectedAndActualDisplayStrings
          throw new TDMLException(msg)
        }
    }
  }
}

case class DefinedSchema(xml: Node, parent: DFDLTestSuite) {
  /* Because the TDMLRunner is dumb and the thing that reads in the xml has no
   * knowledge of tdml.xsd, we have to set a default value here even though one
   * is specified in tdml.xsd.
   * */
  final val DEFAULT_ELEMENT_FORM_DEFAULT_VALUE = "qualified"

  val name = (xml \ "@name").text.toString
  val elementFormDefault = {
    val value = (xml \ "@elementFormDefault").text.toString

    if (value == "") DEFAULT_ELEMENT_FORM_DEFAULT_VALUE
    else value
  }

  val defineFormats = (xml \ "defineFormat")
  val defaultFormats = (xml \ "format")
  val defineVariables = (xml \ "defineVariable")
  val defineEscapeSchemes = (xml \ "defineEscapeScheme")

  val globalElementDecls = {
    val res = (xml \ "element")
    res
  }
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
  lazy val xsdSchema =
    SchemaUtils.dfdlTestSchema(dfdlTopLevels, xsdTopLevels, fileName = fileName, schemaScope = xml.scope, elementFormDefault = elementFormDefault)
}

case class DefinedConfig(xml: Node, parent: DFDLTestSuite) {
  val name = (xml \ "@name").text.toString
  val externalVariableBindings = (xml \ "externalVariableBindings").headOption
  val tunables = (scala.xml.Utility.trim(xml) \ "tunables").headOption /* had to add trim here to get rid of #PCDATA */

  // Add additional compiler tunable variables here

  val fileName = parent.ts.attribute(XMLUtils.INT_NS, XMLUtils.FILE_ATTRIBUTE_NAME) match {
    case Some(seqNodes) => seqNodes.toString
    case None => ""
  }
}

sealed abstract class DocumentContentType
case object ContentTypeText extends DocumentContentType
case object ContentTypeByte extends DocumentContentType
case object ContentTypeBits extends DocumentContentType
case object ContentTypeFile extends DocumentContentType
// TODO: add capability to specify character set encoding into which text is to be converted (all UTF-8 currently)

sealed abstract class BitOrderType
case object LSBFirst extends BitOrderType
case object MSBFirst extends BitOrderType

sealed abstract class ByteOrderType
case object RTL extends ByteOrderType
case object LTR extends ByteOrderType

case class Document(d: NodeSeq, parent: TestCase) {
  lazy val documentExplicitBitOrder = (d \ "@bitOrder").toString match {
    case "LSBFirst" => Some(LSBFirst)
    case "MSBFirst" => Some(MSBFirst)
    case "" => None
    case _ => Assert.invariantFailed("invalid bit order.")
  }

  private lazy val nDocumentParts = dataDocumentParts.length

  lazy val documentBitOrder: BitOrderType = {
    this.documentExplicitBitOrder match {
      case Some(order) => order
      case None => {
        // analyze the child parts
        val groups = dataDocumentParts.groupBy(_.explicitBitOrder).map {
          case (key, seq) => (key, seq.length)
        }
        if (groups.get(Some(MSBFirst)) == Some(nDocumentParts)) MSBFirst // all are msb first
        else if (groups.get(Some(LSBFirst)) == Some(nDocumentParts)) LSBFirst // all are lsb first
        else if (groups.get(None) == Some(nDocumentParts)) MSBFirst // everything is silent on bit order.
        else {
          // Some mixture of explicit and non-explicit bitOrder
          Assert.usageError(
            "Must specify bitOrder on document element when parts have a mixture of bit orders.")
        }
      }
    }
  }

  private lazy val Seq(<document>{ children @ _* }</document>) = d

  private val actualDocumentPartElementChildren = children.toList.flatMap {
    child =>
      child match {
        case <documentPart>{ _* }</documentPart> => {
          List((child \ "@type").toString match {
            case "text" => new TextDocumentPart(child, this)
            case "byte" => new ByteDocumentPart(child, this)
            case "bits" => new BitsDocumentPart(child, this)
            case "file" => new FileDocumentPart(child, this)
            case _ => Assert.invariantFailed("invalid content type.")

          })
        }
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
        case scala.xml.PCData(s) => // ok
        case scala.xml.EntityRef(_) => //ok
        case _: scala.xml.Atom[_] => //ok. Things like &lt; come through as this. Should be EntityRef("lt")
        case x => Assert.usageError("Illegal TDML data document content '" + x + "'")
      }
    }
  }

  private lazy val unCheckedDocumentParts: Seq[DocumentPart] = {
    val udp =
      if (actualDocumentPartElementChildren.length > 0) actualDocumentPartElementChildren
      else List(new TextDocumentPart(<documentPart type="text">{ children }</documentPart>, this))
    udp
  }

  private lazy val dataDocumentParts = {
    val dps = unCheckedDocumentParts.collect { case dp: DataDocumentPart => dp }
    dps
  }

  private lazy val fileParts = {
    val fps = unCheckedDocumentParts.collect { case fp: FileDocumentPart => fp }
    Assert.usage(fps.length == 0 ||
      (fps.length == 1 && dataDocumentParts.length == 0),
      "There can be only one documentPart of type file, and it must be the only documentPart.")
    fps
  }

  private[tdml] lazy val documentParts = {
    checkForBadBitOrderTransitions(dataDocumentParts)
    dataDocumentParts ++ fileParts
  }

  /**
   * A method because it is easier to unit test it
   */
  private def checkForBadBitOrderTransitions(dps: Seq[DataDocumentPart]) {
    if (dps.length <= 1) return
    // these are the total lengths BEFORE the component
    val lengths = dps.map { _.lengthInBits }
    val cumulativeDocumentPartLengthsInBits = lengths.scanLeft(0) { case (sum, num) => sum + num }
    val docPartBitOrders = dps.map { _.partBitOrder }
    val transitions = docPartBitOrders zip docPartBitOrders.tail zip cumulativeDocumentPartLengthsInBits.tail zip dps
    transitions.foreach {
      case (((bitOrderPrior, bitOrderHere), cumulativeLength), docPart) => {
        // println("transition " + bitOrderPrior + " " + bitOrderHere + " " + cumulativeLength)
        Assert.usage(
          (bitOrderPrior == bitOrderHere) || ((cumulativeLength % 8) == 0),
          "bitOrder can only change on a byte boundary.")
      }
    }
  }

  /**
   * When data is coming from the TDML file as small test data in
   * DataDocumentParts, then
   * Due to alignment, and bits-granularity issues, everything is lowered into
   * bits first, and then concatenated, and then converted back into bytes
   *
   * These are all lazy val, since if data is coming from a file these aren't
   * needed at all.
   */
  final lazy val documentBits = {
    val nFragBits = (nBits.toInt % 8)
    val nAddOnBits = if (nFragBits == 0) 0 else 8 - nFragBits
    val addOnBits = "0" * nAddOnBits
    val bitsFromParts = dataDocumentParts.map { _.contentAsBits }
    val allPartsBits = documentBitOrder match {
      case MSBFirst => bitsFromParts.flatten
      case LSBFirst => {
        val x = bitsFromParts.map { _.map { _.reverse } }
        val rtlBits = x.flatten.mkString.reverse
        val ltrBits = rtlBits.reverse.sliding(8, 8).map { _.reverse }.toList
        ltrBits
      }
    }
    val allBits = allPartsBits.mkString.sliding(8, 8).toList
    if (allBits == Nil) Nil
    else {
      val lastByte = this.documentBitOrder match {
        case MSBFirst => allBits.last + addOnBits
        case LSBFirst => addOnBits + allBits.last
      }
      val res = allBits.dropRight(1) :+ lastByte
      res
    }
  }

  final lazy val nBits: Long =
    documentParts.map {
      _.nBits
    } sum

  final lazy val documentBytes = bits2Bytes(documentBits)

  /**
   * this 'data' is the kind our parser's parse method expects.
   * Note: this is def data so that the input is re-read every time.
   * Needed if you run the same test over and over.
   */
  final def data = {
    if (isDPFile) {
      // direct I/O to the file. No 'bits' lowering involved.
      val dp = documentParts(0).asInstanceOf[FileDocumentPart]
      val input = dp.fileDataInput
      input
    } else {
      // assemble the input from the various pieces, having lowered
      // everything to bits.
      val bytes = documentBytes.toArray
      // println("data size is " + bytes.length)
      val inputStream = new java.io.ByteArrayInputStream(bytes);
      val rbc = java.nio.channels.Channels.newChannel(inputStream);
      rbc.asInstanceOf[DFDL.Input]
    }
  }

  /**
   * data coming from a file?
   */
  val isDPFile = {
    val res = documentParts.length > 0 &&
      documentParts(0).isInstanceOf[FileDocumentPart]
    if (res) {
      Assert.usage(documentParts.length == 1, "There can be only one documentPart of type file, and it must be the only documentPart.")
    }
    res
  }

}

class TextDocumentPart(part: Node, parent: Document) extends DataDocumentPart(part, parent) {

  private def err(encName: String, partBitOrd: BitOrderType) = {
    Assert.usageError("encoding %s requires bitOrder='%s'".format(encName, partBitOrd))
  }

  lazy val encoder = {
    val upperName = encodingName.toUpperCase
    val cs = CharsetUtils.getCharset(upperName)
    cs match {
      case bitEnc: NonByteSizeCharset => {
        (bitEnc.requiredBitOrder, partBitOrder) match {
          case (BitOrder.LeastSignificantBitFirst, LSBFirst) => //ok
          case (BitOrder.MostSignificantBitFirst, MSBFirst) => //ok
          case (BitOrder.LeastSignificantBitFirst, _) => err(upperName, LSBFirst)
          case _ => err(upperName, MSBFirst)
        }
      }
      case _ => //ok
    }
    val enc = cs.newEncoder()
    enc
  }

  lazy val textContentWithoutEntities = {
    if (replaceDFDLEntities) {
      try { EntityReplacer { _.replaceAll(partRawContent) } }
      catch {
        case e: Exception =>
          Assert.abort(e.getMessage())
      }
    } else partRawContent
  }

  /**
   * Result is sequence of strings, each string representing a byte or
   * partial byte using '1' and '0' characters for the bits.
   */
  def encodeUtf8ToBits(s: String): Seq[String] = {
    // Fails here if we use getBytes("UTF-8") because that uses the utf-8 encoder,
    // and that will fail on things like unpaired surrogate characters that we allow
    // in our data and our infoset.
    // So instead we must do our own UTF-8-like encoding of the data
    // so that we can put in codepoints we want.
    //System.out.println("encodeUtf8ToBits")
    val bytes = UTF8Encoder.utf8LikeEncode(textContentWithoutEntities).toArray
    val res = bytes.map { b => (b & 0xFF).toBinaryString.reverse.padTo(8, '0').reverse }.toList
    res
  }

  def encodeWithNonByteSizeEncoder(s: String, byteSize: Int): Seq[String] = {
    //System.out.println("encodeWith3BitEncoder")
    val bb = ByteBuffer.allocate(4 * s.length)
    val cb = CharBuffer.wrap(s)
    val coderResult = encoder.encode(cb, bb, true)
    Assert.invariant(coderResult == CoderResult.UNDERFLOW)
    bb.flip()
    val res = (0 to bb.limit() - 1).map { bb.get(_) }
    // val bitsAsString = bytes2Bits(res.toArray)
    val enc = encoder.asInstanceOf[NonByteSizeCharsetEncoder]
    val nBits = s.length * enc.bitWidthOfACodeUnit
    val bitStrings = res.map { b => (b & 0xFF).toBinaryString.reverse.padTo(8, '0').reverse }.toList
    val allBits = bitStrings.reverse.mkString.takeRight(nBits)
    val bitChunks = allBits.reverse.sliding(byteSize, byteSize).map { _.reverse }.toList
    bitChunks
  }

  def encodeWith8BitEncoder(s: String): Seq[String] = {
    //System.out.println("encodeWith8BitEncoder")
    val bb = ByteBuffer.allocate(4 * s.length)
    val cb = CharBuffer.wrap(s)
    val coderResult = encoder.encode(cb, bb, true)
    Assert.invariant(coderResult == CoderResult.UNDERFLOW)
    bb.flip()
    val res = (0 to bb.limit() - 1).map { bb.get(_) }
    // val bitsAsString = bytes2Bits(res.toArray)
    // val nBits = bb.limit() * 8
    val bitStrings = res.map { b => (b & 0xFF).toBinaryString.reverse.padTo(8, '0').reverse }.toList
    bitStrings
  }

  lazy val dataBits = {
    val bytesAsStrings =
      encoder.charset() match {
        case nbs: NonByteSizeCharset =>
          encodeWithNonByteSizeEncoder(textContentWithoutEntities, nbs.bitWidthOfACodeUnit)
        case _ =>
          encodeWith8BitEncoder(textContentWithoutEntities)
      }
    bytesAsStrings
  }
}

class ByteDocumentPart(part: Node, parent: Document) extends DataDocumentPart(part, parent) {
  val validHexDigits = "0123456789abcdefABCDEF"

  lazy val dataBits = {
    val hexBytes = partByteOrder match {
      case LTR => {
        val ltrDigits = hexDigits.sliding(2, 2).toList
        ltrDigits
      }
      case RTL => {
        val rtlDigits = hexDigits.reverse.sliding(2, 2).toList.map { _.reverse }
        rtlDigits
      }
    }
    val bits = hexBytes.map { hex2Bits(_) }
    bits
  }

  // Note: anything that is not a valid hex digit (or binary digit for binary) is simply skipped
  // TODO: we should check for whitespace and other characters we want to allow, and verify them.
  // TODO: Or better, validate this in the XML Schema for tdml via a pattern facet
  // TODO: Consider whether to support a comment syntax. When showing data examples this may be useful.
  //
  lazy val hexDigits = partRawContent.flatMap { ch => if (validHexDigits.contains(ch)) List(ch) else Nil }

}
class BitsDocumentPart(part: Node, parent: Document) extends DataDocumentPart(part, parent) {
  // val validBinaryDigits = "01"

  // lazy val bitContentToBytes = bits2Bytes(bitDigits).toList

  lazy val bitDigits = {
    val res = partRawContent.split("[^01]").mkString
    res
  }

  lazy val dataBits = partByteOrder match {
    case LTR => {
      val ltrBigits = bitDigits.sliding(8, 8).toList
      ltrBigits
    }
    case RTL => {
      val rtlBigits =
        bitDigits.reverse.sliding(8, 8).toList.map { _.reverse }
      rtlBigits
    }
  }
}

class FileDocumentPart(part: Node, parent: Document) extends DocumentPart(part, parent) with Logging {

  override lazy val nBits =
    if (lengthInBytes != -1L) lengthInBytes * 8
    else -1L // signifies we do not know how many.

  lazy val (url, lengthInBytes) = {
    val maybeURI = parent.parent.parent.findTDMLResource(partRawContent.trim())
    val uri = maybeURI.getOrElse(throw new FileNotFoundException("TDMLRunner: data file '" + partRawContent + "' was not found"))
    val url = uri.toURL
    if (url.getProtocol() == "file") {
      val file = new File(uri)
      (url, file.length())
    } else
      (url, -1L)
  }

  lazy val fileDataInput = {
    val is = url.openStream()
    val rbc = Channels.newChannel(is)
    rbc.asInstanceOf[DFDL.Input]
  }

}

/**
 * Base class for all document parts that contain data directly expressed in the XML
 */
sealed abstract class DataDocumentPart(part: Node, parent: Document)
  extends DocumentPart(part, parent) {

  def dataBits: Seq[String]

  lazy val lengthInBits = dataBits.map { _.length } sum
  override lazy val nBits: Long = lengthInBits

  lazy val contentAsBits = dataBits

}

/**
 * Base class for all document parts
 */
sealed abstract class DocumentPart(part: Node, parent: Document) {

  def nBits: Long

  lazy val explicitBitOrder: Option[BitOrderType] = {
    val bitOrd = (part \ "@bitOrder").toString match {

      case "LSBFirst" => Some(LSBFirst)
      case "MSBFirst" => Some(MSBFirst)
      case "" => None
      case _ => Assert.invariantFailed("invalid bit order.")
    }
    Assert.usage(!isInstanceOf[FileDocumentPart],
      "bitOrder may not be specified on document parts of type 'file'")
    bitOrd
  }

  lazy val partBitOrder = explicitBitOrder.getOrElse(parent.documentBitOrder)

  lazy val partByteOrder = {
    val bo = (part \ "@byteOrder").toString match {
      case "RTL" => {
        Assert.usage(partBitOrder == LSBFirst, "byteOrder RTL can only be used with bitOrder LSBFirst")
        RTL
      }
      case "LTR" => LTR
      case "" => LTR
      case _ => Assert.invariantFailed("invalid byte order.")
    }
    Assert.usage(this.isInstanceOf[ByteDocumentPart] || this.isInstanceOf[BitsDocumentPart],
      "byteOrder many only be specified for document parts of type 'byte' or 'bits'")
    bo
  }

  /**
   * Only trim nodes that aren't PCData (aka <![CDATA[...]]>)
   */
  lazy val trimmedParts = part.child flatMap { childNode =>
    childNode match {
      case scala.xml.PCData(s) => Some(childNode)
      case scala.xml.Text(s) => Some(childNode)
      //      {
      //        // can't just use s.trim here as that would remove explicit
      //        // carriage returns like &#x0D; if they have already been
      //        // replaced by the corresponding character.
      //        val trimmedEnd = s.replaceFirst("\\ +$", "") // spaces only
      //        val trimmed = trimmedEnd.replaceFirst("^\\ +", "") // spaces only
      //        if (trimmed.length == 0) None
      //        else Some(scala.xml.Text(trimmed))
      //      }
      case scala.xml.Comment(_) => None
      case scala.xml.EntityRef(_) => Some(childNode)
      case _: scala.xml.Atom[_] => Some(childNode) // Things like &lt; come through as this. Should be EntityRef
      case _ => Assert.invariantFailed("unrecognized child part in TextDocumentPart: " + childNode)
    }
  }

  lazy val partRawContent = trimmedParts.text

  lazy val replaceDFDLEntities: Boolean = {
    val res = (part \ "@replaceDFDLEntities")
    if (res.length == 0) { false }
    else {
      Assert.usage(this.isInstanceOf[TextDocumentPart])
      res(0).toString().toBoolean
    }
  }

  lazy val encodingName: String = {
    val res = (part \ "@encoding").text
    if (res.length == 0) { "utf-8" }
    else {
      Assert.usage(this.isInstanceOf[TextDocumentPart])
      res
    }
  }.trim.toUpperCase()

}

case class Infoset(i: NodeSeq, parent: TestCase) {
  val Seq(dfdlInfoset) = (i \ "dfdlInfoset").map { node => new DFDLInfoset(node, this) }
  val contents = dfdlInfoset.contents
}

case class DFDLInfoset(di: Node, parent: Infoset) {

  val infosetNodeSeq = {
    (di \ "@type").toString match {
      case "infoset" | "" => di.child.filter { _.isInstanceOf[scala.xml.Elem] }
      case "file" => {
        val path = di.text.trim()
        val maybeURI = parent.parent.parent.findTDMLResource(path)
        val uri = maybeURI.getOrElse(throw new FileNotFoundException("TDMLRunner: infoset file '" + path + "' was not found"))
        val elem = scala.xml.XML.load(uri.toURL)
        elem
      }
      case value => Assert.abort("Uknown value for type attribute on dfdlInfoset: " + value)
    }
  }

  val rawContents = {
    Assert.usage(infosetNodeSeq.size == 1, "dfdlInfoset element must contain a single root element")
    val c = infosetNodeSeq(0)
    c
  }

  val Seq(contents) = {
    val c = rawContents
    val expected = c // Utility.trim(c) // must be exactly one root element in here.
    val expectedNoAttrs = try {
      XMLUtils.removeAttributes(expected)
    } catch {
      case e: Exception => throw new TDMLException(e)
    }
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

  def hasDiagnostics: Boolean = diagnosticNodes.length > 0
}

case class ExpectedErrors(node: NodeSeq, parent: TestCase)
  extends ErrorWarningBase(node, parent) {

  val diagnosticNodes = node \\ "error"

}

case class ExpectedWarnings(node: NodeSeq, parent: TestCase)
  extends ErrorWarningBase(node, parent) {

  val diagnosticNodes = node \\ "warning"

}

case class ExpectedValidationErrors(node: NodeSeq, parent: TestCase)
  extends ErrorWarningBase(node, parent) {

  val diagnosticNodes = node \\ "error"

}

object UTF8Encoder {
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

  def byteList(args: Int*) = args.map { _.toByte }

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
      // val byte1 = (cp >> 24) & 0xFF
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

}

class TDMLInfosetOutputter() extends InfosetOutputter {
  private val jsonWriter = new StringWriter()
  private val xmlWriter = new StringWriter()

  private val scalaOut = new ScalaXMLInfosetOutputter()
  private val jdomOut = new JDOMInfosetOutputter()
  private val w3cdomOut = new W3CDOMInfosetOutputter()
  private val jsonOut = new JsonInfosetOutputter(jsonWriter)
  private val xmlOut = new XMLTextInfosetOutputter(xmlWriter)

  private val outputters = Seq(xmlOut, scalaOut, jdomOut, w3cdomOut, jsonOut)

  override def reset(): Unit = { outputters.foreach(_.reset()) }

  override def startSimple(simple: DISimple): Boolean = {
    if (!outputters.forall(_.startSimple(simple)))
      throw new TDMLException("startSimple failed")
    true
  }

  override def endSimple(simple: DISimple): Boolean = {
    if (!outputters.forall(_.endSimple(simple)))
      throw new TDMLException("endSimple failed")
    true
  }

  override def startComplex(complex: DIComplex): Boolean = {
    if (!outputters.forall(_.startComplex(complex)))
      throw new TDMLException("startComplex failed")
    true
  }

  override def endComplex(complex: DIComplex): Boolean = {
    if (!outputters.forall(_.endComplex(complex)))
      throw new TDMLException("endComplex failed")
    true
  }

  override def startArray(array: DIArray): Boolean = {
    if (!outputters.forall(_.startArray(array)))
      throw new TDMLException("startArray failed")
    true
  }

  override def endArray(array: DIArray): Boolean = {
    if (!outputters.forall(_.endArray(array)))
      throw new TDMLException("endArray failed")
    true
  }

  override def startDocument(): Boolean = {
    if (!outputters.forall(_.startDocument()))
      throw new TDMLException("startDocument failed")
    true
  }

  override def endDocument(): Boolean = {
    if (!outputters.forall(_.endDocument()))
      throw new TDMLException("endDocument failed")
    true
  }

  def getResult() = scalaOut.getResult

  def toInfosetInputter() = {
    val scalaIn = new ScalaXMLInfosetInputter(scalaOut.getResult)
    val jdomIn = new JDOMInfosetInputter(jdomOut.getResult)
    val w3cdomIn = new W3CDOMInfosetInputter(w3cdomOut.getResult)
    val jsonIn = new JsonInfosetInputter(new StringReader(jsonWriter.toString))
    val xmlIn = new XMLTextInfosetInputter(new StringReader(xmlWriter.toString))
    new TDMLInfosetInputter(scalaIn, Seq(jdomIn, w3cdomIn, jsonIn, xmlIn))
  }
}

class TDMLInfosetInputter(val scalaInputter: ScalaXMLInfosetInputter, others: Seq[InfosetInputter]) extends InfosetInputter {

  override def getEventType(): InfosetInputterEventType = {
    val res = scalaInputter.getEventType()
    if (!others.forall(_.getEventType() == res))
      throw new TDMLException("getEventType does not match")
    res
  }

  override def getLocalName(): String = {
    val res = scalaInputter.getLocalName()
    if (!others.forall(_.getLocalName() == res))
      throw new TDMLException("getLocalName does not match")
    res
  }

  override def getNamespaceURI(): String = {
    val res = scalaInputter.getNamespaceURI()
    val resIsEmpty = res == null || res == ""
    val othersMatch = others.forall { i =>
      if (!i.supportsNamespaces) {
        true
      } else {
        val ns = i.getNamespaceURI()
        val nsIsEmpty = ns == null || ns == ""
        // some inputters return null for no namespace, some return empty
        // string, we consider those the same
        ns == res || (resIsEmpty && nsIsEmpty)
      }
    }
    if (!othersMatch)
      throw new TDMLException("getNamespaceURI does not match")
    res
  }

  override def getSimpleText(primType: NodeInfo.Kind): String = {
    val res = scalaInputter.getSimpleText(primType)
    val resIsEmpty = res == null || res == ""
    val othersmatch = others.forall { i =>
      val st = i.getSimpleText(primType)
      val stIsEmpty = st == null || res == ""
      val areSame = res == st || (resIsEmpty && stIsEmpty)
      if (areSame) {
        true
      } else {
        if (i.isInstanceOf[JsonInfosetInputter]) {
          // the json infoset inputter maintains CRLF, but XML converts CRLF to
          // LF. So if this is Json, then compare with the CRLF converted to LF
          res == st.replace("\r\n", "\n")
        } else {
          false
        }
      }
    }

    if (!othersmatch)
      throw new TDMLException("getSimpleText does not match")
    res
  }

  override def isNilled(): MaybeBoolean = {
    val res = scalaInputter.isNilled()
    if (!others.forall(_.isNilled() == res))
      throw new TDMLException("isNilled does not match")
    res
  }

  override def hasNext(): Boolean = {
    val res = scalaInputter.hasNext()
    if (!others.forall(_.hasNext() == res))
      throw new TDMLException("hasNext does not match")
    res
  }

  override def next(): Unit = {
    scalaInputter.next
    others.foreach(_.next)
  }

  override def fini: Unit = {
    scalaInputter.fini
    others.foreach(_.fini)
  }

  override val supportsNamespaces = true
}
