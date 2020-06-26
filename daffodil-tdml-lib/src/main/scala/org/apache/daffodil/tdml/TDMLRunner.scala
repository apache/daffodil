/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.tdml

import java.io.File
import java.io.FileNotFoundException
import java.net.URI

import scala.xml.Node
import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.SAXParseException
import scala.xml.transform._
import org.apache.daffodil.api.DaffodilSchemaSource
import org.apache.daffodil.api.DaffodilTunables
import org.apache.daffodil.api.DataLocation
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.api.EmbeddedSchemaSource
import org.apache.daffodil.api.URISchemaSource
import org.apache.daffodil.api.UnitTestSchemaSource
import org.apache.daffodil.api.ValidationMode
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.exceptions.UnsuppressableException
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.util.Logging
import org.apache.daffodil.util.Misc
import org.apache.daffodil.util.Misc.bits2Bytes
import org.apache.daffodil.util.Misc.hex2Bits
import org.apache.daffodil.util.SchemaUtils
import org.apache.daffodil.xml.DaffodilXMLLoader
import org.apache.daffodil.xml._
import org.apache.daffodil.processors.charset.CharsetUtils
import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.LongBuffer
import java.nio.charset.CoderResult
import java.io.ByteArrayInputStream

import scala.language.postfixOps
import java.nio.file.Files
import java.io.InputStream

import org.apache.commons.io.IOUtils
import org.apache.daffodil.cookers.EntityReplacer
import org.apache.daffodil.configuration.ConfigurationLoader
import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.schema.annotation.props.gen.BinaryFloatRep
import org.apache.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import org.apache.daffodil.schema.annotation.props.gen.UTF16Width
import org.apache.daffodil.util.MaybeInt
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.processors.charset.BitsCharsetDecoder
import org.apache.daffodil.processors.charset.BitsCharsetEncoder
import org.apache.daffodil.processors.charset.BitsCharsetNonByteSize
import org.apache.daffodil.processors.charset.BitsCharsetNonByteSizeEncoder
import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.io.InputSourceDataInputStream
import java.nio.charset.{Charset => JavaCharset}
import java.io.OutputStream

import org.apache.daffodil.tdml.processor.TDMLParseResult
import org.apache.daffodil.tdml.processor.TDMLDFDLProcessor
import org.apache.daffodil.tdml.processor.TDMLResult
import org.apache.daffodil.tdml.processor.TDMLUnparseResult
import org.apache.daffodil.tdml.processor.TDML
import org.apache.daffodil.tdml.processor.AbstractTDMLDFDLProcessorFactory

import scala.util.Try
import org.apache.daffodil.processors.HasSetDebugger

/**
 * Parses and runs tests expressed in IBM's contributed tdml "Test Data Markup Language"
 */

sealed trait RoundTrip {
  def propValueName: String
}

/**
 * Test is just a parse test, or just an unparse, with no round trip involved.
 */
case object NoRoundTrip extends RoundTrip {
  override def propValueName = "none"
}

/**
 * Test round trips with a single pass. Unparse produces exactly the original data.
 */
case object OnePassRoundTrip extends RoundTrip {
  override def propValueName = "onePass"
}

/**
 * Unparse doesn't produce original data, but equivalent canonical data which
 * if reparsed in a second parse pass, produces the same infoset as the first
 * parse.
 */
case object TwoPassRoundTrip extends RoundTrip {
  override def propValueName = "twoPass"
}

/**
 * Unparse doesn't produce original data, parsing it doesn't produce the
 * same infoset, but an equivalent infoset which if unparsed, reproduces
 * the first unparse output.
 */
case object ThreePassRoundTrip extends RoundTrip {
  override def propValueName = "threePass"
}

private[tdml] object DFDLTestSuite {

  /**
   * Use to convert round trip default into enum value
   */
  def standardizeRoundTrip(enumStr: String): RoundTrip =
    enumStr match {
      case "false" | "none" => NoRoundTrip
      case "true" | "onePass" => OnePassRoundTrip
      case "twoPass" => TwoPassRoundTrip
      case "threePass" => ThreePassRoundTrip
      case other => Assert.invariantFailed("String '%s' not valid for round trip".format(other))
    }

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
 * defaultRoundTripDefault the round trip default for the test suite will be
 * taken from this value if it is not specified on the testSuite itself.
 *
 * defaultValidationDefault the validation default for the test suite will be
 * taken from this value if it is not specified on the testSuite itself.
 *
 * defaultImplementationsDefault the implementations default for the test suite will be
 * taken from this value if it is not specified on the testSuite itself.
 *
 * shouldDoErrorComparisonOnCrossTests controls whether negative test error messages  are compared
 * during cross testing, or the tests are just run to determine that they fail.
 *
 * shouldDoWarningComparisonOnCrossTests controls whether test warning messages  are compared
 * during cross testing.
 */

class DFDLTestSuite private[tdml] (
  val __nl: Null, // this extra arg allows us to make this primary constructor package private so we can deprecate the one generally used.
  aNodeFileOrURL: Any,
  validateTDMLFile: Boolean,
  val validateDFDLSchemas: Boolean,
  val compileAllTopLevel: Boolean,
  val defaultRoundTripDefault: RoundTrip,
  val defaultValidationDefault: String,
  val defaultImplementationsDefault: Seq[String],
  val shouldDoErrorComparisonOnCrossTests: Boolean,
  val shouldDoWarningComparisonOnCrossTests: Boolean)
  extends Logging
  with HasSetDebugger {

  // Uncomment to force conversion of all test suites to use Runner(...) instead.
  // That avoids creating the test suites repeatedly, but also leaks memory unless
  // you have an @AfterClass shutdown method in the object that calls runner.reset() at end.
  //
  // @deprecated("2016-12-30", "Use Runner(...) instead.")
  def this(
    aNodeFileOrURL: Any,
    validateTDMLFile: Boolean = true,
    validateDFDLSchemas: Boolean = true,
    compileAllTopLevel: Boolean = false,
    defaultRoundTripDefault: RoundTrip = Runner.defaultRoundTripDefaultDefault,
    defaultValidationDefault: String = Runner.defaultValidationDefaultDefault,
    defaultImplementationsDefault: Seq[String] = Runner.defaultImplementationsDefaultDefault,
    shouldDoErrorComparisonOnCrossTests: Boolean = Runner.defaultShouldDoErrorComparisonOnCrossTests,
    shouldDoWarningComparisonOnCrossTests: Boolean = Runner.defaultShouldDoWarningComparisonOnCrossTests) =
    this(null, aNodeFileOrURL, validateTDMLFile, validateDFDLSchemas, compileAllTopLevel,
      defaultRoundTripDefault,
      defaultValidationDefault,
      defaultImplementationsDefault,
      shouldDoErrorComparisonOnCrossTests,
      shouldDoWarningComparisonOnCrossTests)

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
      loadingExceptions = exception :: loadingExceptions
    }

    def error(exception: SAXParseException) = {
      loadingExceptions = exception :: loadingExceptions
      isLoadingError = true
    }

    def fatalError(exception: SAXParseException) = {
      error(exception)
    }
  }

  var isLoadingError: Boolean = false

  var loadingExceptions: List[Exception] = Nil

  def loadingDiagnosticMessages: String = {
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

      val src = UnitTestSchemaSource(tsNode, "", Some(tmpDir))
      loader.load(src)
      //
      (tsNode, src.uriForLoading)
    }
    case tdmlFile: File => {
      log(LogLevel.Info, "loading TDML file: %s", tdmlFile)
      val uri = tdmlFile.toURI()
      val newNode = loader.load(URISchemaSource(uri))
      val res = (newNode, uri)
      log(LogLevel.Debug, "done loading TDML file: %s", tdmlFile)
      res
    }
    case uri: URI => {
      val newNode = loader.load(URISchemaSource(uri))
      val res = (newNode, uri)
      res
    }
    case _ => Assert.usageError("not a Node, File, or URL")
  } // end match

  lazy val isTDMLFileValid = !this.isLoadingError

  var checkAllTopLevel: Boolean = compileAllTopLevel

  def setCheckAllTopLevel(flag: Boolean): Unit = {
    checkAllTopLevel = flag
  }

  val parserTestCases = (ts \ "parserTestCase").map { node => ParserTestCase(node, this) }
  //
  // Note: IBM started this TDML file format. They call an unparser test a "serializer" test.
  // We call it an UnparserTestCase
  //
  val unparserTestCases = (ts \ "unparserTestCase").map { node => UnparserTestCase(node, this) }
  val testCases: Seq[TestCase] = parserTestCases ++ unparserTestCases
  val duplicateTestCases = testCases.groupBy {
    _.tcName
  }.filter { case (name, seq) => seq.length > 1 }
  if (duplicateTestCases.nonEmpty) {
    duplicateTestCases.foreach {
      case (name, _) =>
        System.err.println("TDML Runner: More than one test case for name '%s'.".format(name))
    }
  }
  val testCaseMap = testCases.map { tc => (tc.tcName -> tc) }.toMap
  val suiteName = (ts \ "@suiteName").text
  val suiteID = (ts \ "@ID").text
  val description = (ts \ "@description").text
  val defaultRoundTrip = {
    val str = (ts \ "@defaultRoundTrip").text
    if (str == "") defaultRoundTripDefault else DFDLTestSuite.standardizeRoundTrip(str)
  }
  val defaultValidation = {
    val str = (ts \ "@defaultValidation").text
    if (str == "") defaultValidationDefault else str
  }
  val defaultConfig = {
    val str = (ts \ "@defaultConfig").text
    str
  }
  val defaultImplementations = {
    val str = (ts \ "@defaultImplementations").text
    if (str == "") defaultImplementationsDefault
    else {
      // parse the str to get a list of strings
      str.split("""\s+""").toSeq
    }
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

  def runAllTests(schema: Option[Node] = None): Unit = {
    if (isTDMLFileValid)
      testCases.map { _.run(schema) }
    else {
      log(LogLevel.Error, "TDML file %s is not valid.", tsURI)
    }
  }

  var areTracing = false
  def trace = {
    areTracing = true
    this
  }

  var areDebugging = false
  override def setDebugging(flag: Boolean) = {
    areDebugging = flag
  }

  var daffodilDebugger: AnyRef = null
  override def setDebugger(db: AnyRef) = {
    daffodilDebugger = db
  }

  def runOneTest(testName: String, schema: Option[Node] = None, leakCheck: Boolean = false): Unit = {
    if (leakCheck) {
      System.gc()
      Thread.sleep(1) // needed to give tools like jvisualvm ability to "grab on" quickly
    }
    if (isTDMLFileValid) {
      loadingExceptions.foreach { le =>  log(LogLevel.Warning, le.toString) }
      val testCase = testCases.find(_.tcName == testName)
      testCase match {
        case None => throw TDMLException("test " + testName + " was not found.", None)
        case Some(tc) => {
          tc.run(schema)
        }
      }
    } else {
      log(LogLevel.Error, "TDML file %s is not valid.", tsURI)
      val msgs = this.loadingExceptions.map { _.toString }.mkString("\n")
      throw TDMLException(msgs, None)
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
    Misc.searchResourceOption(resName, Some(tsURI))
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

  /**
   * Test case execution is strongly sequentialized due to this central var.
   *
   * Whenever we compute a new processor we just side-effect this state.
   *
   * Note getting rid of this var would require substantial code restructuring, which is why
   * that wasn't done when this code was updated to call the processor.withXYZ methods which return a
   * new processor.
   */
  protected final var processor: TDMLDFDLProcessor = null

  lazy val defaultRoundTrip: RoundTrip = parent.defaultRoundTrip
  lazy val defaultValidation: String = parent.defaultValidation

  private lazy val defaultImplementations: Seq[String] = parent.defaultImplementations
  private lazy val tcImplementations = (testCaseXML \ "@implementations").text
  private lazy val implementationStrings =
    if (tcImplementations == "") defaultImplementations
    else tcImplementations.split("""\s+""").toSeq

  protected def toss(t: Throwable, implString: Option[String]) = {
    t match {
      case e: TDMLException => throw e
      case e: UnsuppressableException => throw e
      case s: scala.util.control.ControlThrowable => throw s
      case _ => throw TDMLException(t, implString)
    }
  }

  final def isCrossTest(implString: String) = implString != "daffodil"

  val removeLineColInfo = new RewriteRule {
    override def transform(n: Node) = n match {
      case e: Elem => {
        e.copy(attributes = n.attributes.filter(a =>
          (a.prefixedKey != "dafint:" + XMLUtils.COLUMN_ATTRIBUTE_NAME) &&
            (a.prefixedKey != "dafint:" + XMLUtils.LINE_ATTRIBUTE_NAME) &&
            (a.prefixedKey != "dafint:" + XMLUtils.FILE_ATTRIBUTE_NAME)))
      }
      case _ => n
    }
  }

  val stripLineColInfo = new RuleTransformer(removeLineColInfo)

  final def isNegativeTest = optExpectedErrors.isDefined

  lazy val tdmlDFDLProcessorFactory: AbstractTDMLDFDLProcessorFactory = {
    import scala.language.existentials

    val className = "org.apache.daffodil.tdml.processor.TDMLDFDLProcessorFactory"

    //
    // If you haven't seen it before. Check out this Try(...) idiom.
    // Much cleaner than the messy nest of individual try/catches for each case.
    //
    // We're not catching anything here, but we could have surrounded the tryInstance.get call with a
    // single tier of catch, with cases for all the various throws that could have happened anywhere on
    // the three lines of actions that can throw various things.
    //
    // of course this will allocate an object, so not for tightest inner loops, but the code
    // cleanup is substantial.
    //
    val clazz = Try(Class.forName(className))
    val constructor = clazz.map { _.getDeclaredConstructor() }
    val tryInstance = constructor.map { _.newInstance().asInstanceOf[AbstractTDMLDFDLProcessorFactory] }
    val res = tryInstance.recover {
      case th =>
        toss(th, None) // encapsulates as TDMLException and throws.
    }.get
    res
  }

  lazy val document = (testCaseXML \ "document").headOption.map { node => Document(node, this) }
  lazy val optExpectedOrInputInfoset = (testCaseXML \ "infoset").headOption.map { node => new Infoset(node, this) }
  lazy val optExpectedErrors: Option[ExpectedErrors] = (testCaseXML \ "errors").headOption.map { node => ExpectedErrors(node, this) }
  lazy val optExpectedWarnings: Option[ExpectedWarnings] = (testCaseXML \ "warnings").headOption.map { node => ExpectedWarnings(node, this) }
  lazy val optExpectedValidationErrors: Option[ExpectedValidationErrors] = (testCaseXML \ "validationErrors").headOption.map { node => ExpectedValidationErrors(node, this) }

  val tcName = (testCaseXML \ "@name").text
  lazy val tcID = (testCaseXML \ "@ID").text
  lazy val id = tcName + (if (tcID != "") "(" + tcID + ")" else "")
  lazy val rootAttrib = (testCaseXML \ "@root").text

  lazy val (infosetRootName, infosetRootNamespaceString) =
    if (this.optExpectedOrInputInfoset.isDefined) {
      val infoset = optExpectedOrInputInfoset.get.dfdlInfoset.contents
      (infoset.label, infoset.namespace)
    } else (null, null)

  lazy val rootName = {
    if (rootAttrib == "") infosetRootName
    else if (this.optExpectedOrInputInfoset.isDefined) {
      if (infosetRootName != rootAttrib)
        throw TDMLException("root attribute name: %s, does not match the name of the root element of the infoset: %s.".format(
          rootAttrib, infosetRootName), None)
      rootAttrib
    } else rootAttrib
  }

  lazy val rootNamespaceString = {
    if (optExpectedOrInputInfoset.isDefined)
      infosetRootNamespaceString
    else if (embeddedSchema.isDefined)
      XMLUtils.EXAMPLE_NAMESPACE.toString
    else
      null
  }

  lazy val model = (testCaseXML \ "@model").text
  lazy val config = (testCaseXML \ "@config").text
  lazy val tcRoundTrip: String = (testCaseXML \ "@roundTrip").text
  lazy val roundTrip: RoundTrip =
    if (tcRoundTrip == "") defaultRoundTrip else DFDLTestSuite.standardizeRoundTrip(tcRoundTrip)
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
  lazy val expectsValidationError = if (optExpectedValidationErrors.isDefined) optExpectedValidationErrors.get.hasDiagnostics else false

  protected def runProcessor(
    compileResult: TDML.CompileResult,
    expectedData: Option[InputStream],
    nBits: Option[Long],
    errors: Option[ExpectedErrors],
    warnings: Option[ExpectedWarnings],
    validationErrors: Option[ExpectedValidationErrors],
    validationMode: ValidationMode.Type,
    roundTrip: RoundTrip,
    implString: Option[String]): Unit

  private def retrieveBindings(cfg: DefinedConfig, tunable: DaffodilTunables): Seq[Binding] = {
    val bindings: Seq[Binding] = cfg.externalVariableBindings match {
      case None => Seq.empty
      case Some(bindingsNode) => Binding.getBindings(bindingsNode)
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

  lazy val embeddedSchema = parent.findEmbeddedSchema(model)

  def getSuppliedSchema(schemaArg: Option[Node]): DaffodilSchemaSource = {
    val schemaURI = parent.findSchemaFileName(model)
    val suppliedSchema = (schemaArg, embeddedSchema, schemaURI) match {
      case (None, None, None) => throw TDMLException("Model '" + model + "' was not passed, found embedded in the TDML file, nor as a schema file.", None)
      case (None, Some(_), Some(_)) => throw TDMLException("Model '" + model + "' is ambiguous. There is an embedded model with that name, AND a file with that name.", None)
      case (Some(node), _, _) => {
        // unit test case. There is no URI/file location
        if (model != "") throw TDMLException("You supplied a model attribute, and a schema argument. Can't have both.", None)
        // note that in this case, since a node was passed in, this node has no file/line/col information on it
        // so error messages will end up being about some temp file.
        UnitTestSchemaSource(node, tcName)
      }
      case (None, Some(defSchema), None) => {
        Assert.invariant(model != "") // validation of the TDML should prevent this

        /* The file, line, and column attributes we add for tracking purposes
         * cause issues with the IBM DFDL processor as it doesn't allow
         * attributes that it doesn't understand, so we must remove them */
        val processedSchema = {
          if (isCrossTest(this.tdmlDFDLProcessorFactory.implementationName)) {
            stripLineColInfo(defSchema.xsdSchema)
          } else {
            defSchema.xsdSchema
          }
        }

        EmbeddedSchemaSource(processedSchema, defSchema.name)
      }
      case (None, None, Some(uri)) => {
        //
        // In this case, we have a real TDML file (or resource) to open
        URISchemaSource(uri)
      }
    } // end match
    suppliedSchema
  }

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
        throw TDMLException("The " + attrName + " '" + cfgName + "' was not found either as a embedded config, nor as a file.", None)
      case (name, Some(_), Some(_)) if name != "" =>
        throw TDMLException("The " + attrName + " '" + cfgName + "' is ambiguous. There is an embedded config with that name, AND a file with that name.", None)
    }
    optDefinedConfig
  }

  // Throws an exception marking a test as not compatible based on what classes
  // are on the classpath. If Junit is on the classpath, assume we are in a
  // Junit test and throw the AssumptionViolatedException to mark a test as
  // skipped rather than failed. Otherwise, if Junit is not on the classpath
  // just throw a standard "not compatible" exception and let the caller figure
  // out the right way to handle it.
  private def testNotCompatible(testName: String, implementationName: Option[String]) = {
    import scala.language.existentials

    val tdmlException = new TDMLTestNotCompatibleException(testName, implementationName)

    val junitExceptionClassName = "org.junit.AssumptionViolatedException"
    val junitExceptionClass = Try(Class.forName(junitExceptionClassName))
    val junitExceptionConstructor = junitExceptionClass.map {
      _.getDeclaredConstructor(classOf[String], classOf[Throwable])
    }
    val junitExceptionInstance = junitExceptionConstructor.map {
      _.newInstance(tdmlException.getMessage, tdmlException).asInstanceOf[Exception]
    }

    val exceptionToThrow = junitExceptionInstance.getOrElse(tdmlException)
    throw exceptionToThrow
  }

  lazy val cfg: Option[DefinedConfig] = {
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

  lazy val defaultTunables: Map[String, String] = cfg match {
    case None => Map.empty
    case Some(definedConfig) => retrieveTunables(definedConfig)
  }

  lazy val tunables: Map[String, String] = cfg match {
    case None => defaultTunables
    case Some(embeddedConfig) => retrieveTunablesCombined(defaultTunables, embeddedConfig)
  }

  lazy val tunableObj = DaffodilTunables(tunables)

  lazy val externalVarBindings: Seq[Binding] = cfg match {
    case None => Seq.empty
    case Some(definedConfig) => retrieveBindings(definedConfig, tunableObj)
  }

  def run(schemaArg: Option[Node] = None): Unit = {
    val suppliedSchema = getSuppliedSchema(schemaArg)

    var impl: AbstractTDMLDFDLProcessorFactory = this.tdmlDFDLProcessorFactory
    val implString = Some(impl.implementationName)
    //
    // Should we run the test?
    //
    val implName = impl.implementationName
    val istrings = implementationStrings
    val useThisImpl = istrings.contains(implName)
    if (!useThisImpl) {
      // throws an exception marking a test as not compatible
      testNotCompatible(this.tcName, implString)
    } else {
      // run the test.

      impl = impl.withValidateDFDLSchemas(parent.validateDFDLSchemas).withTunables(tunables).withCheckAllTopLevel(parent.checkAllTopLevel)

      val optInputOrExpectedData = document.map {
        _.data
      }
      val nBits: Option[Long] = document.map {
        _.nBits
      }

      val useSerializedProcessor =
        if (validationMode == ValidationMode.Full) false
        else if (optExpectedWarnings.isDefined) false
        else true

      val compileResult: TDML.CompileResult = impl.getProcessor(suppliedSchema, useSerializedProcessor, Option(rootName), Option(rootNamespaceString))
      val newCompileResult : TDML.CompileResult = compileResult.right.map {
        case (diags, proc: TDMLDFDLProcessor) =>
          // warnings are checked elsewhere for expected ones.
          val newProc: TDMLDFDLProcessor =
            proc.withDebugging(parent.areDebugging).withTracing(parent.areTracing)
          val newNewProc =
            if (parent.areDebugging &&
              (parent.daffodilDebugger ne null)) {
              newProc.withDebugger(parent.daffodilDebugger)
          } else {
             newProc
          }
          (diags, newNewProc)
      }
      runProcessor(newCompileResult, optInputOrExpectedData, nBits, optExpectedErrors, optExpectedWarnings, optExpectedValidationErrors, validationMode,
        roundTrip, implString)

    }
  }

  protected def checkDiagnosticMessages(
    diagnostics: Seq[Throwable],
    errors: ExpectedErrors,
    optWarnings: Option[ExpectedWarnings],
    implString: Option[String]): Unit = {
    Assert.usage(this.isNegativeTest)

    // check for any test-specified errors or warnings
    if (!isCrossTest(implString.get) ||
      parent.shouldDoErrorComparisonOnCrossTests)
      VerifyTestCase.verifyAllDiagnosticsFound(diagnostics, Some(errors), implString)

    if (!isCrossTest(implString.get) ||
      parent.shouldDoWarningComparisonOnCrossTests)
      VerifyTestCase.verifyAllDiagnosticsFound(diagnostics, optWarnings, implString)
  }
}

case class ParserTestCase(ptc: NodeSeq, parentArg: DFDLTestSuite)
  extends TestCase(ptc, parentArg) {

  lazy val optExpectedInfoset = this.optExpectedOrInputInfoset

  override def runProcessor(
    compileResult: TDML.CompileResult,
    optDataToParse: Option[InputStream],
    optLengthLimitInBits: Option[Long],
    optExpectedErrors: Option[ExpectedErrors],
    optExpectedWarnings: Option[ExpectedWarnings],
    optExpectedValidationErrors: Option[ExpectedValidationErrors],
    validationMode: ValidationMode.Type,
    roundTrip: RoundTrip,
    implString: Option[String]) = {

    val nBits = optLengthLimitInBits.get
    val dataToParse = optDataToParse.get

    (optExpectedInfoset, optExpectedErrors) match {
      case (Some(_), None) => {
        compileResult.left.foreach { diags => throw TDMLException(diags, implString) }
        compileResult.right.foreach {
          case (_, proc) => {
            processor = proc
            runParseExpectSuccess(dataToParse, nBits, optExpectedWarnings, optExpectedValidationErrors, validationMode, roundTrip, implString)
          }
        }
      }

      case (None, Some(errors)) => {
        compileResult.left.foreach { diags =>
          checkDiagnosticMessages(diags, errors, optExpectedWarnings, implString)
        }
        compileResult.right.foreach {
          case (_, proc) => {
            processor = proc
            runParseExpectErrors(dataToParse, nBits, optExpectedErrors.get, optExpectedWarnings, optExpectedValidationErrors, validationMode, implString)
          }
        }
      }

      case _ => Assert.invariantFailed("Should be Some None, or None Some only.")
    }
  }

  def runParseExpectErrors(
    dataToParse: InputStream,
    lengthLimitInBits: Long,
    errors: ExpectedErrors,
    optWarnings: Option[ExpectedWarnings],
    optValidationErrors: Option[ExpectedValidationErrors],
    validationMode: ValidationMode.Type,
    implString: Option[String]): Unit = {

    val optExtVarDiag =
      try {
        processor = processor.withExternalDFDLVariables(externalVarBindings)
        None
      } catch {
        case diag: Diagnostic =>
          Some(diag)
      }

    val (parseResult, diagnostics, isError) = {
      if (processor.isError || optExtVarDiag.isDefined) {
        val noParseResult: TDMLParseResult = null
        val allDiags: Seq[Diagnostic] = processor.getDiagnostics ++ optExtVarDiag
        (noParseResult, allDiags , true)
      } else {
        val actual =
          try {
            processor.parse(dataToParse, lengthLimitInBits)
          } catch {
            case t: Throwable => toss(t, implString)
          }

        // we should never need blobs if we're expecting an error even if we
        // don't get errors. So clean them up immediately
        actual.getBlobPaths.foreach { blobPath =>
          Files.delete(blobPath)
        }

        val isErr: Boolean =
          if (actual.isProcessingError) true
          else {
            //
            // didn't get an error.
            // If we're not at the end of data, synthesize an error for left-over-data
            //
            val loc: DataLocation = actual.currentLocation

            if (!loc.isAtEnd) {
              val leftOverMsg = "Left over data. Consumed %s bit(s) with %s bit(s) remaining.".format(loc.bitPos1b - 1, lengthLimitInBits - (loc.bitPos1b - 1))
              actual.addDiagnostic(new TDMLDiagnostic(leftOverMsg, implString))
              true
            } else {
              false
            }
          }

        val diagnostics = processor.getDiagnostics ++ actual.getDiagnostics
        (actual, diagnostics, isErr)
      }
    }
    if (!isError) {
      throw TDMLException("Expected error. Didn't get one. Actual result was\n" + parseResult.getResult.toString, implString)
    }

    checkDiagnosticMessages(diagnostics, errors, optWarnings, implString)
  }

  /**
   * Returns outputter containing the parse infoset. Throws if there isn't one.
   */
  private def doParseExpectSuccess(
    testData: Array[Byte],
    testInfoset: Infoset,
    lengthLimitInBits: Long,
    implString: Option[String]): TDMLParseResult = {

    val testDataLength = lengthLimitInBits

    val optExtVarDiag =
      try {
        processor = processor.withExternalDFDLVariables(externalVarBindings)
        None
      } catch {
        case diag: Diagnostic =>
          Some(diag)
      }
    if (optExtVarDiag.isDefined) {
      toss(optExtVarDiag.get, implString)
    } else {
      val actual = processor.parse(new ByteArrayInputStream(testData), testDataLength)

      if (actual.isProcessingError) {
        // Means there was an error, not just warnings.
        val diagObjs = actual.getDiagnostics
        if (diagObjs.length == 1) throw TDMLException(diagObjs.head, implString)
        val diags = actual.getDiagnostics.map(_.getMessage()).mkString("\n")
        throw TDMLException(diags, implString)
      }
      actual
    }
  }

  private def verifyLeftOverData(actual: TDMLParseResult, lengthLimitInBits: Long, implString: Option[String]) = {
    val loc: DataLocation = actual.currentLocation

    val leftOverException = if (!loc.isAtEnd) {
      val leftOverMsg = "Left over data. Consumed %s bit(s) with %s bit(s) remaining.".format(
        loc.bitPos1b - 1, lengthLimitInBits - (loc.bitPos1b - 1))
      Some(TDMLException(leftOverMsg, implString))
    } else None

    leftOverException.map {
      throw _
    } // if we get here, throw the left over data exception.
  }

  private def verifyParseResults(
    actual: TDMLParseResult,
    testInfoset: Infoset,
    implString: Option[String]) = {
    val resultXmlNode = actual.getResult
    VerifyTestCase.verifyParserTestData(resultXmlNode, testInfoset, implString)

    (shouldValidate, expectsValidationError) match {
      case (true, true) => {
        VerifyTestCase.verifyAllDiagnosticsFound(actual.getDiagnostics, optExpectedValidationErrors, implString) // verify all validation errors were found
        Assert.invariant(actual.isValidationError)
      }
      case (true, false) => {
        VerifyTestCase.verifyNoValidationErrorsFound(actual, implString) // Verify no validation errors from parser
        Assert.invariant(!actual.isValidationError)
      }
      case (false, true) => throw TDMLException("Test case invalid. Validation is off but the test expects an error.", implString)
      case (false, false) => // Nothing to do here.
    }

    val allDiags = processor.getDiagnostics ++ actual.getDiagnostics
    if (!isCrossTest(implString.get) ||
      parent.shouldDoWarningComparisonOnCrossTests)
      VerifyTestCase.verifyAllDiagnosticsFound(allDiags, optExpectedWarnings, implString)
  }

  /**
   * Do an unparse. Report any actual diagnostics, but don't compare
   * the unparsed data output.
   */
  private def doOnePassRoundTripUnparseExpectSuccess(
    outStream: OutputStream, // stream where unparsed data is written
    parseResult: TDMLParseResult, // result from prior parse.
    implString: Option[String]): TDMLUnparseResult = {

    // in a one pass round trip, the parse test is entirely over, and the infoset comparison
    // MUST have succeeded. We now just are trying to unparse and we must get back
    // exactly the original input data.

    val unparseResult = processor.unparse(parseResult, outStream)
    if (unparseResult.isProcessingError) {
      val diagObjs = processor.getDiagnostics ++ unparseResult.getDiagnostics
      if (diagObjs.length == 1) throw TDMLException(diagObjs.head, implString)
      throw TDMLException(diagObjs, implString)
    }
    unparseResult
  }

  private def doTwoPassRoundTripExpectSuccess(
    implString: Option[String],
    parseResult: TDMLParseResult,
    firstParseTestData: Array[Byte],
    testInfoset: Infoset,
    passesLabel: String = TwoPassRoundTrip.propValueName): (TDMLParseResult, Array[Byte], Long) = {
    val outStream = new java.io.ByteArrayOutputStream()
    val unparseResult: TDMLUnparseResult = doOnePassRoundTripUnparseExpectSuccess(outStream, parseResult, implString)
    val isUnparseOutputDataMatching =
      try {
        VerifyTestCase.verifyUnparserTestData(new ByteArrayInputStream(firstParseTestData), outStream, implString)
        true
      } catch {
        case _: TDMLException => {
          false
          //
          // We got the failure we expect for a two-pass test on the unparse.
          //
        }
      }
    if (isUnparseOutputDataMatching) {
      val msg = ("Expected data from first unparse of %s to NOT match original input, but it did match." +
        "\nShould this really be a %s test?").format(passesLabel, passesLabel)
      throw TDMLException(msg, implString)
    }
    // Try parse again, consuming the canonicalized data from the prior unparse.
    val reParseTestData = outStream.toByteArray
    val reParseTestDataLength = unparseResult.bitPos0b
    if (reParseTestDataLength >= 0) {
      // verify enough bytes for the bits.
      val fullBytesNeeded = (reParseTestDataLength + 7) / 8
      if (reParseTestData.length != fullBytesNeeded) {
        throw TDMLException("Unparse result data was was %d bytes, but the result length (%d bits) requires %d bytes.".format(
          reParseTestData.length, reParseTestDataLength, fullBytesNeeded), implString)
      }
    }
    val actual = doParseExpectSuccess(reParseTestData, testInfoset, reParseTestDataLength, implString)
    (actual, reParseTestData, reParseTestDataLength)
  }

  def runParseExpectSuccess(
    dataToParse: InputStream,
    lengthLimitInBits: Long,
    warnings: Option[ExpectedWarnings],
    validationErrors: Option[ExpectedValidationErrors],
    validationMode: ValidationMode.Type,
    roundTripArg: RoundTrip,
    implString: Option[String]): Unit = {

    val roundTrip = roundTripArg // change to OnePassRoundTrip to force all parse tests to round trip (to see which fail to round trip)

    if (processor.isError) {
      val diagObjs = processor.getDiagnostics
      if (diagObjs.length == 1) throw diagObjs.head
      throw TDMLException(diagObjs, implString)
    }
    processor = processor.withValidationMode(validationMode)

    val firstParseTestData = IOUtils.toByteArray(dataToParse)
    val testInfoset = optExpectedInfoset.get

    val firstParseResult = doParseExpectSuccess(firstParseTestData, testInfoset, lengthLimitInBits, implString)

    roundTrip match {
      case NoRoundTrip | OnePassRoundTrip => {
        verifyParseResults(firstParseResult, testInfoset, implString)
        verifyLeftOverData(firstParseResult, lengthLimitInBits, implString)
      }
      case TwoPassRoundTrip => {
        // don't compare first parse pass. Because it may or may not match.
        //
        // There's really two different kinds of tests here that we're not distinguishing, which
        // are
        // (a) those where the infoset is what is expected, but doesn't unparse to the same
        // data as original, which then still reparses to that same infoset.
        // (b) those where the infoset isn't what is expected, but unparses to something which
        // then parses to what is expected.
      }
      case ThreePassRoundTrip => {
        //
        // Arguably, there are two different kinds of tests here that we've selected just
        // one of:
        // (a) the original infoset is NOT a match. The later steady-state infoset
        // differs from this original infoset and matches the expected
        // (b) the original infoset is a match, and reflects steady state. Just the unparse from it
        // doesn't match the original input data stream. But reparsing that data stream produces the same
        // infoset which demonstrates steady state.
        //
        // We can tell these apart, by just remembering whether this infoset matches or not
        // rather than reporting a problem here. If after the reparse we get the expected
        // infoset also, then this was NOT an error, and having the same infoset indicates
        // steady state. If after the reparse we get the expected
        // infoset but this one was NOT matching the expected, then we must unparse again to be sure we
        // are at steady state.
        //
        // Instead we choose to just not check this initial infoset at all, and always behave
        // as if it was NOT a match.
        //
        //        val isFirstParseInfosetMatching =
        //          try {
        //            verifyParseResults(processor, firstParseResult, testInfoset, implString)
        //            verifyLeftOverData(firstParseResult, lengthLimitInBits, implString)
        //            true
        //          } catch {
        //            case t: TDMLException =>
        //              false
        //          }
        //        if (isFirstParseInfosetMatching) {
        //          val msg = ("Expected infoset from first parse of data to NOT match expected infoset, but it did match." +
        //            "\nShould this really be a %s test?").format(roundTrip.propValueName)
        //          throw TDMLException(msg, implString)
        //        }
      }
    }

    // if we get here, the parse test passed. If we don't get here then some exception was
    // thrown either during the run of the test or during the comparison.

    roundTrip match {
      case NoRoundTrip => {
        // done. Do nothing else.

        // Done with the first parse result, safe to clean up blobs if there
        // was success. This won't get called on failure, which is fine--leave
        // blobs around for debugging
        firstParseResult.getBlobPaths.foreach { blobPath =>
          Files.delete(blobPath)
        }
      }
      case OnePassRoundTrip => {
        val outStream = new java.io.ByteArrayOutputStream()

        doOnePassRoundTripUnparseExpectSuccess(outStream, firstParseResult, implString)

        // It has to work, as this is one pass round trip. We expect it to unparse
        // directly back to the original input form.

        VerifyTestCase.verifyUnparserTestData(new ByteArrayInputStream(firstParseTestData), outStream, implString)

        // Done with the first parse result, safe to clean up blobs if there
        // was success. This won't get called on failure, which is fine--leave
        // blobs around for debugging
        firstParseResult.getBlobPaths.foreach { blobPath =>
          Files.delete(blobPath)
        }
      }
      case TwoPassRoundTrip => {
        //
        // In two-pass, the unparse comparison of data from first unparse
        // to the original input data MUST fail.
        // We need to unparse, then parse again to have the comparison work
        // thereby showing that while the output data is different, it is
        // equivalent in that re-parsing that data produces the same infoset
        // that parsing the original data did.
        //

        val (actual, _, reParseTestDataLength) =
          doTwoPassRoundTripExpectSuccess(
            implString,
            firstParseResult,
            firstParseTestData,
            testInfoset)
        verifyParseResults(actual, testInfoset, implString)
        verifyLeftOverData(actual, reParseTestDataLength, implString)
        // if it doesn't pass, it will throw out of here.

        // Done with the first and second parse resultrs, safe to clean up
        // blobs if there was success. This won't get called on failure, which
        // is fine--leave blobs around for debugging
        firstParseResult.getBlobPaths.foreach { blobPath =>
          Files.delete(blobPath)
        }
        actual.getBlobPaths.foreach { blobPath =>
          Files.delete(blobPath)
        }
      }
      case ThreePassRoundTrip => {
        //
        // In three-pass, the unparse comparison of data from first unparse
        // to the original input data MUST fail.
        // We need to unparse, then parse again and the infoset comparison
        // must ALSO fail.
        // Then we unparse again, and get same data as the first unparse.
        // At that point we're in steady state.
        //
        // This mode is needed due to asymmetric separator suppression policies
        // like anyEmpty which allow a separator for a zero-length optional element to
        // appear in the data, but when unparsing will not output this separator,
        // so when reparsed, the infoset won't have the element.
        //
        val (secondParseResult, reParseTestData, reParseTestDataLength) =
          doTwoPassRoundTripExpectSuccess(
            implString,
            firstParseResult,
            firstParseTestData,
            testInfoset,
            ThreePassRoundTrip.propValueName)
        //
        // The infoset from the reparse should be the final
        // steady state infoset, which is what the test case
        // should put as the expected infoset.
        //
        // So we just verify normally here.
        //
        verifyParseResults(secondParseResult, testInfoset, implString)
        verifyLeftOverData(secondParseResult, reParseTestDataLength, implString)
        //
        // So now we do the third pass unparse and compare this output with the
        // first unparsed output.
        //
        // We get to reuse the one-pass code here.
        val thirdPassOutStream = new java.io.ByteArrayOutputStream()
        doOnePassRoundTripUnparseExpectSuccess(thirdPassOutStream, firstParseResult, implString)
        VerifyTestCase.verifyUnparserTestData(new ByteArrayInputStream(reParseTestData), thirdPassOutStream, implString)

        // Done with the first parse result and second parse results. Safe to
        // clean up blobs if there was success. Leave them around for debugging
        // if there was a failure
        firstParseResult.getBlobPaths.foreach { blobPath =>
          Files.delete(blobPath)
        }
        secondParseResult.getBlobPaths.foreach { blobPath =>
          Files.delete(blobPath)
        }
      }
    }

  }
}

case class UnparserTestCase(ptc: NodeSeq, parentArg: DFDLTestSuite)
  extends TestCase(ptc, parentArg) {

  lazy val optInputInfoset = this.optExpectedOrInputInfoset

  lazy val inputInfoset = optInputInfoset.get

  def runProcessor(
    compileResult: TDML.CompileResult,
    optExpectedData: Option[InputStream],
    optNBits: Option[Long],
    optErrors: Option[ExpectedErrors],
    optWarnings: Option[ExpectedWarnings],
    optValidationErrors: Option[ExpectedValidationErrors],
    validationMode: ValidationMode.Type,
    roundTrip: RoundTrip,
    implString: Option[String]) = {

    (optExpectedData, optErrors) match {
      case (Some(expectedData), None) => {
        compileResult.left.foreach { diags => throw TDMLException(diags, implString) }
        compileResult.right.foreach {
          case (warnings, proc) => {
            processor = proc
            runUnparserExpectSuccess(expectedData, optWarnings, roundTrip, implString)
          }
        }
      }

      case (_, Some(errors)) => {
        compileResult.left.foreach { diags =>
          checkDiagnosticMessages(diags, errors, optWarnings, implString)
        }
        compileResult.right.foreach {
          case (_, proc) => {
            processor = proc
            runUnparserExpectErrors(optExpectedData, errors, optWarnings, implString)
          }
        }
      }
      case _ => Assert.impossibleCase()
    }

  }

  def runUnparserExpectSuccess(
    expectedData: InputStream,
    optWarnings: Option[ExpectedWarnings],
    roundTrip: RoundTrip,
    implString: Option[String]): Unit = {

    Assert.usage(roundTrip ne TwoPassRoundTrip) // not supported for unparser test cases.

    val infosetXML: Node = {
      if (isCrossTest(implString.get)) {
        stripLineColInfo(this.inputInfoset.dfdlInfoset.contents)
      } else {
        this.inputInfoset.dfdlInfoset.contents
      }
    }

    val outStream = new java.io.ByteArrayOutputStream()
    val actual =
      try {
        processor = processor.withExternalDFDLVariables(externalVarBindings)
        processor.unparse(infosetXML, outStream)
      } catch {
        case t: Throwable => toss(t, implString)
      }
    if (actual.isProcessingError)
      throw TDMLException(actual.getDiagnostics, implString)

    //
    // Test that we are getting the number of full bytes needed.
    val testData = outStream.toByteArray
    val testDataLength = actual.finalBitPos0b
    if (testDataLength >= 0) {
      val fullBytesNeeded = (testDataLength + 7) / 8
      if (testData.length != fullBytesNeeded) {
        throw TDMLException("Unparse result data was was %d bytes, but the result length (%d bits) requires %d bytes.".format(
          testData.length, testDataLength, fullBytesNeeded), implString)
      }
    }

    if (actual.isScannable) {
      // all textual in one encoding, so we can do display of results
      // in terms of text so the user can see what is going on.
      VerifyTestCase.verifyTextData(expectedData, outStream, actual.encodingName, implString)
    } else {
      // data is not all textual, or in mixture of encodings
      // So while we can still use the encoding as a heuristic,
      // we will need to treat as Hex bytes as well.
      VerifyTestCase.verifyBinaryOrMixedData(expectedData, outStream, implString)
    }
    val allDiags = actual.getDiagnostics ++ processor.getDiagnostics
    if (!isCrossTest(implString.get) ||
      parent.shouldDoWarningComparisonOnCrossTests)
      VerifyTestCase.verifyAllDiagnosticsFound(allDiags, optWarnings, implString)

    if (roundTrip eq OnePassRoundTrip) {

      val parseActual =
        try {
          processor.parse(new ByteArrayInputStream(outStream.toByteArray), testDataLength)
        } catch {
          case t: Throwable => toss(t, implString)
        }

      if (parseActual.isProcessingError) {
        // Means there was an error, not just warnings.
        val diagObjs = parseActual.getDiagnostics
        if (diagObjs.length == 1) throw diagObjs.head
        val diags = parseActual.getDiagnostics.map(_.getMessage()).mkString("\n")
        throw TDMLException(diags, implString)
      }
      val loc: DataLocation = parseActual.currentLocation

      val leftOverException = if (!loc.isAtEnd) {
        val leftOverMsg = "Left over data. Consumed %s bit(s) with %s bit(s) remaining.".format(loc.bitPos1b - 1, testDataLength - (loc.bitPos1b - 1))
        println(leftOverMsg)
        Some(TDMLException(leftOverMsg, implString))
      } else None

      val xmlNode = parseActual.getResult
      VerifyTestCase.verifyParserTestData(xmlNode, inputInfoset, implString)
      if (!isCrossTest(implString.get) ||
        parent.shouldDoWarningComparisonOnCrossTests)
        VerifyTestCase.verifyAllDiagnosticsFound(actual.getDiagnostics, optWarnings, implString)

      (shouldValidate, expectsValidationError) match {
        case (true, true) => {
          VerifyTestCase.verifyAllDiagnosticsFound(actual.getDiagnostics, optExpectedValidationErrors, implString) // verify all validation errors were found
          Assert.invariant(actual.isValidationError)
        }
        case (true, false) => {
          VerifyTestCase.verifyNoValidationErrorsFound(actual, implString) // Verify no validation errors from parser
          Assert.invariant(!actual.isValidationError)
        }
        case (false, true) => throw TDMLException("Test case invalid. Validation is off but the test expects an error.", implString)
        case (false, false) => // Nothing to do here.
      }

      leftOverException.map {
        throw _
      } // if we get here, throw the left over data exception.

      // Done with the parse results, safe to clean up blobs if there was
      // success. This won't get called on failure, which is fine--leave blobs
      // around for debugging
      parseActual.getBlobPaths.foreach { blobPath =>
        Files.delete(blobPath)
      }

    }
  }

  def runUnparserExpectErrors(
    optExpectedData: Option[InputStream],
    errors: ExpectedErrors,
    optWarnings: Option[ExpectedWarnings],
    implString: Option[String]): Unit = {

    val optExtVarDiag =
      try {
        processor = processor.withExternalDFDLVariables(externalVarBindings)
        None
      } catch {
        case diag: Diagnostic =>
          Some(diag)
      }

    val diagnostics = {
      if (processor.isError || optExtVarDiag.isDefined)
        processor.getDiagnostics ++ optExtVarDiag
      else {
        val outStream = new java.io.ByteArrayOutputStream()
        val infosetXML = {
          if (optInputInfoset.isEmpty)
            throw TDMLException("No infoset specified, but one is required to run the test.", implString)
          if (isCrossTest(implString.get)) {
            stripLineColInfo(inputInfoset.dfdlInfoset.contents)
          } else {
            inputInfoset.dfdlInfoset.contents
          }
        }
        val actual =
          try {
            processor.unparse(infosetXML, outStream)
          } catch {
            case t: Throwable => toss(t, implString)
          }

        val dataErrors = {
          optExpectedData.flatMap { data =>
            try {
              if (actual.isScannable) {
                // all textual in one encoding, so we can do display of results
                // in terms of text so the user can see what is going on.
                VerifyTestCase.verifyTextData(data, outStream, actual.encodingName, implString)
              } else {
                // data is not all textual, or in mixture of encodings
                // So while we can still use the encoding as a heuristic,
                // we will need to treat as Hex bytes as well.
                VerifyTestCase.verifyBinaryOrMixedData(data, outStream, implString)
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

    if (diagnostics.isEmpty)
      throw TDMLException("Unparser test expected error. Didn't get one.", implString)
    checkDiagnosticMessages(diagnostics, errors, optWarnings, implString)
  }
}

object VerifyTestCase {
  def verifyParserTestData(actual: Node, infoset: Infoset, implString: Option[String]): Unit = {

    val expected = infoset.contents

    try {
      XMLUtils.compareAndReport(expected, actual)
    } catch {
      case e: Exception =>
        throw TDMLException(e, implString)
    }
  }

  def verifyUnparserTestData(expectedData: InputStream, actualOutStream: java.io.ByteArrayOutputStream,
    implString: Option[String]): Unit = {
    val actualBytes = actualOutStream.toByteArray

    val expectedBytes = IOUtils.toByteArray(expectedData)
    // example data was of size 0 (could not read anything). We're not supposed to get any actual data.
    if (expectedBytes.isEmpty && actualBytes.nonEmpty) {
      throw TDMLException("Unexpected data was created.", implString)
    }

    val readCount = expectedBytes.length

    lazy val actualAsString = Misc.remapBytesToStringOfVisibleGlyphs(actualBytes)
    lazy val expectedAsString = Misc.remapBytesToStringOfVisibleGlyphs(expectedBytes)
    if (actualBytes.length != readCount) {
      throw TDMLException("Output data length %s for '%s' doesn't match expected value %s for '%s'.".format(
        actualBytes.length, actualAsString,
        readCount, expectedAsString), implString)
    }

    val pairs = expectedBytes zip actualBytes zip Stream.from(1)
    pairs.foreach {
      case ((expected, actual), index) =>
        if (expected != actual) {
          val msg = ("Unparsed data differs at byte %d. Expected 0x%02x. Actual was 0x%02x.\n" +
            "Data (as strings) was\n(actual) '%s'\n(expected) '%s'.").format(index, expected, actual, actualAsString, expectedAsString)
          throw TDMLException(msg, implString)
        }
    }
  }

  def verifyAllDiagnosticsFound(actualDiags: Seq[Throwable], expectedDiags: Option[ErrorWarningBase],
    implString: Option[String]) = {

    val actualDiagMsgs = actualDiags.map {
      _.toString
    }
    val expectedDiagMsgs = expectedDiags.map {
      _.messages
    }.getOrElse(Nil)

    if (expectedDiags.isDefined && actualDiags.isEmpty) {
      throw TDMLException(
        """"Diagnostic message(s) were expected but not found."""" +
          "\n" +
          """Expected: """ + expectedDiagMsgs.mkString("\n") +
          (if (actualDiagMsgs.isEmpty)
            "\n No diagnostic messages were issued."
          else
            "\n The actual diagnostics messages were: " + actualDiagMsgs.mkString("\n")),
        implString)
    }

    // must find each expected warning message within some actual warning message.
    expectedDiagMsgs.foreach {
      expected =>
        {
          val wasFound = actualDiagMsgs.exists {
            actual => actual.toLowerCase.contains(expected.toLowerCase)
          }
          if (!wasFound) {
            throw TDMLException(
              """Did not find diagnostic message """" +
                expected +
                """" in any of the actual diagnostic messages: """ + "\n" +
                actualDiagMsgs.mkString("\n"),
              implString)
          }
        }
    }
  }

  def verifyNoValidationErrorsFound(actual: TDMLResult, implString: Option[String]) = {
    val actualDiags = actual.getDiagnostics.filter(d => d.isValidation)
    if (actualDiags.nonEmpty) {
      val actualDiagMsgs = actualDiags.map {
        _.toString()
      }
      throw TDMLException("Validation errors found where none were expected by the test case.\n" +
        actualDiagMsgs.mkString("\n"), implString)
    }
  }

  def decodeDataToString(decoder: BitsCharsetDecoder, bytes: Array[Byte]): String = {
    class FormatInfoForTDMLDecode extends FormatInfo {
      // Decoders don't actually need much of the formatinfo. Byte size
      // decoders don't ever refernce byte/bitOrder, they just read raw bytes
      // from the underlying data stream. And non-byte size encoders do care,
      // but only support bigEndian/LSBF. There should never be encoding
      // errors, so the error policy doesn't really matter.
      override def byteOrder: ByteOrder = ByteOrder.BigEndian

      override def bitOrder: BitOrder = BitOrder.LeastSignificantBitFirst

      override def encodingErrorPolicy: EncodingErrorPolicy = EncodingErrorPolicy.Replace

      private def doNotUse = Assert.usageError("Should not be used")

      override def encoder: BitsCharsetEncoder = doNotUse

      override def decoder: BitsCharsetDecoder = doNotUse

      override def fillByte: Byte = doNotUse

      override def binaryFloatRep: BinaryFloatRep = doNotUse

      override def maybeCharWidthInBits: MaybeInt = doNotUse

      override def maybeUTF16Width: Maybe[UTF16Width] = doNotUse

      override def encodingMandatoryAlignmentInBits: Int = doNotUse

      override def tunable: DaffodilTunables = doNotUse

      override def regexMatchBuffer: CharBuffer = doNotUse

      override def regexMatchBitPositionBuffer: LongBuffer = doNotUse
    }

    val dis = InputSourceDataInputStream(bytes)
    val finfo = new FormatInfoForTDMLDecode
    val cb = CharBuffer.allocate(256)
    val sb = new StringBuilder(256)
    while ({
      val numDecoded = decoder.decode(dis, finfo, cb); numDecoded > 0
    }) {
      cb.flip()
      sb.append(cb)
      cb.clear()
    }
    sb.toString
  }

  def verifyTextData(expectedData: InputStream, actualOutStream: java.io.ByteArrayOutputStream, encodingName: String,
    implString: Option[String]): Unit = {
    // Getting this decoder and decoding the bytes to text this way is
    // necessary, as opposed to toString(encodingName), because it is possible
    // that encodingName is a custom DFDL specific decoder (e.g. 7-bit ASCII)
    // that Java does not know about.
    val decoder = CharsetUtils.getCharset(encodingName).newDecoder()
    val actualBytes = actualOutStream.toByteArray
    val expectedBytes = IOUtils.toByteArray(expectedData)

    if (actualBytes == expectedBytes) {
      return
    }

    // input and output weren't the same, decode the data for helpful output
    val actualText = decodeDataToString(decoder, actualBytes)
    val expectedText = decodeDataToString(decoder, expectedBytes)
    expectedData.close()
    if (expectedText.length == 0) {
      // example data was of size 0 (could not read anything). We're not supposed to get any actual data.
      if (actualText.length > 0) {
        throw TDMLException("Unexpected data '%s' was created.".format(actualText), implString)
      }
      return // we're done. Nothing equals nothing.
    }

    // compare expected data to what was output.
    val maxTextCharsToShow = 100

    def trimToMax(str: String) = {
      if (str.length <= maxTextCharsToShow) str
      else str.substring(0, maxTextCharsToShow) + "..."
    }

    val actualCharsToShow = if (actualText.length == 0) "" else " for '" + trimToMax(actualText) + "'"
    val expectedCharsToShow = if (expectedText.length == 0) "" else " for '" + trimToMax(expectedText) + "'"
    if (actualText.length != expectedText.length) {
      val actualCharsToShow = if (actualText.length == 0) "" else " for '" + trimToMax(actualText) + "'"
      val expectedCharsToShow = if (expectedText.length == 0) "" else " for '" + trimToMax(expectedText) + "'"
      throw TDMLException("output data length " + actualText.length + actualCharsToShow +
        " doesn't match expected length " + expectedText.length + expectedCharsToShow, implString)
    }

    val pairs = expectedText.toSeq zip actualText.toSeq zip Stream.from(1)
    pairs.foreach {
      case ((expected, actual), index) =>
        if (expected != actual) {
          val msg = "Unparsed data differs at character %d. Expected '%s'. Actual was '%s'. Expected data %s, actual data %s".format(
            index, expected, actual, expectedCharsToShow, actualCharsToShow)
          throw TDMLException(msg, implString)
        }
    }
  }

  private val cs8859 = JavaCharset.forName("iso-8859-1")

  def verifyBinaryOrMixedData(expectedData: InputStream, actualOutStream: java.io.ByteArrayOutputStream,
    implString: Option[String]): Unit = {
    val actualBytes = actualOutStream.toByteArray
    lazy val actual8859String = cs8859.newDecoder().decode(ByteBuffer.wrap(actualBytes)).toString()
    lazy val displayableActual = Misc.remapControlsAndLineEndingsToVisibleGlyphs(actual8859String)

    val expectedBytes = IOUtils.toByteArray(expectedData)
    lazy val expected8859String = cs8859.newDecoder().decode(ByteBuffer.wrap(expectedBytes)).toString()
    lazy val displayableExpected = Misc.remapControlsAndLineEndingsToVisibleGlyphs(expected8859String)

    lazy val expectedAndActualDisplayStrings = "\n" +
      "Excected data (as iso8859-1): " + displayableExpected + "\n" +
      "Actual data   (as iso8859-1): " + displayableActual + "\n" +
      "Expected data (as hex): " + Misc.bytes2Hex(expectedBytes) + "\n" +
      "Actual data   (as hex): " + Misc.bytes2Hex(actualBytes)

    val readCount = expectedBytes.length
    expectedData.close()
    if (readCount == 0) {
      // example data was of size 0 (could not read anything). We're not supposed to get any actual data.
      if (actualBytes.nonEmpty) {
        throw TDMLException("Unexpected data was created: '" + displayableActual + "'", implString)
      }
      return // we're done. Nothing equals nothing.
    }

    // compare expected data to what was output.
    if (actualBytes.length != readCount) {
      val bytesToShow = if (actualBytes.isEmpty) "" else " for " + Misc.bytes2Hex(actualBytes)
      throw TDMLException("output data length " + actualBytes.length + bytesToShow +
        " doesn't match expected length " + readCount + " for " + Misc.bytes2Hex(expectedBytes) +
        expectedAndActualDisplayStrings, implString)
    }

    val pairs = expectedBytes zip actualBytes zip Stream.from(1)
    pairs.foreach {
      case ((expected, actual), index) =>
        if (expected != actual) {
          val msg = "Unparsed data differs at byte %d. Expected 0x%02x. Actual was 0x%02x.".format(index, expected, actual) +
            expectedAndActualDisplayStrings
          throw TDMLException(msg, implString)
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

  val importIncludes = globalImports ++ globalIncludes
  val dfdlTopLevels = defineFormats ++ defaultFormats ++ defineVariables ++ defineEscapeSchemes
  val xsdTopLevels = globalElementDecls ++ globalSimpleTypeDefs ++
    globalComplexTypeDefs ++ globalGroupDefs
  val fileName = parent.ts.attribute(XMLUtils.INT_NS, XMLUtils.FILE_ATTRIBUTE_NAME) match {
    case Some(seqNodes) => seqNodes.toString
    case None => ""
  }
  lazy val xsdSchema =
    SchemaUtils.dfdlTestSchema(importIncludes, dfdlTopLevels, xsdTopLevels, fileName = fileName, schemaScope = xml.scope, elementFormDefault = elementFormDefault)
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
  if (actualDocumentPartElementChildren.nonEmpty) {
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
      if (actualDocumentPartElementChildren.nonEmpty) actualDocumentPartElementChildren
      else List(new TextDocumentPart(<documentPart type="text">{ children }</documentPart>, this))
    udp
  }

  private lazy val dataDocumentParts = {
    val dps = unCheckedDocumentParts.collect { case dp: DataDocumentPart => dp }
    dps
  }

  private lazy val fileParts = {
    val fps = unCheckedDocumentParts.collect { case fp: FileDocumentPart => fp }
    Assert.usage(
      fps.isEmpty ||
        (fps.length == 1 && dataDocumentParts.isEmpty),
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
  private def checkForBadBitOrderTransitions(dps: Seq[DataDocumentPart]): Unit = {
    if (dps.length <= 1) return
    // these are the total lengths BEFORE the component
    val lengths = dps.map {
      _.lengthInBits
    }
    val cumulativeDocumentPartLengthsInBits = lengths.scanLeft(0) { case (sum, num) => sum + num }
    val docPartBitOrders = dps.map {
      _.partBitOrder
    }
    val transitions = docPartBitOrders zip docPartBitOrders.tail zip cumulativeDocumentPartLengthsInBits.tail zip dps
    transitions.foreach {
      case (((bitOrderPrior, bitOrderHere), cumulativeLength), docPart) => {
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
    val bitsFromParts = dataDocumentParts.map {
      _.contentAsBits
    }
    val allPartsBits = documentBitOrder match {
      case MSBFirst => bitsFromParts.flatten
      case LSBFirst => {
        val x = bitsFromParts.map {
          _.map {
            _.reverse
          }
        }
        val rtlBits = x.flatten.mkString.reverse
        val ltrBits = rtlBits.reverse.sliding(8, 8).map {
          _.reverse
        }.toList
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
      val dp = documentParts.head.asInstanceOf[FileDocumentPart]
      val input = dp.fileDataInput
      input
    } else {
      // assemble the input from the various pieces, having lowered
      // everything to bits.
      val bytes = documentBytes
      new java.io.ByteArrayInputStream(bytes)
    }
  }

  /**
   * data coming from a file?
   */
  val isDPFile = {
    val res = documentParts.nonEmpty &&
      documentParts.head.isInstanceOf[FileDocumentPart]
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
      case bitEnc: BitsCharsetNonByteSize => {
        (bitEnc.requiredBitOrder, partBitOrder) match {
          case (BitOrder.LeastSignificantBitFirst, LSBFirst) => //ok
          case (BitOrder.MostSignificantBitFirst, MSBFirst) => //ok
          case (BitOrder.LeastSignificantBitFirst, _) => err(upperName, LSBFirst)
          case _ => err(upperName, MSBFirst)
        }
      }
      case null => Assert.usageError("Unsupported encoding: " + encodingName + ". Supported encodings: " + CharsetUtils.supportedEncodingsString)
      case _ => // ok
    }
    val enc = cs.newEncoder()
    enc
  }

  lazy val textContentWithoutEntities = {
    if (replaceDFDLEntities) {
      try {
        EntityReplacer {
          _.replaceAll(partRawContent)
        }
      } catch {
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
    val bb = ByteBuffer.allocate(4 * s.length)
    val cb = CharBuffer.wrap(s)
    val coderResult = encoder.encode(cb, bb, true)
    Assert.invariant(coderResult == CoderResult.UNDERFLOW)
    bb.flip()
    val res = (0 to bb.limit() - 1).map {
      bb.get(_)
    }
    val enc = encoder.asInstanceOf[BitsCharsetNonByteSizeEncoder]
    val nBits = s.length * enc.bitsCharset.bitWidthOfACodeUnit
    val bitStrings = res.map { b => (b & 0xFF).toBinaryString.reverse.padTo(8, '0').reverse }.toList
    val allBits = bitStrings.reverse.mkString.takeRight(nBits)
    val bitChunks = allBits.reverse.sliding(byteSize, byteSize).map {
      _.reverse
    }.toList
    bitChunks
  }

  def encodeWith8BitEncoder(s: String): Seq[String] = {
    val bb = ByteBuffer.allocate(4 * s.length)
    val cb = CharBuffer.wrap(s)
    val coderResult = encoder.encode(cb, bb, true)
    Assert.invariant(coderResult == CoderResult.UNDERFLOW)
    bb.flip()
    val res = (0 to bb.limit() - 1).map {
      bb.get(_)
    }
    val bitStrings = res.map { b => (b & 0xFF).toBinaryString.reverse.padTo(8, '0').reverse }.toList
    bitStrings
  }

  lazy val dataBits = {
    val bytesAsStrings =
      encoder.bitsCharset match {
        case nbs: BitsCharsetNonByteSize =>
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
        val rtlDigits = hexDigits.reverse.sliding(2, 2).toList.map {
          _.reverse
        }
        rtlDigits
      }
    }
    val bits = hexBytes.map {
      hex2Bits(_)
    }
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
        bitDigits.reverse.sliding(8, 8).toList.map {
          _.reverse
        }
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

  /**
   * Must be def, so that if you run a test repeatedly it will supply the data
   * again every time.
   */
  def fileDataInput = {
    val is = url.openStream()
    is
  }

}

/**
 * Base class for all document parts that contain data directly expressed in the XML
 */
sealed abstract class DataDocumentPart(part: Node, parent: Document)
  extends DocumentPart(part, parent) {

  def dataBits: Seq[String]

  lazy val lengthInBits = dataBits.map {
    _.length
  } sum
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
    Assert.usage(
      !isInstanceOf[FileDocumentPart],
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
    Assert.usage(
      this.isInstanceOf[ByteDocumentPart] || this.isInstanceOf[BitsDocumentPart],
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
      case scala.xml.Comment(_) => None
      case scala.xml.EntityRef(_) => Some(childNode)
      case _: scala.xml.Atom[_] => Some(childNode) // Things like &lt; come through as this. Should be EntityRef
      case _ => Assert.invariantFailed("unrecognized child part in TextDocumentPart: " + childNode)
    }
  }

  lazy val partRawContent = trimmedParts.text

  lazy val replaceDFDLEntities: Boolean = {
    val res = (part \ "@replaceDFDLEntities")
    if (res.length == 0) {
      false
    } else {
      Assert.usage(this.isInstanceOf[TextDocumentPart])
      res(0).toString().toBoolean
    }
  }

  lazy val encodingName: String = {
    val res = (part \ "@encoding").text
    if (res.length == 0) {
      "utf-8"
    } else {
      Assert.usage(this.isInstanceOf[TextDocumentPart])
      res
    }
  }.trim.toUpperCase()

}

case class Infoset(i: NodeSeq, parent: TestCase) {
  val Seq(dfdlInfoset) = (i \ "dfdlInfoset").map { node => DFDLInfoset(node, this) }
  val contents = dfdlInfoset.contents
}

case class DFDLInfoset(di: Node, parent: Infoset) {

  val infosetNodeSeq = {
    (di \ "@type").toString match {
      case "infoset" | "" => di.child.filter {
        _.isInstanceOf[scala.xml.Elem]
      }
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

  val contents = {
    Assert.usage(infosetNodeSeq.size == 1, "dfdlInfoset element must contain a single root element")
    val c = infosetNodeSeq.head
    c
  }
}

abstract class ErrorWarningBase(n: NodeSeq, parent: TestCase) {
  lazy val matchAttrib = (n \ "@match").text

  protected def diagnosticNodes: Seq[Node]

  lazy val messages = diagnosticNodes.map {
    _.text
  }

  def hasDiagnostics: Boolean = diagnosticNodes.nonEmpty
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
    bytes
  }

  def byteList(args: Int*) = args.map {
    _.toByte
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
