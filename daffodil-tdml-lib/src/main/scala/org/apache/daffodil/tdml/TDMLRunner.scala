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

import java.io.ByteArrayInputStream
import java.io.File
import java.io.FileNotFoundException
import java.io.InputStream
import java.io.OutputStream
import java.net.URI
import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.LongBuffer
import java.nio.charset.CoderResult
import java.nio.charset.StandardCharsets
import scala.collection.compat.immutable.LazyList
import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Try
import scala.xml.Elem
import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.SAXParseException

import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.io.processors.charset.BitsCharsetDecoder
import org.apache.daffodil.io.processors.charset.BitsCharsetEncoder
import org.apache.daffodil.io.processors.charset.BitsCharsetNonByteSize
import org.apache.daffodil.io.processors.charset.BitsCharsetNonByteSizeEncoder
import org.apache.daffodil.io.processors.charset.CharsetUtils
import org.apache.daffodil.lib.Implicits.using
import org.apache.daffodil.lib.api.DaffodilConfig
import org.apache.daffodil.lib.api.DaffodilSchemaSource
import org.apache.daffodil.lib.api.DaffodilTunables
import org.apache.daffodil.lib.api.DataLocation
import org.apache.daffodil.lib.api.Diagnostic
import org.apache.daffodil.lib.api.EmbeddedSchemaSource
import org.apache.daffodil.lib.api.TDMLImplementation
import org.apache.daffodil.lib.api.URISchemaSource
import org.apache.daffodil.lib.api.UnitTestSchemaSource
import org.apache.daffodil.lib.api.ValidationMode
import org.apache.daffodil.lib.cookers.EntityReplacer
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.UnsuppressableException
import org.apache.daffodil.lib.externalvars.Binding
import org.apache.daffodil.lib.schema.annotation.props.gen.BinaryFloatRep
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.lib.schema.annotation.props.gen.EncodingErrorPolicy
import org.apache.daffodil.lib.schema.annotation.props.gen.UTF16Width
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.MaybeInt
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.util.Misc.bits2Bytes
import org.apache.daffodil.lib.util.Misc.hex2Bits
import org.apache.daffodil.lib.util.Misc.uriToDiagnosticFile
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.DaffodilXMLLoader
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.tdml.DiagnosticType.DiagnosticType
import org.apache.daffodil.tdml.processor.AbstractTDMLDFDLProcessorFactory
import org.apache.daffodil.tdml.processor.TDML
import org.apache.daffodil.tdml.processor.TDMLDFDLProcessor
import org.apache.daffodil.tdml.processor.TDMLParseResult
import org.apache.daffodil.tdml.processor.TDMLUnparseResult

import org.apache.commons.io.IOUtils

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

/**
 * TDML test suite runner
 *
 * Keep this independent of Daffodil, so that it can be used to run tests against other DFDL implementations as well.
 * E.g., it should only need an API specified as a collection of Scala traits, and some simple way to inject
 * dependency on one factory to create processors.
 *
 * Use the optTDMLImplementation arg to override which TDML implementation runs the test.
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
 *
 * defaultIgnoreUnexpectedWarningsDefault specifies whether a test should ignore unexpected warnings (i.e it creates
 * warnings, but there is no tdml:warnings element)
 *
 * defaultIgnoreUnexpectedValidationErrorsDefault specifies whether a test should ignore unexpected validation errors (i.e it creates
 * validation errors, but there is no tdml:validationErrors element)
 */

class DFDLTestSuite private[tdml] (
  aNodeFileOrURISchemaSource: Any,
  val optTDMLImplementation: Option[TDMLImplementation],
  validateTDMLFile: Boolean,
  val validateDFDLSchemas: Boolean,
  val compileAllTopLevel: Boolean,
  val defaultRoundTripDefault: RoundTrip,
  val defaultValidationDefault: String,
  val defaultImplementationsDefault: Seq[String],
  val shouldDoErrorComparisonOnCrossTests: Boolean,
  val shouldDoWarningComparisonOnCrossTests: Boolean,
  val defaultIgnoreUnexpectedWarningsDefault: Boolean,
  val defaultIgnoreUnexpectedValidationErrorsDefault: Boolean
) {

  val TMP_DIR = System.getProperty("java.io.tmpdir", ".")

  aNodeFileOrURISchemaSource match {
    case _: URISchemaSource => // ok
    case _: File => // ok
    case _: scala.xml.Node => // ok
    case x =>
      Assert.usageError("argument was not a scala.xmlNode, File, or URISchemaSource: " + x)
  }

  /*
  This class and related use almost all lazy evaluation.

  The style here is to avoid doing anything complex with the TDML file
  at object construction time
  because the parsing and verification of the TDML file is a very complex
  endeavor best decomposed into lazy val and methods of this class.

  This means errors mostly occur after the object is constructed, which
  allows for things like accumulating diagnostics on this object.

  This style also allows unit tests to test parts of the TDML
  system in isolation from each other.

  We also depend on laziness because often we run just one test from a
  large TDML file containing many test cases. We do have to construct
  the test cases to find the one test case to run, but we are lazy about
  computing the details of the test case objects so we only incur the
  overhead of creating the one we're running.
   */

  // Use optTDMLImplementation if it was passed in, otherwise use our
  // default TDML implementation (normally daffodil unless overridden
  // by setting the environment variable TDML_IMPLEMENTATION)
  lazy val defaultTDMLImplementation: TDMLImplementation = {
    val envName = "TDML_IMPLEMENTATION"
    val value = sys.env.getOrElse(envName, "")
    val optImpl = TDMLImplementation.optionStringToEnum(envName, value)
    // Warn if value of environment variable is not a valid implementation
    if (value.nonEmpty && optImpl.isEmpty)
      throw TDMLException(s"$envName=$value does not give a valid TDML implementation", None)
    optImpl.getOrElse(TDMLImplementation.Daffodil)
  }

  lazy val tdmlDFDLProcessorFactory: AbstractTDMLDFDLProcessorFactory = {
    import scala.language.existentials

    // Let optTDMLImplementation override defaultTDMLImplementation
    val tdmlImplementation = optTDMLImplementation.getOrElse(defaultTDMLImplementation)

    // Right now both daffodil and ibm use the same classname loaded from different
    // classpaths, but we may find a better way to cross test with ibm in the future
    val className = tdmlImplementation match {
      case TDMLImplementation.Daffodil =>
        "org.apache.daffodil.processor.tdml.TDMLDFDLProcessorFactory"
      case TDMLImplementation.DaffodilC =>
        "org.apache.daffodil.processor.tdml.DaffodilCTDMLDFDLProcessorFactory"
      case TDMLImplementation.Ibm =>
        "org.apache.daffodil.processor.tdml.TDMLDFDLProcessorFactory"
    }

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
    val tryInstance = constructor.map {
      _.newInstance().asInstanceOf[AbstractTDMLDFDLProcessorFactory]
    }
    val res = tryInstance.recover {
      // encapsulate exception as TDMLException and throw
      case e => throw TDMLException(e, None)
    }.get
    res
  }

  lazy val errorHandler = new org.xml.sax.ErrorHandler {
    def warning(exception: SAXParseException) = {
      loadingExceptions += exception
    }

    def error(exception: SAXParseException) = {
      loadingExceptions += exception
    }

    def fatalError(exception: SAXParseException) = {
      error(exception)
    }
  }

  val loadingExceptions: mutable.Set[Exception] = mutable.Set.empty

  def loadingDiagnosticMessages: String = {
    val msgs = loadingExceptions.map { _.toString() }.mkString(" ")
    msgs
  }

  /**
   * our loader here accumulates load-time errors here on the
   * test suite object.
   */
  lazy val loader = new DaffodilXMLLoader(errorHandler)
  lazy val optTDMLSchema =
    if (validateTDMLFile) {
      Some(XMLUtils.tdmlURI)
    } else {
      None
    }

  lazy val (tsRaw, tsURISchemaSource) = aNodeFileOrURISchemaSource match {
    case tsNode: Node => {
      //
      // We were passed a literal schema node. This is for unit testing
      // purposes.
      //
      val tmpDir = new File(TMP_DIR, "daffodil")
      tmpDir.mkdirs()

      val nameHint = Seq((tsNode \@ "suiteName"), (tsNode \ "defineSchema" \@ "name"))
        .filterNot(Misc.isNullOrBlank)
        .mkString("_")

      val src = UnitTestSchemaSource(tsNode, nameHint, Some(tmpDir))

      loader.load(
        src,
        optTDMLSchema,
        addPositionAttributes = true
      ) // want line numbers for TDML
      //
      (tsNode, src)
    }
    case tdmlFile: File => {
      Logger.log.info(s"loading TDML file: ${tdmlFile}")
      val uri = tdmlFile.toURI()
      val schemaSource = URISchemaSource(tdmlFile, uri)
      val newNode =
        loader.load(
          schemaSource,
          optTDMLSchema,
          addPositionAttributes = true
        )
      val res = (newNode, schemaSource)
      Logger.log.debug(s"done loading TDML file: ${tdmlFile}")
      res
    }
    case uriSchemaSource: URISchemaSource => {
      val newNode =
        loader.load(
          uriSchemaSource,
          optTDMLSchema,
          addPositionAttributes = true
        )
      val res = (newNode, uriSchemaSource)
      res
    }
    case _ => Assert.usageError("not a Node, File, or URL")
  } // end match

  lazy val ts = {
    if (tsRaw eq null) {
      // must have been a loader error
      reportLoadingErrors()
    } else {
      tsRaw
    }
  }

  lazy val isTDMLFileValid = {
    (ts ne null) &&
    loadingExceptions.isEmpty
  }

  def reportLoadingErrors(): Nothing = {
    throw TDMLException(loadingExceptions.toSeq, None)
  }

  var checkAllTopLevel: Boolean = compileAllTopLevel

  def setCheckAllTopLevel(flag: Boolean): Unit = {
    checkAllTopLevel = flag
  }

  lazy val parserTestCases = (ts \ "parserTestCase").map { node => ParserTestCase(node, this) }
  //
  // Note: IBM started this TDML file format. They call an unparser test a "serializer" test.
  // We call it an UnparserTestCase
  //
  lazy val unparserTestCases = (ts \ "unparserTestCase").map { node =>
    UnparserTestCase(node, this)
  }

  lazy val testCases = {
    val tcs: Seq[TestCase] = parserTestCases ++ unparserTestCases
    ensureUnique("parser or unparser test cases", tcs) { _.tcName }
    tcs
  }

  lazy val testCaseMap = testCases.map { tc => (tc.tcName -> tc) }.toMap
  lazy val suiteName = (ts \ "@suiteName").text
  lazy val suiteID = (ts \ "@ID").text
  lazy val description = (ts \ "@description").text
  lazy val defaultRoundTrip = {
    val str = (ts \ "@defaultRoundTrip").text
    if (str == "") defaultRoundTripDefault else DFDLTestSuite.standardizeRoundTrip(str)
  }
  lazy val defaultValidation = {
    val str = (ts \ "@defaultValidation").text
    if (str == "") defaultValidationDefault else str
  }
  lazy val defaultValidationMode = ValidationMode.fromString(defaultValidation)

  lazy val defaultConfig = {
    val str = (ts \ "@defaultConfig").text
    str
  }
  lazy val defaultImplementations = {
    val str = (ts \ "@defaultImplementations").text
    if (str == "") defaultImplementationsDefault
    else {
      // parse the str to get a list of strings
      str.split("""\s+""").toSeq
    }
  }
  lazy val defaultIgnoreUnexpectedWarnings = {
    val str = (ts \ "@defaultIgnoreUnexpectedWarnings").text
    if (str == "") defaultIgnoreUnexpectedWarningsDefault else str.toBoolean
  }
  lazy val defaultIgnoreUnexpectedValidationErrors = {
    val str = (ts \ "@defaultIgnoreUnexpectedValidationErrors").text
    if (str == "") defaultIgnoreUnexpectedValidationErrorsDefault else str.toBoolean
  }

  lazy val embeddedSchemas = {
    val res = (ts \ "defineSchema").map { node => DefinedSchema(node, this) }
    ensureUnique("defineSchema", res) { _.name }
    res
  }

  lazy val embeddedConfigs = {
    val res = (ts \ "defineConfig").map { node => DefinedConfig(node, this) }
    ensureUnique("defineConfig", res) { _.name }
    res
  }

  def runAllTests(): Unit = {
    if (isTDMLFileValid)
      testCases.map { _.run() }
    else {
      throw TDMLException(
        s"TDML file ${tsURISchemaSource.diagnosticFile.getPath} is not valid.",
        None
      )
    }
  }

  var areTracing = false
  def trace = {
    areTracing = true
    this
  }

  var areDebugging = false
  def setDebugging(flag: Boolean) = {
    areDebugging = flag
  }

  var daffodilDebugger: AnyRef = null
  def setDebugger(db: AnyRef) = {
    daffodilDebugger = db
  }

  def runOneTest(testName: String, leakCheck: Boolean = false): Unit = {
    if (leakCheck) {
      System.gc()
      Thread.sleep(1) // needed to give tools like jvisualvm ability to "grab on" quickly
    }
    val testCase = testCaseMap.get(testName) // causes loading
    if (isTDMLFileValid) {
      // TODO: DAFFODIL-2410
      // display warnings if there are any
      // Note: this simple approach is just too verbose.
      // We need a better approach that doesn't repeat these endlessly for every
      // test in a suite.
      // loadingExceptions.foreach { le =>  Logger.log.warn(le.toString) }
      testCase match {
        case None => throw TDMLException("test " + testName + " was not found.", None)
        case Some(tc) => {
          tc.run()
        }
      }
    } else {
      reportLoadingErrors()
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
    Misc.searchResourceOption(resName, Some(tsURISchemaSource.uri))
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

  def ensureUnique[T](name: String, seq: Seq[T])(f: T => String): Unit = {
    val grouped = seq.groupBy(f)
    val dups = grouped.filter { case (key, s) => s.size > 1 }
    if (dups.size > 0) {
      throw TDMLException(
        "Duplicate definitions found for " + name + ": " + dups.keys.mkString(", "),
        None
      )
    }
  }

  /**
   * Get the CompileResult from a TDMLCompileResultCache.
   *
   * This uses one of two different caches to look for compile results:
   *
   * Embedded schemas cannot be shared among different DFDL Test Suites, so
   * they are stored in a cache local to this test suite. This avoids having to
   * run the expiration logic for such compile results, and the compile results
   * will be garbage collected when this DFDL Test Suite is garbage collected.
   *
   * Non-embedded schemas may need to be shared between different test suites
   * if different Runners/.tdml files reference the same schema with same
   * compile options. So they are stored in a global cache, with care used to
   * ensure we mark them as finished so they can expire and be removed when not
   * used.
   */
  def getCompileResult(
    impl: AbstractTDMLDFDLProcessorFactory,
    suppliedSchema: DaffodilSchemaSource,
    useSerializedProcessor: Boolean,
    optRootName: Option[String],
    optRootNamespace: Option[String],
    tunables: Map[String, String]
  ): TDML.CompileResult = {

    val key = TDMLCompileResultCacheKey(
      impl.implementationName,
      suppliedSchema,
      useSerializedProcessor,
      optRootName,
      optRootNamespace,
      tunables
    )

    val isEmbedded = suppliedSchema.isInstanceOf[EmbeddedSchemaSource]
    val cache =
      if (isEmbedded) {
        embeddedCompileResultCache
      } else {
        // we are using the global tdml compile result cache--we must store the
        // key associated with this compile result so that when cleanUp is
        // called for this DFDLTestSuite we can notify the global tdml compile
        // result cache that we are finished with this key and it can be expired
        globalTDMLCompileResultCacheKeys += key
        GlobalTDMLCompileResultCache.cache
      }

    val compileResult = cache.getCompileResult(
      impl,
      key,
      defaultValidationMode
    )
    compileResult
  }

  // Embedded schemas cannot be shared between different DFDLTestSuites. So
  // there is no need to store them in the global cache. Embedded compile
  // results are instead store in this cache that lives on the DFDLTestSuite.
  // When this DFDLTestSuite is garbage collected, so too are the embedded
  // schema compile results. There is no expire time needed, since these
  // results will only live for the duration of the DFDLTestSuite
  private val embeddedCompileResultCache = TDMLCompileResultCache(None)

  // Used to keep track of which keys this DFDLTestSuite stored in the global
  // TDML compile result cache. When the cleanUp function is called, we can
  // mark all these keys as finished so that they can expire. Note that this
  // means if a Runner does not call reset and this cleanUp function is not
  // called, the associated compile results will not expire
  private val globalTDMLCompileResultCacheKeys = mutable.Set[TDMLCompileResultCacheKey]()

  def cleanUp(): Unit = {
    GlobalTDMLCompileResultCache.cache.setEntriesFinished(globalTDMLCompileResultCacheKeys)
  }

}

abstract class TestCase(testCaseXML: NodeSeq, val parent: DFDLTestSuite) {

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
  lazy val defaultValidationMode: ValidationMode.Type = parent.defaultValidationMode
  lazy val defaultIgnoreUnexpectedWarnings: Boolean = parent.defaultIgnoreUnexpectedWarnings
  lazy val defaultIgnoreUnexpectedValidationErrors: Boolean =
    parent.defaultIgnoreUnexpectedValidationErrors

  private lazy val defaultImplementations: Seq[String] = parent.defaultImplementations
  private lazy val tcImplementations = (testCaseXML \ "@implementations").text
  private lazy val implementationStrings =
    if (tcImplementations == "") defaultImplementations
    else tcImplementations.split("""\s+""").toSeq

  def toss(t: Throwable, implString: Option[String]) = {
    t match {
      case e: TDMLException => throw e
      case e: UnsuppressableException => throw e
      case s: scala.util.control.ControlThrowable => throw s
      case _ => throw TDMLException(t, implString)
    }
  }

  final def isCrossTest(implString: String) = implString != "daffodil"

  final def isNegativeTest = optExpectedErrors.isDefined

  lazy val document = (testCaseXML \ "document").headOption.map { node => Document(node, this) }
  lazy val optExpectedOrInputInfoset = (testCaseXML \ "infoset").headOption.map { node =>
    new Infoset(node, this)
  }
  lazy val optExpectedErrors: Option[Seq[ExpectedErrors]] = {
    val s = (testCaseXML \ "errors").map { node => ExpectedErrors(node, this) }
    if (s.nonEmpty) Some(s) else None
  }
  lazy val optExpectedWarnings: Option[Seq[ExpectedWarnings]] = {
    val s = (testCaseXML \ "warnings").map { node => ExpectedWarnings(node, this) }
    if (s.nonEmpty) Some(s) else None
  }
  lazy val optExpectedValidationErrors: Option[Seq[ExpectedValidationErrors]] = {
    val s = (testCaseXML \ "validationErrors").map { node =>
      ExpectedValidationErrors(node, this)
    }
    if (s.nonEmpty) Some(s) else None
  }

  val tcName = (testCaseXML \ "@name").text
  lazy val tcID = (testCaseXML \ "@ID").text
  lazy val id = tcName + (if (tcID != "") "(" + tcID + ")" else "")
  lazy val rootAttrib = (testCaseXML \ "@root").text
  lazy val optRootNSAttrib: Option[String] = {
    val attr = testCaseXML.asInstanceOf[Elem].attribute("rootNS")
    if (attr.isDefined) {
      Some(testCaseXML \@ "rootNS")
    } else {
      None
    }
  }

  lazy val (infosetRootName, infosetRootNamespaceString) =
    if (this.optExpectedOrInputInfoset.isDefined) {
      val infoset = optExpectedOrInputInfoset.get.dfdlInfoset.contents
      (infoset.label, infoset.namespace)
    } else (null, null)

  lazy val rootName = {
    if (rootAttrib == "") infosetRootName
    else if (this.optExpectedOrInputInfoset.isDefined) {
      if (infosetRootName != rootAttrib)
        throw TDMLException(
          "root attribute name: %s, does not match the name of the root element of the infoset: %s."
            .format(rootAttrib, infosetRootName),
          None
        )
      rootAttrib
    } else rootAttrib
  }

  def getRootNamespaceString() = {
    if (this.optRootNSAttrib.isDefined) {
      optRootNSAttrib.get
    } else if (optExpectedOrInputInfoset.isDefined)
      infosetRootNamespaceString
    else if (optEmbeddedSchema.isDefined)
      XMLUtils.EXAMPLE_NAMESPACE.toString
    else {
      // For some TDML Processors, we have to provide
      // the root namespace. They don't provide a way to search
      // for an element when just the name is unambiguous.
      // So since nothing was provided, we just grab the
      // target namespace URI (if any) from the primary
      // schema file. If that turns out to be wrong, then
      // the test case has to have an explicit rootNS attribute.
      val schemaSource = getSuppliedSchema()

      val schemaNode =
        try {
          val node = parent.loader.load(
            schemaSource,
            Some(XMLUtils.schemaForDFDLSchemas),
            addPositionAttributes = true
          ) // want line numbers for schemas
          if (node ne null) node else <dummy/>
        } catch {
          // any exception while loading then we just use a dummy node.
          case e: SAXParseException => <dummy/>
        }
      val tns = (schemaNode \ "@targetNamespace").text
      val nsURIString = {
        if (tns != "") tns
        else null
      }
      nsURIString
    }
  }

  lazy val model = (testCaseXML \ "@model").text
  lazy val config = (testCaseXML \ "@config").text
  lazy val tcRoundTrip: String = (testCaseXML \ "@roundTrip").text
  lazy val roundTrip: RoundTrip =
    if (tcRoundTrip == "") defaultRoundTrip else DFDLTestSuite.standardizeRoundTrip(tcRoundTrip)
  lazy val tcIgnoreUnexpectedWarnings: String = (testCaseXML \ "@ignoreUnexpectedWarnings").text
  lazy val ignoreUnexpectedWarnings: Boolean =
    if (tcIgnoreUnexpectedWarnings == "") defaultIgnoreUnexpectedWarnings
    else tcIgnoreUnexpectedWarnings.toBoolean
  lazy val tcIgnoreUnexpectedValidationErrors: String =
    (testCaseXML \ "@ignoreUnexpectedValidationErrors").text
  lazy val ignoreUnexpectedValidationErrors: Boolean =
    if (tcIgnoreUnexpectedValidationErrors == "") defaultIgnoreUnexpectedValidationErrors
    else tcIgnoreUnexpectedValidationErrors.toBoolean

  lazy val description = (testCaseXML \ "@description").text
  lazy val unsupported = (testCaseXML \ "@unsupported").text match {
    case "true" => true
    case "false" => false
    case _ => false
  }
  lazy val validationMode: ValidationMode.Type = (testCaseXML \ "@validation").text match {
    case "" => defaultValidationMode
    case mode => ValidationMode.fromString(mode)
  }

  protected def runProcessor(
    compileResult: TDML.CompileResult,
    expectedData: Option[InputStream],
    nBits: Option[Long],
    errors: Option[Seq[ExpectedErrors]],
    warnings: Option[Seq[ExpectedWarnings]],
    validationErrors: Option[Seq[ExpectedValidationErrors]],
    validationMode: ValidationMode.Type,
    roundTrip: RoundTrip,
    implString: Option[String]
  ): Unit

  lazy val optEmbeddedSchema = parent.findEmbeddedSchema(model).map { defSchema =>
    defSchema.schemaSource
  }

  lazy val optSchemaFileURI =
    if (model == "") None
    else parent.findSchemaFileName(model)

  def getSuppliedSchema(): DaffodilSchemaSource = {
    val suppliedSchema = (optEmbeddedSchema, optSchemaFileURI) match {
      case (None, None) =>
        throw TDMLException(
          "Model '" + model + "' was not passed, found embedded in the TDML file, nor as a schema file.",
          None
        )
      case (Some(_), Some(_)) =>
        throw TDMLException(
          "Model '" + model + "' is ambiguous. There is an embedded model with that name, AND a file with that name.",
          None
        )
      case (Some(embeddedSchemaSource), None) => {
        Assert.invariant(model != "") // validation of the TDML should prevent this
        embeddedSchemaSource
      }
      case (None, Some(uri)) => {
        //
        // In this case, we have a real TDML file (or resource) to open
        URISchemaSource(Misc.uriToDiagnosticFile(uri), uri)
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
        val node = parent.loader.load(
          URISchemaSource(uriToDiagnosticFile(uri), uri),
          Some(XMLUtils.dafextURI)
        )
        if (node eq null)
          throw TDMLException(parent.loadingExceptions.toSeq, None)
        val definedConfig = DefinedConfig(node, parent)
        Some(definedConfig)
      }
      case (name, None, None) if name != "" =>
        throw TDMLException(
          "The " + attrName + " '" + cfgName + "' was not found either as a embedded config, nor as a file.",
          None
        )
      case (name, Some(_), Some(_)) if name != "" =>
        throw TDMLException(
          "The " + attrName + " '" + cfgName + "' is ambiguous. There is an embedded config with that name, AND a file with that name.",
          None
        )
      case _ => None
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

    val exceptionToThrow = junitExceptionInstance.getOrElse(
      // We create a new exception here so that we can propagate the
      // exception as to why we are not able to construct
      // an org.junit.AssumptionViolatedException.
      //
      // This can be useful if nothing is catching this exception.
      // The CLI catches this, but won't display the messaging that
      // elaborates on the cause, because the CLI doesn't utilize junit,
      // so it expects this to fail.
      //
      // But if this isn't caught, and propagates to top level,
      // then we'll get the diagnostic messaging, including about the
      // cause.
      //
      new TDMLTestNotCompatibleException(
        testName,
        implementationName,
        Option(junitExceptionInstance.failed.get)
      )
    )
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

  lazy val tunables = cfg.map { _.tunablesMap }.getOrElse(Map.empty)

  lazy val tunableObj = DaffodilTunables(tunables)

  lazy val externalVarBindings: Seq[Binding] =
    cfg.map { _.externalVariableBindings }.getOrElse(Seq())

  def run(): Unit = {
    val suppliedSchema = getSuppliedSchema()

    var impl: AbstractTDMLDFDLProcessorFactory = parent.tdmlDFDLProcessorFactory
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

      impl = impl
        .withValidateDFDLSchemas(parent.validateDFDLSchemas)
        .withTunables(tunables)
        .withCheckAllTopLevel(parent.checkAllTopLevel)

      val optInputOrExpectedData = document.map {
        _.data
      }
      val nBits: Option[Long] = document.map {
        _.nBits
      }

      val useSerializedProcessor =
        if (validationMode == ValidationMode.Full) false
        else if (defaultValidationMode == ValidationMode.Full) false
        else if (optExpectedWarnings.isDefined) false
        else true

      val rootNamespaceString = getRootNamespaceString()

      val compileResult: TDML.CompileResult = parent.getCompileResult(
        impl,
        suppliedSchema,
        useSerializedProcessor,
        Option(rootName),
        Option(rootNamespaceString),
        tunables
      )

      val newCompileResult: TDML.CompileResult = compileResult.map {
        case (diags, proc: TDMLDFDLProcessor) =>
          // warnings are checked elsewhere for expected ones.
          val newProc: TDMLDFDLProcessor =
            proc.withDebugging(parent.areDebugging).withTracing(parent.areTracing)
          val newNewProc =
            if (
              parent.areDebugging &&
              (parent.daffodilDebugger ne null)
            ) {
              newProc.withDebugger(parent.daffodilDebugger)
            } else {
              newProc
            }
          (diags, newNewProc)
      }
      try {
        runProcessor(
          newCompileResult,
          optInputOrExpectedData,
          nBits,
          optExpectedErrors,
          optExpectedWarnings,
          optExpectedValidationErrors,
          validationMode,
          roundTrip,
          implString
        )
      } finally {
        // runProcessor may have set the "processor" variable to the DataProcessor used for the
        // test. This variable only exists as an easy way to avoid having to pass the
        // DataProcessor around to a bunch of functions. At this point this TestCase is done
        // with the DataProcessor, so we set it to null so it can be garbage collected if
        // nothing else uses it. This is common if a test case calls withXYZ to create a
        // temporary copy of a cached DataProcessor but with different variables, tunables,
        // validation mode, etc. used only for this specific test. This is especially important
        // if a DataProcessor stores any large information like a Xerces validator.
        processor = null
      }
    }
  }

  protected def checkDiagnosticMessages(
    diagnostics: Seq[Diagnostic],
    optErrors: Option[Seq[ExpectedErrors]],
    optWarnings: Option[Seq[ExpectedWarnings]],
    optValidationErrors: Option[Seq[ExpectedValidationErrors]],
    implString: Option[String]
  ): Unit = {

    // check for any test-specified errors or warnings
    if (
      !isCrossTest(implString.get) ||
      parent.shouldDoErrorComparisonOnCrossTests
    ) {
      val errorDiagnostics = diagnostics.filter(d => d.isError && !d.isValidation)
      VerifyTestCase.verifyDiagnosticsFound(
        errorDiagnostics,
        optErrors,
        ignoreUnexpectedDiags = false,
        DiagnosticType.Error,
        implString
      )
    }

    if (
      !isCrossTest(implString.get) ||
      parent.shouldDoWarningComparisonOnCrossTests
    ) {
      val warningDiagnostics = diagnostics.filterNot(d => d.isValidation || d.isError)
      VerifyTestCase.verifyDiagnosticsFound(
        warningDiagnostics,
        optWarnings,
        ignoreUnexpectedWarnings,
        DiagnosticType.Warning,
        implString
      )
    }

    if (!isCrossTest(implString.get)) {
      val validationErrorDiagnostics = diagnostics.filter(diag => diag.isValidation)
      VerifyTestCase.verifyDiagnosticsFound(
        validationErrorDiagnostics,
        optValidationErrors,
        ignoreUnexpectedDiags = ignoreUnexpectedValidationErrors,
        DiagnosticType.ValidationError,
        implString
      )
    }
  }
}

case class ParserTestCase(ptc: NodeSeq, parentArg: DFDLTestSuite)
  extends TestCase(ptc, parentArg) {

  lazy val optExpectedInfoset = this.optExpectedOrInputInfoset

  override def runProcessor(
    compileResult: TDML.CompileResult,
    optDataToParse: Option[InputStream],
    optLengthLimitInBits: Option[Long],
    optExpectedErrors: Option[Seq[ExpectedErrors]],
    optExpectedWarnings: Option[Seq[ExpectedWarnings]],
    optExpectedValidationErrors: Option[Seq[ExpectedValidationErrors]],
    validationMode: ValidationMode.Type,
    roundTrip: RoundTrip,
    implString: Option[String]
  ) = {

    Assert.usage(
      optLengthLimitInBits.isDefined,
      "TDML tests should always have a length limit."
    )
    val nBits = optLengthLimitInBits.get

    val dataToParse = optDataToParse.get

    (optExpectedInfoset, optExpectedErrors) match {
      case (Some(_), None) => {
        compileResult match {
          case Left(diags) =>
            checkDiagnosticMessages(
              diags,
              optExpectedErrors,
              optExpectedWarnings,
              optExpectedValidationErrors,
              implString
            )
          case Right((diags, proc)) => {
            processor = proc
            runParseExpectSuccess(
              dataToParse,
              nBits,
              optExpectedWarnings,
              optExpectedValidationErrors,
              validationMode,
              roundTrip,
              implString,
              diags
            )
          }
        }
      }

      case (None, Some(_)) => {
        compileResult match {
          case Left(diags) =>
            checkDiagnosticMessages(
              diags,
              optExpectedErrors,
              optExpectedWarnings,
              optExpectedValidationErrors,
              implString
            )
          case Right((diags, proc)) => {
            processor = proc
            runParseExpectErrors(
              dataToParse,
              nBits,
              optExpectedErrors,
              optExpectedWarnings,
              optExpectedValidationErrors,
              validationMode,
              implString,
              diags
            )
          }
        }
      }
      case _ =>
        throw TDMLException(
          "Either tdml:infoset or tdml:errors must be present in the test.",
          implString
        )
    }
  }

  def runParseExpectErrors(
    dataToParse: InputStream,
    lengthLimitInBits: Long,
    optErrors: Option[Seq[ExpectedErrors]],
    optWarnings: Option[Seq[ExpectedWarnings]],
    optValidationErrors: Option[Seq[ExpectedValidationErrors]],
    validationMode: ValidationMode.Type,
    implString: Option[String],
    compileWarnings: Seq[Diagnostic]
  ): Unit = {

    try {
      processor = processor
        .withExternalDFDLVariables(externalVarBindings)
        .withValidationMode(validationMode)
    } catch {
      case e: Exception => throw TDMLException(e, implString)
    }

    val (parseResult, diagnostics, isError) = {
      val actual =
        try {
          processor.parse(dataToParse, lengthLimitInBits)
        } catch {
          case t: Throwable =>
            toss(t, implString)
        }

      // we should never need blobs if we're expecting an error even if we
      // don't get errors. So clean them up immediately
      actual.cleanUp()

      val isErr: Boolean =
        if (actual.isProcessingError) true
        else {
          //
          // didn't get an error.
          // If we're not at the end of data, synthesize an error for left-over-data
          //
          val loc: DataLocation = actual.currentLocation

          if (loc.bitPos1b >= 0 && loc.bitPos1b <= lengthLimitInBits) {
            val leftOverMsg =
              "Left over data. Consumed %s bit(s) with %s bit(s) remaining.".format(
                loc.bitPos1b - 1,
                lengthLimitInBits - (loc.bitPos1b - 1)
              )
            actual.addDiagnostic(new TDMLDiagnostic(leftOverMsg, implString))
            true
          } else {
            false
          }
        }

      val diagnostics = compileWarnings ++ actual.getDiagnostics
      (actual, diagnostics, isErr)
    }
    if (!isError) {
      toss(
        TDMLException(
          "Expected error. Didn't get one. Actual result was\n" +
            parseResult.getResult.toString,
          implString
        ),
        implString
      )
    }

    checkDiagnosticMessages(
      diagnostics,
      optErrors,
      optWarnings,
      optValidationErrors,
      implString
    )
  }

  /**
   * Returns outputter containing the parse infoset. Throws if there isn't one.
   */
  private def doParseExpectSuccess(
    testData: Array[Byte],
    testInfoset: Infoset,
    lengthLimitInBits: Long,
    implString: Option[String]
  ): TDMLParseResult = {

    val testDataLength = lengthLimitInBits

    try {
      processor = processor.withExternalDFDLVariables(externalVarBindings)
    } catch {
      case e: Exception => throw TDMLException(e, implString)
    }

    val actual = processor.parse(new ByteArrayInputStream(testData), testDataLength)
    val diagObjs = actual.getDiagnostics

    if (actual.isProcessingError) {
      // Means there was an error, not just warnings.
      if (diagObjs.length == 1) throw TDMLException(diagObjs.head, implString)
      val diags = actual.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw TDMLException(diags, implString)
    } else {
      // If we think we've succeeded, verify there are no errors
      // captured in the diagnostics. Otherwise there's probably
      // an internal bug causing us to miss setting isProcessingError
      val hasErrorDiags = diagObjs.exists { diag =>
        diag.isError && !diag.isValidation
      }
      Assert.invariant(!hasErrorDiags)
    }
    actual
  }

  private def verifyLeftOverData(
    actual: TDMLParseResult,
    lengthLimitInBits: Long,
    implString: Option[String]
  ) = {
    val loc: DataLocation = actual.currentLocation

    val leftOverException = if (loc.bitPos1b >= 0 && loc.bitPos1b < lengthLimitInBits) {
      val leftOverMsg = "Left over data. Consumed %s bit(s) with %s bit(s) remaining.".format(
        loc.bitPos1b - 1,
        lengthLimitInBits - (loc.bitPos1b - 1)
      )
      Some(TDMLException(leftOverMsg, implString))
    } else None

    leftOverException.map {
      throw _
    } // if we get here, throw the left over data exception.
  }

  private def verifyParseResults(
    compileWarnings: Seq[Diagnostic],
    actual: TDMLParseResult,
    testInfoset: Infoset,
    implString: Option[String]
  ) = {
    val resultXmlNode = actual.getResult
    VerifyTestCase.verifyParserTestData(resultXmlNode, testInfoset, implString)

    val allDiags = compileWarnings ++ actual.getDiagnostics
    checkDiagnosticMessages(
      allDiags,
      None,
      optExpectedWarnings,
      optExpectedValidationErrors,
      implString
    )
  }

  /**
   * Do an unparse. Report any actual diagnostics, but don't compare
   * the unparsed data output.
   */
  private def doOnePassRoundTripUnparseExpectSuccess(
    outStream: OutputStream, // stream where unparsed data is written
    parseResult: TDMLParseResult, // result from prior parse.
    implString: Option[String]
  ): TDMLUnparseResult = {

    // in a one pass round trip, the parse test is entirely over, and the infoset comparison
    // MUST have succeeded. We now just are trying to unparse and we must get back
    // exactly the original input data.

    val unparseResult = processor.unparse(parseResult, outStream)
    if (unparseResult.isProcessingError) {
      val diagObjs = unparseResult.getDiagnostics
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
    passesLabel: String = TwoPassRoundTrip.propValueName
  ): (TDMLParseResult, Array[Byte], Long) = {
    val outStream = new java.io.ByteArrayOutputStream()
    val unparseResult: TDMLUnparseResult =
      doOnePassRoundTripUnparseExpectSuccess(outStream, parseResult, implString)
    val isUnparseOutputDataMatching =
      try {
        VerifyTestCase.verifyUnparserTestData(
          new ByteArrayInputStream(firstParseTestData),
          outStream,
          implString
        )
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
      val msg =
        ("Expected data from first unparse of %s to NOT match original input, but it did match." +
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
        throw TDMLException(
          "Unparse result data was was %d bytes, but the result length (%d bits) requires %d bytes."
            .format(reParseTestData.length, reParseTestDataLength, fullBytesNeeded),
          implString
        )
      }
    }
    val actual =
      doParseExpectSuccess(reParseTestData, testInfoset, reParseTestDataLength, implString)
    (actual, reParseTestData, reParseTestDataLength)
  }

  def runParseExpectSuccess(
    dataToParse: InputStream,
    lengthLimitInBits: Long,
    warnings: Option[Seq[ExpectedWarnings]],
    validationErrors: Option[Seq[ExpectedValidationErrors]],
    validationMode: ValidationMode.Type,
    roundTripArg: RoundTrip,
    implString: Option[String],
    compileWarnings: Seq[Diagnostic]
  ): Unit = {

    val roundTrip =
      roundTripArg // change to OnePassRoundTrip to force all parse tests to round trip (to see which fail to round trip)

    processor = processor.withValidationMode(validationMode)

    val firstParseTestData = IOUtils.toByteArray(dataToParse)
    val testInfoset = optExpectedInfoset.get

    val firstParseResult =
      doParseExpectSuccess(firstParseTestData, testInfoset, lengthLimitInBits, implString)

    roundTrip match {
      case NoRoundTrip | OnePassRoundTrip => {
        verifyParseResults(compileWarnings, firstParseResult, testInfoset, implString)
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
        //            verifyParseResults(compileWarnings, processor, firstParseResult, testInfoset, implString)
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
        firstParseResult.cleanUp()
      }
      case OnePassRoundTrip => {
        val outStream = new java.io.ByteArrayOutputStream()

        doOnePassRoundTripUnparseExpectSuccess(outStream, firstParseResult, implString)

        // It has to work, as this is one pass round trip. We expect it to unparse
        // directly back to the original input form.

        VerifyTestCase.verifyUnparserTestData(
          new ByteArrayInputStream(firstParseTestData),
          outStream,
          implString
        )

        // Done with the first parse result, safe to clean up blobs if there
        // was success. This won't get called on failure, which is fine--leave
        // blobs around for debugging
        firstParseResult.cleanUp()
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
            testInfoset
          )
        verifyParseResults(compileWarnings, actual, testInfoset, implString)
        verifyLeftOverData(actual, reParseTestDataLength, implString)
        // if it doesn't pass, it will throw out of here.

        // Done with the first and second parse resultrs, safe to clean up
        // blobs if there was success. This won't get called on failure, which
        // is fine--leave blobs around for debugging
        firstParseResult.cleanUp()
        actual.cleanUp()
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
            ThreePassRoundTrip.propValueName
          )
        //
        // The infoset from the reparse should be the final
        // steady state infoset, which is what the test case
        // should put as the expected infoset.
        //
        // So we just verify normally here.
        //
        verifyParseResults(compileWarnings, secondParseResult, testInfoset, implString)
        verifyLeftOverData(secondParseResult, reParseTestDataLength, implString)
        //
        // So now we do the third pass unparse and compare this output with the
        // first unparsed output.
        //
        // We get to reuse the one-pass code here.
        val thirdPassOutStream = new java.io.ByteArrayOutputStream()
        doOnePassRoundTripUnparseExpectSuccess(thirdPassOutStream, firstParseResult, implString)
        VerifyTestCase.verifyUnparserTestData(
          new ByteArrayInputStream(reParseTestData),
          thirdPassOutStream,
          implString
        )

        // Done with the first parse result and second parse results. Safe to
        // clean up blobs if there was success. Leave them around for debugging
        // if there was a failure
        firstParseResult.cleanUp()
        secondParseResult.cleanUp()
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
    optErrors: Option[Seq[ExpectedErrors]],
    optWarnings: Option[Seq[ExpectedWarnings]],
    optValidationErrors: Option[Seq[ExpectedValidationErrors]],
    validationMode: ValidationMode.Type,
    roundTrip: RoundTrip,
    implString: Option[String]
  ) = {

    (optExpectedData, optErrors) match {
      case (Some(expectedData), None) => {
        compileResult match {
          case Left(diags) => throw TDMLException(diags, implString)
          case Right((diags, proc)) => {
            processor = proc
            runUnparserExpectSuccess(
              expectedData,
              optWarnings,
              optValidationErrors,
              roundTrip,
              implString,
              diags
            )
          }
        }
      }

      case (_, Some(_)) => {
        compileResult match {
          case Left(diags) =>
            checkDiagnosticMessages(
              diags,
              optErrors,
              optWarnings,
              optValidationErrors,
              implString
            )
          case Right((diags, proc)) => {
            processor = proc
            runUnparserExpectErrors(
              optExpectedData,
              optErrors,
              optWarnings,
              optValidationErrors,
              implString,
              diags
            )
          }
        }
      }
      case _ =>
        throw TDMLException(
          "Either tdml:document or tdml:errors must be present in the test.",
          implString
        )
    }

  }

  def runUnparserExpectSuccess(
    expectedData: InputStream,
    optWarnings: Option[Seq[ExpectedWarnings]],
    optValidationErrors: Option[Seq[ExpectedValidationErrors]],
    roundTrip: RoundTrip,
    implString: Option[String],
    compileWarnings: Seq[Diagnostic]
  ): Unit = {

    Assert.usage(roundTrip ne TwoPassRoundTrip) // not supported for unparser test cases.

    val infosetXML: Node = inputInfoset.dfdlInfoset.contents

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
        throw TDMLException(
          "Unparse result data was %d bytes, but the result length (%d bits) requires %d bytes."
            .format(testData.length, testDataLength, fullBytesNeeded),
          implString
        )
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

    val allDiags = actual.getDiagnostics ++ compileWarnings
    checkDiagnosticMessages(
      allDiags,
      None,
      optWarnings,
      optValidationErrors,
      implString
    )

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

      val leftOverException = if (loc.bitPos1b >= 0 && loc.bitPos1b < testDataLength) {
        //
        // For this to happen (and have test coverage) we need an unparserTestCase
        // which is roundTrip onePass, and where the parse doesn't consume all
        // the data.
        //
        val leftOverMsg =
          "Left over data. Consumed %s bit(s) with %s bit(s) remaining.".format(
            loc.bitPos1b - 1,
            testDataLength - (loc.bitPos1b - 1)
          )
        Some(TDMLException(leftOverMsg, implString))
      } else None

      val xmlNode = parseActual.getResult
      VerifyTestCase.verifyParserTestData(xmlNode, inputInfoset, implString)

      leftOverException.map {
        throw _
      } // if we get here, throw the left over data exception.

      // Done with the parse results, safe to clean up blobs if there was
      // success. This won't get called on failure, which is fine--leave blobs
      // around for debugging
      parseActual.cleanUp()
    }

    // Done with the unparse results, safe to clean up any temporary files
    // if they were not already cleaned up by parseActual.cleanUp() above
    actual.cleanUp()
  }

  def runUnparserExpectErrors(
    optExpectedData: Option[InputStream],
    optErrors: Option[Seq[ExpectedErrors]],
    optWarnings: Option[Seq[ExpectedWarnings]],
    optValidationErrors: Option[Seq[ExpectedValidationErrors]],
    implString: Option[String],
    compileWarnings: Seq[Diagnostic]
  ): Unit = {

    try {
      processor = processor.withExternalDFDLVariables(externalVarBindings)
    } catch {
      case e: Exception => throw TDMLException(e, implString)
    }

    val outStream = new java.io.ByteArrayOutputStream()
    val infosetXML = {
      if (optInputInfoset.isEmpty)
        throw TDMLException(
          "No infoset specified, but one is required to run the test.",
          implString
        )
      inputInfoset.dfdlInfoset.contents
    }
    val actual =
      try {
        processor.unparse(infosetXML, outStream)
      } catch {
        case t: Throwable =>
          toss(t, implString)
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
            Some(new TDMLDiagnostic(x.getMessage, implString))
        }
      }
    }

    // Done with the unparse results, safe to clean up any temporary files
    actual.cleanUp()

    val diagnostics = compileWarnings ++ actual.getDiagnostics ++ dataErrors

    if (diagnostics.isEmpty)
      throw TDMLException("Unparser test expected error. Didn't get one.", implString)
    checkDiagnosticMessages(
      diagnostics,
      optErrors,
      optWarnings,
      optValidationErrors,
      implString
    )
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

  def verifyUnparserTestData(
    expectedData: InputStream,
    actualOutStream: java.io.ByteArrayOutputStream,
    implString: Option[String]
  ): Unit = {
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
      throw TDMLException(
        "Output data length %s for '%s' doesn't match expected value %s for '%s'."
          .format(actualBytes.length, actualAsString, readCount, expectedAsString),
        implString
      )
    }

    val pairs = expectedBytes.zip(actualBytes).zip(LazyList.from(1))
    pairs.foreach { case ((expected, actual), index) =>
      if (expected != actual) {
        val msg = ("Unparsed data differs at byte %d. Expected 0x%02x. Actual was 0x%02x.\n" +
          "Data (as strings) was\n(actual) '%s'\n(expected) '%s'.").format(
          index,
          expected,
          actual,
          actualAsString,
          expectedAsString
        )
        throw TDMLException(msg, implString)
      }
    }
  }

  /**
   * Do diagnostics verification
   *
   * @param actualDiagsFiltered                       Actual diagnostics produced filtered by diagnostic type
   * @param expectedDiags                             Expected diagnostics from test cases
   * @param ignoreUnexpectedDiags                     flag for ignoring unexpected diagnostics
   * @param diagnosticType                            Enumeration (Warning, Error or ValidationError) describing the
   *                                                  type of diagnostic being verified
   * @param implString                                Implementation string
   */
  def verifyDiagnosticsFound(
    actualDiagsFiltered: Seq[Diagnostic],
    expectedDiags: Option[Seq[ErrorWarningBase]],
    ignoreUnexpectedDiags: Boolean,
    diagnosticType: DiagnosticType,
    implString: Option[String]
  ): Unit = {

    val expectedDiagMsgs = expectedDiags
      .map { d =>
        d.flatMap(_.messages)
      }
      .getOrElse(Nil)

    val actualDiagsFilteredMessages = actualDiagsFiltered.map(_.toString())

    // throw exception if we have no actualDiags, but have expected diagnostics
    if (expectedDiags.isDefined && actualDiagsFiltered.isEmpty) {
      throw TDMLException(
        """"Diagnostic message(s) were expected but not found."""" +
          "\n" +
          """Expected: """ + expectedDiagMsgs.mkString("\n") +
          (if (actualDiagsFiltered.isEmpty)
             s"\n No ${diagnosticType} diagnostic messages were issued."
           else
             "\n The actual diagnostics messages were: " + actualDiagsFilteredMessages.mkString(
               "\n"
             )),
        implString
      )
    }

    val hasUnexpectedDiags = expectedDiags.isEmpty && actualDiagsFiltered.nonEmpty
    if (hasUnexpectedDiags && !ignoreUnexpectedDiags) {
      val flagMessage = diagnosticType match {
        case DiagnosticType.ValidationError => "ignoreUnexpectedValidationErrors = false and "
        case DiagnosticType.Warning => "ignoreUnexpectedWarnings = false and "
        case DiagnosticType.Error => ""
      }
      throw TDMLException(
        s"${flagMessage}test does not expect ${diagnosticType} diagnostics, but created the following: " +
          s"${actualDiagsFilteredMessages.mkString("\n")}",
        implString
      )
    }

    expectedDiags.map { diags =>
      diags.map { diag =>
        val matchAttr = diag.matchAttrib
        val diagsFound = diag.messages.map { d =>
          // make case insensitive
          val wasFound =
            actualDiagsFilteredMessages.exists(ad => ad.toLowerCase.contains(d.toLowerCase))
          (d, wasFound)
        }
        matchAttr match {
          case "all" => {
            if (!diagsFound.forall(_._2)) {
              throw TDMLException(
                s"""Did not find diagnostic ${diag.diagnosticType} message """" +
                  diagsFound.find(_._2 == false).get._1 +
                  """" in any of the actual diagnostic messages: """ + "\n" +
                  actualDiagsFilteredMessages.mkString("\n"),
                implString
              )
            }
          }
          case "none" => {
            if (diagsFound.exists(_._2)) {
              throw TDMLException(
                s"""Found one of diagnostic ${diag.diagnosticType} message """" +
                  diagsFound.find(_._2 == true).get._1 +
                  """" in one of the actual diagnostic messages: """ + "\n" +
                  actualDiagsFilteredMessages.mkString("\n"),
                implString
              )
            }
          }
          case _ => throw TDMLException("Only match=all or match=none supported", implString)
        }
      }
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

    using(InputSourceDataInputStream(bytes)) { dis =>
      val finfo = new FormatInfoForTDMLDecode
      val cb = CharBuffer.allocate(256)
      val sb = new StringBuilder(256)
      while ({
        val numDecoded = decoder.decode(dis, finfo, cb);
        numDecoded > 0
      }) {
        cb.flip()
        sb.append(cb)
        cb.clear()
      }
      sb.toString
    }
  }

  def verifyTextData(
    expectedData: InputStream,
    actualOutStream: java.io.ByteArrayOutputStream,
    encodingName: String,
    implString: Option[String]
  ): Unit = {
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

    val actualCharsToShow =
      if (actualText.length == 0) "" else " for '" + trimToMax(actualText) + "'"
    val expectedCharsToShow =
      if (expectedText.length == 0) "" else " for '" + trimToMax(expectedText) + "'"
    if (actualText.length != expectedText.length) {
      val actualCharsToShow =
        if (actualText.length == 0) "" else " for '" + trimToMax(actualText) + "'"
      val expectedCharsToShow =
        if (expectedText.length == 0) "" else " for '" + trimToMax(expectedText) + "'"
      throw TDMLException(
        "output data length " + actualText.length + actualCharsToShow +
          " doesn't match expected length " + expectedText.length + expectedCharsToShow,
        implString
      )
    }

    val pairs = expectedText.toSeq.zip(actualText.toSeq).zip(LazyList.from(1))
    pairs.foreach { case ((expected, actual), index) =>
      if (expected != actual) {
        val msg =
          "Unparsed data differs at character %d. Expected '%s'. Actual was '%s'. Expected data %s, actual data %s"
            .format(index, expected, actual, expectedCharsToShow, actualCharsToShow)
        throw TDMLException(msg, implString)
      }
    }
  }

  def verifyBinaryOrMixedData(
    expectedData: InputStream,
    actualOutStream: java.io.ByteArrayOutputStream,
    implString: Option[String]
  ): Unit = {
    val actualBytes = actualOutStream.toByteArray
    lazy val actual8859String =
      StandardCharsets.ISO_8859_1.newDecoder().decode(ByteBuffer.wrap(actualBytes)).toString()
    lazy val displayableActual =
      Misc.remapStringToVisibleGlyphs(actual8859String)

    val expectedBytes = IOUtils.toByteArray(expectedData)
    lazy val expected8859String =
      StandardCharsets.ISO_8859_1.newDecoder().decode(ByteBuffer.wrap(expectedBytes)).toString()
    lazy val displayableExpected =
      Misc.remapStringToVisibleGlyphs(expected8859String)

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
        throw TDMLException(
          "Unexpected data was created: '" + displayableActual + "'",
          implString
        )
      }
      return // we're done. Nothing equals nothing.
    }

    // compare expected data to what was output.
    if (actualBytes.length != readCount) {
      val bytesToShow = if (actualBytes.isEmpty) "" else " for " + Misc.bytes2Hex(actualBytes)
      throw TDMLException(
        "output data length " + actualBytes.length + bytesToShow +
          " doesn't match expected length " + readCount + " for " + Misc.bytes2Hex(
            expectedBytes
          ) +
          expectedAndActualDisplayStrings,
        implString
      )
    }

    val pairs = expectedBytes.zip(actualBytes).zip(LazyList.from(1))
    pairs.foreach { case ((expected, actual), index) =>
      if (expected != actual) {
        val msg = "Unparsed data differs at byte %d. Expected 0x%02x. Actual was 0x%02x."
          .format(index, expected, actual) +
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

  // TODO: We want this default to be false eventually.
  // The rationale is if people want a default namespace
  // in their TDML defineSchemas, they should define it. We shouldn't tack
  // such a sensitive thing on automatically. (81 tests fail in daffodil-test
  // if you change this however.)
  //
  final val DEFAULT_USE_DEFAULT_NAMESPACE_VALUE = true

  val name = (xml \ "@name").text.toString
  val elementFormDefault = {
    val value = (xml \ "@elementFormDefault").text.toString

    if (value == "") DEFAULT_ELEMENT_FORM_DEFAULT_VALUE
    else value
  }
  val useDefaultNamespace = {
    val value = (xml \ "@useDefaultNamespace").text.toString
    if (value == "") DEFAULT_USE_DEFAULT_NAMESPACE_VALUE
    else value.toBoolean
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
  private val parentDiagnosticFileName = parent.tsURISchemaSource.diagnosticFile.getPath
  val fileName = if (parentDiagnosticFileName.nonEmpty) {
    parentDiagnosticFileName
  } else {
    parent.ts.attribute(XMLUtils.INT_NS, XMLUtils.FILE_ATTRIBUTE_NAME) match {
      case Some(seqNodes) => seqNodes.toString
      case None => ""
    }
  }
  lazy val xsdSchema =
    SchemaUtils.dfdlTestSchema(
      importIncludes,
      dfdlTopLevels,
      xsdTopLevels,
      fileName = fileName,
      schemaScope = xml.scope,
      elementFormDefault = elementFormDefault,
      useDefaultNamespace = useDefaultNamespace
    )

  lazy val schemaSource = EmbeddedSchemaSource(xsdSchema, name)
}

case class DefinedConfig(xml: Node, parent: DFDLTestSuite) {
  val name = (xml \ "@name").text.toString
  val dafConfig = DaffodilConfig.fromXML(xml)
  def externalVariableBindings = dafConfig.externalVariableBindings
  def tunablesMap = dafConfig.tunablesMap

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
        val groups = dataDocumentParts.groupBy(_.explicitBitOrder).map { case (key, seq) =>
          (key, seq.length)
        }
        if (groups.get(Some(MSBFirst)) == Some(nDocumentParts)) MSBFirst // all are msb first
        else if (groups.get(Some(LSBFirst)) == Some(nDocumentParts))
          LSBFirst // all are lsb first
        else if (groups.get(None) == Some(nDocumentParts))
          MSBFirst // everything is silent on bit order.
        else {
          // Some mixture of explicit and non-explicit bitOrder
          Assert.usageError(
            "Must specify bitOrder on document element when parts have a mixture of bit orders."
          )
        }
      }
    }
  }

  private lazy val Seq(<document>{children @ _*}</document>) = d

  private val actualDocumentPartElementChildren = children.toList.flatMap { child =>
    child match {
      case <documentPart>{_*}</documentPart> => {
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
        case <documentPart>{_*}</documentPart> => // ok
        case scala.xml.Text(s) if (s.matches("""\s+""")) => // whitespace text nodes ok
        case scala.xml.Comment(_) => // ok
        case scala.xml.PCData(s) => // ok
        case scala.xml.EntityRef(_) => // ok
        case _: scala.xml.Atom[
              _
            ] => // ok. Things like &lt; come through as this. Should be EntityRef("lt")
        case x => Assert.usageError("Illegal TDML data document content '" + x + "'")
      }
    }
  }

  private lazy val unCheckedDocumentParts: Seq[DocumentPart] = {
    val udp =
      if (actualDocumentPartElementChildren.nonEmpty) actualDocumentPartElementChildren
      else List(new TextDocumentPart(<documentPart type="text">{children}</documentPart>, this))
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
      "There can be only one documentPart of type file, and it must be the only documentPart."
    )
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
    val cumulativeDocumentPartLengthsInBits = lengths.scanLeft(0) { case (sum, num) =>
      sum + num
    }
    val docPartBitOrders = dps.map {
      _.partBitOrder
    }
    val transitions = docPartBitOrders
      .zip(docPartBitOrders.tail)
      .zip(cumulativeDocumentPartLengthsInBits.tail)
      .zip(dps)
    transitions.foreach {
      case (((bitOrderPrior, bitOrderHere), cumulativeLength), docPart) => {
        Assert.usage(
          (bitOrderPrior == bitOrderHere) || ((cumulativeLength % 8) == 0),
          "bitOrder can only change on a byte boundary."
        )
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
        val ltrBits = rtlBits.reverse
          .sliding(8, 8)
          .map {
            _.reverse
          }
          .toList
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
      Assert.usage(
        documentParts.length == 1,
        "There can be only one documentPart of type file, and it must be the only documentPart."
      )
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
          case (BitOrder.LeastSignificantBitFirst, LSBFirst) => // ok
          case (BitOrder.MostSignificantBitFirst, MSBFirst) => // ok
          case (BitOrder.LeastSignificantBitFirst, _) => err(upperName, LSBFirst)
          case _ => err(upperName, MSBFirst)
        }
      }
      case null =>
        Assert.usageError(
          "Unsupported encoding: " + encodingName + ". Supported encodings: " + CharsetUtils.supportedEncodingsString
        )
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
    // System.out.println("encodeUtf8ToBits")
    val bytes = UTF8Encoder.utf8LikeEncode(textContentWithoutEntities).toArray
    val res = bytes.map { b => (b & 0xff).toBinaryString.reverse.padTo(8, '0').reverse }.toList
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
    val bitStrings = res.map { b =>
      (b & 0xff).toBinaryString.reverse.padTo(8, '0').reverse
    }.toList
    val allBits = bitStrings.reverse.mkString.takeRight(nBits)
    val bitChunks = allBits.reverse
      .sliding(byteSize, byteSize)
      .map {
        _.reverse
      }
      .toList
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
    val bitStrings = res.map { b =>
      (b & 0xff).toBinaryString.reverse.padTo(8, '0').reverse
    }.toList
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

  lazy val hexDigits = CanonData.canonicalizeData(validHexDigits, partRawContent)

}

final class BitsDocumentPart(part: Node, parent: Document)
  extends DataDocumentPart(part, parent) {
  val validBits = "01"

  lazy val bitDigits = CanonData.canonicalizeData(validBits, partRawContent)

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

class FileDocumentPart(part: Node, parent: Document) extends DocumentPart(part, parent) {

  override lazy val nBits =
    if (lengthInBytes != -1L) lengthInBytes * 8
    else -1L // signifies we do not know how many.

  lazy val (url, lengthInBytes) = {
    val maybeURI = parent.parent.parent.findTDMLResource(partRawContent.trim())
    val uri = maybeURI.getOrElse(
      throw new FileNotFoundException(
        "TDMLRunner: data file '" + partRawContent + "' was not found"
      )
    )
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
      "bitOrder may not be specified on document parts of type 'file'"
    )
    bitOrd
  }

  lazy val partBitOrder = explicitBitOrder.getOrElse(parent.documentBitOrder)

  lazy val partByteOrder = {
    val bo = (part \ "@byteOrder").toString match {
      case "RTL" => {
        Assert.usage(
          partBitOrder == LSBFirst,
          "byteOrder RTL can only be used with bitOrder LSBFirst"
        )
        RTL
      }
      case "LTR" => LTR
      case "" => LTR
      case _ => Assert.invariantFailed("invalid byte order.")
    }
    Assert.usage(
      this.isInstanceOf[ByteDocumentPart] || this.isInstanceOf[BitsDocumentPart],
      "byteOrder many only be specified for document parts of type 'byte' or 'bits'"
    )
    bo
  }

  /**
   * Only trim nodes that aren't PCData (aka <![CDATA[...]]>)
   */
  lazy val trimmedParts = part.child.flatMap { childNode =>
    childNode match {
      case scala.xml.PCData(s) => Some(childNode)
      case scala.xml.Text(s) => Some(childNode)
      case scala.xml.Comment(_) => None
      case scala.xml.EntityRef(_) => Some(childNode)
      case _: scala.xml.Atom[_] =>
        Some(childNode) // Things like &lt; come through as this. Should be EntityRef
      case _ =>
        Assert.invariantFailed("unrecognized child part in TextDocumentPart: " + childNode)
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

object CanonData {
  private lazy val doubleForwardPattern = "//.*".r
  private lazy val openClosePattern = "(?s)/[*].*?[*]/".r
  private lazy val noWarnCharsSet = "|()[].Xx \n\r"

  /*
   * Allow "//" and "/* */" to act as comments.
   * Any valid XML characters not explicitly allowed are also considered comments and are removed.
   */
  def canonicalizeData(validCharactersSet: String, userData: String): String = {
    var doWarning: Boolean = false

    // Remove the comments (//) and (/* */)
    val noCommentsForward = doubleForwardPattern.replaceAllIn(userData, "")
    val noCommentsBothFormats = openClosePattern.replaceAllIn(noCommentsForward, "")

    // Throw exception if /* or */ still found. This means user input was not formatted correctly.
    if (noCommentsBothFormats.contains("/*") || noCommentsBothFormats.contains("*/")) {
      throw TDMLException("Improper formatting of /* */ style comment", None)
    }

    // Check value of the characters, if invalid character found create log and skip over it
    val validData = noCommentsBothFormats.filter { ch =>
      if (validCharactersSet.contains(ch)) true
      else {
        if (!noWarnCharsSet.contains(ch)) doWarning = true
        false
      }
    }

    if (doWarning) {
      Logger.log.warn(
        "Data contains invalid character(s). Consider using a comment (// or /* */)."
      )
    }

    validData
  }
}

case class Infoset(i: NodeSeq, parent: TestCase) {
  lazy val Seq(dfdlInfoset) = (i \ "dfdlInfoset").map { node => DFDLInfoset(node, this) }
  lazy val contents = dfdlInfoset.contents
}

case class DFDLInfoset(di: Node, parent: Infoset) {

  private lazy val testCase: TestCase = parent.parent
  private lazy val loader = testCase.parent.loader
  private val ty: String = {
    val t = (di \ "@type").text.trim
    if (t.isEmpty) "infoset" else t
  }

  private val elemOrStr: Either[Elem, String] = {
    val (elems, others) = di.child.partition(_.isInstanceOf[scala.xml.Elem])
    (elems, others.text.trim) match {
      case (Seq(elem: Elem), "") if (ty == "infoset") => Left(elem)
      case (Seq(), str) if (ty == "file") => Right(str)
      case _ =>
        Assert.usageError(
          """dfdlInfoset element must contain a single root element or a file path (when 'type="file"')."""
        )
    }
  }

  lazy val contents: Elem = {
    elemOrStr match {
      case Left(elem) => elem
      case Right(path) => infosetNodeFromFile(path)
    }
  }

  private def infosetNodeFromFile(path: String): Elem = {

    val maybeURI = parent.parent.parent.findTDMLResource(path)
    val uri = maybeURI.getOrElse(
      throw new FileNotFoundException(
        "TDMLRunner: infoset file '" + path + "' was not found"
      )
    )
    val infosetSrc = URISchemaSource(uriToDiagnosticFile(uri), uri)

    val testSuite = testCase.parent
    val before = testSuite.loadingExceptions.clone()

    val elem = loader.load(infosetSrc, None) // no schema
    //
    // TODO: DAFFODIL-288 validate the infoset also
    // You can pass the optDataSchema, which appears to be the correct thing
    // but in many cases it doesn't seem to be able to resolve things.
    //
    // val elem: Node = loader.load(infosetSrc, optDataSchema)

    // The expected infoset is loaded using the normalizeCRLFtoLF mode (which
    // is the default), so MS-DOS/Windows CRLFs in expected data XML files will
    // have been converted into LF at this point.
    //
    // This is quite an expensive check however, so leaving it out for now
    // if ((elem ne null)) Assert.invariant(!elem.toString.contains("\r"))

    val nAfter = testSuite.loadingExceptions.size
    val hasMoreExceptions = before.size < nAfter
    if (hasMoreExceptions) {
      val newExceptions = (testSuite.loadingExceptions.diff(before)).toSeq
      testCase.toss(TDMLException(newExceptions, None), None)
    }
    elem.asInstanceOf[Elem]
  }

}

object DiagnosticType extends Enumeration {
  type DiagnosticType = Value
  val Error, Warning, ValidationError = Value
}

abstract class ErrorWarningBase(n: NodeSeq, parent: TestCase) {
  lazy val matchAttrib = if ((n \ "@match").text == "") {
    "all"
  } else {
    (n \ "@match").text
  }

  def isError: Boolean
  def diagnosticType: DiagnosticType

  protected def diagnosticNodes: Seq[Node]

  lazy val messages: Seq[String] = diagnosticNodes.map {
    _.text.trim
  }

  def hasDiagnostics: Boolean = diagnosticNodes.nonEmpty
}

case class ExpectedErrors(node: NodeSeq, parent: TestCase)
  extends ErrorWarningBase(node, parent) {

  val diagnosticNodes = (node \\ "error")
  override def isError = true
  override def diagnosticType: DiagnosticType = DiagnosticType.Error

}

case class ExpectedWarnings(node: NodeSeq, parent: TestCase)
  extends ErrorWarningBase(node, parent) {

  val diagnosticNodes = (node \\ "warning")
  override def isError = false
  override def diagnosticType: DiagnosticType = DiagnosticType.Warning

}

case class ExpectedValidationErrors(node: NodeSeq, parent: TestCase)
  extends ErrorWarningBase(node, parent) {

  val diagnosticNodes = (node \\ "error")
  override def isError = true
  override def diagnosticType: DiagnosticType = DiagnosticType.ValidationError

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
    Assert.usage(prev <= 0xffff)
    Assert.usage(c <= 0xffff)
    Assert.usage(next <= 0xffff)

    val i = c.toInt
    val byte1 = ((i >> 8) & 0xff)
    val byte2 = (i & 0xff)

    def threeByteEncode() = {
      val low6 = byte2 & 0x3f
      val mid6 = ((byte1 & 0x0f) << 2) | (byte2 >> 6)
      val high4 = byte1 >> 4
      byteList(high4 | 0xe0, mid6 | 0x80, low6 | 0x80)
    }

    /**
     * create 4-byte utf-8 encoding from surrogate pair found
     * in a scala string.
     */
    def fourByteEncode(leadingSurrogate: Char, trailingSurrogate: Char) = {
      val h = leadingSurrogate.toInt // aka 'h for high surrogate'
      val l = trailingSurrogate.toInt // aka 'l for low surrogate'
      val cp = 0x10000 + ((h - 0xd800) * 0x400) + (l - 0xdc00)
      val byte2 = (cp >> 16) & 0xff
      val byte3 = (cp >> 8) & 0xff
      val byte4 = cp & 0xff
      val low6 = byte4 & 0x3f
      val midlow6 = ((byte3 & 0x0f) << 2) | (byte4 >> 6)
      val midhig6 = ((byte2 & 0x03) << 4) | byte3 >> 4
      val high3 = byte2 >> 2
      byteList(high3 | 0xf0, midhig6 | 0x80, midlow6 | 0x80, low6 | 0x80)
    }

    val res = i match {
      case _ if (i <= 0x7f) => byteList(byte2)
      case _ if (i <= 0x7ff) => {
        val low6 = byte2 & 0x3f
        val high5 = ((byte1 & 0x07) << 2) | (byte2 >> 6)
        byteList(high5 | 0xc0, low6 | 0x80)
      }
      case _ if (Character.isHighSurrogate(c)) => {
        // High (initial) Surrogate character case.
        if (Character.isLowSurrogate(next)) {
          // Next codepoint is a low surrogate.
          // We need to create a 4-byte representation from the
          // two surrogate characters.
          fourByteEncode(c, next)
        } else {
          // isolated high surrogate codepoint case.
          threeByteEncode()
        }
      }
      case _ if (Character.isLowSurrogate(c)) => {
        // Low (subsequent) Surrogate character case.
        if (Character.isHighSurrogate(prev)) {
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
      case _ if (i <= 0xffff) => {
        threeByteEncode()
      }

      case _ => Assert.invariantFailed("char code out of range.")
    }
    res
  }

}

case class TDMLCompileResultCacheKey(
  impl: String,
  suppliedSchema: DaffodilSchemaSource,
  useSerializedProcessor: Boolean,
  optRootName: Option[String],
  optRootNamespace: Option[String],
  tunables: Map[String, String]
)

/**
 * Stores the compile result as well as the time when this compile result
 * should be removed from the cache.
 *
 * If optExpireTimeMillis is set to None, it means a DFDLTestSuite exists that
 * is using this compile result, and so it will not expire.
 *
 * If optExpireTimeMillis is set to Some, it means no DFDLTestSuite currently
 * uses the compile result, and it can be expired once the current time reaches
 * the Some value time
 */
case class TDMLCompileResultCacheValue(
  compileResult: TDML.CompileResult,
  var optExpireTimeMillis: Option[Long] = None
)

/**
 * A cache for TDML Compile Results.
 *
 * If entryExpireDurationSeconds is None, entries in this cache will never expire.
 *
 * If entryExpireDurationSeconds is Some, entries in this cache will expire.
 * When a DFDLTestSuite is finished, we set the optExpireTimeMillis field in
 * the associated entries to to the current time + the value. The entries will
 * be removed from the cache when we reach that time. This means that compile
 * results are guaranteed to exist for entryExpireDurationSeconds after a
 * DFDLTestSuite is done with it. This should hopefully allow enough time for
 * other DFDLTestSuites that use the schema to start and run tests using these
 * schema. If another DFDLTestSuite does so (i.e. we have a cache hit) we reset
 * the entries optExpireTimeMillis back to None, causing it to not expire until
 * the new DFDLTestSuite finishes and sets the optExpireTimeMillis back to
 * Some.
 */
case class TDMLCompileResultCache(entryExpireDurationSeconds: Option[Long]) {

  private val cache =
    new mutable.HashMap[TDMLCompileResultCacheKey, TDMLCompileResultCacheValue]()

  private def removeExpiredEntries(): Unit = {
    val now = System.currentTimeMillis()
    val keysToRemove = cache.filterNot { case (k, v) =>
      v.optExpireTimeMillis.forall(now < _)
    }.keys
    keysToRemove.foreach(cache.remove)
  }

  def setEntriesFinished(keys: mutable.Set[TDMLCompileResultCacheKey]): Unit =
    this.synchronized {
      val now = System.currentTimeMillis()
      val expireTime = Some(now + (entryExpireDurationSeconds.get * 1000))
      keys.foreach { key =>
        val optVal = cache.get(key)
        if (optVal.isDefined) {
          optVal.get.optExpireTimeMillis = expireTime
        }
      }
    }

  def getCompileResult(
    impl: AbstractTDMLDFDLProcessorFactory,
    key: TDMLCompileResultCacheKey,
    defaultValidationMode: ValidationMode.Type
  ): TDML.CompileResult = this.synchronized {

    if (entryExpireDurationSeconds.isDefined) {
      removeExpiredEntries()
    }

    val optVal = cache.get(key)
    if (optVal.isDefined) {
      val v = optVal.get
      // If some other DFDL Test Suite set an expire time for this compile
      // result it may be scheduled to be expired. But this cache hit means
      // that some other DFDLTestSuite started and is running a test with the
      // same compile result. By setting the expire time of this entry to None,
      // we essentially reset this expire time, ensuring that this entry exists
      // for expire duration after the DFDLTestSuite finishes.
      v.optExpireTimeMillis = None
      v.compileResult
    } else {
      val compileResult = impl.getProcessor(
        key.suppliedSchema,
        key.useSerializedProcessor,
        key.optRootName,
        key.optRootNamespace,
        key.tunables
      )
      compileResult match {
        case Left(diags) => {
          // a Left must have at least one error diagnostic if we don't get a processor
          Assert.invariant(diags.exists(_.isError))
        }
        case Right((diags, _)) => {
          // if a Right has diags, they must all be warnings
          Assert.invariant(diags.forall(!_.isError))
        }
      }

      // if compileResult is Right and we got a processor, set the processor validation mode to
      // the default validation mode of the test suite. This is useful in cases where the
      // default validation mode is on/full, in which case the Xerces validator will be compiled
      // and added to the DataProcessor that we cache. This way any tests that do not change the
      // validation mode from the default will be able to use the same validator so we won't
      // have to rebuild it for every test. This saves memory and should be significantly
      // faster.
      val value = compileResult.map { case (diags, proc) =>
        (diags, proc.withValidationMode(defaultValidationMode))
      }
      cache += (key -> TDMLCompileResultCacheValue(value, None))
      value
    }
  }

}

object GlobalTDMLCompileResultCache {
  val expireTimeSeconds = 30
  val cache = {
    new TDMLCompileResultCache(Some(expireTimeSeconds))
  }
}
