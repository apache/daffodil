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

package org.apache.daffodil.japi

import java.io.File
import java.net.URI
import java.nio.channels.ReadableByteChannel
import java.nio.channels.WritableByteChannel
import scala.jdk.CollectionConverters._

import org.apache.daffodil.core.compiler.{ Compiler => SCompiler }
import org.apache.daffodil.core.compiler.{ InvalidParserException => SInvalidParserException }
import org.apache.daffodil.core.compiler.{ ProcessorFactory => SProcessorFactory }
import org.apache.daffodil.core.dsom.ExpressionCompilers
import org.apache.daffodil.core.dsom.walker.RootView
import org.apache.daffodil.japi.debugger._
import org.apache.daffodil.japi.infoset._
import org.apache.daffodil.japi.io.InputSourceDataInputStream
import org.apache.daffodil.japi.packageprivate._
import org.apache.daffodil.lib.api.URISchemaSource
import org.apache.daffodil.lib.api.Validator
import org.apache.daffodil.lib.api.{ DataLocation => SDataLocation }
import org.apache.daffodil.lib.api.{ Diagnostic => SDiagnostic }
import org.apache.daffodil.lib.api.{ LocationInSchemaFile => SLocationInSchemaFile }
import org.apache.daffodil.lib.api.{ WithDiagnostics => SWithDiagnostics }
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.xml.DFDLCatalogResolver
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.api.DFDL.{
  DaffodilUnhandledSAXException => SDaffodilUnhandledSAXException
}
import org.apache.daffodil.runtime1.api.DFDL.{
  DaffodilUnparseContentHandler => SDaffodilUnparseContentHandler
}
import org.apache.daffodil.runtime1.api.DFDL.{
  DaffodilUnparseErrorSAXException => SDaffodilUnparseErrorSAXException
}
import org.apache.daffodil.runtime1.api.MetadataHandler
import org.apache.daffodil.runtime1.debugger.Debugger
import org.apache.daffodil.runtime1.debugger.{ InteractiveDebugger => SInteractiveDebugger }
import org.apache.daffodil.runtime1.debugger.{ TraceDebuggerRunner => STraceDebuggerRunner }
import org.apache.daffodil.runtime1.processors.{
  DaffodilParseXMLReader => SDaffodilParseXMLReader
}
import org.apache.daffodil.runtime1.processors.{ DataProcessor => SDataProcessor }
import org.apache.daffodil.runtime1.processors.{
  ExternalVariableException => SExternalVariableException
}
import org.apache.daffodil.runtime1.processors.{
  InvalidUsageException => SInvalidUsageException
}
import org.apache.daffodil.runtime1.processors.{ ParseResult => SParseResult }
import org.apache.daffodil.runtime1.processors.{ UnparseResult => SUnparseResult }

import org.xml.sax.Attributes
import org.xml.sax.ContentHandler
import org.xml.sax.DTDHandler
import org.xml.sax.EntityResolver
import org.xml.sax.ErrorHandler
import org.xml.sax.InputSource
import org.xml.sax.Locator
import org.xml.sax.SAXException
import org.xml.sax.XMLReader

/**
 * API Suitable for Java programmers to use.
 */
class Daffodil private {
  // Having this empty but private companion class removes the constructor from
  // JAPI Javadocs, as well as prevents the creation of a Daffodil class,
  // forcing one to use the static methods on the Daffodil object
}

object Daffodil {

  /**
   * Create a new object used to compiled DFDL schemas
   *
   * @return new object to compile DFDL schemas
   */
  def compiler(): Compiler = {
    new Compiler(SCompiler())
  }

}

/**
 * Compile DFDL schemas into [[ProcessorFactory]]'s or reload saved parsers into [[DataProcessor]]'s.
 *
 * Do not use the Compiler constructor to create a Compiler. Instead, use [[Daffodil#compiler()]].
 */
class Compiler private[japi] (private var sCompiler: SCompiler) {

  private def copy(sCompiler: SCompiler = sCompiler) = new Compiler(sCompiler)

  /**
   * Compile DFDL schema file into a [[ProcessorFactory]]
   *
   * To allow jar-file packaging, (where schema files might be part of a jar),
   * it is recommended to use [[Compiler#compileSource]] instead.
   *
   * @param schemaFile DFDL schema file used to create a [[ProcessorFactory]].
   * @return [[ProcessorFactory]] used to create [[DataProcessor]](s). Must check [[ProcessorFactory#isError]] before using it.
   * @throws java.io.IOException if an I/O error occurs while reading the schemaFile
   */
  @throws(classOf[java.io.IOException])
  def compileFile(schemaFile: File): ProcessorFactory = compileFile(schemaFile, null, null)

  /**
   * Compile DFDL schema file into a [[ProcessorFactory]]
   *
   * To allow jar-file packaging, (where schema files might be part of a jar),
   * it is recommended to use [[Compiler#compileSource]] instead.
   *
   * @param schemaFile DFDL schema file used to create a [[ProcessorFactory]].
   * @param rootName name of root element, or null to choose automatically from first element of schema.
   * @param rootNamespace String of namespace of the root element, or null to infer automatically when unambiguous. Pass "" (empty string) for No Namespace.*
   * @return [[ProcessorFactory]] used to create [[DataProcessor]](s). Must check [[ProcessorFactory#isError]] before using it.
   * @throws java.io.IOException if an I/O error occurs while reading the schemaFile
   */
  @throws(classOf[java.io.IOException])
  def compileFile(
    schemaFile: File,
    rootName: String,
    rootNamespace: String
  ): ProcessorFactory = {

    val pf = sCompiler.compileFile(schemaFile, Option(rootName), Option(rootNamespace))
    new ProcessorFactory(pf)
  }

  /**
   * Compile DFDL schema source into a [[ProcessorFactory]]
   *
   * @param uri URI of DFDL schema file used to create a [[ProcessorFactory]].
   * @return [[ProcessorFactory]] used to create [[DataProcessor]](s). Must check [[ProcessorFactory#isError]] before using it.
   * @throws java.io.IOException if an I/O error occurs while reading the uri
   */
  @throws(classOf[java.io.IOException])
  def compileSource(uri: URI): ProcessorFactory = compileSource(uri, null, null)

  /**
   * Compile DFDL schema source into a [[ProcessorFactory]]
   *
   * @param uri URI of DFDL schema file used to create a [[ProcessorFactory]].
   * @param rootName name of root element, or null to choose automatically from first element of schema.
   * @param rootNamespace String of namespace of the root element, or null to infer automatically when unambiguous. Pass "" (empty string) for No Namespace.
   * @return [[ProcessorFactory]] used to create [[DataProcessor]](s). Must check [[ProcessorFactory#isError]] before using it.
   * @throws java.io.IOException if an I/O error occurs while reading the uri
   */
  @throws(classOf[java.io.IOException])
  def compileSource(uri: URI, rootName: String, rootNamespace: String): ProcessorFactory = {
    val source = URISchemaSource(Misc.uriToDiagnosticFile(uri), uri)
    val pf = sCompiler.compileSource(source, Option(rootName), Option(rootNamespace))
    new ProcessorFactory(pf)
  }

  /**
   * Compile DFDL resource name into a [[ProcessorFactory]]
   *
   * @param name Resource name of a DFDL schema used to create a [[ProcessorFactory]].
   * @return [[ProcessorFactory]] used to create [[DataProcessor]](s). Must check [[ProcessorFactory#isError]] before using it.
   * @throws java.io.IOException if an I/O error occurs while reading the uri
   * @throws java.io.FileNotFoundException if the resource could not be found
   */
  @throws(classOf[java.io.IOException])
  def compileResource(name: String): ProcessorFactory = compileResource(name, null, null)

  /**
   * Compile DFDL resource name into a [[ProcessorFactory]]
   *
   * @param name Resource name of a DFDL schema used to create a [[ProcessorFactory]].
   * @param rootName name of root element, or null to choose automatically from first element of schema.
   * @param rootNamespace String of namespace of the root element, or null to infer automatically when unambiguous. Pass "" (empty string) for No Namespace.
   * @return [[ProcessorFactory]] used to create [[DataProcessor]](s). Must check [[ProcessorFactory#isError]] before using it.
   * @throws java.io.IOException if an I/O error occurs while reading the uri
   * @throws java.io.FileNotFoundException if the resource could not be found
   */
  @throws(classOf[java.io.IOException])
  @throws(classOf[java.io.FileNotFoundException])
  def compileResource(
    name: String,
    rootName: String,
    rootNamespace: String
  ): ProcessorFactory = {
    val uri = Misc.getRequiredResource(name)
    val source = URISchemaSource(new File(name), uri)
    val pf = sCompiler.compileSource(source, Option(rootName), Option(rootNamespace))
    new ProcessorFactory(pf)
  }

  /**
   * Reload a saved parser from a file
   *
   * To allow jar-file packaging, (where the savedParser might be part of a jar),
   * it is recommended to use the other version of [[Compiler#reload(java.nio.channels.ReadableByteChannel)]] where the argument is
   * a [[java.nio.channels.ReadableByteChannel]] for a saved parser.
   *
   * @param savedParser file of a saved parser, created with [[DataProcessor#save(java.nio.channels.WritableByteChannel)]]
   *
   * @return [[DataProcessor]] used to parse data. Must check [[DataProcessor#isError]] before using it.
   *
   * @throws InvalidParserException if the file is not a valid saved parser.
   */
  @throws(classOf[InvalidParserException])
  def reload(savedParser: File): DataProcessor = {
    try {
      new DataProcessor(sCompiler.reload(savedParser).asInstanceOf[SDataProcessor])
    } catch {
      case ex: SInvalidParserException => throw new InvalidParserException(ex)
    }
  }

  /**
   * Reload a saved parser from a [[java.nio.channels.ReadableByteChannel]]
   *
   * @param savedParser [[java.nio.channels.ReadableByteChannel]] of a saved parser, created with [[DataProcessor#save(java.nio.channels.WritableByteChannel)]]
   *
   * @return [[DataProcessor]] used to parse data. Must check [[DataProcessor#isError]] before using it.
   *
   * @throws InvalidParserException if the file is not a valid saved parser.
   */
  @throws(classOf[InvalidParserException])
  def reload(savedParser: ReadableByteChannel): DataProcessor = {
    try {
      new DataProcessor(sCompiler.reload(savedParser).asInstanceOf[SDataProcessor])
    } catch {
      case ex: SInvalidParserException => throw new InvalidParserException(ex)
    }
  }

  /**
   * Create a new Compiler instance having a specific Daffodil tunable parameter value.
   *
   * @see <a target="_blank" href='https://daffodil.apache.org/configuration/#tunable-parameters'>Tunable Parameters</a> - list of tunables names of default values
   *
   * @param tunable name of the tunable parameter
   * @param value value of the tunable parameter
   */
  def withTunable(tunable: String, value: String): Compiler = {
    copy(sCompiler = sCompiler.withTunable(tunable, value))
  }

  /**
   * Create a new Compiler instance having specific Daffodil tunable parameter values.
   *
   * @see <a target="_blank" href='https://daffodil.apache.org/configuration/#tunable-parameters'>Tunable Parameters</a> - list of tunables names of default values
   *
   * @param tunables a map of key/value pairs, where the key is the tunable name and the value is the value it will get.
   */
  def withTunables(tunables: java.util.AbstractMap[String, String]): Compiler = {
    copy(sCompiler = sCompiler.withTunables(tunables.asScala.toMap))
  }
}

/**
 * Factory to create [[DataProcessor]]'s, used for parsing data
 *
 * Do not use the ProcessorFactry constructor to create a [[ProcessorFactory]].
 * Instead, use [[Compiler#compileFile(java.io.File)]]
 */
class ProcessorFactory private[japi] (private var pf: SProcessorFactory)
  extends WithDiagnostics(pf) {

  private def copy(pf: SProcessorFactory = pf) = new ProcessorFactory(pf)

  /**
   * Get a new [[ProcessorFactory]] having a global element specified as the root of DFDL Schema to start parsing.
   *
   * @param name name of the root node
   * @param namespace namespace of the root node. Set to empty string to specify
   *                  no namespace. Set to to NULL to figure out the namespace.
   */
  def withDistinguishedRootNode(name: String, namespace: String): ProcessorFactory =
    copy(pf = pf.withDistinguishedRootNode(name, namespace))

  /**
   * Create a [[DataProcessor]]
   *
   * @param path path to an element to use as the parsing root, relative to the distinguished root node. Currently, must be set to "/"
   * @return [[DataProcessor]] used to parse data. Must check [[DataProcessor#isError]] before using it.
   */
  def onPath(path: String) = {
    val dp = pf.onPath(path).asInstanceOf[SDataProcessor]
    val res = new DataProcessor(dp)
    res.isError() // ensure all errors have been detected before we return the DP
    res
  }

  /**
   * Wrapper class used to mimic a Scala object namespacing experimental APIs
   */
  protected class ExperimentalWrapper(val rootView: RootView)

  /**
   * Exposes the RootView object corresponding to this ProcessorFactory.  This can
   * be used to start a walk using the walkFromRoot method in a DSOM Walker.
   */
  lazy val experimental: ExperimentalWrapper = new ExperimentalWrapper(pf.rootView)

}

/**
 * Abstract class that adds diagnostic information to classes that extend it.
 *
 * When a function returns a class that extend this, one should call
 * [[WithDiagnostics#isError]] on that class before performing any further
 * actions. If an error exists, any use of that class, aside from those
 * functions in [[WithDiagnostics]], is invalid and will result in an
 * Exception.
 */
abstract class WithDiagnostics private[japi] (wd: SWithDiagnostics) extends Serializable {

  /**
   * Determine if any errors occurred in the creation of the parent object.
   *
   * @return true if it represents an error, false otherwise
   */
  def isError() = wd.isError

  /**
   * Get the list of [[Diagnostic]]'s created during the construction of the parent object
   *
   * @return list of [[Diagnostic]]'s. May contain errors or warnings, and so may be non-empty even if [[WithDiagnostics#isError]] is false.
   */
  def getDiagnostics: java.util.List[Diagnostic] =
    wd.getDiagnostics.map(new Diagnostic(_)).asJava
}

/**
 * Class containing diagnostic information
 */
class Diagnostic private[japi] (d: SDiagnostic) {

  /**
   * Get all diagnostic information, including mode name, schema context, and data location
   *
   * Future versions of Daffodil will change getMessage to return only the message portion of a
   * diagnostic, matching the behavior of getMessageOnly--users should call toString() instead.
   *
   * @return diagnostic message in string form
   */
  def getMessage(): String = d.toString()

  /**
   * Get only the message part of a diagnostic.
   *
   * This does not include mode name, schema context, or data location information
   *
   * @return diagnostic message in string form
   */
  def getMessageOnly(): String = d.getMessage()

  /**
   * Get the diagnostic mode name
   *
   * @return diagnostic mode name in string form
   */
  def getModeName(): String = d.getModeName()

  /**
   * Get a string containing the mode name, message, schema location, and data location combined
   * into a single string
   *
   * @return all diagnostic information as a string
   */
  override def toString() = d.toString

  /**
   * Get data location information relevant to this diagnostic object.
   *
   * For example, this might be a file name, and position within the file.
   *
   * @return list of [[DataLocation]]'s related to this diagnostic
   */
  def getDataLocations: java.util.List[DataLocation] =
    d.getDataLocations.map(new DataLocation(_)).asJava

  /**
   * Get schema location information relevant to this diagnostic object.
   *
   * For example, this might be a file name of a schema, and position within the schema file.
   *
   * @return list of [[LocationInSchemaFile]]'s related to this diagnostic.
   */
  def getLocationsInSchemaFiles: java.util.List[LocationInSchemaFile] =
    d.getLocationsInSchemaFiles.map(new LocationInSchemaFile(_)).asJava

  /**
   * Determine if a diagnostic object represents an error or something less serious.
   *
   * @return true if it represents an error, false otherwise
   */
  def isError = d.isError

  /**
   * Get the cause of that cause this diagnostic
   *
   * @return the exception that caused the diagnostic
   */
  def getSomeCause: Throwable = d.getSomeCause.get

  /**
   * Get the message that caused this diagnostic
   *
   * @return the message that caused the diagnostic
   */
  def getSomeMessage: String = d.getSomeMessage.get
}

/**
 * Information related to a location in data
 */
class DataLocation private[japi] (dl: SDataLocation) {
  override def toString() = dl.toString

  /**
   * Get the position of the data, in bits, using 1-based indexing
   *
   * @return position of the data in bits
   */
  def bitPos1b() = dl.bitPos1b

  /**
   * Get the position of the data, in bytes, using 1-based indexing
   *
   * @return position of the data in bytes
   */
  def bytePos1b() = dl.bytePos1b
}

/**
 * Information related to locations in DFDL schema files
 */
class LocationInSchemaFile private[japi] (lsf: SLocationInSchemaFile) {

  /**
   * Get the description of the location file, for example, containing file and line number information
   */
  override def toString() = lsf.locationDescription
}

/**
 * Compiled version of a DFDL Schema, used to parse data and get the DFDL infoset
 */
class DataProcessor private[japi] (private var dp: SDataProcessor)
  extends WithDiagnostics(dp)
  with Serializable {

  private def copy(dp: SDataProcessor = dp) = new DataProcessor(dp)

  /**
   * Obtain a new [[DataProcessor]] instance with debugging enabled or disabled.
   *
   * Before enabling, [[DataProcessor#withDebugger(Debugger)]] or [[DataProcessor#withDebuggerRunner(DebuggerRunner)]]
   * must be called to obtain a [[DataProcessor]] with a non-null debugger.
   *
   * @param flag true to enable debugging, false to disabled
   */
  def withDebugging(flag: Boolean): DataProcessor = {
    copy(dp = dp.withDebugging(flag))
  }

  /**
   * Obtain a new [[DataProcessor]] with a specified debugger runner.
   *
   * @param dr debugger runner
   */
  def withDebuggerRunner(dr: DebuggerRunner): DataProcessor = {
    val debugger = newDebugger(dr)
    copy(dp = dp.withDebugger(debugger))
  }

  /**
   * Obtain a new [[DataProcessor]] with a specified debugger.
   *
   * @param dbg debugger
   */
  def withDebugger(dbg: Debugger): DataProcessor = {
    copy(dp = dp.withDebugger(dbg))
  }

  private def newDebugger(dr: DebuggerRunner) = {
    val runner = dr match {
      case tdr: TraceDebuggerRunner => new STraceDebuggerRunner()
      case dr: DebuggerRunner => new JavaInteractiveDebuggerRunner(dr)
      case null => null
    }
    val debugger = if (runner != null) {
      new SInteractiveDebugger(runner, ExpressionCompilers)
    } else {
      null
    }
    debugger
  }

  /**
   * Obtain a new [[DataProcessor]] having a specific validation mode
   *
   * @param mode mode to control validation
   * @throws InvalidUsageException if mode is not a valid ValidateMode value
   */
  @throws(classOf[InvalidUsageException])
  def withValidationMode(mode: ValidationMode): DataProcessor = {
    try { copy(dp = dp.withValidationMode(ValidationConversions.modeToScala(mode))) }
    catch { case e: SInvalidUsageException => throw new InvalidUsageException(e) }
  }

  /**
   * Obtain a new [[DataProcessor]] having a specific validator
   * @param validator validator instance
   */
  def withValidator(validator: Validator): DataProcessor =
    copy(dp =
      dp.withValidationMode(org.apache.daffodil.lib.api.ValidationMode.Custom(validator))
    )

  /**
   * Obtain a new [[DataProcessor]] with external variables read from a Daffodil configuration file
   *
   * @see <a target="_blank" href='https://daffodil.apache.org/configuration/'>Daffodil Configuration File</a> - Daffodil configuration file format
   *
   * @param extVars file to read DFDL variables from.
   * @throws ExternalVariableException if an error occurs while setting an external variable
   */
  @throws(classOf[ExternalVariableException])
  def withExternalVariables(extVars: File): DataProcessor = {
    try { copy(dp = dp.withExternalVariables(extVars)) }
    catch {
      case e: SExternalVariableException => throw new ExternalVariableException(e.getMessage)
    }
  }

  /**
   * Obtain a new [[DataProcessor]] with multiple DFDL variables set.
   *
   * @param extVars a map of key/value pairs, where the key is the variable
   *                name, and the value is the value of the variable. The key
   *                may be preceded by a string of the form "{namespace}" to
   *                define a namespace for the variable. If preceded with "{}",
   *                then no namespace is used. If not preceded by anything,
   *                then Daffodil will figure out the namespace.
   * @throws ExternalVariableException if an error occurs while setting an external variable
   */
  @throws(classOf[ExternalVariableException])
  def withExternalVariables(extVars: java.util.AbstractMap[String, String]): DataProcessor = {
    try { copy(dp = dp.withExternalVariables(extVars.asScala.toMap)) }
    catch {
      case e: SExternalVariableException => throw new ExternalVariableException(e.getMessage)
    }
  }

  /**
   * Save the DataProcessor
   *
   * The resulting output can be reloaded by [[Compiler#reload(java.nio.channels.ReadableByteChannel)]].
   * Note that external variable bindings are not saved, and will not be restored upon reloading.
   *
   * @param output the byte channel to write the [[DataProcessor]] to
   */
  def save(output: WritableByteChannel): Unit = dp.save(output)

  /**
   * Walks the handler over the runtime [[org.apache.daffodil.runtime1.api.Metadata]] structures.
   * These provide information about name, namespace, type, simple/complex, etc.
   *
   * This is used to interface Daffodil runtime1 metadata to the metadata structures
   * of other software systems.
   *
   * See [[org.apache.daffodil.runtime1.api.MetadataHandler]] for more motivating materials about
   * runtime1 metadata walking.
   *
   * @param handler - the handler is called-back during the walk as each metadata structure is encountered.
   */
  def walkMetadata(handler: MetadataHandler): Unit = dp.walkMetadata(handler)

  /**
   *  Obtain a new [[DaffodilParseXMLReader]] from the current [[DataProcessor]].
   */
  def newXMLReaderInstance: DaffodilParseXMLReader =
    new DaffodilParseXMLReader(xmlrdr =
      dp.newXMLReaderInstance.asInstanceOf[SDaffodilParseXMLReader]
    )

  /**
   *  Obtain a new [[DaffodilUnparseContentHandler]] from the current [[DataProcessor]].
   */
  def newContentHandlerInstance(output: WritableByteChannel): DaffodilUnparseContentHandler =
    new DaffodilUnparseContentHandler(dp.newContentHandlerInstance(output))

  /**
   * Parse input data from an InputSourceDataInputStream and output the infoset to an InfosetOutputter
   *
   * @param input data to be parsed
   * @param output the InfosetOutputter that will be used to output the infoset
   * @return an object which contains the result, and/or diagnostic information.
   */
  def parse(input: InputSourceDataInputStream, output: InfosetOutputter): ParseResult = {
    val pr = dp.parse(input.dis, output).asInstanceOf[SParseResult]
    new ParseResult(pr)
  }

  /**
   * Unparse an InfosetInputter
   *
   * @param input the infoset inputter to use for unparsing
   * @param output the byte channel to write the data to
   * @return an object with contains diagnostic information
   */
  def unparse(input: InfosetInputter, output: WritableByteChannel): UnparseResult = {
    val ur = dp.unparse(input, output).asInstanceOf[SUnparseResult]
    new UnparseResult(ur)
  }
}

/**
 * Result of calling [[DataProcessor#parse(InputSourceDataInputStream, InfosetOutputter)]], containing
 * the diagnostic information, and the final data location
 */
class ParseResult private[japi] (pr: SParseResult) extends WithDiagnostics(pr) {

  /**
   * Get the [[DataLocation]] where the parse completed
   *
   * @return the data location where the parse completed
   */
  def location(): DataLocation = new DataLocation(pr.resultState.currentLocation)

  /**
   * Determine if any processing errors occurred. isError() will always return
   * true if this returns true.
   *
   * @return true if any processing errors occurred, false otherwise.
   */
  def isProcessingError(): Boolean = pr.isProcessingError

  /**
   * Determine if all validation checks passed based on the validation mode of
   * the DataProcessor. If validation mode is Off, this will always return
   * false. This is only meaningful when isProcessingError() is false.
   * isError() will always return true if this return true.
   *
   * @return true if any validation errors occurred, false otherwise.
   */
  def isValidationError(): Boolean = pr.isValidationError
}

/**
 * Result of calling [[DataProcessor#unparse(InfosetInputter, java.nio.channels.WritableByteChannel)]],
 * containing diagnostic information
 */
class UnparseResult private[japi] (ur: SUnparseResult) extends WithDiagnostics(ur) {}

/**
 * This exception will be thrown as a result of attempting to reload a saved parser
 * that is invalid (not a parser file, corrupt, etc.) or
 * is not in the GZIP format.
 */
class InvalidParserException private[japi] (
  cause: org.apache.daffodil.core.compiler.InvalidParserException
) extends Exception(cause.getMessage(), cause.getCause())

/**
 * This exception will be thrown as a result of an invalid usage of the Daffodil API
 */
class InvalidUsageException private[japi] (
  cause: org.apache.daffodil.runtime1.processors.InvalidUsageException
) extends Exception(cause.getMessage(), cause.getCause())

/**
 * This exception will be thrown if an error occurs when setting an external variable. Example of errors include:
 * - Ambiguity in variable to set
 * - Variable definition not found in a schema
 * - Variable value does not have a valid type with regards to the variable type
 * - Variable cannot be set externally
 */
class ExternalVariableException private[japi] (message: String) extends Exception(message)

/**
 * This exception is thrown when UnparseResult.isError returns true while
 * unparsing an infoset with an XMLReader and a DaffodilUnparseContentHandler. If
 * caught, the DaffodilUnparseContentHandler.getUnparseResult function can be
 * used to get the UnparseResult and error diagnostics
 */
class DaffodilUnparseErrorSAXException private[japi] (
  exception: SDaffodilUnparseErrorSAXException
) extends SAXException(exception.getMessage)

/**
 * This exception is thrown when and unexpected error occurs while unparsing an
 * infoset with an XMLReader and a DaffodilUnparseContentHandler. If caught,
 * the DaffodilUnparseContentHandler.getUnparseResult returns null. This most
 * likely represents a bug in Daffodil.
 */
class DaffodilUnhandledSAXException private[japi] (exception: SDaffodilUnhandledSAXException)
  extends SAXException(exception.getMessage, new Exception(exception.getCause))

/**
 * The full URIs needed for setting/getting properties for the [[DaffodilParseXMLReader]]
 */
object DaffodilParseXMLReader {

  /**
   * Property name to get the [[ParseResult]] from the [[DaffodilParseXMLReader]]. This property is read only.
   */
  val DAFFODIL_SAX_URN_PARSERESULT: String = XMLUtils.DAFFODIL_SAX_URN_PARSERESULT

  /**
   * Property name to get/set blob directory as String from the [[DaffodilParseXMLReader]]
   */
  val DAFFODIL_SAX_URN_BLOBDIRECTORY: String = XMLUtils.DAFFODIL_SAX_URN_BLOBDIRECTORY

  /**
   * Property name to get/set blob prefix as String from the [[DaffodilParseXMLReader]]
   */
  val DAFFODIL_SAX_URN_BLOBPREFIX: String = XMLUtils.DAFFODIL_SAX_URN_BLOBPREFIX

  /**
   * Property name to get/set blob suffix as String from the [[DaffodilParseXMLReader]]
   */
  val DAFFODIL_SAX_URN_BLOBSUFFIX: String = XMLUtils.DAFFODIL_SAX_URN_BLOBSUFFIX
}

/**
 * SAX method of parsing schema and getting the DFDL Infoset via some
 * org.xml.sax.ContentHandler, based on the org.xml.sax.XMLReader interface
 */
class DaffodilParseXMLReader private[japi] (xmlrdr: SDaffodilParseXMLReader) extends XMLReader {

  /**
   * Get the value of the feature flag
   * @param name feature flag whose value is to be retrieved
   * @return value of the feature flag
   */
  override def getFeature(name: String): Boolean = xmlrdr.getFeature(name)

  /**
   * Set the value of the feature flag
   * @param name feature flag to be set
   * @param value value to be assigned to feature flag
   */
  override def setFeature(name: String, value: Boolean): Unit = xmlrdr.setFeature(name, value)

  /**
   * Get the value of the property
   * @param name property whose value is to be retrieved
   * @return value of the property
   */
  override def getProperty(name: String): AnyRef = {
    val res = xmlrdr.getProperty(name)
    if (name == DaffodilParseXMLReader.DAFFODIL_SAX_URN_PARSERESULT) {
      val pr = new ParseResult(res.asInstanceOf[SParseResult])
      pr
    } else {
      res
    }
  }

  /**
   * Set the value of the property
   * @param name property whose value is to be set
   * @param value value to be assigned to the property
   */
  override def setProperty(name: String, value: AnyRef): Unit = xmlrdr.setProperty(name, value)

  /**
   * Register an entity resolver
   * @param resolver entity resolver to be registered
   */
  override def setEntityResolver(resolver: EntityResolver): Unit =
    xmlrdr.setEntityResolver(resolver)

  /**
   * Return the registered entity resolver
   * @return registered entity resolver or null
   */
  override def getEntityResolver: EntityResolver = xmlrdr.getEntityResolver

  /**
   * Register a DTD Handler
   * @param handler DTD Handler to be registered
   */
  override def setDTDHandler(handler: DTDHandler): Unit = xmlrdr.setDTDHandler(handler)

  /**
   * Retrieve registered DTD Handler
   * @return registered DTD Handler or null
   */
  override def getDTDHandler: DTDHandler = xmlrdr.getDTDHandler

  /**
   * Register a content handler
   * @param handler content handler to be registered
   */
  override def setContentHandler(handler: ContentHandler): Unit =
    xmlrdr.setContentHandler(handler)

  /**
   * Retrieve registered content handler
   * @return registered content handler or null
   */
  override def getContentHandler: ContentHandler = xmlrdr.getContentHandler

  /**
   * Register an error handler
   * @param handler error handler to be registered
   */
  override def setErrorHandler(handler: ErrorHandler): Unit =
    xmlrdr.setErrorHandler(handler)

  /**
   * Retrieve registered error handler
   * @return registered error handler or null
   */
  override def getErrorHandler: ErrorHandler = xmlrdr.getErrorHandler

  /**
   * Parse input data from an InputSource. Infoset can be retrieved via the registered
   * contentHandler and diagnostics via the registered errorHandler
   * @param input data to be parsed
   */
  override def parse(input: InputSource): Unit = xmlrdr.parse(input)

  /**
   * Parse data from a system identifier/URI. This method is not supported by the API.
   * @param systemId URI for data to be parsed
   */
  override def parse(systemId: String): Unit = xmlrdr.parse(systemId)

  /**
   * Parse input data from an InputSourceDataInputStream. Infoset can retrieved via the registered
   * contentHandler and diagnostics via the registered errorHandler
   * @param isdis data to be parsed
   */
  def parse(isdis: InputSourceDataInputStream): Unit = xmlrdr.parse(isdis.dis)

  /**
   * Parse input data from an InputStream. Infoset can retrieved via the registered contentHandler
   * and diagnostics via the registered errorHandler
   * @param stream data to be parsed
   */
  def parse(stream: java.io.InputStream): Unit = xmlrdr.parse(stream)

  /**
   * Parse input data from an array of bytes. Infoset can retrieved via the registered
   * contentHandler and diagnostics via the registered errorHandler
   * @param arr data to be parsed
   */
  def parse(arr: Array[Byte]): Unit = xmlrdr.parse(arr)
}

/**
 * Accepts SAX callback events from any SAX XMLReader for unparsing
 */
class DaffodilUnparseContentHandler private[japi] (
  sContentHandler: SDaffodilUnparseContentHandler
) extends ContentHandler {

  /**
   * Returns the result of the SAX unparse containing diagnostic information. In the case of an
   * DaffodilUnhandledSAXException, this will return null.
   */
  def getUnparseResult: UnparseResult = {
    val ur = sContentHandler.getUnparseResult.asInstanceOf[SUnparseResult]
    if (ur == null) null
    else new UnparseResult(ur)
  }

  override def setDocumentLocator(locator: Locator): Unit =
    sContentHandler.setDocumentLocator(locator)

  override def startDocument(): Unit =
    try {
      sContentHandler.startDocument()
    } catch {
      case e: SDaffodilUnparseErrorSAXException => throw new DaffodilUnparseErrorSAXException(e)
      case e: SDaffodilUnhandledSAXException => throw new DaffodilUnhandledSAXException(e)
    }

  override def endDocument(): Unit =
    try {
      sContentHandler.endDocument()
    } catch {
      case e: SDaffodilUnparseErrorSAXException => throw new DaffodilUnparseErrorSAXException(e)
      case e: SDaffodilUnhandledSAXException => throw new DaffodilUnhandledSAXException(e)
    }

  override def startPrefixMapping(prefix: String, uri: String): Unit =
    sContentHandler.startPrefixMapping(prefix, uri)
  override def endPrefixMapping(prefix: String): Unit =
    sContentHandler.endPrefixMapping(prefix)

  override def startElement(
    uri: String,
    localName: String,
    qName: String,
    atts: Attributes
  ): Unit =
    try {
      sContentHandler.startElement(uri, localName, qName, atts)
    } catch {
      case e: SDaffodilUnparseErrorSAXException => throw new DaffodilUnparseErrorSAXException(e)
      case e: SDaffodilUnhandledSAXException => throw new DaffodilUnhandledSAXException(e)
    }

  override def endElement(uri: String, localName: String, qName: String): Unit =
    try {
      sContentHandler.endElement(uri, localName, qName)
    } catch {
      case e: SDaffodilUnparseErrorSAXException => throw new DaffodilUnparseErrorSAXException(e)
      case e: SDaffodilUnhandledSAXException => throw new DaffodilUnhandledSAXException(e)
    }

  override def characters(ch: Array[Char], start: Int, length: Int): Unit =
    sContentHandler.characters(ch, start, length)
  override def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit =
    sContentHandler.ignorableWhitespace(ch, start, length)
  override def processingInstruction(target: String, data: String): Unit =
    sContentHandler.processingInstruction(target, data)
  override def skippedEntity(name: String): Unit =
    sContentHandler.skippedEntity(name)
}

/**
 * Returns the EntityResolver used by Daffodil to resolve import/include
 * schemaLocations.
 *
 * The entity resolver attempts to resolve namespaces and systemId's in the
 * following order:
 *
 * 1. Use an org.apache.xml.resolver.Catalog/CatalogManager. By default the
 *    Catalog only includes the daffodil-built-in-catalog.xml, but additional
 *    catalogs can be added by putting CatalogManager.properties on the
 *    classpath when daffodil is run.
 *
 * 2. If not resolved in step 1, schemaLocations are resolved relative to the
 *    importing schema URI, which could either be a file on the filesystem or in
 *    a jar on the classpath.
 *
 * The EntityResolver isn't thread safe, but it also is expensive and stateful,
 * so we use ThreadLocal to only create one instance per thread.
 */
object DaffodilXMLEntityResolver {
  def getEntityResolver: org.xml.sax.EntityResolver = DFDLCatalogResolver.get
  def getXMLEntityResolver: org.apache.xerces.xni.parser.XMLEntityResolver =
    DFDLCatalogResolver.get
  def getLSResourceResolver: org.w3c.dom.ls.LSResourceResolver = DFDLCatalogResolver.get
}
