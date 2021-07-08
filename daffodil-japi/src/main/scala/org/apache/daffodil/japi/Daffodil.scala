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
import java.nio.channels.Channels
import java.nio.channels.ReadableByteChannel
import java.nio.channels.WritableByteChannel

import scala.collection.JavaConverters._

import org.apache.daffodil.api.DFDL.{ DaffodilUnhandledSAXException => SDaffodilUnhandledSAXException }
import org.apache.daffodil.api.DFDL.{ DaffodilUnparseErrorSAXException => SDaffodilUnparseErrorSAXException }
import org.apache.daffodil.api.URISchemaSource
import org.apache.daffodil.api.Validator
import org.apache.daffodil.api.{ DataLocation => SDataLocation }
import org.apache.daffodil.api.{ Diagnostic => SDiagnostic }
import org.apache.daffodil.api.{ LocationInSchemaFile => SLocationInSchemaFile }
import org.apache.daffodil.api.{ WithDiagnostics => SWithDiagnostics }
import org.apache.daffodil.compiler.{ Compiler => SCompiler }
import org.apache.daffodil.compiler.{ InvalidParserException => SInvalidParserException }
import org.apache.daffodil.compiler.{ ProcessorFactory => SProcessorFactory }
import org.apache.daffodil.debugger.Debugger
import org.apache.daffodil.debugger.{ InteractiveDebugger => SInteractiveDebugger }
import org.apache.daffodil.debugger.{ TraceDebuggerRunner => STraceDebuggerRunner }
import org.apache.daffodil.dsom.ExpressionCompilers
import org.apache.daffodil.dsom.walker.RootView
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.externalvars.ExternalVariablesLoader
import org.apache.daffodil.japi.debugger._
import org.apache.daffodil.japi.infoset._
import org.apache.daffodil.japi.io.InputSourceDataInputStream
import org.apache.daffodil.japi.logger._
import org.apache.daffodil.japi.packageprivate._
import org.apache.daffodil.processors.{ DaffodilParseXMLReader => SDaffodilParseXMLReader }
import org.apache.daffodil.processors.{ DaffodilUnparseContentHandler => SDaffodilUnparseContentHandler }
import org.apache.daffodil.processors.{ DataProcessor => SDataProcessor }
import org.apache.daffodil.processors.{ ExternalVariableException => SExternalVariableException }
import org.apache.daffodil.processors.{ InvalidUsageException => SInvalidUsageException }
import org.apache.daffodil.processors.{ ParseResult => SParseResult }
import org.apache.daffodil.processors.{ UnparseResult => SUnparseResult }
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.xml.NS
import org.apache.daffodil.xml.XMLUtils

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
    new Daffodil // silence warning about unused private constructor
    new Compiler(SCompiler())
  }

  /**
   * Set the LogWriter to use to capture logging messages from Daffodil
   *
   * @param lw log writer to capture logging messages
   */
  @deprecated("Use Log4j for logging", "3.2.0")
  def setLogWriter(lw: LogWriter): Unit = {
  }

  /**
   * Set the maximum logging level
   *
   * @param lvl log level
   */
  @deprecated("Use Log4j for logging", "3.2.0")
  def setLoggingLevel(lvl: LogLevel): Unit = {
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
  def compileFile(schemaFile: File,
    rootName: String,
    rootNamespace: String): ProcessorFactory = {

    val pf = sCompiler.compileFile(schemaFile, Option(rootName), Option(rootNamespace))
    pf.isError
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
  def compileSource(uri: URI,
    rootName: String,
    rootNamespace: String): ProcessorFactory = {
    val source = URISchemaSource(uri)
    val pf = sCompiler.compileSource(source, Option(rootName), Option(rootNamespace))
    new ProcessorFactory(pf.asInstanceOf[SProcessorFactory])
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
   * Specify a global element to be the root of DFDL Schema to start parsing
   *
   * @param name name of the root node
   * @param namespace namespace of the root node. Set to empty string to specify
   *                  no namespace. Set to to NULL to figure out the namespace.
   */
  @deprecated("Pass arguments to compileSource, or compileFile.", "2.6.0")
  def setDistinguishedRootNode(name: String, namespace: String): Unit =
    sCompiler = sCompiler.withDistinguishedRootNode(name, namespace)

  /**
   * Set the value of a DFDL variable
   *
   * @param name name of the variable
   * @param namespace namespace of the variable. Set to empty string to specify
   *                  no namespace. Set to to NULL to figure out the namespace.
   * @param value value to so the variable to
   */
  @deprecated("Use DataProcessor.withExternalVariable.", "2.6.0")
  def setExternalDFDLVariable(name: String, namespace: String, value: String): Unit = {
    val bindings = Seq(Binding(name, Some(NS(namespace)), value))
    sCompiler = sCompiler.withExternalDFDLVariablesImpl(bindings)
  }

  /**
   * Set the value of multiple DFDL variables
   *
   * @param extVarsMap a may of key/value pairs, where the key is the variable
   *                   name, and the value is the value of the variable. The key
   *                   may be preceded by a string of the form "{namespace}" to
   *                   define a namespace for the variable. If preceded with "{}",
   *                   then no namespace is used. With not preceded by "{namespace}",
   *                   then Daffodil will figure out the namespace.
   */
  @deprecated("Use DataProcessor.withExternalVariables.", "2.6.0")
  def setExternalDFDLVariables(extVarsMap: java.util.AbstractMap[String, String]): Unit = {
    val extVars = ExternalVariablesLoader.mapToBindings(extVarsMap.asScala.toMap)
    sCompiler = sCompiler.withExternalDFDLVariablesImpl(extVars)
  }

  /**
   * Read external variables from a Daffodil configuration file
   *
   * @see <a target="_blank" href='https://daffodil.apache.org/configuration/'>Daffodil Configuration File</a> - Daffodil configuration file format
   *
   * @param extVarsFile file to read DFDL variables from.
   */
  @deprecated("Use DataProcessor.withExternalVariables.", "2.6.0")
  def setExternalDFDLVariables(extVarsFile: File): Unit = {
    val extVars = ExternalVariablesLoader.fileToBindings(extVarsFile)
    sCompiler = sCompiler.withExternalDFDLVariablesImpl(extVars)
  }

  /**
   * Enable/disable DFDL validation of resulting infoset with the DFDL schema
   *
   * @param value true to enable validation, false to disabled
   */
  @deprecated("Do not use this method. DFDL schema validation should be performed.", "2.6.0")
  def setValidateDFDLSchemas(value: Boolean): Unit =
    sCompiler = sCompiler.withValidateDFDLSchemas(value)


  /**
   * Set a Daffodil tunable parameter
   *
   * @see <a target="_blank" href='https://daffodil.apache.org/configuration/#tunable-parameters'>Tunable Parameters</a> - list of tunables names of default values
   *
   * @param tunable name of the tunable parameter to set.
   * @param value value of the tunable parameter to set
   */
  @deprecated("Use withTunable.", "2.6.0")
  def setTunable(tunable: String, value: String): Unit = {
    sCompiler = sCompiler.withTunable(tunable, value)
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
   * Set the value of multiple tunable parameters
   *
   * @see <a target="_blank" href='https://daffodil.apache.org/configuration/#tunable-parameters'>Tunable Parameters</a> - list of tunables names of default values
   *
   * @param tunables a map of key/value pairs, where the key is the tunable name and the value is the value to set it to
   */
  @deprecated("Use withTunables.", "2.6.0")
  def setTunables(tunables: java.util.AbstractMap[String, String]): Unit = {
    sCompiler = sCompiler.withTunables(tunables.asScala.toMap)
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
   * Specify a global element to be the root of DFDL Schema to start parsing
   *
   * @param name name of the root node
   * @param namespace namespace of the root node. Set to empty string to specify
   *                  no namespace. Set to to NULL to figure out the namespace.
   */
  @deprecated("Use withDistinguishedRootNode.", "2.6.0")
  def setDistinguishedRootNode(name: String, namespace: String): Unit =
    pf = pf.withDistinguishedRootNode(name, namespace)

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
    new DataProcessor(dp)
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
abstract class WithDiagnostics private[japi] (wd: SWithDiagnostics)
  extends Serializable {

  /**
   * Determine if any errors occurred in the creation of the parent object.
   *
   * @return true if no errors occurred, false otherwise
   */
  def isError() = wd.isError

  /**
   * Determine if this object can be used in any future parse activities
   *
   * @return true it is safe to proceed, false otherwise
   */
  @deprecated("Use !isError() to determine if it is safe to proceed","2.0.0")
  def canProceed() = !isError

  /**
   * Get the list of [[Diagnostic]]'s created during the construction of the parent object
   *
   * @return list of [[Diagnostic]]'s. May contain errors or warnings, and so may be non-empty even if [[WithDiagnostics#isError]] is false.
   */
  def getDiagnostics: java.util.List[Diagnostic] = wd.getDiagnostics.map(new Diagnostic(_)).asJava
}

/**
 * Class containing diagnostic information
 */
class Diagnostic private[japi] (d: SDiagnostic) {

  /**
   * Get the diagnostic message
   *
   * @return diagnostic message in string form
   */
  def getMessage(): String = d.getMessage()

  override def toString() = d.toString

  /**
   * Get data location information relevant to this diagnostic object.
   *
   * For example, this might be a file name, and position within the file.
   *
   * @return list of [[DataLocation]]'s related to this diagnostic
   */
  def getDataLocations: java.util.List[DataLocation] = d.getDataLocations.map(new DataLocation(_)).asJava

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
   * Determine if we're positioned at the end of data.
   *
   * Blocks until either one byte of data can be read, or end-of-data
   * is encountered.
   *
   * It is generally not advised to use this on network TCP data streams
   * as it will block waiting for the sender of data to provide more data
   * or close the stream.
   *
   * @return boolean indicating whether we are known to be positioned at
   *         the end of data.
   */
  @deprecated("Use comparison of bitPos1b() with expected position instead.", "3.1.0")
  def isAtEnd() = dl.isAtEnd

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
   * Enable/disable debugging.
   *
   * Before enabling, [[DataProcessor#withDebugger]] or [[DataProcessor#withDebuggerRunner(DebuggerRunner)]] must be
   * called with a non-null debugger.
   *
   * @param flag true to enable debugging, false to disabled
   */
  @deprecated("Use withDebugging.", "2.6.0")
  def setDebugging(flag: Boolean): Unit = {
    dp = dp.withDebugging(flag)
  }

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
   * Set the debugger runner
   *
   * @param dr debugger runner
   */
  @deprecated("Use withDebuggerRunner.", "2.6.0")
  def setDebugger(dr: DebuggerRunner): Unit = {
    val debugger = newDebugger(dr)
    dp = dp.withDebugger(debugger)
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
   * Set validation mode
   *
   * @param mode mode to control validation
   * @throws InvalidUsageException if mode is not a valid ValidateMode value
   */
  @deprecated("Use withValidationMode.", "2.6.0")
  @throws(classOf[InvalidUsageException])
  def setValidationMode(mode: ValidationMode): Unit = {
    try { dp = dp.withValidationMode(ValidationConversions.modeToScala(mode)) }
    catch { case e: SInvalidUsageException => throw new InvalidUsageException(e) }
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
    copy(dp = dp.withValidationMode(org.apache.daffodil.api.ValidationMode.Custom(validator)))

  /**
   * Read external variables from a Daffodil configuration file
   *
   * @see <a target="_blank" href='https://daffodil.apache.org/configuration/'>Daffodil Configuration File</a> - Daffodil configuration file format
   *
   * @param extVars file to read DFDL variables from.
   * @throws ExternalVariableException if an error occurs while setting an external variable
   */
  @deprecated("Use withExternalVariables.", "2.6.0")
  @throws(classOf[ExternalVariableException])
  def setExternalVariables(extVars: File): Unit = {
    //$COVERAGE-OFF$
    try { dp = dp.withExternalVariables(extVars) }
    catch { case e: SExternalVariableException => throw new ExternalVariableException(e.getMessage) }
    //$COVERAGE-ON$
  }

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
    catch { case e: SExternalVariableException => throw new ExternalVariableException(e.getMessage) }
  }

  /**
   * Set the value of multiple DFDL variables
   *
   * @param extVars a map of key/value pairs, where the key is the variable
   *                name, and the value is the value of the variable. The key
   *                may be preceded by a string of the form "{namespace}" to
   *                define a namespace for the variable. If preceded with "{}",
   *                then no namespace is used. If not preceded by anything,
   *                then Daffodil will figure out the namespace.
   * @throws ExternalVariableException if an error occurs while setting an external variable
   */
  @deprecated("Use withExternalVariables.", "2.6.0")
  @throws(classOf[ExternalVariableException])
  def setExternalVariables(extVars: Map[String, String]) = {
    //$COVERAGE-OFF$
    try { dp = dp.withExternalVariables(extVars) }
    catch { case e: SExternalVariableException => throw new ExternalVariableException(e.getMessage) }
    //$COVERAGE-ON$
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
  @deprecated("Use withExternalVariables that accepts a Java AbstractMap", "3.0")
  @throws(classOf[ExternalVariableException])
  def withExternalVariables(extVars: Map[String, String]): DataProcessor = {
    //$COVERAGE-OFF$
    try { copy(dp = dp.withExternalVariables(extVars)) }
    catch { case e: SExternalVariableException => throw new ExternalVariableException(e.getMessage) }
    //$COVERAGE-ON$
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
  def withExternalVariables(extVars: java.util.AbstractMap[String, String]): DataProcessor  = {
    try { copy(dp = dp.withExternalVariables(extVars.asScala.toMap)) }
    catch { case e: SExternalVariableException => throw new ExternalVariableException(e.getMessage) }
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
   *  Obtain a new [[DaffodilParseXMLReader]] from the current [[DataProcessor]].
   */
  def newXMLReaderInstance: DaffodilParseXMLReader =
    new DaffodilParseXMLReader(xmlrdr = dp.newXMLReaderInstance.asInstanceOf[SDaffodilParseXMLReader])

  /**
   *  Obtain a new [[DaffodilUnparseContentHandler]] from the current [[DataProcessor]].
   */
  def newContentHandlerInstance(output: WritableByteChannel): DaffodilUnparseContentHandler =
    new DaffodilUnparseContentHandler(sContentHandler =
      dp.newContentHandlerInstance(output).asInstanceOf[SDaffodilUnparseContentHandler])

  /**
   * Parse input data with a specified length
   *
   * @param input data to be parsed
   * @param lengthLimitInBits the length of the input data in bits, or -1 if no limit
   * @return an object which contains the result, and/or diagnostic information.
   */
  @deprecated("Use parse(InputSourceDataInputStream, InfosetOutputter) to parse the data and get the infoset representation from the InfosetOutputter instead of ParseResult#result()","2.2.0")
  def parse(input: ReadableByteChannel, lengthLimitInBits: Long): ParseResult = {
    val is = Channels.newInputStream(input)
    val dis = new InputSourceDataInputStream(is)
    if (lengthLimitInBits > 0) {
      dis.dis.setBitLimit0b(MaybeULong(lengthLimitInBits))
    }
    val output = new JDOMInfosetOutputter()
    val pr = dp.parse(dis.dis, output).asInstanceOf[SParseResult]
    new ParseResult(pr, Maybe(output))
  }

  /**
   * Parse input data without specifying a length
   *
   * @param input data to be parsed
   * @return an object which contains the result, and/or diagnostic information.
   */
  @deprecated("Use parse(InputSourceDataInputStream, InfosetOutputter) to parse the data and get the infoset representation from the InfosetOutputter instead of ParseResult#result()", "2.2.0")
  def parse(input: ReadableByteChannel): ParseResult = parse(input, -1)

  /**
   * Parse input data with a specified length
   *
   * @param input data to be parsed
   * @param output the InfosetOutputter that will be used to output the infoset
   * @param lengthLimitInBits the length of the input data in bits, or -1 if no limit
   * @return an object which contains the result, and/or diagnostic information.
   */
  @deprecated("Use parse(InputSourceDataInputStream, InfosetOutputter) to parse the data and get the infoset representation from the InfosetOutputter instead of ParseResult#result()", "2.2.0")
  def parse(input: ReadableByteChannel, output: InfosetOutputter, lengthLimitInBits: Long): ParseResult = {
    val is = Channels.newInputStream(input)
    val dis = new InputSourceDataInputStream(is)
    if (lengthLimitInBits > 0) {
      dis.dis.setBitLimit0b(MaybeULong(lengthLimitInBits))
    }
    val pr = dp.parse(dis.dis, output).asInstanceOf[SParseResult]
    new ParseResult(pr, Nope)
  }

  /**
   * Parse input data without specifying a length
   *
   * Use this when you don't know how big the data is.
   *
   * @param input data to be parsed
   * @param output the InfosetOutputter that will be used to output the infoset
   * @return an object which contains the result, and/or diagnostic information.
   */
  @deprecated("Use parse(InputSourceDataInputStream, InfosetOutputter) to parse the data and get the infoset representation from the InfosetOutputter instead of ParseResult#result()", "2.2.0")
  def parse(input: ReadableByteChannel, output: InfosetOutputter): ParseResult = parse(input, output, -1)

  /**
   * Parse input data from an InputSourceDataInputStream and output the infoset to an InfosetOutputter
   *
   * @param input data to be parsed
   * @param output the InfosetOutputter that will be used to output the infoset
   * @return an object which contains the result, and/or diagnostic information.
   */
  def parse(input: InputSourceDataInputStream, output: InfosetOutputter): ParseResult = {
    val pr = dp.parse(input.dis, output).asInstanceOf[SParseResult]
    new ParseResult(pr, Nope)
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

  /**
   * Unparse a JDOM2 infoset
   *
   * @param output the byte channel to write the data to
   * @param infoset the infoset to unparse, as a jdom event cursor
   * @return an object with contains the result and/or diagnostic information
   */
  @deprecated("Use unparse(InfosetInputter, WritableByteChannel)", "2.0.0")
  def unparse(output: WritableByteChannel, infoset: org.jdom2.Document): UnparseResult = {
    val input = new JDOMInfosetInputter(infoset)
    unparse(input, output)
  }
}

/**
 * Result of calling [[DataProcessor#parse(java.nio.channels.ReadableByteChannel, InfosetOutputter, long)]], containing
 * the diagnostic information, and the final data location
 */
class ParseResult private[japi] (pr: SParseResult, deprecatedOutput: Maybe[JDOMInfosetOutputter])
  extends WithDiagnostics(pr) {

  /**
   * Get the resulting infoset as a jdom2 Document
   *
   * @throws InvalidUsageException if you call this when isError is true
   *         because in that case there is no result document.
   *
   * @return a jdom2 Document representing the DFDL infoset for the parsed data
   */
  @deprecated("Use parse(ReadableByteChannel, InfosetInputter) to parse the data and get the infoset representation from the InfosetOutputter","2.0.0")
  @throws(classOf[InvalidUsageException])
  def result(): org.jdom2.Document = {
    // When this result function is removed due to deprecation, we should also
    // remove the deprecatedOutput parameter to the ParseResult constructor
    if (deprecatedOutput.isDefined) {
      deprecatedOutput.get.getResult()
    } else {
      val ex = new org.apache.daffodil.processors.InvalidUsageException(
        "When passing an InfosetOutputter to parse(), you must get the infoset result from the InfosetOutputter instead of the ParseResult.")
      throw new InvalidUsageException(ex)
    }
  }

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
   * @return true if any processing errors occured, false otherwise.
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
class UnparseResult private[japi] (ur: SUnparseResult)
  extends WithDiagnostics(ur) {
}

/**
 * This exception will be thrown as a result of attempting to reload a saved parser
 * that is invalid (not a parser file, corrupt, etc.) or
 * is not in the GZIP format.
 */
class InvalidParserException private[japi] (cause: org.apache.daffodil.compiler.InvalidParserException) extends Exception(cause.getMessage(), cause.getCause())

/**
 * This exception will be thrown as a result of an invalid usage of the Daffodil API
 */
class InvalidUsageException private[japi] (cause: org.apache.daffodil.processors.InvalidUsageException) extends Exception(cause.getMessage(), cause.getCause())

/**
 * This exception will be thrown if an error occurs when setting an external variable. Example of errors include:
 * - Ambiguity in variable to set
 * - Variable definition not found in a schema
 * - Variable value does not have a valid type with regards to the variable type
 * - Variable cannot be set externally
 */
class ExternalVariableException private[japi] (message: String) extends Exception(message)

/**
 * This exception will be thrown when unparseResult.isError returns true during a SAX Unparse
 */
class DaffodilUnparseErrorSAXException private[japi] (exception: SDaffodilUnparseErrorSAXException)
  extends org.xml.sax.SAXException(exception.getMessage)

/**
 * This exception will be thrown when an unexpected error occurs during the SAX unparse
 */
class DaffodilUnhandledSAXException private[japi] (exception: SDaffodilUnhandledSAXException)
  extends org.xml.sax.SAXException(exception.getMessage, new Exception(exception.getCause))

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
class DaffodilParseXMLReader private[japi] (xmlrdr: SDaffodilParseXMLReader) extends org.xml.sax.XMLReader {
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
      val pr = new ParseResult(res.asInstanceOf[SParseResult], Nope)
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
  override def setEntityResolver(resolver: org.xml.sax.EntityResolver): Unit =
    xmlrdr.setEntityResolver(resolver)

  /**
   * Return the registered entity resolver
   * @return registered entity resolver or null
   */
  override def getEntityResolver: org.xml.sax.EntityResolver = xmlrdr.getEntityResolver

  /**
   * Register a DTD Handler
   * @param handler DTD Handler to be registered
   */
  override def setDTDHandler(handler: org.xml.sax.DTDHandler): Unit = xmlrdr.setDTDHandler(handler)

  /**
   * Retrieve registered DTD Handler
   * @return registered DTD Handler or null
   */
  override def getDTDHandler: org.xml.sax.DTDHandler = xmlrdr.getDTDHandler

  /**
   * Register a content handler
   * @param handler content handler to be registered
   */
  override def setContentHandler(handler: org.xml.sax.ContentHandler): Unit =
    xmlrdr.setContentHandler(handler)

  /**
   * Retrieve registered content handler
   * @return registered content handler or null
   */
  override def getContentHandler: org.xml.sax.ContentHandler = xmlrdr.getContentHandler

  /**
   * Register an error handler
   * @param handler error handler to be registered
   */
  override def setErrorHandler(handler: org.xml.sax.ErrorHandler): Unit =
    xmlrdr.setErrorHandler(handler)

  /**
   * Retrieve registered error handler
   * @return registered error handler or null
   */
  override def getErrorHandler: org.xml.sax.ErrorHandler = xmlrdr.getErrorHandler

  /**
   * Parse input data from an InputSource. Infoset can be retrieved via the registered
   * contentHandler and diagnostics via the registered errorHandler
   * @param input data to be parsed
   */
  override def parse(input: org.xml.sax.InputSource): Unit = xmlrdr.parse(input)

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
class DaffodilUnparseContentHandler private[japi] (sContentHandler: SDaffodilUnparseContentHandler)
  extends org.xml.sax.ContentHandler {

  private val contentHandler: org.xml.sax.ContentHandler = sContentHandler

  /**
   * Returns the result of the SAX unparse containing diagnostic information. In the case of an
   * DaffodilUnhandledSAXException, this will return null.
   */
  def getUnparseResult: UnparseResult = {
    val ur = sContentHandler.getUnparseResult.asInstanceOf[SUnparseResult]
    if (ur == null) null
    else new UnparseResult(ur)
  }

  override def setDocumentLocator(locator: org.xml.sax.Locator): Unit =
    contentHandler.setDocumentLocator(locator)

  override def startDocument(): Unit =
    try {
      contentHandler.startDocument()
    } catch {
      case e: SDaffodilUnparseErrorSAXException => throw new DaffodilUnparseErrorSAXException(e)
      case e: SDaffodilUnhandledSAXException => throw  new DaffodilUnhandledSAXException(e)
    }

  override def endDocument(): Unit =
    try {
      contentHandler.endDocument()
    } catch {
      case e: SDaffodilUnparseErrorSAXException => throw new DaffodilUnparseErrorSAXException(e)
      case e: SDaffodilUnhandledSAXException => throw  new DaffodilUnhandledSAXException(e)
    }

  override def startPrefixMapping(prefix: String, uri: String): Unit =
    contentHandler.startPrefixMapping(prefix, uri)
  override def endPrefixMapping(prefix: String): Unit =
    contentHandler.endPrefixMapping(prefix)

  override def startElement(uri: String, localName: String, qName: String, atts: org.xml.sax.Attributes): Unit =
    try {
      contentHandler.startElement(uri, localName, qName, atts)
    } catch {
      case e: SDaffodilUnparseErrorSAXException => throw new DaffodilUnparseErrorSAXException(e)
      case e: SDaffodilUnhandledSAXException => throw  new DaffodilUnhandledSAXException(e)
    }

  override def endElement(uri: String, localName: String, qName: String): Unit =
    try {
      contentHandler.endElement(uri, localName, qName)
    } catch {
      case e: SDaffodilUnparseErrorSAXException => throw new DaffodilUnparseErrorSAXException(e)
      case e: SDaffodilUnhandledSAXException => throw  new DaffodilUnhandledSAXException(e)
    }

  override def characters(ch: Array[Char], start: Int, length: Int): Unit =
    contentHandler.characters(ch, start, length)
  override def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit =
    contentHandler.ignorableWhitespace(ch, start, length)
  override def processingInstruction(target: String, data: String): Unit =
    contentHandler.processingInstruction(target, data)
  override def skippedEntity(name: String): Unit =
    contentHandler.skippedEntity(name)
}
