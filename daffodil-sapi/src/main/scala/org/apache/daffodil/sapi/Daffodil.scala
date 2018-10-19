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

package org.apache.daffodil.sapi

import org.apache.daffodil.compiler.{ Compiler => SCompiler }
import org.apache.daffodil.sapi.debugger._
import org.apache.daffodil.sapi.logger._
import org.apache.daffodil.sapi.packageprivate._
import org.apache.daffodil.sapi.infoset._
import org.apache.daffodil.sapi.io.InputSourceDataInputStream
import org.apache.daffodil.debugger.{ InteractiveDebugger => SInteractiveDebugger }
import org.apache.daffodil.debugger.{ TraceDebuggerRunner => STraceDebuggerRunner }
import org.apache.daffodil.api.{ Diagnostic => SDiagnostic }
import java.io.File
import java.nio.channels.Channels
import java.nio.channels.ReadableByteChannel
import java.nio.channels.WritableByteChannel
import org.apache.daffodil.api.{ DataLocation => SDataLocation }
import org.apache.daffodil.api.{ Diagnostic => SDiagnostic }
import org.apache.daffodil.api.{ LocationInSchemaFile => SLocationInSchemaFile }
import org.apache.daffodil.api.{ WithDiagnostics => SWithDiagnostics }
import org.apache.daffodil.compiler.{ ProcessorFactory => SProcessorFactory }
import org.apache.daffodil.processors.{ DataProcessor => SDataProcessor }
import org.apache.daffodil.processors.{ ParseResult => SParseResult }
import org.apache.daffodil.processors.{ UnparseResult => SUnparseResult }
import org.apache.daffodil.util.{ ConsoleWriter => SConsoleWriter }
import org.apache.daffodil.util.{ FileWriter => SFileWriter }
import org.apache.daffodil.util.{ LogWriter => SLogWriter }
import org.apache.daffodil.util.{ LoggingDefaults => SLoggingDefaults }
import org.apache.daffodil.util.{ NullLogWriter => SNullLogWriter }
import org.apache.daffodil.externalvars.ExternalVariablesLoader
import org.apache.daffodil.dsom.ExpressionCompilers
import org.apache.daffodil.compiler.{ InvalidParserException => SInvalidParserException }
import org.apache.daffodil.processors.{ InvalidUsageException => SInvalidUsageException }
import java.net.URI
import org.apache.daffodil.api.URISchemaSource
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.util.MaybeULong

private class Daffodil private {
  // Having this empty but private companion class removes the constructor from
  // Scaladocs, as well as prevents the creation of a Daffodil class,
  // forcing one to use the static methods on the Daffodil object
}

/**
 * Factory object to create a [[Compiler]] and set global configurations
 */
object Daffodil {

  /** Create a new object used to compiled DFDL schemas */
  def compiler(): Compiler = {
    new Daffodil // silence warning about unused private constructor above.
    new Compiler()
  }

  /** Set the LogWriter to use to capture logging messages from Daffodil */
  def setLogWriter(lw: LogWriter): Unit = {
    val slw: SLogWriter = lw match {
      case clw: ConsoleLogWriter => SConsoleWriter
      case flw: FileLogWriter => new SFileWriter(flw.getFile)
      case nlw: NullLogWriter => SNullLogWriter
      case _ => new JavaLogWriter(lw)
    }
    SLoggingDefaults.setLogWriter(slw)
  }

  /** Set the maximum logging level */
  def setLoggingLevel(lvl: LogLevel.Value): Unit = {
    SLoggingDefaults.setLoggingLevel(LoggingConversions.levelToScala(lvl))
  }

}

/**
 * Validation modes for validating the resulting infoset against the DFDL schema
 */
object ValidationMode extends Enumeration {
  type ValidationMode = Value
  val Off = Value(10)
  val Limited = Value(20)
  val Full = Value(30)
}

/**
 * Compile DFDL schemas into [[ProcessorFactory]]'s or reload saved parsers into [[DataProcessor]]'s.
 */
class Compiler private[sapi] () {

  private val sCompiler = SCompiler()

  /**
   * Compile DFDL schema file into a [[ProcessorFactory]]
   *
   * To allow jar-file packaging, (where schema files might be part of a jar),
   * it is recommended to use [[Compiler.compileSource]] instead.
   *
   * @param schemaFile DFDL schema file used to create a [[ProcessorFactory]].
   * @return [[ProcessorFactory]] used to create [[DataProcessor]](s). Must check [[ProcessorFactory.isError]] before using it.
   */
  @throws(classOf[java.io.IOException])
  def compileFile(schemaFile: File): ProcessorFactory = {
    val pf = sCompiler.compileFile(schemaFile)
    pf.isError
    new ProcessorFactory(pf)
  }

  /**
   * Compile DFDL schema source into a [[ProcessorFactory]]
   *
   * @param uri URI of DFDL schema file used to create a [[ProcessorFactory]].
   * @return [[ProcessorFactory]] used to create [[DataProcessor]](s). Must check [[ProcessorFactory.isError]] before using it.
   */
  @throws(classOf[java.io.IOException])
  def compileSource(uri: URI): ProcessorFactory = {
    val source = new URISchemaSource(uri)
    val pf = sCompiler.compileSource(source)
    new ProcessorFactory(pf.asInstanceOf[SProcessorFactory])
  }

  /**
   * Reload a saved parser from a file
   *
   * To allow jar-file packaging, (where the savedParser might be part of a jar),
   * it is recommended to use the other version of [[Compiler.reload(savedParser:java\.nio\.channels\.ReadableByteChannel)* Compiler.reload]] where the argument is
   * a java.nio.channels.ReadableByteChannel for a saved parser.
   *
   * @param savedParser file of a saved parser, created with [[DataProcessor.save]]
   * @return [[DataProcessor]] used to parse data. Must check [[DataProcessor.isError]] before using it.
   * @throws InvalidParserException if the file is not a valid saved parser.
   */
  def reload(savedParser: File): DataProcessor = {
    try {
      new DataProcessor(sCompiler.reload(savedParser).asInstanceOf[SDataProcessor])
    } catch {
      case ex: SInvalidParserException => throw new InvalidParserException(ex)
    }
  }

  /**
   * Reload a saved parser from a java.nio.channels.ReadableByteChannel
   *
   * @param savedParser java.nio.channels.ReadableByteChannel of a saved parser, created with [[DataProcessor.save]]
   * @return [[DataProcessor]] used to parse data. Must check [[DataProcessor.isError]] before using it.
   * @throws InvalidParserException if the file is not a valid saved parser.
   */
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
  def setDistinguishedRootNode(name: String, namespace: String): Unit =
    sCompiler.setDistinguishedRootNode(name, namespace)

  /**
   * Set the value of a DFDL variable
   *
   * @param name name of the variable
   * @param namespace namespace of the variable. Set to empty string to specify
   *                  no namespace. Set to to NULL to figure out the namespace.
   * @param value value to so the variable to
   */
  def setExternalDFDLVariable(name: String, namespace: String, value: String): Unit = {
    sCompiler.setExternalDFDLVariable(name, namespace, value)
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
  def setExternalDFDLVariables(extVarsMap: Map[String, String]): Unit = {
    val extVars = ExternalVariablesLoader.getVariables(extVarsMap)
    sCompiler.setExternalDFDLVariables(extVars)
  }

  /**
   * Read external variables from a Daffodil configuration file
   *
   * @see <a target="_blank" href='https://daffodil.apache.org/configuration/'>Daffodil Configuration File</a> - Daffodil configuration file format
   *
   * @param extVarsFile file to read DFDL variables from.
   */
  def setExternalDFDLVariables(extVarsFile: File): Unit = {
    sCompiler.setExternalDFDLVariables(extVarsFile)
  }

  /**
   * Enable/disable DFDL validation of resulting infoset with the DFDL schema
   *
   * @param value true to enable validation, false to disabled
   */
  def setValidateDFDLSchemas(value: Boolean): Unit = sCompiler.setValidateDFDLSchemas(value)

  /**
   * Set a Daffodil tunable parameter
   *
   * @see <a target="_blank" href='https://daffodil.apache.org/configuration/#tunable-parameters'>Tunable Parameters</a> - list of tunables names of default values
   *
   * @param tunable name of the tunable parameter to set.
   * @param value value of the tunable parameter to set
   */
  def setTunable(tunable: String, value: String): Unit = {
    sCompiler.setTunable(tunable, value)
  }

  /**
   * Set the value of multiple tunable parameters
   *
   * @see <a target="_blank" href='https://daffodil.apache.org/configuration/#tunable-parameters'>Tunable Parameters</a> - list of tunables names of default values
   *
   * @param tunables a map of key/value pairs, where the key is the tunable name and the value is the value to set it to
   */
  def setTunables(tunables: Map[String, String]): Unit = {
    sCompiler.setTunables(tunables.toMap)
  }
}

/**
 * Factory to create [[DataProcessor]]'s, used for parsing data
 */
class ProcessorFactory private[sapi] (pf: SProcessorFactory)
  extends WithDiagnostics(pf) {

  /**
   * Specify a global element to be the root of DFDL Schema to start parsing
   *
   * @param name name of the root node
   * @param namespace namespace of the root node. Set to empty string to specify
   *                  no namespace. Set to to NULL to figure out the namespace.
   */
  def setDistinguishedRootNode(name: String, namespace: String): Unit =
    pf.setDistinguishedRootNode(name, namespace)

  /**
   * Create a [[DataProcessor]]
   *
   * @param path path to an element to use as the parsing root, relative to the distinguished root node. Currently, must be set to "/"
   * @return [[DataProcessor]] used to parse data. Must check [[DataProcessor.isError]] before using it.
   */
  def onPath(path: String) = {
    val dp = pf.onPath(path).asInstanceOf[SDataProcessor]
    new DataProcessor(dp)
  }

}

/**
 * Abstract class that adds diagnostic information to classes that extend it.
 *
 * When a function returns a class that extend this, one should call
 * [[WithDiagnostics.isError]] on that class before performing any further
 * actions. If an error exists, any use of that class, aside from those
 * functions in [[WithDiagnostics]], is invalid and will result in an
 * Exception.
 */
abstract class WithDiagnostics private[sapi] (wd: SWithDiagnostics) {

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
   * @return list of [[Diagnostic]]'s. May contain errors or warnings, and so may be non-empty even if [[WithDiagnostics.isError]] is false.
   */
  def getDiagnostics: Seq[Diagnostic] = wd.getDiagnostics.map { new Diagnostic(_) }
}

/**
 * Class containing diagnostic information
 */
class Diagnostic private[sapi] (d: SDiagnostic) {

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
  def getDataLocations: Seq[DataLocation] = d.getDataLocations.map { new DataLocation(_) }

  /**
   * Get schema location information relevant to this diagnostic object.
   *
   * For example, this might be a file name of a schema, and position within the schema file.
   *
   * @return list of [[LocationInSchemaFile]]'s related to this diagnostic.
   */
  def getLocationsInSchemaFiles: Seq[LocationInSchemaFile] =
    d.getLocationsInSchemaFiles.map { new LocationInSchemaFile(_) }

  /**
   * Determine if a diagnostic object represents an error or something less serious.
   *
   * @return true if it represents an error, false otherwise
   */
  def isError = d.isError

  /**
   * Positively get these things. No returning 'null' and making caller figure out
   * whether to look for cause object.
   */
  def getSomeCause: Throwable = d.getSomeCause.get
  def getSomeMessage: String = d.getSomeMessage.get
}

/**
 * Information related to a location in data
 */
class DataLocation private[sapi] (dl: SDataLocation) {
  override def toString() = dl.toString

  /**
   * Determine if this data location is at the end of the input data
   *
   * @return true if this represents the end of the input data, false otherwise
   */
  def isAtEnd() = dl.isAtEnd

  /**
   * Get the position of the data, in bits, using 1-based indexing
   */
  def bitPos1b() = dl.bitPos1b

  /**
   * Get the position of the data, in bytes, using 1-based indexing
   */
  def bytePos1b() = dl.bytePos1b
}

/**
 * Information related to locations in DFDL schema files
 */
class LocationInSchemaFile private[sapi] (lsf: SLocationInSchemaFile) {
  /**
   * Get the description of the location file, for example, containing file and line number information
   */
  override def toString() = lsf.locationDescription
}

/**
 * Compiled version of a DFDL Schema, used to parse data and get the DFDL infoset
 */
class DataProcessor private[sapi] (dp: SDataProcessor)
  extends WithDiagnostics(dp) {

  /**
   * Enable/disable debugging.
   *
   * Before enabling, [[DataProcessor.setDebugger]] must be called with a non-null debugger.
   *
   * @param flag true to enable debugging, false to disabled
   */
  def setDebugging(b: Boolean) = dp.setDebugging(b)

  /**
   * Set the debugger runer
   *
   * @param dr debugger runner
   */
  def setDebugger(dr: DebuggerRunner) {
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
    dp.setDebugger(debugger)
  }

  /**
   * Set validation mode
   *
   * @param mode mode to control validation
   */
  def setValidationMode(mode: ValidationMode.Value): Unit = {
    try { dp.setValidationMode(ValidationConversions.modeToScala(mode)) }
    catch { case e: SInvalidUsageException => throw new InvalidUsageException(e) }
  }

  /**
   * Read external variables from a Daffodil configuration file
   *
   * @see <a target="_blank" href='https://daffodil.apache.org/configuration/'>Daffodil Configuration File</a> - Daffodil configuration file format
   *
   * @param extVars file to read DFDL variables from.
   */
  def setExternalVariables(extVars: File): Unit = dp.setExternalVariables(extVars)

  /**
   * Set the value of multiple DFDL variables
   *
   * @param extVars a map of key/value pairs, where the key is the variable
   *                name, and the value is the value of the variable. The key
   *                may be preceded by a string of the form "{namespace}" to
   *                define a namespace for the variable. If preceded with "{}",
   *                then no namespace is used. If not preceded by anything,
   *                then Daffodil will figure out the namespace.
   */
  def setExternalVariables(extVars: Map[String, String]) = dp.setExternalVariables(extVars)

  /**
   * Save the DataProcessor
   *
   * The resulting output can be reloaded by [[Compiler.reload(savedParser:java\.nio\.channels\.ReadableByteChannel)* Compiler.reload]].
   * @param output the byte channel to write the [[DataProcessor]] to
   */
  def save(output: WritableByteChannel): Unit = dp.save(output)

  /**
   * Parse input data with a specified length
   *
   * @param input data to be parsed
   * @param lengthLimitInBits the length of the input data in bits, or -1 if no limit
   * @return an object which contains the result, and/or diagnostic information.
   */
  @deprecated("Use parse(InputSourceDataInputStream, InfosetOutputter) to parse the data and get the infoset representation from the InfosetOutputter instead of ParseResult.result()","2.2.0")
  def parse(input: ReadableByteChannel, lengthLimitInBits: Long): ParseResult = {
    val is = Channels.newInputStream(input)
    val dis = new InputSourceDataInputStream(is)
    if (lengthLimitInBits > 0) {
      dis.dis.setBitLimit0b(MaybeULong(lengthLimitInBits))
    }
    val output = new ScalaXMLInfosetOutputter()
    val pr = dp.parse(dis.dis, output).asInstanceOf[SParseResult]
    new ParseResult(pr, Maybe(output))
  }

  /**
   * Parse input data without specifying a length
   *
   * @param input data to be parsed
   * @return an object which contains the result, and/or diagnostic information.
   */
  @deprecated("Use parse(InputSourceDataInputStream, InfosetOutputter) to parse the data and get the infoset representation from the InfosetOutputter instead of ParseResult.result()","2.2.0")
  def parse(input: ReadableByteChannel): ParseResult = parse(input, -1)

  /**
   * Parse input data with a specified length
   *
   * @param input data to be parsed
   * @param output the InfosetOutputter that will be used to output the infoset
   * @param lengthLimitInBits the length of the input data in bits, or -1 if no limit
   * @return an object which contains the result, and/or diagnostic information.
   */
  @deprecated("Use parse(InputSourceDataInputStream, InfosetOutputter) to parse the data and get the infoset representation from the InfosetOutputter instead of ParseResult.result()","2.2.0")
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
  @deprecated("Use parse(InputSourceDataInputStream, InfosetOutputter) to parse the data and get the infoset representation from the InfosetOutputter instead of ParseResult.result()","2.2.0")
  def parse(input: ReadableByteChannel, output: InfosetOutputter): ParseResult = parse(input, output, -1)

  /**
   * Parse input data from an InputSourceDataInputStream and output the infoset to an InfosetOutputter
   *
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
   * Unparse a scala.xml.Node infoset
   *
   * @param output the byte channel to write the data to
   * @param infoset the infoset to unparse, as a scala xml Node
   * @return an object with contains the result and/or diagnostic information
   */
  @deprecated("Use unparse(InfosetInputter, WritableByteChannel)", "2.0.0")
  def unparse(output: WritableByteChannel, infoset: scala.xml.Node): UnparseResult = {
    val input = new ScalaXMLInfosetInputter(infoset)
    unparse(input, output)
  }
}

/**
 * Result of calling [[DataProcessor.parse(input:org\.apache\.daffodil* DataProcessor.parse]], containing
 * any diagnostic information, and the final data location
 */
class ParseResult private[sapi] (pr: SParseResult, deprecatedOutput: Maybe[ScalaXMLInfosetOutputter])
  extends WithDiagnostics(pr) {

  /**
   * Get the resulting infoset as a scala.xml.Node
   *
   * @throws InvalidUsageException if you call this when isError is true
   *         because in that case there is no result document.
   *
   * @return a scala.xml.Node representing the DFDL infoset for the parsed data
   */
  @deprecated("ParseResult carrying the infoset representation is deprecated. Intead, use parse(ReadableByteChannel, InfosetInputter) to parse the data and get the infoset representation from the InfosetOutputter","2.0.0")
  @throws(classOf[InvalidUsageException])
  def result(): scala.xml.Node = {
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
 * Result of calling [[DataProcessor.unparse(input* DataProcessor.unparse]],
 * containing diagnostic information
 */
class UnparseResult private[sapi] (ur: SUnparseResult)
  extends WithDiagnostics(ur) {
}

/**
 * This exception will be thrown as a result of attempting to reload a saved parser
 * that is invalid (not a parser file, corrupt, etc.) or
 * is not in the GZIP format.
 */
class InvalidParserException(cause: org.apache.daffodil.compiler.InvalidParserException) extends Exception(cause.getMessage(), cause.getCause())

/**
 * This exception will be thrown as a result of an invalid usage of the Daffodil API
 */
class InvalidUsageException(cause: org.apache.daffodil.processors.InvalidUsageException) extends Exception(cause.getMessage(), cause.getCause())
