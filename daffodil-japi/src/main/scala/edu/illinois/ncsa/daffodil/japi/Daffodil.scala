/* Copyright (c) 2013 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.japi

import edu.illinois.ncsa.daffodil.compiler.{ Compiler => SCompiler }
import edu.illinois.ncsa.daffodil.japi.debugger._
import edu.illinois.ncsa.daffodil.japi.logger._
import edu.illinois.ncsa.daffodil.japi.packageprivate._
import edu.illinois.ncsa.daffodil.debugger.{ InteractiveDebugger => SInteractiveDebugger }
import edu.illinois.ncsa.daffodil.debugger.{ TraceDebuggerRunner => STraceDebuggerRunner }
import edu.illinois.ncsa.daffodil.api.{ Diagnostic => SDiagnostic }
import scala.collection.JavaConversions._
import edu.illinois.ncsa.daffodil.api.DFDL
import java.io.File
import java.nio.channels.ReadableByteChannel
import java.nio.channels.WritableByteChannel
import scala.collection.JavaConversions.seqAsJavaList
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.api.{ DataLocation => SDataLocation }
import edu.illinois.ncsa.daffodil.api.{ Diagnostic => SDiagnostic }
import edu.illinois.ncsa.daffodil.api.{ LocationInSchemaFile => SLocationInSchemaFile }
import edu.illinois.ncsa.daffodil.api.{ WithDiagnostics => SWithDiagnostics }
import edu.illinois.ncsa.daffodil.compiler.{ ProcessorFactory => SProcessorFactory }
import edu.illinois.ncsa.daffodil.processors.{ DataProcessor => SDataProcessor }
import edu.illinois.ncsa.daffodil.processors.{ ParseResult => SParseResult }
import edu.illinois.ncsa.daffodil.processors.{ UnparseResult => SUnparseResult }
import edu.illinois.ncsa.daffodil.util.{ ConsoleWriter => SConsoleWriter }
import edu.illinois.ncsa.daffodil.util.{ FileWriter => SFileWriter }
import edu.illinois.ncsa.daffodil.util.{ LogWriter => SLogWriter }
import edu.illinois.ncsa.daffodil.util.{ LoggingDefaults => SLoggingDefaults }
import edu.illinois.ncsa.daffodil.util.{ NullLogWriter => SNullLogWriter }
import edu.illinois.ncsa.daffodil.externalvars.ExternalVariablesLoader
import edu.illinois.ncsa.daffodil.xml.JDOMUtils
import edu.illinois.ncsa.daffodil.xml.XMLEventCursorFromInput
import edu.illinois.ncsa.daffodil.dsom.ExpressionCompilers
import edu.illinois.ncsa.daffodil.compiler.{ InvalidParserException => SInvalidParserException }
import edu.illinois.ncsa.daffodil.processors.{ InvalidUsageException => SInvalidUsageException }
import java.net.URI
import edu.illinois.ncsa.daffodil.api.URISchemaSource

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
    new Compiler()
  }

  /**
   * Set the LogWriter to use to capture logging messages from Daffodil
   *
   * @param lw log writer to capture logging messages
   */
  def setLogWriter(lw: LogWriter): Unit = {
    val slw: SLogWriter = lw match {
      case clw: ConsoleLogWriter => SConsoleWriter
      case flw: FileLogWriter => new SFileWriter(flw.getFile)
      case nlw: NullLogWriter => SNullLogWriter
      case _ => new JavaLogWriter(lw)
    }
    SLoggingDefaults.setLogWriter(slw)
  }

  /**
   * Set the maximum logging level
   *
   * @param lvl log level
   */
  def setLoggingLevel(lvl: LogLevel): Unit = {
    SLoggingDefaults.setLoggingLevel(LoggingConversions.levelToScala(lvl))
  }

}

/**
 * Compile DFDL schemas into [[ProcessorFactory]]'s or reload saved parsers into [[DataProcessor]]'s.
 *
 * Do not use the Compiler constructor to create a Compiler. Instead, use [[Daffodil#compiler()]].
 */
class Compiler private[japi] () {

  private val sCompiler = SCompiler()

  /**
   * Compile DFDL schema file into a [[ProcessorFactory]]
   *
   * To allow jar-file packaging, (where schema files might be part of a jar),
   * it is recommended to use [[Compiler#compileSource]] instead.
   *
   * @param schemaFile DFDL schema file used to create a [[ProcessorFactory]].
   * @return [[ProcessorFactory]] used to create [[DataProcessor]](s). Must check [[ProcessorFactory#isError]] before using it.
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
   * @return [[ProcessorFactory]] used to create [[DataProcessor]](s). Must check [[ProcessorFactory#isError]] before using it.
   */
  @throws(classOf[java.io.IOException])
  def compileSource(uri: URI): ProcessorFactory = {
    val source = URISchemaSource(uri)
    val pf = sCompiler.compileSource(source)
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
  def reload(savedParser: DFDL.Input): DataProcessor = {
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
  def setExternalDFDLVariables(extVarsMap: java.util.AbstractMap[String, String]): Unit = {
    val extVars = ExternalVariablesLoader.getVariables(extVarsMap.toMap)
    sCompiler.setExternalDFDLVariables(extVars)
  }

  /**
   * Read external variables from a Daffodil configuration file
   *
   * @see <a target="_blank" href='https://opensource.ncsa.illinois.edu/confluence/display/DFDL/Configuration+File'>Daffodil Configuration File</a> - Daffodil configuration file format
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
   * @see <a target="_blank" href='https://opensource.ncsa.illinois.edu/confluence/display/DFDL/Configuration+File#ConfigurationFile-TunableParameters'>Tunable Parameters</a> - list of tunables names of default values
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
   * @see <a target="_blank" href='https://opensource.ncsa.illinois.edu/confluence/display/DFDL/Configuration+File#ConfigurationFile-TunableParameters'>Tunable Parameters</a> - list of tunables names of default values
   *
   * @param tunables a map of key/value pairs, where the key is the tunable name and the value is the value to set it to
   */
  def setTunables(tunables: java.util.AbstractMap[String, String]): Unit = {
    sCompiler.setTunables(tunables.toMap)
  }
}

/**
 * Factory to create [[DataProcessor]]'s, used for parsing data
 *
 * Do not use the ProcessorFactry constructor to create a [[ProcessorFactory]].
 * Instead, use [[Compiler#compileFile(java.io.File)]]
 */
class ProcessorFactory private[japi] (pf: SProcessorFactory)
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
   * @return [[DataProcessor]] used to parse data. Must check [[DataProcessor#isError]] before using it.
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
 * [[WithDiagnostics#isError]] or [[WithDiagnostics#canProceed]] on that class
 * before performing any further actions. If an error exists, any use of that
 * class, aside from those functions in [[WithDiagnostics]], is invalid and
 * will result in an Exception.
 */
abstract class WithDiagnostics private[japi] (wd: SWithDiagnostics) {

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
  def canProceed() = wd.canProceed

  /**
   * Get the list of [[Diagnostic]]'s created during the construction of the parent object
   *
   * @return list of [[Diagnostic]]'s. May contain errors or warnings, and so may be non-empty even if [[WithDiagnostics#isError]] is false or [[WithDiagnostics#canProceed]] is true.
   */
  def getDiagnostics: java.util.List[Diagnostic] = wd.getDiagnostics.map { new Diagnostic(_) } // implicitly converts to the Java collection
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
  def getMessage(): String = d.getMessage

  override def toString() = d.toString

  /**
   * Get data location information relevant to this diagnostic object.
   *
   * For example, this might be a file name, and position within the file.
   *
   * @return list of [[DataLocation]]'s related to this diagnostic
   */
  def getDataLocations: java.util.List[DataLocation] = d.getDataLocations.map { new DataLocation(_) }

  /**
   * Get schema location information relevant to this diagnostic object.
   *
   * For example, this might be a file name of a schema, and position within the schema file.
   *
   * @return list of [[LocationInSchemaFile]]'s related to this diagnostic.
   */
  def getLocationsInSchemaFiles: java.util.List[LocationInSchemaFile] =
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
   *
   * @return the exception that caused the diagnostic
   */
  def getSomeCause: Throwable = d.getSomeCause.get
  def getSomeMessage: String = d.getSomeMessage.get
}

/**
 * Information related to a location in data
 */
class DataLocation private[japi] (dl: SDataLocation) {
  override def toString() = dl.toString

  /**
   * Determine if this data location is at the end of the input data
   *
   * @return true if this represents the end of the input data, false otherwise
   */
  def isAtEnd() = dl.isAtEnd

  /**
   * Get the position of the data, in bits, using 1-based indexing
   *
   * @return position of the data in bites
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
class DataProcessor private[japi] (dp: SDataProcessor)
  extends WithDiagnostics(dp) {

  /**
   * Enable/disable debugging.
   *
   * Before enabling, [[DataProcessor#setDebugger(DebuggerRunner)]] must be called with a non-null debugger.
   *
   * @param flag true to enable debugging, false to disabled
   */
  def setDebugging(flag: Boolean) {
    dp.setDebugging(flag)
  }

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
  @throws(classOf[InvalidUsageException])
  def setValidationMode(mode: ValidationMode): Unit = {
    try { dp.setValidationMode(ValidationConversions.modeToScala(mode)) }
    catch { case e: SInvalidUsageException => throw new InvalidUsageException(e) }
  }

  /**
   * Read external variables from a Daffodil configuration file
   *
   * @see <a target="_blank" href='https://opensource.ncsa.illinois.edu/confluence/display/DFDL/Configuration+File'>Daffodil Configuration File</a> - Daffodil configuration file format
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
   * The resulting output can be reloaded by [[Compiler#reload(java.nio.channels.ReadableByteChannel)]].
   *
   * @param output the byte channel to write the [[DataProcessor]] to
   */
  def save(output: DFDL.Output): Unit = dp.save(output)

  /**
   * Parse input data with a specified length
   *
   * @param input data to be parsed
   * @param lengthLimitInBits the length of the input data in bits. This must
   *                          be the actual length in bits if you want the
   *                          location().isAtEnd() function to work. If value
   *                          is -1, the isAtEnd() function will always return true.
   * @return an object which contains the result, and/or diagnostic information.
   */
  def parse(input: ReadableByteChannel, lengthLimitInBits: Long): ParseResult = {
    val pr = dp.parse(input, lengthLimitInBits).asInstanceOf[SParseResult]
    new ParseResult(pr)
  }

  /**
   * Parse input data without specifying a length
   *
   * Use this when you don't know how big the data is. Note that the isAtEnd()
   * does not work properly and will always return -1. If you need isAtEnd() to
   * work, you must use [[DataProcessor#parse(java.nio.channels.ReadableByteChannel, long)]] and
   * specify the length of the data.
   *
   * @param input data to be parsed
   * @return an object which contains the result, and/or diagnostic information.
   */
  def parse(input: ReadableByteChannel): ParseResult = parse(input, -1)

  /**
   * Unparse a JDOM2 infoset
   *
   * @param output the byte channel to write the data to
   * @param infoset the infoset to unparse, as a jdom event cursor
   * @return an object with contains the result and/or diagnostic information
   */
  def unparse(output: WritableByteChannel, infoset: org.jdom2.Document): UnparseResult = {
    // write the infoset to a string and create and XMLEventCursor
    // TODO: add support for SAX input so we do not need to convert to a string (e.g. InfosetCursorFromSAX)
    val rawFormat = org.jdom2.output.Format.getRawFormat()
    val xmlOutputter = new org.jdom2.output.XMLOutputter(rawFormat)
    val string = xmlOutputter.outputString(infoset)

    val xmlEventCursor = new XMLEventCursorFromInput(scala.io.Source.fromString(string))
    val ur = dp.unparse(output, xmlEventCursor).asInstanceOf[SUnparseResult]
    new UnparseResult(ur)
  }
}

/**
 * Result of calling [[DataProcessor#parse(java.nio.channels.ReadableByteChannel, long)]], containing
 * the resulting infoset, any diagnostic information, and the final data
 * location
 */
class ParseResult private[japi] (pr: SParseResult)
  extends WithDiagnostics(pr) {

  /**
   * Get the resulting infoset as a jdom2 Document
   *
   * @throws IllegalStateException if you call this when isError is true
   *         because in that case there is no result document.
   *
   * @return a jdom2 Document representing the DFDL infoset for the parsed data
   */
  @throws(classOf[IllegalStateException])
  def result(): org.jdom2.Document = {
    val doc = new org.jdom2.Document()
    val rootElement = JDOMUtils.elem2Element(pr.result)
    doc.setRootElement(rootElement)
    doc
  }

  /**
   * Get the [[DataLocation]] where the parse completed
   *
   * @return the data location where the parse completed
   */
  def location(): DataLocation = new DataLocation(pr.resultState.currentLocation)
}

/**
 * Result of calling [[DataProcessor#unparse(java.nio.channels.WritableByteChannel, org.jdom2.Document)]],
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
class InvalidParserException private[japi] (cause: edu.illinois.ncsa.daffodil.compiler.InvalidParserException) extends Exception(cause.getMessage(), cause.getCause())

/**
 * This exception will be thrown as a result of an invalid usage of the Daffodil API
 */
class InvalidUsageException private[japi] (cause: edu.illinois.ncsa.daffodil.processors.InvalidUsageException) extends Exception(cause.getMessage(), cause.getCause())
