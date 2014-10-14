package edu.illinois.ncsa.daffodil.japi

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

import edu.illinois.ncsa.daffodil.compiler.{ Compiler => SCompiler }
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.japi.debugger._
import edu.illinois.ncsa.daffodil.debugger.{ Debugger => SDebugger }
import edu.illinois.ncsa.daffodil.debugger.{ InteractiveDebugger => SInteractiveDebugger }
import edu.illinois.ncsa.daffodil.api.{ Diagnostic => SDiagnostic }
import scala.collection.JavaConversions._
import edu.illinois.ncsa.daffodil.api.DFDL
import java.io.File
import java.io.IOException
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
import edu.illinois.ncsa.daffodil.util.{ ConsoleWriter => SConsoleWriter }
import edu.illinois.ncsa.daffodil.util.{ FileWriter => SFileWriter }
import edu.illinois.ncsa.daffodil.util.{ Glob => SGlob }
import edu.illinois.ncsa.daffodil.util.{ LogLevel => SLogLevel }
import edu.illinois.ncsa.daffodil.util.{ LogWriter => SLogWriter }
import edu.illinois.ncsa.daffodil.util.{ LoggingDefaults => SLoggingDefaults }
import edu.illinois.ncsa.daffodil.util.{ NullLogWriter => SNullLogWriter }
import edu.illinois.ncsa.daffodil.api.{ ValidationMode => SValidationMode }
import edu.illinois.ncsa.daffodil.processors.{ VariableMap => SVariableMap }
import scala.xml.Node
import edu.illinois.ncsa.daffodil.externalvars.ExternalVariablesLoader
import edu.illinois.ncsa.daffodil.externalvars.Binding
import edu.illinois.ncsa.daffodil.xml.JDOMUtils

/**
 * API Suitable for Java programmers to use.
 */
object Daffodil {

  def compiler(): Compiler = new Compiler()

  def setLogWriter(lw: LogWriter): Unit = {
    val slw: SLogWriter = lw match {
      case clw: ConsoleLogWriter => SConsoleWriter
      case flw: FileLogWriter => new SFileWriter(flw.file)
      case nlw: NullLogWriter => SNullLogWriter
      case _ => new JavaLogWriter(lw)
    }
    SLoggingDefaults.setLogWriter(slw)
  }

  def setLoggingLevel(lvl: LogLevel): Unit = {
    SLoggingDefaults.setLoggingLevel(SLogLevel.fromJava(lvl))
  }

  def setDebugging(flag: Boolean) {
    SDebugger.setDebugging(flag)
  }

  def setDebugger(dr: DebuggerRunner) {
    val debugger =
      if (dr != null) {
        val runner = new JavaInteractiveDebuggerRunner(dr)
        new SInteractiveDebugger(runner)
      } else {
        null
      }
    SDebugger.setDebugger(debugger)
  }
}

class Compiler {
  private val sCompiler = SCompiler()

  @throws(classOf[java.io.IOException])
  def compile(schemaFiles: Array[File]): ProcessorFactory = {
    val (_, pf) = sCompiler.compileInternal(schemaFiles.toSeq)
    new ProcessorFactory(pf)
  }

  def reload(savedParser: File): DFDL.DataProcessor = {
    sCompiler.reload(savedParser)
  }

  def setDistinguishedRootNode(name: String, namespace: String): Unit =
    sCompiler.setDistinguishedRootNode(name, namespace)

  def setExternalDFDLVariable(name: String, namespace: String, value: String): Unit = {
    sCompiler.setExternalDFDLVariable(name, namespace, value)
  }

  def setExternalDFDLVariables(extVarsMap: Map[String, String]): Unit = {
    val extVars = ExternalVariablesLoader.getVariables(extVarsMap)
    sCompiler.setExternalDFDLVariables(extVars)
  }

  def setExternalDFDLVariables(extVarsFile: File): Unit = {
    sCompiler.setExternalDFDLVariables(extVarsFile)
  }

  def setValidateDFDLSchemas(value: Boolean): Unit = sCompiler.setValidateDFDLSchemas(value)

  def setTunable(tunable: String, value: String): Unit = {
    sCompiler.setTunable(tunable, value)
  }

  def setTunables(tunables: Map[String, String]): Unit = {
    sCompiler.setTunables(tunables)
  }
}

class ProcessorFactory(pf: SProcessorFactory)
  extends WithDiagnostics(pf) {

  def setDistinguishedRootNode(name: String, namespace: String): Unit =
    pf.setDistinguishedRootNode(name, namespace)

  def onPath(path: String) = {
    val dp = pf.onPath(path).asInstanceOf[SDataProcessor]
    new DataProcessor(dp)
  }

}

abstract class WithDiagnostics(wd: SWithDiagnostics) {
  def isError = wd.isError
  def canProceed = wd.canProceed
  def getDiagnostics: java.util.List[Diagnostic] = wd.getDiagnostics.map { new Diagnostic(_) } // implicitly converts to the java collection
}

class Diagnostic(d: SDiagnostic) {
  def getMessage(): String = d.getMessage

  override def toString() = d.toString
  /**
   * Get data location information relevant to this diagnostic object.
   *
   * For example, this might be a file name, and position within the file.
   */
  def getDataLocations: java.util.List[DataLocation] = d.getDataLocations.map { new DataLocation(_) }

  /**
   * Get schema location information relevant to this diagnostic object.
   *
   * For example, this might be a file name of a schema, and position within the schema file.
   */
  def getLocationsInSchemaFiles: java.util.List[LocationInSchemaFile] =
    d.getLocationsInSchemaFiles.map { new LocationInSchemaFile(_) }

  /**
   * Determine if a diagnostic object represents an error or something less serious.
   */
  def isError = d.isError

  /**
   * Positively get these things. No returning 'null' and making caller figure out
   * whether to look for cause object.
   */
  def getSomeCause: Throwable = d.getSomeCause.get
  def getSomeMessage: String = d.getSomeMessage.get
}

class DataLocation(dl: SDataLocation) {
  override def toString() = dl.toString
  def isAtEnd() = dl.isAtEnd
  def bitPos1b() = dl.bitPos1b
  def bytePos1b() = dl.bytePos1b
}

class LocationInSchemaFile(lsf: SLocationInSchemaFile) {
  override def toString() = lsf.locationDescription
}

class DataProcessor(dp: SDataProcessor)
  extends WithDiagnostics(dp) {

  def setValidationMode(mode: ValidationMode): Unit = dp.setValidationMode(SValidationMode.fromJava(mode))
  def getValidationMode(): ValidationMode = SValidationMode.forJava(dp.getValidationMode)

  def setExternalVariables(extVars: File): Unit = dp.setExternalVariables(extVars)
  def setExternalVariables(extVars: Seq[Binding]) = dp.setExternalVariables(extVars)
  def setExternalVariables(extVars: Map[String, String]) = dp.setExternalVariables(extVars)
  def getVariables(): SVariableMap = dp.getVariables

  def save(output: DFDL.Output): Unit = dp.save(output)

  /**
   * Unparses (that is, serializes) data to the output, returns an object which contains any diagnostics.
   */
  // def unparse(output: WritableByteChannel, doc: org.jdom2.Document): JUnparseResult

  /**
   * Returns an object which contains the result, and/or diagnostic information.
   *
   * You must pass the 2nd argument as the actual length in bits if you want
   * the location().atEnd() function to work. If you pass -1 or don't specify the argument
   * then the atEnd() function does not work (always returns true)
   */
  def parse(input: ReadableByteChannel, lengthLimitInBits: Long): ParseResult = {
    val pr = dp.parse(input, lengthLimitInBits).asInstanceOf[SParseResult]
    new ParseResult(pr)
  }

  /**
   * Returns an object which contains the result, and/or diagnostic information.
   *
   * Use this when you don't know how big the data is.
   * On the returned result, the location().atEnd() function does not work properly.
   * However, the bitPos() and bytePos() functions do work correctly.
   */
  def parse(input: ReadableByteChannel): ParseResult = parse(input, -1)

}

class ParseResult(pr: SParseResult)
  extends WithDiagnostics(pr) {

  /**
   * Throws IllegalStateException if you call this when isError is true
   * because in that case there is no result document.
   */
  def result(): org.jdom2.Document = {
    val doc = new org.jdom2.Document()
    val rootElement = JDOMUtils.elem2Element(pr.result)
    doc.setRootElement(rootElement)
    doc
  }

  def location(): DataLocation = new DataLocation(pr.resultState.currentLocation)
}

abstract class LogWriter {
  def write(level: LogLevel, logID: String, msg: String): Unit

  def prefix(level: LogLevel, logID: String): String = ""
  def suffix(level: LogLevel, logID: String): String = ""

  def log(level: LogLevel, logID: String, msg: String, args: java.util.List[Any]): Unit = {
    val message =
      if (args.size > 0) {
        msg.format(args: _*)
      } else {
        msg
      }
    val p = prefix(level, logID)
    val s = suffix(level, logID)
    write(level, logID, p + message + s)
  }
}

/* These three classes are all empty and are not ever actually used. They are
 * just place holders. Whenever the Java API uses one of these, it is
 * translated to the appropriate scala log writer */
class ConsoleLogWriter extends LogWriter {
  def write(level: LogLevel, logID: String, msg: String): Unit = {}
}

class NullLogWriter extends LogWriter {
  def write(level: LogLevel, logID: String, msg: String): Unit = {}
}

class FileLogWriter(val file: File) extends LogWriter {
  def write(level: LogLevel, logID: String, msg: String): Unit = {}
}

private class JavaLogWriter(logWriter: LogWriter)
  extends SLogWriter {

  protected def write(msg: String): Unit = {}

  override def log(logID: String, glob: SGlob) {
    if (logWriter != null) {
      logWriter.log(SLogLevel.forJava(glob.lvl), logID, glob.msg, glob.args)
    }
  }
}
