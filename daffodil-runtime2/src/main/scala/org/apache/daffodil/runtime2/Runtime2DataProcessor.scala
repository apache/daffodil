package org.apache.daffodil.runtime2

import java.io.File
import java.io.InputStream
import java.io.OutputStream

import org.apache.daffodil.api.DFDL.Output
import org.apache.daffodil.api.DataLocation
import org.apache.daffodil.api.DFDL
import org.apache.daffodil.api.DaffodilTunables
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.api.ValidationMode
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.processors.ProcessorResult
import org.apache.daffodil.processors.VariableMap
import org.apache.daffodil.processors.WithDiagnosticsImpl
import org.xml.sax.ErrorHandler
import os.Path
import os.Pipe

import scala.xml.SAXParseException

/**
 * Effectively a scala proxy object that does its work via the underlying C-code.
 * Will need to consider how to use features of underlying C-code to get infoset,
 * walk infoset, generate XML for use by TDML tests.
 */
class Runtime2DataProcessor(executableFile: Path) extends DFDL.DataProcessorBase {
  /**
   * Returns a data processor with all the same state, but the validation mode changed to that of the argument.
   *
   * Note that the default validation mode is "off", that is, no validation is performed.
   */
  override def withValidationMode(mode: ValidationMode.Type): DFDL.DataProcessor = ???

  override def withTunable(name: String, value: String): DFDL.DataProcessor = ???

  override def withTunables(tunables: Map[String, String]): DFDL.DataProcessor = ???

  override def withExternalVariables(extVars: Map[String, String]): DFDL.DataProcessor = ???

  override def withExternalVariables(extVars: File): DFDL.DataProcessor = ???

  override def withExternalVariables(extVars: Seq[Binding]): DFDL.DataProcessor = ???

  override def validationMode: ValidationMode.Type = ???

  override def getTunables(): DaffodilTunables = ???

  override def save(output: Output): Unit = ???

  override def variableMap: VariableMap = ???

  override def setValidationMode(mode: ValidationMode.Type): Unit = ???

  override def setExternalVariables(extVars: Map[String, String]): Unit = ???

  override def setExternalVariables(extVars: File): Unit = ???

  override def setExternalVariables(extVars: File, tunable: DaffodilTunables): Unit = ???

  override def setExternalVariables(extVars: Seq[Binding]): Unit = ???

  override def setTunable(tunable: String, value: String): Unit = ???

  override def setTunables(tunables: Map[String, String]): Unit = ???

  /**
   * Unparses (that is, serializes) data to the output, returns an object which contains any diagnostics.
   */
  def unparse(input: InputStream, output: OutputStream): DFDL.UnparseResult = ???

  /**
   * Returns an object which contains the result, and/or diagnostic information.
   */
  def parse(input: InputStream): Unit = {
    val tempDir = os.temp.dir()
    val infile = tempDir / "infile"
    val outfile = tempDir / "outfile"
    try {
      os.write(infile, input)
      val result = os.proc(executableFile, "-I", "xml", "-o", outfile, infile).call(cwd = tempDir, stderr = Pipe)
      if (!result.out.text.isEmpty || !result.err.text.isEmpty) {
        val msg = "Unexpected output:\n"  + result.out.text + result.err.text
        System.err.println(msg)
      }
    } catch {
      case e: os.SubprocessException =>
        val msg = e.getMessage + ": err.text=" + e.result.err.text
        System.err.println(msg)
    }
  }

  /**
   * If multiple diagnostic messages can be created by an action, then this
   * returns a sequence of multiple diagnostic objects. If the message is
   * a fatal runtime issue, then this might be a singleton list, or it could be
   * a bunch of warnings followed by a fatal runtime error.
   *
   * The order of the sequence is important. When the diagnostics are about
   * a file of text, then diagnostics that are about lines earlier in the file
   * are earlier in the list.
   */
  override def getDiagnostics: Seq[Diagnostic] = ???

  /**
   * This predicate indicates whether the object in question succeeded or failed
   * at whatever some action was trying to do. That is to say,
   * do the diagnostics contain a hard error, or do the diagnostics
   * only contain warnings and/or advisory content. If false then only warnings
   * and other non-fatal diagnostics have appeared, so subsequent actions can
   * proceed.
   *
   * The classic example of this is compilation. If only warnings were produced
   * then one can proceed to run the compiled entity.
   */
  override def isError: Boolean = ???
}

class ParseResult(dp: Runtime2DataProcessor, override val resultState: State)
  extends DFDL.ParseResult
    with WithDiagnosticsImpl
    with ErrorHandler {

  override def warning(spe: SAXParseException): Unit = {
    resultState.validationErrorNoContext(spe)
  }
  override def error(spe: SAXParseException): Unit = {
    resultState.validationErrorNoContext(spe)
  }
  override def fatalError(spe: SAXParseException): Unit = {
    resultState.validationErrorNoContext(spe)
  }
}

class State extends DFDL.State {
  override def processorStatus: ProcessorResult = ???

  override def validationStatus: Boolean = ???

  override def diagnostics: Seq[Diagnostic] = ???

  override def currentLocation: DataLocation = ???

  def validationErrorNoContext(spe: SAXParseException): Unit = ???
}