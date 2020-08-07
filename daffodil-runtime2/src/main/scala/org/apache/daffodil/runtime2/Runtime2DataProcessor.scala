package org.apache.daffodil.runtime2

import java.io.File
import java.io.InputStream
import java.io.OutputStream

import org.apache.daffodil.api.DFDL.Output
import org.apache.daffodil.api.DataLocation
import org.apache.daffodil.api.DFDL
import org.apache.daffodil.api.DaffodilTunables
import org.apache.daffodil.api.ValidationMode
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.processors.Failure
import org.apache.daffodil.processors.ProcessorResult
import org.apache.daffodil.processors.Success
import org.apache.daffodil.processors.VariableMap
import org.apache.daffodil.processors.WithDiagnosticsImpl
import org.apache.daffodil.processors.parsers.ParseError
import org.apache.daffodil.util.Maybe
import os.Path
import os.Pipe

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
  def parse(input: InputStream): ParseResult = {
    val tempDir = os.temp.dir()
    val infile = tempDir / "infile"
    val outfile = tempDir / "outfile"
    try {
      os.write(infile, input)
      val result = os.proc(executableFile, "parse", "-I", "xml", "-o", outfile, infile).call(cwd = tempDir, stderr = Pipe)
      if (result.out.text.isEmpty && result.err.text.isEmpty) {
        val parseResult = new ParseResult(this, Success)
        parseResult
      } else {
        val msg = "Unexpected output:\n"  + result.out.text + result.err.text
        val parseError = new ParseError(None, None, None, None, msg)
        val parseResult = new ParseResult(this, Failure(parseError))
        parseResult
      }
    } catch {
      case e: os.SubprocessException =>
        val msg = e.getMessage + ": err.text=" + e.result.err.text
        val parseError = new ParseError(None, None, Maybe.One(e), None, msg)
        val parseResult = new ParseResult(this, Failure(parseError))
        parseResult
    }
  }
}

class ParseResult(dp: Runtime2DataProcessor, override val processorStatus: ProcessorResult)
  extends DFDL.ParseResult
    with DFDL.State
    with WithDiagnosticsImpl {

  override def resultState: DFDL.State = this

  override def validationStatus: Boolean = processorStatus.isSuccess

  override def currentLocation: DataLocation = ???
}
