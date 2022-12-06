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

package org.apache.daffodil.runtime2

import java.io.File
import java.io.InputStream
import java.io.OutputStream
import org.apache.daffodil.api.DFDL
import org.apache.daffodil.api.DaffodilTunables
import org.apache.daffodil.api.DataLocation
import org.apache.daffodil.api.ValidationMode
import org.apache.daffodil.api.ValidationResult
import org.apache.daffodil.api.ValidationWarning
import org.apache.daffodil.dsom.ValidationError
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.processors.Failure
import org.apache.daffodil.processors.ProcessorResult
import org.apache.daffodil.processors.Success
import org.apache.daffodil.processors.VariableMap
import org.apache.daffodil.processors.WithDiagnosticsImpl
import org.apache.daffodil.processors.parsers.ParseError
import org.apache.daffodil.processors.unparsers.UnparseError
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe.Nope

/**
 * Effectively a scala proxy object that does its work via the underlying C-code
 * to get infoset, walk infoset, and generate XML for use by TDML tests.
 */
class Runtime2DataProcessor(executableFile: os.Path) extends DFDL.DataProcessorBase {

  //$COVERAGE-OFF$
  override def withValidationMode(mode: ValidationMode.Type): DFDL.DataProcessor = ???
  override def withTunable(name: String, value: String): DFDL.DataProcessor = ???
  override def withTunables(tunables: Map[String, String]): DFDL.DataProcessor = ???
  override def withExternalVariables(extVars: Map[String, String]): DFDL.DataProcessor = ???
  override def withExternalVariables(extVars: File): DFDL.DataProcessor = ???
  override def withExternalVariables(extVars: Seq[Binding]): DFDL.DataProcessor = ???
  override def withDebugger(dbg:AnyRef): DFDL.DataProcessor = ???
  override def withDebugging(flag: Boolean): DFDL.DataProcessor = ???
  override def save(output: DFDL.Output): Unit = ???
  override def tunables: DaffodilTunables = ???
  override def variableMap: VariableMap = ???
  override def validationMode: ValidationMode.Type = ???
  //$COVERAGE-ON$

  /**
   * Returns an object which contains the result, and/or diagnostic information.
   */
  def parse(input: InputStream): ParseResult = {
    val tempDir = os.temp.dir(dir = null, prefix = "daffodil-runtime2-")
    val infile = tempDir/"infile"
    val outfile = tempDir/"outfile"
    try {
      os.write(infile, input)
      val result = os.proc(executableFile, "-o", outfile, "parse", infile).call(cwd = tempDir, stderr = os.Pipe)
      if (result.out.text.isEmpty && result.err.text.isEmpty) {
        val parseResult = new ParseResult(infile, outfile, Success)
        parseResult
      } else {
        val msg = s"stdout: ${result.out.text} stderr: ${result.err.text}"
        val warning = new ValidationWarning { override def getMessage: String = msg }
        val validationResult = ValidationResult(Seq(warning), Seq.empty)
        val parseResult = new ParseResult(infile, outfile, Success, Option(validationResult))
        parseResult.addDiagnostic(new ValidationError(msg))
        parseResult
      }
    } catch {
      case e: os.SubprocessException =>
        val parseError = if (e.result.out.text.isEmpty && e.result.err.text.isEmpty) {
          new ParseError(Nope, Nope, Maybe(e), Nope)
        } else {
          val msg = s"${e.getMessage} stdout: ${e.result.out.text} stderr: ${e.result.err.text}"
          new ParseError(Nope, Nope, Nope, Maybe(msg))
        }
        val parseResult = new ParseResult(infile, outfile, Failure(parseError))
        parseResult.addDiagnostic(parseError)
        parseResult
    } finally {
      os.remove.all(tempDir)
    }
  }

  /**
   * Unparses (that is, serializes) data to the output, returns an object which contains any diagnostics.
   */
  def unparse(input: InputStream, output: OutputStream): UnparseResult = {
    val tempDir = os.temp.dir(dir = null, prefix = "daffodil-runtime2-")
    val infile = tempDir/"infile"
    val outfile = tempDir/"outfile"
    try {
      os.write(infile, input)
      val result = os.proc(executableFile, "-o", outfile, "unparse", infile).call(cwd = tempDir, stderr = os.Pipe)
      val finalBitPos0b = os.size(outfile) * 8 // File sizes are bytes, so must multiply to get final position in bits
      os.read.stream(outfile).writeBytesTo(output)
      if (result.out.text.isEmpty && result.err.text.isEmpty) {
        val unparseResult = new UnparseResult(finalBitPos0b, Success)
        unparseResult
      } else {
        val msg = s"stdout: ${result.out.text} stderr: ${result.err.text}"
        val unparseError = new UnparseError(Nope, Nope, Nope, Maybe(msg))
        val unparseResult = new UnparseResult(finalBitPos0b, Failure(unparseError))
        unparseResult.addDiagnostic(unparseError)
        unparseResult
      }
    } catch {
      case e: os.SubprocessException =>
        val unparseError = if (e.result.out.text.isEmpty && e.result.err.text.isEmpty) {
          new UnparseError(Nope, Nope, Maybe(e), Nope)
        } else {
          val msg = s"${e.getMessage} stdout: ${e.result.out.text} stderr: ${e.result.err.text}"
          new UnparseError(Nope, Nope, Nope, Maybe(msg))
        }
        val finalBitPos0b = 0L
        val unparseResult = new UnparseResult(finalBitPos0b, Failure(unparseError))
        unparseResult.addDiagnostic(unparseError)
        unparseResult
    } finally {
      os.remove.all(tempDir)
    }
  }
}

object Runtime2DataLocation {
  class Runtime2DataLocation( _bitPos1b: Long,
                             _bytePos1b: Long) extends DataLocation {
    override def bitPos1b: Long = _bitPos1b
    override def bytePos1b: Long = _bytePos1b
  }

  def apply(bitPos1b: Long,
            bytePos1b: Long): DataLocation = {
    new Runtime2DataLocation(bitPos1b, bytePos1b)
  }
}

final class ParseResult(infile: os.Path,
                        outfile: os.Path,
                        override val processorStatus: ProcessorResult,
                        override val validationResult: Option[ValidationResult] = None)
  extends DFDL.ParseResult
    with DFDL.State
    with WithDiagnosticsImpl {

  val loc: DataLocation = {
    val infileLengthInBytes = infile.toIO.length()
    Runtime2DataLocation(infileLengthInBytes * 8, infileLengthInBytes)
  }

  override def resultState: DFDL.State = this

  override def validationStatus: Boolean = validationResult.isEmpty

  override def currentLocation: DataLocation = loc

  // We must read outFile right away (def or lazy val will not work) because the
  // parse method will delete outFile before returning ParseResult, but we must
  // prevent loadFile errors from interrupting ParseResult's construction.
  val infosetAsXML : scala.xml.Elem = {
    val elem = try {
      scala.xml.XML.loadFile(outfile.toIO)
    } catch {
      case _: org.xml.sax.SAXParseException =>
        <dummy></dummy>
    }
    elem
  }
}

final class UnparseResult(val finalBitPos0b: Long,
                          override val processorStatus: ProcessorResult)
  extends DFDL.UnparseResult
    with DFDL.State
    with WithDiagnosticsImpl {

  // Note DataLocation uses 1-based bit/byte positions, so we have to add 1.
  val loc: DataLocation = Runtime2DataLocation(finalBitPos0b + 1, (finalBitPos0b + 1) / 8)

  /**
   * Data is 'scannable' if it consists entirely of textual data, and that data
   * is all in the same encoding.
   */
  override def isScannable: Boolean = false // Safest answer since we don't know for sure

  override def encodingName: String = ??? // We don't need encoding unless isScannable is true

  override def validationStatus: Boolean = processorStatus.isSuccess

  override def currentLocation: DataLocation = loc

  override def resultState: DFDL.State = this
}
