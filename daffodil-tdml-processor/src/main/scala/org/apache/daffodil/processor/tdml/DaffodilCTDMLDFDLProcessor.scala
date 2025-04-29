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

package org.apache.daffodil.processor.tdml

import java.io.FileNotFoundException
import java.io.InputStream
import java.io.OutputStream
import scala.xml.Node
import scala.xml.XML

import org.apache.daffodil.api.validation.{ Validator => JValidator }
import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.lib.externalvars.Binding
import org.apache.daffodil.lib.iapi.DaffodilSchemaSource
import org.apache.daffodil.lib.iapi.DataLocation
import org.apache.daffodil.lib.iapi.Diagnostic
import org.apache.daffodil.lib.iapi.TDMLImplementation
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.runtime1.dsom.SchemaDefinitionDiagnosticBase
import org.apache.daffodil.runtime1.processors.Failure
import org.apache.daffodil.runtime1.processors.ProcessorResult
import org.apache.daffodil.runtime1.processors.Success
import org.apache.daffodil.runtime1.processors.parsers.ParseError
import org.apache.daffodil.runtime1.processors.unparsers.UnparseError
import org.apache.daffodil.tdml.processor.AbstractTDMLDFDLProcessorFactory
import org.apache.daffodil.tdml.processor.TDML
import org.apache.daffodil.tdml.processor.TDMLDFDLProcessor
import org.apache.daffodil.tdml.processor.TDMLParseResult
import org.apache.daffodil.tdml.processor.TDMLUnparseResult

import org.xml.sax.SAXParseException

/**
  * A factory called by the TDML runner to create a TDMLDFDL processor.
  * The factory is immutable and can be shared across threads.
  */
final class DaffodilCTDMLDFDLProcessorFactory(compiler: Compiler)
  extends AbstractTDMLDFDLProcessorFactory {

  def this() = this(Compiler())
  private def copy(compiler: Compiler = compiler) =
    new DaffodilCTDMLDFDLProcessorFactory(compiler)

  override type R = DaffodilCTDMLDFDLProcessorFactory
  override def implementationName: String = TDMLImplementation.DaffodilC.toString
  override def validateDFDLSchemas: Boolean = compiler.validateDFDLSchemas
  override def withValidateDFDLSchemas(validateDFDLSchemas: Boolean): R =
    copy(compiler.withValidateDFDLSchemas(validateDFDLSchemas))
  override def withCheckAllTopLevel(checkAllTopLevel: Boolean): R =
    copy(compiler.withCheckAllTopLevel(checkAllTopLevel))
  override def withTunables(tunables: Map[String, String]): R =
    copy(compiler.withTunables(tunables))

  // Compiles the given schema and returns the result of compiling the
  // schema (only diagnostics on left, or processor too on right)
  override def getProcessor(
    schemaSource: DaffodilSchemaSource,
    useSerializedProcessor: Boolean,
    optRootName: Option[String] = None,
    optRootNamespace: Option[String] = None,
    tunables: Map[String, String]
  ): TDML.CompileResult = {

    // Compile the DFDL schema into diagnostics and/or a processor factory
    val pf = compiler.compileSource(schemaSource, optRootName, optRootNamespace)
    val res = if (pf.isError) {
      Left(pf.getDiagnostics) // DFDL schema compilation diagnostics
    } else {
      // Create a code generator for the C language
      val generator = pf.forLanguage("c")

      // Generate the C code in a temporary unique directory
      val tempDir = os.temp.dir(dir = null, prefix = TDMLImplementation.DaffodilC.toString)
      val codeDir = generator.generateCode(tempDir.toString)

      // Compile the C code into an executable
      val executable = generator.compileCode(codeDir)

      // There is no way to clean up TDMLDFDLProcessors - they are just garbage
      // collected when no longer needed. So recursively mark all generated
      // files as deleteOnExit so we at least clean them up when the JVM exits
      os.walk.stream(tempDir).foreach { _.toIO.deleteOnExit() }

      // Summarize the result of compiling the C code
      val compileResult = if (generator.isError) {
        Left(generator.getDiagnostics) // C compilation diagnostics
      } else {
        // Create a processor for running the executable
        val processor = new DaffodilCTDMLDFDLProcessor(executable)
        // Although we return C compilation diagnostics to TDMLRunner, TDMLRunner
        // won't do anything with them in its usual path
        Right((generator.getDiagnostics, processor))
      }
      compileResult
    }
    res
  }
}

/**
 * A TDMLDFDL processor which runs an executable built from C code for
 * a given DFDL schema.  Deals with TDML XML infosets, feeding to the
 * executable, returning the output created by the executable, etc.
 */
final class DaffodilCTDMLDFDLProcessor(executable: os.Path) extends TDMLDFDLProcessor {

  // We don't pass any options to the executable
  override type R = DaffodilCTDMLDFDLProcessor
  override def withDebugging(onOff: Boolean): R = this
  override def withTracing(onOff: Boolean): R = this
  override def withDebugger(db: AnyRef): R = this
  override def withValidator(validator: JValidator): R = this
  override def withExternalDFDLVariables(externalVarBindings: Seq[Binding]): R = this

  // Parses the input stream to an infoset and returns a TDMLParseResult
  // containing the infoset with any errors and diagnostics.
  //
  // We save the input stream to an input file and run the executable in a
  // subprocess.  The executable will parse data from the input file and write an
  // infoset to an output file.  We return the infoset with any errors and
  // diagnostics messages written by the executable on its standard error and/or
  // output.
  override def parse(input: InputStream, lengthLimitInBits: Long): TDMLParseResult = {
    // Write the input to an input file to let the executable parse it
    val tempDir = os.temp.dir(dir = null, prefix = TDMLImplementation.DaffodilC.toString)
    val infile = tempDir / "infile"
    val outfile = tempDir / "outfile"
    os.write(infile, input)

    // Verify the input file has the correct size TDML runner said it would
    val inputSizeInBits = os.size(infile) * 8
    assert(
      inputSizeInBits == lengthLimitInBits,
      s"$infile has $inputSizeInBits bits, but needed $lengthLimitInBits bits"
    )

    // Parse the input file using the executable and capture its exit status
    val parseResult =
      try {
        // Darwin and MSYS2 support only "daffodil -o outfile parse infile" (all getopt options must come first)
        val result = os
          .proc(executable, "-o", outfile, "parse", infile)
          .call(cwd = tempDir, stderr = os.Pipe)
        val messages = if (result.chunks.nonEmpty) result.toString() else ""
        new DaffodilCTDMLParseResult(lengthLimitInBits, outfile, Success, messages)
      } catch {
        case e: os.SubprocessException =>
          val parseError = new ParseError(Nope, Nope, Maybe(e), Nope)
          new DaffodilCTDMLParseResult(lengthLimitInBits, outfile, Failure(parseError))
      } finally {
        os.remove.all(tempDir)
      }

    parseResult
  }

  // Unparses the infoset and returns a TDMLUnparseResult containing the data with
  // any errors and diagnostics.
  //
  // We save the infoset to an input file and run the executable in a subprocess.
  // The executable will unparse the infoset and write data to an output file.  We
  // return the data with any errors and diagnostics messages written by the
  // executable on its standard error and/or output.
  override def unparse(infosetXML: Node, output: OutputStream): TDMLUnparseResult = {
    // Write the infoset to an input file to let the executable parse it
    val tempDir = os.temp.dir(dir = null, prefix = TDMLImplementation.DaffodilC.toString)
    val infile = tempDir / "infile"
    val outfile = tempDir / "outfile"
    os.write(infile, infosetXML.toString)

    // Unparse the infoset using the executable, capture its exit status, and return the data
    val unparseResult =
      try {
        // Darwin and MSYS2 support only "daffodil -o outfile parse infile" (all getopt options must come first)
        val result = os
          .proc(executable, "-o", outfile, "unparse", infile)
          .call(cwd = tempDir, stderr = os.Pipe)
        os.read.stream(outfile).writeBytesTo(output)
        val finalBitPos0b = os.size(outfile) * 8
        val messages = if (result.chunks.nonEmpty) result.toString() else ""
        new DaffodilCTDMLUnparseResult(finalBitPos0b, Success, messages)
      } catch {
        case e: os.SubprocessException =>
          val finalBitPos0b = os.size(outfile) * 8
          val unparseError = new UnparseError(Nope, Nope, Maybe(e), Nope)
          new DaffodilCTDMLUnparseResult(finalBitPos0b, Failure(unparseError))
      } finally {
        os.remove.all(tempDir)
      }

    unparseResult
  }

  // Complete a round trip from data to infoset and back to data
  override def unparse(
    parseResult: TDMLParseResult,
    outStream: OutputStream
  ): TDMLUnparseResult = {
    val infosetXML = parseResult.getResult
    val res = unparse(infosetXML, outStream)
    res
  }
}

/**
 * A TDML parse result which captures the result of running the executable
 */
final class DaffodilCTDMLParseResult(
  finalBitPos0b: Long,
  outfile: os.Path,
  processorResult: ProcessorResult,
  messages: String = ""
) extends TDMLParseResult {

  private var diagnostics: Seq[Diagnostic] = processorResult match {
    case Success =>
      if (messages.nonEmpty) List(DaffodilCTDMLMessages(messages)) else Nil
    case Failure(cause) => List(cause)
  }

  override def addDiagnostic(diagnostic: Diagnostic): Unit = {
    diagnostics = diagnostics :+ diagnostic
  }

  // We must read outFile right away (def or lazy val will not work) because the parse
  // method will delete outFile before returning the parse result, but we must prevent
  // loadFile errors from interrupting the parse result's construction
  override val getResult: Node = {
    val elem =
      try {
        XML.loadFile(outfile.toIO)
      } catch {
        case _: FileNotFoundException => <noFile></noFile>
        case _: SAXParseException => <parseError></parseError>
      }
    elem
  }

  override def currentLocation: DataLocation = DaffodilCTDMLDataLocation(finalBitPos0b)
  override def isValidationError: Boolean = diagnostics.exists(_.isValidation)
  override def isProcessingError: Boolean = processorResult.isFailure
  override def getDiagnostics: Seq[Diagnostic] = diagnostics
}

/**
 * A TDML diagnostic which captures the executable's standard output/error
 */
final case class DaffodilCTDMLMessages(messages: String)
  extends SchemaDefinitionDiagnosticBase(Nope, Nope, None, Nope, Maybe(messages)) {

  override def isValidation: Boolean = true
  override def isError: Boolean = true
  override protected def modeName: String = TDMLImplementation.DaffodilC.toString
}

/**
 * A TDML data location which captures the executable's final position
 */
object DaffodilCTDMLDataLocation {
  // DataLocation stores 1-based bit/byte positions
  private class DaffodilCTDMLDataLocation(val bitPos1b: Long, val bytePos1b: Long)
    extends DataLocation
  // We pass a 0-based bit position, so we have to add 1 to it
  def apply(finalBitPos0b: Long): DataLocation =
    new DaffodilCTDMLDataLocation(finalBitPos0b + 1, (finalBitPos0b + 1) / 8)
}

/**
 * A TDML unparse result which captures the result of running the executable
 */
final class DaffodilCTDMLUnparseResult(
  override val finalBitPos0b: Long,
  processorResult: ProcessorResult,
  messages: String = ""
) extends TDMLUnparseResult {

  private val diagnostics: Seq[Diagnostic] = processorResult match {
    case Success =>
      if (messages.nonEmpty) List(DaffodilCTDMLMessages(messages)) else Nil
    case Failure(cause) => List(cause)
  }

  override def bitPos0b: Long = finalBitPos0b
  override def isScannable: Boolean = false // binary data is not scannable
  override def encodingName: String = "" // encoding needed only if scannable
  override def isValidationError: Boolean = diagnostics.exists(_.isValidation)
  override def isProcessingError: Boolean = processorResult.isFailure
  override def getDiagnostics: Seq[Diagnostic] = diagnostics
}
