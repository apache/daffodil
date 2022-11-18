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

package org.apache.daffodil.tdml.processor

import java.io.ByteArrayInputStream

import org.apache.daffodil.api._
import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.runtime2.ParseResult
import org.apache.daffodil.runtime2.Runtime2DataProcessor
import org.apache.daffodil.runtime2.UnparseResult

import scala.xml.Node

final class Runtime2TDMLDFDLProcessorFactory private(
  private var compiler: Compiler,
  private var checkAllTopLevel: Boolean,
  validateDFDLSchemasArg: Boolean)
  extends AbstractTDMLDFDLProcessorFactory {

  override def validateDFDLSchemas = validateDFDLSchemasArg

  override type R = Runtime2TDMLDFDLProcessorFactory

  override def implementationName = TDMLImplementation.DaffodilC.toString

  def this() = this(compiler = Compiler(validateDFDLSchemas = true),
    checkAllTopLevel = false,
    validateDFDLSchemasArg = true)

  private def copy(
    compiler: Compiler = compiler,
    checkAllTopLevel: Boolean = checkAllTopLevel,
    validateDFDLSchemas: Boolean = validateDFDLSchemas) =
    new Runtime2TDMLDFDLProcessorFactory(compiler, checkAllTopLevel, validateDFDLSchemas)

  override def withValidateDFDLSchemas(bool: Boolean): Runtime2TDMLDFDLProcessorFactory = {
    copy(compiler = compiler.withValidateDFDLSchemas(bool))
  }

  override def withCheckAllTopLevel(checkAllTopLevel: Boolean): Runtime2TDMLDFDLProcessorFactory = {
    copy(compiler = compiler.withCheckAllTopLevel(checkAllTopLevel))
  }

  override def withTunables(tunables: Map[String, String]): Runtime2TDMLDFDLProcessorFactory =
    copy(compiler = compiler.withTunables(tunables))

  // Return result is a TDML.CompileResult - so it's the result
  // of compiling the schema for the test.
  override def getProcessor(
    schemaSource: DaffodilSchemaSource,
    useSerializedProcessor: Boolean,
    optRootName: Option[String] = None,
    optRootNamespace: Option[String] = None,
    tunables: Map[String, String]): TDML.CompileResult = {

    // Compile the DFDL schema into a ProcessorFactory
    val pf = compiler.compileSource(schemaSource, optRootName, optRootNamespace)
    val res = if (pf.isError) {
      Left(pf.getDiagnostics) // DFDL schema compilation diagnostics
    } else {
      // Create a CodeGenerator from the DFDL schema for the C language
      val generator = pf.forLanguage("c")

      // Generate the C source code in a temporary unique directory
      val tempDir = os.temp.dir(dir = null, prefix = "daffodil-runtime2-")
      val codeDir = generator.generateCode(tempDir.toString)

      // Compile the generated code into an executable
      val executable = generator.compileCode(codeDir)

      // There is is no way to clean up TDMLDFDLProcessors, they are just garbage
      // collected when no longer needed. So recursively mark all generated
      // files as deleteOnExit so we at least clean them up when the JVM exits
      os.walk.stream(tempDir).foreach { _.toIO.deleteOnExit() }

      // Summarize the result of compiling the schema for the test
      val compileResult = if (generator.isError) {
        Left(generator.getDiagnostics) // C code compilation diagnostics
      } else {
        // Create a processor for running the test using the executable, passing it
        // tempDir so its cleanUp function will delete tempDir for us
        val processor = new Runtime2TDMLDFDLProcessor(tempDir, executable)
        // Although we return generator diagnostics to TDMLRunner, TDMLRunner won't
        // do anything with them in its usual path
        Right((generator.getDiagnostics, processor))
      }
      compileResult
    }
    res
  }

}

/**
 * Delegates all execution, error gathering, error access to the Runtime2DataProcessor.
 * The responsibility of this class is just for TDML matching up. That is dealing with
 * TDML XML Infosets, feeding to the unparser, creating XML from the result created by
 * the Runtime2DataProcessor. All the "real work" is done by Runtime2DataProcessor.
 */
class Runtime2TDMLDFDLProcessor(tempDir: os.Path, executable: os.Path)
  extends TDMLDFDLProcessor {

  override type R = Runtime2TDMLDFDLProcessor

  private val dataProcessor = new Runtime2DataProcessor(executable)

  override def withDebugging(b: Boolean): Runtime2TDMLDFDLProcessor = this

  override def withTracing(bool: Boolean): Runtime2TDMLDFDLProcessor = this

  override def withDebugger(db: AnyRef): Runtime2TDMLDFDLProcessor = this

  override def withValidationMode(validationMode: ValidationMode.Type): Runtime2TDMLDFDLProcessor = this

  override def withExternalDFDLVariables(externalVarBindings: Seq[Binding]): Runtime2TDMLDFDLProcessor = this

  // No need to report errors from this class itself
  override def isError: Boolean = false
  override def getDiagnostics: Seq[Diagnostic] = Seq.empty

  // Run the C code, collect and save the infoset with any errors and
  // diagnostics, and return a [[TDMLParseResult]] summarizing the result.
  // The C code will run in a subprocess, parse the input stream, write
  // an XML infoset on its standard output, and write any error messages
  // on its standard output (all done in [[Runtime2DataProcessor.parse]]).
  override def parse(is: java.io.InputStream, lengthLimitInBits: Long): TDMLParseResult = {
    // TODO: pass lengthLimitInBits to the C program to tell it how big the data is
    val pr = dataProcessor.parse(is)
    new Runtime2TDMLParseResult(pr)
  }

  // Run the C code, collect and save the unparsed data with any errors and
  // diagnostics, and return a [[TDMLUnparseResult]] summarizing the result.
  // The C code will run in a subprocess, unparse the input stream, write
  // the unparsed data on its standard output, and write any error messages
  // on its standard output (all done in [[Runtime2DataProcessor.unparse]]).
  override def unparse(infosetXML: scala.xml.Node, outStream: java.io.OutputStream): TDMLUnparseResult = {
    val inStream = new ByteArrayInputStream(infosetXML.toString.getBytes())
    val upr = dataProcessor.unparse(inStream, outStream)
    new Runtime2TDMLUnparseResult(upr)
  }

  def unparse(parseResult: TDMLParseResult, outStream: java.io.OutputStream): TDMLUnparseResult = {
    unparse(parseResult.getResult, outStream)
  }
}

final class Runtime2TDMLParseResult(pr: ParseResult) extends TDMLParseResult {
  override def addDiagnostic(failure: Diagnostic): Unit = pr.addDiagnostic(failure)

  override def getResult: Node = pr.infosetAsXML

  override def currentLocation: DataLocation = pr.currentLocation

  override def isValidationError: Boolean = pr.isValidationError

  override def isProcessingError: Boolean = pr.isProcessingError

  override def getDiagnostics: Seq[Diagnostic] = pr.getDiagnostics
}

final class Runtime2TDMLUnparseResult(upr: UnparseResult) extends TDMLUnparseResult {
  override def bitPos0b: Long = upr.finalBitPos0b

  override def finalBitPos0b: Long = upr.finalBitPos0b

  override def isScannable: Boolean = upr.isScannable

  override def encodingName: String = upr.encodingName

  override def isValidationError: Boolean = upr.isValidationError

  override def isProcessingError: Boolean = upr.isProcessingError

  override def getDiagnostics: Seq[Diagnostic] = upr.getDiagnostics
}
