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

package org.apache.daffodil.tdml.processor.runtime2

import java.nio.file.Path
import java.nio.file.Paths

import org.apache.daffodil.api._
import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.debugger.InteractiveDebugger
import org.apache.daffodil.debugger.TraceDebuggerRunner
import org.apache.daffodil.dsom.ExpressionCompilers
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.runtime2.generators.CodeGeneratorState
import org.apache.daffodil.runtime2.GeneratedCodeCompiler
import org.apache.daffodil.runtime2.Runtime2DataProcessor
import org.apache.daffodil.tdml.processor._
import org.apache.daffodil.tdml.SchemaDataProcessorCache
import org.apache.daffodil.tdml.TDMLInfosetOutputter

import scala.xml.Node

final class TDMLDFDLProcessorFactory private(
  private var compiler: Compiler,
  private var checkAllTopLevel: Boolean,
  validateDFDLSchemasArg: Boolean)
  extends AbstractTDMLDFDLProcessorFactory {

  override def validateDFDLSchemas = validateDFDLSchemasArg

  override type R = TDMLDFDLProcessorFactory

  override def implementationName = "daffodil"

  def this() = this(compiler = Compiler(validateDFDLSchemas = true),
    checkAllTopLevel = false,
    validateDFDLSchemasArg = true)

  private def copy(
    compiler: Compiler = compiler,
    checkAllTopLevel: Boolean = checkAllTopLevel,
    validateDFDLSchemas: Boolean = validateDFDLSchemas) =
    new TDMLDFDLProcessorFactory(compiler, checkAllTopLevel, validateDFDLSchemas)

  /**
   * Deprecated methods must be implemented. Some are just stubs though now.
   */
  @deprecated("Use withValidateDFDLSchemas.", "2.6.0")
  override def setValidateDFDLSchemas(bool: Boolean): Unit = {
    compiler = compiler.withValidateDFDLSchemas(bool)
  }

  override def withValidateDFDLSchemas(bool: Boolean): TDMLDFDLProcessorFactory = {
    copy(compiler = compiler.withValidateDFDLSchemas(bool))
  }

  @deprecated("Use withCheckAllTopLevel.", "2.6.0")
  override def setCheckAllTopLevel(checkAllTopLevel: Boolean): Unit = {
    compiler = compiler.withCheckAllTopLevel(checkAllTopLevel)
  }

  override def withCheckAllTopLevel(checkAllTopLevel: Boolean): TDMLDFDLProcessorFactory = {
    copy(compiler = compiler.withCheckAllTopLevel(checkAllTopLevel))
  }

  @deprecated("Use withTunables.", "2.6.0")
  override def setTunables(tunables: Map[String, String]): Unit =
    compiler = compiler.withTunables(tunables)

  override def withTunables(tunables: Map[String, String]): TDMLDFDLProcessorFactory =
    copy(compiler = compiler.withTunables(tunables))

  @deprecated("Use DaffodilTDMLDFDLProcessor.setExternalDFDLVariables.", "2.6.0")
  override def setExternalDFDLVariables(externalVarBindings: Seq[Binding]): Unit =
    compiler = compiler.withExternalDFDLVariablesImpl(externalVarBindings)

  override def withExternalDFDLVariables(externalVarBindings: Seq[Binding]): TDMLDFDLProcessorFactory =
    copy(compiler = compiler.withExternalDFDLVariablesImpl(externalVarBindings))

  @deprecated("Use arguments to getProcessor()", "2.6.0")
  override def setDistinguishedRootNode(name: String, namespace: String): Unit =
    compiler = compiler.withDistinguishedRootNode(name, namespace)

  /**
   * This doesn't fetch a serialized processor, it runs whatever the processor is
   * through a serialize then unserialize path to get a processor as if
   * it were being fetched from a file.
   */
  private def generateProcessor(pf: DFDL.ProcessorFactory, useSerializedProcessor: Boolean): SchemaDataProcessorCache.Types.CompileResult = ???

  private def compileProcessor(
    schemaSource: DaffodilSchemaSource,
    useSerializedProcessor: Boolean,
    optRootName: Option[String],
    optRootNamespace: Option[String]): SchemaDataProcessorCache.Types.CompileResult = {
    val pf = compiler.compileSource(schemaSource, optRootName, optRootNamespace)
    val diags = pf.getDiagnostics
    if (pf.isError) {
      Left(diags)
    } else {
      val res = this.generateProcessor(pf, useSerializedProcessor)
      res
    }
  }

  // We're doing to replace this method with different code.
  // Return result is a TDML.CompileResult - so it's the result
  // of compiling the schema for the test.
  override def getProcessor(
    schemaSource: DaffodilSchemaSource,
    useSerializedProcessor: Boolean,
    optRootName: Option[String] = None,
    optRootNamespace: Option[String] = None): TDML.CompileResult = {
    val pf = compiler.compileSource(schemaSource, optRootName, optRootNamespace)
    val compilerSourceDiags = pf.getDiagnostics
    val res = if (pf.isError) {
      Left(compilerSourceDiags)
    } else {
      val p = pf.onPath("/")
      val codeGeneratorState: CodeGeneratorState = ??? //p.generateCode().asInstanceOf[CodeGeneratorState]
      // actually compile C code in this line.  Do we do it in CodeGeneratorState,
      // or a separate object?  I like a separate object we can plug in, different
      // implementations depending on platform, etc.
      val compiler = new GeneratedCodeCompiler(pf)
      compiler.compile(codeGeneratorState)
      val compileResult = if (pf.isError) {
        val codeGeneratorDiags = pf.getDiagnostics
        Left(codeGeneratorDiags)
      } else {
        val compilerGenDiags = pf.getDiagnostics
        Right((compilerGenDiags, new Runtime2TDMLDFDLProcessor(compiler.dataProcessor)))
      }
      compileResult
    }
    res
  }

}

/**
 * Delegates all execution, error gathering, error access to the Runtime2DataProcessor object.
 * The responsibility of this class is just for TDML matching up. That is dealing with TDML
 * XML Infosets, feeding to the unparser, creating XML from the result created by the
 * Runtime2DataProcessor object. All the "real work" is done by Runtime2DataProcessor.
 * @param dp Object to delegate to
 */
class Runtime2TDMLDFDLProcessor(dp: Runtime2DataProcessor) extends TDMLDFDLProcessor {

  override type R = Runtime2TDMLDFDLProcessor
  type CodegenResult = Either[Seq[Diagnostic], (Seq[Diagnostic], CodeGeneratorState)]

  private lazy val builtInTracer = new InteractiveDebugger(new TraceDebuggerRunner, ExpressionCompilers)

  private lazy val blobDir = Paths.get(System.getProperty("java.io.tmpdir"), "daffodil-tdml", "blobs")
  private def blobPrefix = ""
  private def blobSuffix = ".bin"

  @deprecated("Use withDebugging.", "2.6.0")
  override def setDebugging(b: Boolean) = ???

  override def withDebugging(b: Boolean): Runtime2TDMLDFDLProcessor = ???

  @deprecated("Use withTracing.", "2.6.0")
  override def setTracing(bool: Boolean): Unit = ???

  override def withTracing(bool: Boolean): Runtime2TDMLDFDLProcessor = ???

  @deprecated("Use withDebugger.", "2.6.0")
  override def setDebugger(db: AnyRef): Unit = ???

  override def withDebugger(db: AnyRef): Runtime2TDMLDFDLProcessor = ???

  @deprecated("Use withValidationMode.", "2.6.0")
  override def setValidationMode(validationMode: ValidationMode.Type): Unit = ???

  override def withValidationMode(validationMode: ValidationMode.Type): Runtime2TDMLDFDLProcessor = ???

  @deprecated("Use withExternalDFDLVariables.", "2.6.0")
  override def setExternalDFDLVariables(externalVarBindings: Seq[Binding]): Unit = ???

  override def withExternalDFDLVariables(externalVarBindings: Seq[Binding]): Runtime2TDMLDFDLProcessor = ???

  // Actually run the C code and save the error to be returned here
  override def isError: Boolean = ???

  override def getDiagnostics: Seq[Diagnostic] = ???

  // This part will change a lot (it will execute C code instead).
  // Whatever the parse produces needs to be converted into XML for comparison.
  // We'll need a way to convert, say, a C struct to XML, and XML to C struct.
  // The C code will need a bunch of toXML methods so it can produce output
  // for comparison.
  override def parse(is: java.io.InputStream, lengthLimitInBits: Long): TDMLParseResult = ???
    // We will run the generated and compiled C code, collect and save any errors
    // and diagnostics to be returned in isError and getDiagnostics, and build an
    // infoset.  Our context here is TDML-related, so we need to move that functionality
    // to something generic that we call from here, you're saying.  I got it.  So we
    // put that more generic "RunGeneratedCode" functionality in runtime2 too... in
    // another package, not parser, maybe runtime2 itself?  Should we call this generic
    // class RuntimeState?  Yes, get rid of PState.

    // Call the C program via subprocess.  Have it parse the input stream and
    // return the XML result on its standard output.  Errors and diagnostics
    // can come back via standard error.
    // Or: define a result object structure (XML-based probably) that includes
    // both infoset and errors/diagnostics.  Segfaults probably still will generate
    // something on stderr.  What should diags/errors look like?  text lines, XML?
    // difficult to expect XML, but suddenly read segfault output or other output.
    // Start diags with recognizable prefix that tells us our runtime code made them
    // lines without that prefix are captured as generic errors

    // pass lengthLimitInBits to the C program to tell it how big the data is


  override def unparse(infosetXML: scala.xml.Node, outStream: java.io.OutputStream): TDMLUnparseResult = ???
//    val output = java.nio.channels.Channels.newChannel(outStream)
//
//    val scalaInputter = new ScalaXMLInfosetInputter(infosetXML)
//    // We can't compare against other inputters since we only have scala XML,
//    // but we still need to use the TDMLInfosetInputter since it may make TDML
//    // specific modifications to the input infoset (e.g. blob paths)
//    val otherInputters = Seq.empty
//    val inputter = new TDMLInfosetInputter(scalaInputter, otherInputters)
//    val actual = cg.unparse(inputter, output).asInstanceOf[UnparseResult]
//    output.close()
//    new DaffodilTDMLUnparseResult(actual, outStream)
//  }

  def unparse(parseResult: TDMLParseResult, outStream: java.io.OutputStream): TDMLUnparseResult = ???
//    val dafpr = parseResult.asInstanceOf[Runtime2TDMLParseResult]
//    val inputter = dafpr.inputter
//    val output = java.nio.channels.Channels.newChannel(outStream)
//    val actual = cg.unparse(inputter, output)
//    output.close()
//    new DaffodilTDMLUnparseResult(actual, outStream)
//  }

}

final class Runtime2TDMLParseResult(actual: DFDL.ParseResult, outputter: TDMLInfosetOutputter) extends TDMLParseResult {

  override def getResult: Node = outputter.getResult()

  override def getBlobPaths: Seq[Path] = outputter.getBlobPaths()

  def inputter = outputter.toInfosetInputter()

  override def isProcessingError: Boolean = actual.isProcessingError

  override def isValidationError: Boolean = actual.isValidationError

  override def getDiagnostics: Seq[Diagnostic] = actual.getDiagnostics

  override def currentLocation: DataLocation = actual.resultState.currentLocation

  override def addDiagnostic(diag: Diagnostic): Unit = actual.addDiagnostic(diag)
}

final class DaffodilTDMLUnparseResult(actual: DFDL.UnparseResult, outStream: java.io.OutputStream) extends TDMLUnparseResult {

  private val ustate = actual.resultState.asInstanceOf[UState]

  override def finalBitPos0b = ustate.dataOutputStream.maybeAbsBitPos0b.get

  override def isScannable: Boolean = actual.isScannable

  override def encodingName: String = actual.encodingName

  override def isValidationError: Boolean = actual.isValidationError

  override def isProcessingError: Boolean = actual.isProcessingError

  override def getDiagnostics: Seq[Diagnostic] = actual.getDiagnostics

  override def bitPos0b: Long = ustate.bitPos0b
}
