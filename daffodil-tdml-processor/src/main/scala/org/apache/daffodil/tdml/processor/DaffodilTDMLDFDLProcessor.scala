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

import java.nio.channels.Channels
import java.nio.file.Path
import java.nio.file.Paths

import scala.xml.Node

import org.apache.daffodil.api._
import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.debugger.Debugger
import org.apache.daffodil.debugger.InteractiveDebugger
import org.apache.daffodil.debugger.TraceDebuggerRunner
import org.apache.daffodil.dsom.ExpressionCompilers
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.processors.UnparseResult
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.tdml.SchemaDataProcessorCache
import org.apache.daffodil.tdml.TDMLInfosetInputter
import org.apache.daffodil.tdml.TDMLInfosetOutputter
import org.apache.daffodil.util.MaybeULong

final class TDMLDFDLProcessorFactory private (
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

  @deprecated("Use withValidateDFDLSchemas.", "2.6.0")
  override def setValidateDFDLSchemas(bool: Boolean): Unit = {
    compiler = compiler.withValidateDFDLSchemas(bool)
  }

  def withValidateDFDLSchemas(bool: Boolean): TDMLDFDLProcessorFactory = {
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
  private def generateProcessor(pf: DFDL.ProcessorFactory, useSerializedProcessor: Boolean): SchemaDataProcessorCache.Types.CompileResult = {
    val p = pf.onPath("/")
    if (p.isError) {
      val diags = p.getDiagnostics
      Left(diags)
    } else {
      val dp =
        if (useSerializedProcessor) {
          val os = new java.io.ByteArrayOutputStream()
          val output = Channels.newChannel(os)
          p.save(output)

          val is = new java.io.ByteArrayInputStream(os.toByteArray)
          val input = Channels.newChannel(is)
          compiler.reload(input)
        } else p
      val diags = p.getDiagnostics
      Right((diags, dp))
    }
  }

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

  override def getProcessor(
    schemaSource: DaffodilSchemaSource,
    useSerializedProcessor: Boolean,
    optRootName: Option[String] = None,
    optRootNamespace: Option[String] = None): TDML.CompileResult = {
    val cacheResult: SchemaDataProcessorCache.Types.CompileResult = schemaSource match {
      case uss: URISchemaSource =>
        SchemaDataProcessorCache.compileAndCache(uss, useSerializedProcessor,
          checkAllTopLevel,
          optRootName,
          optRootNamespace) {
            compileProcessor(uss, useSerializedProcessor, optRootName, optRootNamespace)
          }
      case _ => {
        compileProcessor(schemaSource, useSerializedProcessor, optRootName, optRootNamespace)
      }
    }
    val res = cacheResult match {
      case Right((diags, dp)) => Right((diags, new DaffodilTDMLDFDLProcessor(dp)))
      case Left(diags) => Left(diags)
    }
    res
  }

}

class DaffodilTDMLDFDLProcessor private (private var dp: DataProcessor) extends TDMLDFDLProcessor {

  override type R = DaffodilTDMLDFDLProcessor

  def this(ddp: DFDL.DataProcessor) = this(ddp.asInstanceOf[DataProcessor])

  private def copy(dp: DataProcessor = dp) = new DaffodilTDMLDFDLProcessor(dp)

  private lazy val builtInTracer = new InteractiveDebugger(new TraceDebuggerRunner, ExpressionCompilers)

  private lazy val blobDir = Paths.get(System.getProperty("java.io.tmpdir"), "daffodil-tdml", "blobs")
  private def blobPrefix = ""
  private def blobSuffix = ".bin"

  @deprecated("Use withDebugging.", "2.6.0")
  override def setDebugging(b: Boolean) =
    dp = dp.withDebugging(b)

  override def withDebugging(b: Boolean): DaffodilTDMLDFDLProcessor =
    copy(dp = dp.withDebugging(b))

  @deprecated("Use withTracing.", "2.6.0")
  override def setTracing(bool: Boolean): Unit = {
     dp = newTracing(bool)
  }

  override def withTracing(bool: Boolean): DaffodilTDMLDFDLProcessor = {
    copy(dp = newTracing(bool))
  }

  private def newTracing(bool: Boolean) =
    if (bool) {
      dp.withDebugger(builtInTracer).withDebugging(true)
    } else {
      dp.withDebugging(false)
    }

  @deprecated("Use withDebugger.", "2.6.0")
  override def setDebugger(db: AnyRef): Unit = {
    Assert.usage(dp ne null)
    val d = dp.asInstanceOf[Debugger]
    dp = dp.withDebugger(d)
  }

  override def withDebugger(db: AnyRef): DaffodilTDMLDFDLProcessor = {
    Assert.usage(dp ne null)
    val d = dp.asInstanceOf[Debugger]
    copy(dp = dp.withDebugger(d))
  }

  @deprecated("Use withValidationMode.", "2.6.0")
  override def setValidationMode(validationMode: ValidationMode.Type): Unit =
    dp = dp.withValidationMode(validationMode)

  override def withValidationMode(validationMode: ValidationMode.Type): DaffodilTDMLDFDLProcessor =
    copy(dp = dp.withValidationMode(validationMode))

  @deprecated("Use withExternalDFDLVariables.", "2.6.0")
  override def setExternalDFDLVariables(externalVarBindings: Seq[Binding]): Unit =
    dp = dp.withExternalVariables(externalVarBindings)

  override def withExternalDFDLVariables(externalVarBindings: Seq[Binding]): DaffodilTDMLDFDLProcessor =
    copy(dp = dp.withExternalVariables(externalVarBindings))

  override def isError: Boolean = dp.isError

  override def getDiagnostics: Seq[Diagnostic] = dp.getDiagnostics

  override def parse(is: java.io.InputStream, lengthLimitInBits: Long): TDMLParseResult = {

    val outputter = new TDMLInfosetOutputter()
    outputter.setBlobAttributes(blobDir, blobPrefix, blobSuffix)

    val dis = InputSourceDataInputStream(is)
    if (lengthLimitInBits >= 0 && lengthLimitInBits % 8 != 0) {
      // Only set the bit limit if the length is not a multiple of 8. In that
      // case, we aren't expected to consume all the data and need a bitLimit
      // to prevent messages about left over bits.
      dis.setBitLimit0b(MaybeULong(lengthLimitInBits))
    }
    val actual = dp.parse(dis, outputter)

    new DaffodilTDMLParseResult(actual, outputter)
  }

  override def unparse(infosetXML: scala.xml.Node, outStream: java.io.OutputStream): TDMLUnparseResult = {
    val output = java.nio.channels.Channels.newChannel(outStream)

    val scalaInputter = new ScalaXMLInfosetInputter(infosetXML)
    // We can't compare against other inputters since we only have scala XML,
    // but we still need to use the TDMLInfosetInputter since it may make TDML
    // specific modifications to the input infoset (e.g. blob paths)
    val otherInputters = Seq.empty
    val inputter = new TDMLInfosetInputter(scalaInputter, otherInputters)
    val actual = dp.unparse(inputter, output).asInstanceOf[UnparseResult]
    output.close()
    new DaffodilTDMLUnparseResult(actual, outStream)
  }

  def unparse(parseResult: TDMLParseResult, outStream: java.io.OutputStream): TDMLUnparseResult = {
    val dafpr = parseResult.asInstanceOf[DaffodilTDMLParseResult]
    val inputter = dafpr.inputter
    val output = java.nio.channels.Channels.newChannel(outStream)
    val actual = dp.unparse(inputter, output)
    output.close()
    new DaffodilTDMLUnparseResult(actual, outStream)
  }

}

final class DaffodilTDMLParseResult(actual: DFDL.ParseResult, outputter: TDMLInfosetOutputter) extends TDMLParseResult {

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
