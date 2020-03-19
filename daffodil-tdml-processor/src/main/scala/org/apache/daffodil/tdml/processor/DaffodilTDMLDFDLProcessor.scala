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

final class TDMLDFDLProcessorFactory() extends AbstractTDMLDFDLProcessorFactory {

  override def implementationName = "daffodil"

  private lazy val compiler = Compiler(validateDFDLSchemas)

  private var checkAllTopLevel: Boolean = false

  private var validateDFDLSchemas_ : Boolean = true

  override def validateDFDLSchemas = validateDFDLSchemas_

  override def setValidateDFDLSchemas(bool: Boolean): Unit = {
    this.validateDFDLSchemas_ = bool
    compiler.setValidateDFDLSchemas(bool)
  }

  override def setCheckAllTopLevel(checkAllTopLevel: Boolean): Unit = {
    this.checkAllTopLevel = checkAllTopLevel
    compiler.setCheckAllTopLevel(checkAllTopLevel)
  }

  override def setTunables(tunables: Map[String, String]): Unit = compiler.setTunables(tunables)

  override def setExternalDFDLVariables(externalVarBindings: Seq[Binding]): Unit = compiler.setExternalDFDLVariables(externalVarBindings)

  override def setDistinguishedRootNode(name: String, namespace: String): Unit = compiler.setDistinguishedRootNode(name, namespace)

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

  private def compileProcessor(schemaSource: DaffodilSchemaSource, useSerializedProcessor: Boolean): SchemaDataProcessorCache.Types.CompileResult = {
    val pf = compiler.compileSource(schemaSource)
    val diags = pf.getDiagnostics
    if (pf.isError) {
      Left(diags) // throw new TDMLException(diags)
    } else {
      val res = this.generateProcessor(pf, useSerializedProcessor)
      res
    }
  }

  override def getProcessor(schemaSource: DaffodilSchemaSource, useSerializedProcessor: Boolean): TDML.CompileResult = {
    val rootSpec = compiler.rootSpec.get
    val cacheResult: SchemaDataProcessorCache.Types.CompileResult = schemaSource match {
      case uss: URISchemaSource =>
        SchemaDataProcessorCache.compileAndCache(uss, useSerializedProcessor,
          checkAllTopLevel,
          rootSpec.name,
          rootSpec.ns.toString) {
            compileProcessor(uss, useSerializedProcessor)
          }
      case _ => {
        compileProcessor(schemaSource, useSerializedProcessor)
      }
    }
    val res = cacheResult match {
      case Right((diags, dp)) => Right((diags, new DaffodilTDMLDFDLProcessor(dp)))
      case Left(diags) => Left(diags)
    }
    res
  }

}

class DaffodilTDMLDFDLProcessor(ddp: DFDL.DataProcessor) extends TDMLDFDLProcessor {

  private val dp = ddp.asInstanceOf[DataProcessor]

  private lazy val builtInTracer = new InteractiveDebugger(new TraceDebuggerRunner, ExpressionCompilers)

  private val blobDir = Paths.get(System.getProperty("java.io.tmpdir"), "daffodil-tdml", "blobs")
  private val blobPrefix = ""
  private val blobSuffix = ".bin"

  override def setDebugging(b: Boolean) = dp.setDebugging(b)

  override def setTracing(bool: Boolean): Unit = {
    if (bool) {
      dp.setDebugger(builtInTracer)
      dp.setDebugging(true)
    } else {
      dp.setDebugging(false)
    }
  }

  override def setDebugger(db: AnyRef): Unit = {
    Assert.usage(dp ne null)
    val d = dp.asInstanceOf[Debugger]
    dp.setDebugger(d)
  }

  override def setValidationMode(validationMode: ValidationMode.Type): Unit = dp.setValidationMode(validationMode)

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
