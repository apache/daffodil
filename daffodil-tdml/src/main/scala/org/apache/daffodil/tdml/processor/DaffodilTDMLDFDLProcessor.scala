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

import org.apache.daffodil.api._
import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.debugger.Debugger
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.processors.{DataProcessor, UnparseResult}
import org.apache.daffodil.tdml.{SchemaDataProcessorCache, TDMLInfosetOutputter}
import org.apache.daffodil.util.MaybeULong

import scala.xml.Node

final class DaffodilTDMLDFDLProcessorFactory() extends TDMLDFDLProcessorFactory {

  private lazy val compiler = Compiler(validateDFDLSchemas)

  private var checkAllTopLevel: Boolean = false

  private var validateDFDLSchemas_ : Boolean = true

  override def validateDFDLSchemas = validateDFDLSchemas_

  override def setValidateDFDLSchemas(bool: Boolean): Unit = {
    this.validateDFDLSchemas_ = bool
    compiler.setValidateDFDLSchemas(bool)
  }

  override def setCheckAllTopLevel(checkAllTopLevel: Boolean) : Unit = {
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
  private def generateProcessor(pf: DFDL.ProcessorFactory, useSerializedProcessor: Boolean) = {
    val p = pf.onPath("/")
    val diags = p.getDiagnostics
    if (p.isError) Left(diags)
    else {
      val dp =
        if (useSerializedProcessor) {
          val os = new java.io.ByteArrayOutputStream()
          val output = Channels.newChannel(os)
          p.save(output)

          val is = new java.io.ByteArrayInputStream(os.toByteArray)
          val input = Channels.newChannel(is)
          compiler.reload(input)
        } else p

      Right((diags, dp))
    }
  }

  private def compileProcessor(schemaSource: DaffodilSchemaSource, useSerializedProcessor: Boolean)= {
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

  def setDebugging(bool: Boolean): Unit = dp.setDebugging(bool)

  def setDebugger(d: Debugger): Unit = dp.setDebugger(d)

  override def setValidationMode(validationMode: ValidationMode.Type): Unit = dp.setValidationMode(validationMode)

  override def isError: Boolean = dp.isError

  override def getDiagnostics: Seq[Diagnostic] = dp.getDiagnostics

  override def parse(is: java.io.InputStream, lengthLimitInBits: Long): TDMLParseResult = {
    val outputter = new TDMLInfosetOutputter()

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

  override def unparse(infosetXML: scala.xml.Node, outStream: java.io.OutputStream) : TDMLUnparseResult = {
    val output = java.nio.channels.Channels.newChannel(outStream)

    val inputter = new ScalaXMLInfosetInputter(infosetXML)
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
