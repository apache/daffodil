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

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.OutputStreamWriter
import java.nio.channels.Channels
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.util.Using
import scala.xml.Node

import org.apache.daffodil.api
import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.core.dsom.ExpressionCompilers
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.externalvars.Binding
import org.apache.daffodil.lib.iapi._
import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.lib.xml.DaffodilSAXParserFactory
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.lib.xml.XMLUtils.XMLDifferenceException
import org.apache.daffodil.runtime1.debugger.Debugger
import org.apache.daffodil.runtime1.debugger.InteractiveDebugger
import org.apache.daffodil.runtime1.debugger.TraceDebuggerRunner
import org.apache.daffodil.runtime1.iapi.DFDL.DaffodilUnhandledSAXException
import org.apache.daffodil.runtime1.iapi.DFDL.DaffodilUnparseContentHandler
import org.apache.daffodil.runtime1.iapi.DFDL.DaffodilUnparseErrorSAXException
import org.apache.daffodil.runtime1.iapi._
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.runtime1.processors.DaffodilParseOutputStreamContentHandler
import org.apache.daffodil.runtime1.processors.UnparseResult
import org.apache.daffodil.runtime1.processors.unparsers.UState
import org.apache.daffodil.tdml.TDMLException
import org.apache.daffodil.tdml.VerifyTestCase
import org.apache.daffodil.tdml.processor._

import org.apache.commons.io.IOUtils
import org.xml.sax.ErrorHandler
import org.xml.sax.InputSource
import org.xml.sax.SAXParseException

final class TDMLDFDLProcessorFactory private (
  private var compiler: Compiler,
  private var checkAllTopLevel: Boolean,
  validateDFDLSchemasArg: Boolean
) extends AbstractTDMLDFDLProcessorFactory {

  override def validateDFDLSchemas = validateDFDLSchemasArg

  override type R = TDMLDFDLProcessorFactory

  override def implementationName = "daffodil"

  def this() = this(
    compiler = Compiler(validateDFDLSchemas = true),
    checkAllTopLevel = false,
    validateDFDLSchemasArg = true
  )

  private def copy(
    compiler: Compiler = compiler,
    checkAllTopLevel: Boolean = checkAllTopLevel,
    validateDFDLSchemas: Boolean = validateDFDLSchemas
  ) =
    new TDMLDFDLProcessorFactory(compiler, checkAllTopLevel, validateDFDLSchemas)

  def withValidateDFDLSchemas(bool: Boolean): TDMLDFDLProcessorFactory = {
    copy(compiler = compiler.withValidateDFDLSchemas(bool))
  }

  override def withCheckAllTopLevel(checkAllTopLevel: Boolean): TDMLDFDLProcessorFactory = {
    copy(compiler = compiler.withCheckAllTopLevel(checkAllTopLevel))
  }

  override def withTunables(tunables: Map[String, String]): TDMLDFDLProcessorFactory =
    copy(compiler = compiler.withTunables(tunables))

  /**
   * This doesn't fetch a serialized processor, it runs whatever the processor is
   * through a serialize then unserialize path to get a processor as if
   * it were being fetched from a file.
   */
  private def generateProcessor(
    pf: DFDL.ProcessorFactory,
    useSerializedProcessor: Boolean
  ): TDML.CompileResult = {
    val p = pf.onPath("/")
    if (p.isError) {
      val diags = p.getDiagnostics
      Left(diags)
    } else {
      val dp = {
        if (useSerializedProcessor) {
          val os = new java.io.ByteArrayOutputStream()
          val output = Channels.newChannel(os)
          p.save(output)
          val is = new java.io.ByteArrayInputStream(os.toByteArray)
          compiler.reload(is)
        } else p
      }
      val diags = p.getDiagnostics
      Right((diags, new DaffodilTDMLDFDLProcessor(dp.asInstanceOf[DFDL.DataProcessor])))
    }
  }

  private def compileProcessor(
    schemaSource: DaffodilSchemaSource,
    useSerializedProcessor: Boolean,
    optRootName: Option[String],
    optRootNamespace: Option[String]
  ): TDML.CompileResult = {
    val pf = compiler.compileSource(schemaSource, optRootName, optRootNamespace)
    if (pf.isError) {
      val diags = pf.getDiagnostics
      Left(diags)
    } else {
      val res = this.generateProcessor(pf, useSerializedProcessor)
      res
    }
  }

  override def getProcessor(
    schemaSource: DaffodilSchemaSource,
    useSerializedProcessor: Boolean,
    optRootName: Option[String],
    optRootNamespace: Option[String],
    tunables: Map[String, String]
  ): TDML.CompileResult = {
    if (schemaSource.isXSD) {
      val res =
        compileProcessor(schemaSource, useSerializedProcessor, optRootName, optRootNamespace)
      res
    } else {
      val dp = compiler.reload(schemaSource)
      val diags = dp.getDiagnostics
      Assert.invariant(diags.forall { !_.isError })
      Right((diags, new DaffodilTDMLDFDLProcessor(dp.asInstanceOf[DFDL.DataProcessor])))
    }
  }

}

class DaffodilTDMLDFDLProcessor private (private var dp: api.DataProcessor)
  extends TDMLDFDLProcessor {

  override type R = DaffodilTDMLDFDLProcessor

  def this(ddp: DFDL.DataProcessor) = this(ddp.asInstanceOf[api.DataProcessor])

  private def copy(dp: api.DataProcessor = dp) = new DaffodilTDMLDFDLProcessor(dp)

  private lazy val builtInTracer =
    new InteractiveDebugger(new TraceDebuggerRunner, ExpressionCompilers)

  private lazy val blobDir =
    Paths.get(System.getProperty("java.io.tmpdir"), "daffodil-tdml", "blobs")
  private def blobPrefix = ""
  private def blobSuffix = ".bin"

  private lazy val tdmlApiInfosetsEnv = sys.env.getOrElse("DAFFODIL_TDML_API_INFOSETS", "scala")

  override def withDebugging(b: Boolean): DaffodilTDMLDFDLProcessor =
    copy(dp = dp.withDebugging(b))

  override def withTracing(bool: Boolean): DaffodilTDMLDFDLProcessor = {
    copy(dp = newTracing(bool))
  }

  private def newTracing(bool: Boolean) =
    if (bool) {
      dp.withDebugger(builtInTracer).withDebugging(true)
    } else {
      dp.withDebugging(false)
    }

  override def withDebugger(debugger: AnyRef): DaffodilTDMLDFDLProcessor = {
    Assert.usage(debugger ne null)
    val d = debugger.asInstanceOf[Debugger]
    copy(dp = dp.withDebugger(d))
  }

  override def withValidator(
    validator: api.validation.Validator
  ): DaffodilTDMLDFDLProcessor =
    copy(dp = dp.withValidator(validator))

  override def withExternalDFDLVariables(
    externalVarBindings: Seq[Binding]
  ): DaffodilTDMLDFDLProcessor = {
    val map = externalVarBindings.map { b => b.varQName.toString -> b.varValue }.toMap
    copy(dp = dp.withExternalVariables(map))
  }

  override def parse(is: java.io.InputStream, lengthLimitInBits: Long): TDMLParseResult = {
    val (dpInputStream, optSaxInputStream) = if (tdmlApiInfosetsEnv == "all") {
      val arr = IOUtils.toByteArray(is)
      val saxInputStream = new ByteArrayInputStream(arr)
      val dpInputStream = new ByteArrayInputStream(arr)
      (dpInputStream, Some(saxInputStream))
    } else {
      (is, None)
    }
    doParse(dpInputStream, optSaxInputStream, lengthLimitInBits)
  }

  override def unparse(
    infosetXML: scala.xml.Node,
    outStream: java.io.OutputStream
  ): TDMLUnparseResult = {
    val scalaInputter = new ScalaXMLInfosetInputter(infosetXML)
    // We can't compare against other inputters since we only have scala XML,
    // but we still need to use the TDMLInfosetInputter since it may make TDML
    // specific modifications to the input infoset (e.g. blob paths)
    val otherInputters = Seq.empty
    val inputter = new TDMLInfosetInputter(scalaInputter, otherInputters)
    unparse(inputter, infosetXML, outStream)
  }

  def unparse(
    parseResult: TDMLParseResult,
    outStream: java.io.OutputStream
  ): TDMLUnparseResult = {
    val dafpr = parseResult.asInstanceOf[DaffodilTDMLParseResult]
    val inputter = dafpr.inputter
    val resNode = dafpr.getResult
    unparse(inputter, resNode, outStream)
  }

  def unparse(
    inputter: TDMLInfosetInputter,
    infosetXML: scala.xml.Node,
    outStream: java.io.OutputStream
  ): TDMLUnparseResult = {
    val optSaxInstream = if (tdmlApiInfosetsEnv == "all") {
      val bos = new ByteArrayOutputStream()
      val osw = new OutputStreamWriter(bos, StandardCharsets.UTF_8)
      scala.xml.XML.write(osw, infosetXML, "UTF-8", xmlDecl = true, null)
      osw.flush()
      osw.close()
      val sis = new ByteArrayInputStream(bos.toByteArray)
      Some(sis)
    } else {
      None
    }
    doUnparse(inputter, optSaxInstream, outStream)
  }

  def doParse(
    dpInputStream: java.io.InputStream,
    optSaxInputStream: Option[java.io.InputStream] = None,
    lengthLimitInBits: Long
  ): TDMLParseResult = {
    val outputter = if (tdmlApiInfosetsEnv == "all") {
      TDMLInfosetOutputterAll()
    } else {
      TDMLInfosetOutputterScala()
    }
    outputter.setBlobAttributes(blobDir, blobPrefix, blobSuffix)

    Using.resource(InputSourceDataInputStream(dpInputStream)) { dis =>
      // The length limit here should be the length of the document
      // under test. Only set a limit when the end of the document
      // do not match a byte boundary.
      if (lengthLimitInBits % 8 != 0) {
        Assert.usage(lengthLimitInBits >= 0)
        dis.setBitLimit0b(MaybeULong(lengthLimitInBits))
      }

      val actual = dp.parse(dis, outputter)
      if (tdmlApiInfosetsEnv == "all") {
        val saxInputStream = optSaxInputStream.get
        Using.resource(InputSourceDataInputStream(saxInputStream)) { sis =>
          // The length limit here should be the length of the document
          // under test. Only set a limit when the end of the document
          // do not match a byte boundary.
          if (lengthLimitInBits % 8 != 0) {
            Assert.usage(lengthLimitInBits >= 0)
            sis.setBitLimit0b(MaybeULong(lengthLimitInBits))
          }

          val xri = dp.newXMLReaderInstance
          val errorHandler = new DaffodilTDMLSAXErrorHandler()
          val saxOutputStream = new ByteArrayOutputStream()
          val saxHandler =
            new DaffodilParseOutputStreamContentHandler(saxOutputStream, pretty = false)
          xri.setContentHandler(saxHandler)
          xri.setErrorHandler(errorHandler)
          xri.setProperty(XMLUtils.DAFFODIL_SAX_URN_BLOBDIRECTORY, blobDir)
          xri.setProperty(XMLUtils.DAFFODIL_SAX_URN_BLOBPREFIX, blobPrefix)
          xri.setProperty(XMLUtils.DAFFODIL_SAX_URN_BLOBSUFFIX, blobSuffix)

          xri.parse(sis)

          if (!actual.isError && !errorHandler.isError) {
            verifySameParseOutput(outputter.xmlStream, saxOutputStream)
          }
          val dpParseDiag = actual.getDiagnostics.map(_.getMessage())
          val saxParseDiag = errorHandler.getDiagnostics.map(_.getMessage())
          verifySameDiagnostics(dpParseDiag, saxParseDiag)
        }
      }
      new DaffodilTDMLParseResult(actual, outputter)
    }
  }

  def doUnparse(
    dpInputter: TDMLInfosetInputter,
    optSaxInputStream: Option[java.io.InputStream] = None,
    dpOutputStream: java.io.OutputStream
  ): DaffodilTDMLUnparseResult = {

    val dpOutputChannel = java.nio.channels.Channels.newChannel(dpOutputStream)
    val actualDP = dp.unparse(dpInputter, dpOutputChannel).asInstanceOf[UnparseResult]
    dpOutputChannel.close()

    if (tdmlApiInfosetsEnv == "all") {
      val saxInputStream = optSaxInputStream.get
      val saxOutputStream = new ByteArrayOutputStream
      val saxOutputChannel = java.nio.channels.Channels.newChannel(saxOutputStream)
      val unparseContentHandler = dp
        .newContentHandlerInstance(saxOutputChannel)
        .asInstanceOf[DaffodilUnparseContentHandler]
      unparseContentHandler.enableResolutionOfRelativeInfosetBlobURIs()
      val xmlReader = DaffodilSAXParserFactory().newSAXParser.getXMLReader
      xmlReader.setContentHandler(unparseContentHandler)
      xmlReader.setFeature(XMLUtils.SAX_NAMESPACES_FEATURE, true)
      xmlReader.setFeature(XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE, true)

      // kick off SAX Unparsing
      try {
        xmlReader.parse(new InputSource(saxInputStream))
      } catch {
        case e: DaffodilUnhandledSAXException =>
          // In the case of an unexpected errors, catch and throw as TDMLException
          throw TDMLException("Unexpected error during SAX Unparse:" + e, None)
        case _: DaffodilUnparseErrorSAXException =>
        // do nothing as unparseResult and its diagnostics will be handled below
      }

      val actualSAX = unparseContentHandler.getUnparseResult.asInstanceOf[UnparseResult]
      saxOutputChannel.close()
      if (!actualDP.isError && !actualSAX.isError) {
        val dpis = new ByteArrayInputStream(
          dpOutputStream.asInstanceOf[ByteArrayOutputStream].toByteArray
        )
        if (actualDP.isScannable && actualSAX.isScannable) {
          VerifyTestCase.verifyTextData(dpis, saxOutputStream, actualSAX.encodingName, None)
        } else {
          VerifyTestCase.verifyBinaryOrMixedData(dpis, saxOutputStream, None)
        }
      }
      val dpUnparseDiag = actualDP.getDiagnostics.map(_.getMessage())
      val saxUnparseDiag = actualSAX.getDiagnostics.map(_.getMessage())
      verifySameDiagnostics(dpUnparseDiag, saxUnparseDiag)
    }

    new DaffodilTDMLUnparseResult(actualDP, dpOutputStream)
  }

  def verifySameParseOutput(
    dpOutputStream: ByteArrayOutputStream,
    saxOutputStream: ByteArrayOutputStream
  ): Unit = {
    val dpParseOutputString = dpOutputStream.toString("UTF-8")
    val saxParseOutputString = saxOutputStream.toString("UTF-8")

    val dpParseXMLNodeOutput = scala.xml.XML.loadString(dpParseOutputString)
    val saxParseXMLNodeOutput = scala.xml.XML.loadString(saxParseOutputString)

    try {
      XMLUtils.compareAndReport(
        dpParseXMLNodeOutput,
        saxParseXMLNodeOutput,
        checkNamespaces = true,
        checkPrefixes = true
      )
    } catch {
      case e: XMLDifferenceException => {
        throw TDMLException(
          """SAX parse output (actual) does not match DataProcessor Parse output (expected)""" +
            "\n" + e.getMessage,
          None
        )
      }
    }
  }

  private def verifySameDiagnostics(
    seqDiagExpected: Seq[String],
    seqDiagActual: Seq[String]
  ): Unit = {
    val expected = seqDiagExpected.sorted
    val actual = seqDiagActual.sorted

    if (expected != actual) {
      throw TDMLException(
        """SAX parse/unparse diagnostics do not match DataProcessor diagnostics""" +
          "\n" +
          """DataProcessor Parse diagnostics: """ + seqDiagExpected +
          (if (seqDiagActual.isEmpty) {
             "\nNo SAX diagnostics were generated."
           } else {
             "\nSAX Parse diagnostics: " + seqDiagActual
           }),
        None
      )
    }
  }
}

final class DaffodilTDMLParseResult(actual: api.ParseResult, outputter: TDMLInfosetOutputter)
  extends TDMLParseResult {

  override def getResult: Node = outputter.getResult

  override def getBlobPaths: Seq[Path] = outputter.getBlobPaths()

  def inputter = outputter.toInfosetInputter

  override def isProcessingError: Boolean = actual.isProcessingError

  override def isValidationError: Boolean = actual.isValidationError

  override def getDiagnostics: Seq[api.Diagnostic] = actual.getDiagnostics

  override def currentLocation: api.DataLocation = actual.resultState.currentLocation

  override def addDiagnostic(diag: Diagnostic): Unit = actual.addDiagnostic(diag)

  override def cleanUp(): Unit = getBlobPaths.foreach { Files.delete }
}

final class DaffodilTDMLUnparseResult(
  actual: DFDL.UnparseResult,
  outStream: java.io.OutputStream
) extends TDMLUnparseResult {

  private val ustate = actual.resultState.asInstanceOf[UState]

  override def finalBitPos0b = ustate.getDataOutputStream.maybeAbsBitPos0b.get

  override def isScannable: Boolean = actual.isScannable

  override def encodingName: String = actual.encodingName

  override def isValidationError: Boolean = actual.isValidationError

  override def isProcessingError: Boolean = actual.isProcessingError

  override def getDiagnostics: Seq[api.Diagnostic] = actual.getDiagnostics

  override def bitPos0b: Long = ustate.bitPos0b
}

class DaffodilTDMLSAXErrorHandler extends ErrorHandler with WithDiagnostics {
  private var diagnostics: Seq[Diagnostic] = Nil
  private var errorStatus: Boolean = false

  override def warning(exception: SAXParseException): Unit = {
    errorStatus = false
    val embeddedDiagnostic = exception.getCause.asInstanceOf[Diagnostic]
    diagnostics :+= embeddedDiagnostic
  }

  override def error(exception: SAXParseException): Unit = {
    errorStatus = true
    val embeddedDiagnostic = exception.getCause.asInstanceOf[Diagnostic]
    diagnostics :+= embeddedDiagnostic
  }

  override def fatalError(exception: SAXParseException): Unit = {
    error(exception)
  }

  override def getDiagnostics: java.util.List[api.Diagnostic] = diagnostics

  override def isError: Boolean = errorStatus
}
