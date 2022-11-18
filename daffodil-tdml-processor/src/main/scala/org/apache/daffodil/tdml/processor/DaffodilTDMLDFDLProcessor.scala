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
import java.io.ByteArrayOutputStream
import java.io.OutputStreamWriter
import java.nio.channels.Channels
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.xml.Node
import org.apache.commons.io.IOUtils
import org.apache.daffodil.api.DFDL.DaffodilUnhandledSAXException
import org.apache.daffodil.api.DFDL.DaffodilUnparseErrorSAXException
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
import org.apache.daffodil.processors.DaffodilParseOutputStreamContentHandler
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.processors.UnparseResult
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.tdml.TDMLException
import org.apache.daffodil.tdml.TDMLInfosetInputter
import org.apache.daffodil.tdml.TDMLInfosetOutputter
import org.apache.daffodil.tdml.VerifyTestCase
import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.xml.DaffodilSAXParserFactory
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.xml.XMLUtils.XMLDifferenceException
import org.xml.sax.ErrorHandler
import org.xml.sax.InputSource
import org.xml.sax.SAXParseException

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
  private def generateProcessor(pf: DFDL.ProcessorFactory, useSerializedProcessor: Boolean): TDML.CompileResult = {
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
      Right((diags, new DaffodilTDMLDFDLProcessor(dp)))
    }
  }


  private def compileProcessor(
    schemaSource: DaffodilSchemaSource,
    useSerializedProcessor: Boolean,
    optRootName: Option[String],
    optRootNamespace: Option[String]): TDML.CompileResult = {
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
    optRootName: Option[String],
    optRootNamespace: Option[String],
    tunables: Map[String, String]): TDML.CompileResult = {
    if (schemaSource.isXSD) {
      val res = compileProcessor(schemaSource, useSerializedProcessor, optRootName, optRootNamespace)
      res
    } else {
      val dp = compiler.reload(schemaSource)
      val diags = dp.getDiagnostics
      Assert.invariant(diags.forall{! _.isError })
      Right((diags, new DaffodilTDMLDFDLProcessor(dp)))
    }
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

  override def withDebugger(db: AnyRef): DaffodilTDMLDFDLProcessor = {
    Assert.usage(dp ne null)
    val d = dp.asInstanceOf[Debugger]
    copy(dp = dp.withDebugger(d))
  }

  override def withValidationMode(validationMode: ValidationMode.Type): DaffodilTDMLDFDLProcessor =
    copy(dp = dp.withValidationMode(validationMode))

  override def withExternalDFDLVariables(externalVarBindings: Seq[Binding]): DaffodilTDMLDFDLProcessor =
    copy(dp = dp.withExternalVariables(externalVarBindings))

  override def isError: Boolean = dp.isError

  override def getDiagnostics: Seq[Diagnostic] = dp.getDiagnostics

  def parse(uri: java.net.URI, lengthLimitInBits: Long): TDMLParseResult = {
    val url = uri.toURL
    val dpInputStream = url.openStream()
    val saxInputStream = url.openStream()
    doParseWithBothApis(dpInputStream, saxInputStream, lengthLimitInBits)
  }

  def parse(arr: Array[Byte], lengthLimitInBits: Long): TDMLParseResult = {
    val dpInputStream = new ByteArrayInputStream(arr)
    val saxInputStream = new ByteArrayInputStream(arr)
    doParseWithBothApis(dpInputStream, saxInputStream, lengthLimitInBits)
  }

  override def parse(is: java.io.InputStream, lengthLimitInBits: Long): TDMLParseResult = {
    val arr = IOUtils.toByteArray(is)
    parse(arr, lengthLimitInBits)
  }

  override def unparse(infosetXML: scala.xml.Node, outStream: java.io.OutputStream): TDMLUnparseResult = {
    val scalaInputter = new ScalaXMLInfosetInputter(infosetXML)
    // We can't compare against other inputters since we only have scala XML,
    // but we still need to use the TDMLInfosetInputter since it may make TDML
    // specific modifications to the input infoset (e.g. blob paths)
    val otherInputters = Seq.empty
    val inputter = new TDMLInfosetInputter(scalaInputter, otherInputters)
    unparse(inputter, infosetXML, outStream)
  }

  def unparse(parseResult: TDMLParseResult, outStream: java.io.OutputStream): TDMLUnparseResult = {
    val dafpr = parseResult.asInstanceOf[DaffodilTDMLParseResult]
    val inputter = dafpr.inputter
    val resNode = dafpr.getResult
    unparse(inputter, resNode, outStream)
  }

  def unparse(inputter: TDMLInfosetInputter, infosetXML: scala.xml.Node, outStream: java.io.OutputStream): TDMLUnparseResult = {
    val bos = new ByteArrayOutputStream()
    val osw = new OutputStreamWriter(bos, StandardCharsets.UTF_8)
    scala.xml.XML.write(osw, infosetXML, "UTF-8", xmlDecl = true, null)
    osw.flush()
    osw.close()
    val saxInstream = new ByteArrayInputStream(bos.toByteArray)
    doUnparseWithBothApis(inputter, saxInstream, outStream)
  }

  def doParseWithBothApis(dpInputStream: java.io.InputStream, saxInputStream: java.io.InputStream,
    lengthLimitInBits: Long): TDMLParseResult = {
    //
    // TDML Tests MUST have a length limit. Otherwise they cannot determine if
    // there is left-over-data or not without doing more reading from the input stream
    // so as to be sure to hit end-of-data.
    //
    Assert.usage(lengthLimitInBits >= 0)

    val outputter = new TDMLInfosetOutputter()
    outputter.setBlobAttributes(blobDir, blobPrefix, blobSuffix)

    val xri = dp.newXMLReaderInstance
    val errorHandler = new DaffodilTDMLSAXErrorHandler()
    val saxOutputStream = new ByteArrayOutputStream()
    val saxHandler = new DaffodilParseOutputStreamContentHandler(saxOutputStream, pretty = false)
    xri.setContentHandler(saxHandler)
    xri.setErrorHandler(errorHandler)
    xri.setProperty(XMLUtils.DAFFODIL_SAX_URN_BLOBDIRECTORY, blobDir)
    xri.setProperty(XMLUtils.DAFFODIL_SAX_URN_BLOBPREFIX, blobPrefix)
    xri.setProperty(XMLUtils.DAFFODIL_SAX_URN_BLOBSUFFIX, blobSuffix)

    val dis = InputSourceDataInputStream(dpInputStream)
    val sis = InputSourceDataInputStream(saxInputStream)

    dis.setBitLimit0b(MaybeULong(lengthLimitInBits))
    sis.setBitLimit0b(MaybeULong(lengthLimitInBits))

    val actual = dp.parse(dis, outputter)
    xri.parse(sis)

    if (!actual.isError && !errorHandler.isError) {
      verifySameParseOutput(outputter.xmlStream, saxOutputStream)
    }
    val dpParseDiag = actual.getDiagnostics.map(_.getMessage())
    val saxParseDiag = errorHandler.getDiagnostics.map(_.getMessage())
    verifySameDiagnostics(dpParseDiag, saxParseDiag)

    new DaffodilTDMLParseResult(actual, outputter)
  }

  def doUnparseWithBothApis(
    dpInputter: TDMLInfosetInputter,
    saxInputStream: java.io.InputStream,
    dpOutputStream: java.io.OutputStream): DaffodilTDMLUnparseResult = {

    val dpOutputChannel = java.nio.channels.Channels.newChannel(dpOutputStream)
    val saxOutputStream = new ByteArrayOutputStream
    val saxOutputChannel = java.nio.channels.Channels.newChannel(saxOutputStream)
    val unparseContentHandler = dp.newContentHandlerInstance(saxOutputChannel)
    unparseContentHandler.enableInputterResolutionOfRelativeInfosetBlobURIs()
    val xmlReader = DaffodilSAXParserFactory().newSAXParser.getXMLReader
    xmlReader.setContentHandler(unparseContentHandler)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACES_FEATURE, true)
    xmlReader.setFeature(XMLUtils.SAX_NAMESPACE_PREFIXES_FEATURE, true)

    val actualDP = dp.unparse(dpInputter, dpOutputChannel).asInstanceOf[UnparseResult]
    dpOutputChannel.close()
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

    val actualSAX = unparseContentHandler.getUnparseResult
    saxOutputChannel.close()
    if (!actualDP.isError && !actualSAX.isError) {
      val dpis = new ByteArrayInputStream(dpOutputStream.asInstanceOf[ByteArrayOutputStream]
        .toByteArray)
      if (actualDP.isScannable && actualSAX.isScannable) {
        VerifyTestCase.verifyTextData(dpis, saxOutputStream, actualSAX.encodingName, None)
      } else {
        VerifyTestCase.verifyBinaryOrMixedData(dpis, saxOutputStream, None)
      }
    }
    val dpUnparseDiag = actualDP.getDiagnostics.map(_.getMessage())
    val saxUnparseDiag = actualSAX.getDiagnostics.map(_.getMessage())
    verifySameDiagnostics(dpUnparseDiag, saxUnparseDiag)

    new DaffodilTDMLUnparseResult(actualDP, dpOutputStream)
  }

  def verifySameParseOutput(dpOutputStream: ByteArrayOutputStream, saxOutputStream: ByteArrayOutputStream): Unit = {
    val dpParseOutputString = dpOutputStream.toString("UTF-8")
    val saxParseOutputString = saxOutputStream.toString("UTF-8")

    val dpParseXMLNodeOutput = scala.xml.XML.loadString(dpParseOutputString)
    val saxParseXMLNodeOutput = scala.xml.XML.loadString(saxParseOutputString)

    try {
      XMLUtils.compareAndReport(
        dpParseXMLNodeOutput,
        saxParseXMLNodeOutput,
        checkNamespaces = true,
        checkPrefixes = true)
    } catch {
      case e: XMLDifferenceException => {
        throw TDMLException(
          """SAX parse output (actual) does not match DataProcessor Parse output (expected)""" +
            "\n" + e.getMessage, None)
      }
    }
  }

  private def verifySameDiagnostics(seqDiagExpected: Seq[String], seqDiagActual: Seq[String]): Unit = {
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
          }), None)
    }
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

  override def cleanUp(): Unit = getBlobPaths.foreach { Files.delete }
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

  override def getDiagnostics: Seq[Diagnostic] = diagnostics

  override def isError: Boolean = errorStatus
}
