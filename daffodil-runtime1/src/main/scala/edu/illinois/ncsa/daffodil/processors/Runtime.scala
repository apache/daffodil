/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.Implicits._; object INoWarn4 { ImplicitsSuppressUnusedImportWarning() }
import edu.illinois.ncsa.daffodil.equality._; object EqualityNoWarn3 { EqualitySuppressUnusedImportWarning() }
import edu.illinois.ncsa.daffodil.api.WithDiagnostics
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.api.WithDiagnostics
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.dsom.ValidationError
import edu.illinois.ncsa.daffodil.util.Validator
import org.xml.sax.SAXParseException
import org.xml.sax.SAXException
import edu.illinois.ncsa.daffodil.util.ValidationException
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.externalvars.ExternalVariablesLoader
import java.io.File
import edu.illinois.ncsa.daffodil.externalvars.Binding
import java.nio.channels.Channels
import java.nio.channels.FileChannel
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import java.io.ObjectOutputStream
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.api.DaffodilTunableParameters.TunableLimitExceededError
import edu.illinois.ncsa.daffodil.api.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.debugger.Debugger
import java.util.zip.GZIPOutputStream
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.processors.unparsers.InfosetCursor
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.processors.unparsers.UnparseError
import edu.illinois.ncsa.daffodil.dsom.oolag.ErrorAlreadyHandled
import edu.illinois.ncsa.daffodil.events.MultipleEventHandler
import edu.illinois.ncsa.daffodil.io.DataStreamCommon
import edu.illinois.ncsa.daffodil.io.DirectOrBufferedDataOutputStream

/**
 * Implementation mixin - provides simple helper methods
 */
trait WithDiagnosticsImpl extends WithDiagnostics {

  //  final lazy val hasDiagnostics = {
  //    getDiagnostics.size > 0
  //  }
}

class InvalidUsageException(msg: String, cause: Throwable = null) extends Exception(msg, cause)

/**
 * This is the DataProcessor constructed from a saved Parser.
 */
class SerializableDataProcessor(val data: SchemaSetRuntimeData)
    extends DataProcessor(data) {
  override def setValidationMode(mode: ValidationMode.Type): Unit = {
    if (mode == ValidationMode.Full) { throw new InvalidUsageException("'Full' validation not allowed when using a restored parser.") }
    super.setValidationMode(mode)
  }
}

trait HasSetDebugger {
  def setDebugger(dbg: Debugger): Unit
  def setDebugging(b: Boolean): Unit
}
/**
 * The very last aspects of compilation, and the start of the
 * back-end runtime.
 */
class DataProcessor(val ssrd: SchemaSetRuntimeData)
    extends DFDL.DataProcessor with Logging
    with HasSetDebugger with Serializable
    with MultipleEventHandler {

  def setValidationMode(mode: ValidationMode.Type): Unit = { ssrd.validationMode = mode }
  def getValidationMode() = ssrd.validationMode
  def getVariables = ssrd.variables

  @transient private var areDebugging_ = false

  def areDebugging = areDebugging_

  @transient private var optDebugger_ : Option[Debugger] = None

  private def optDebugger = {
    if (optDebugger_ == null) {
      // transient value restored as null
      optDebugger_ = None
    }
    optDebugger_
  }

  def debugger = {
    Assert.invariant(areDebugging)
    optDebugger.get
  }

  def setDebugger(dbg: Debugger) {
    optDebugger_ = Some(dbg)
  }

  def setDebugging(flag: Boolean) {
    areDebugging_ = flag
  }

  def setExternalVariables(extVars: Map[String, String]): Unit = {
    val bindings = ExternalVariablesLoader.getVariables(extVars)
    ExternalVariablesLoader.loadVariables(bindings, ssrd, ssrd.variables)
    ssrd.variables = ExternalVariablesLoader.loadVariables(extVars, ssrd, ssrd.variables)
  }
  def setExternalVariables(extVars: File): Unit = {
    ssrd.variables = ExternalVariablesLoader.loadVariables(extVars, ssrd, ssrd.variables)
  }
  def setExternalVariables(extVars: Seq[Binding]): Unit = {
    ssrd.variables = ExternalVariablesLoader.loadVariables(extVars, ssrd, ssrd.variables)
  }

  override def isError = false // really there is no compiling at all currently, so there can be no errors.

  override def getDiagnostics = ssrd.diagnostics

  def save(output: DFDL.Output): Unit = {
    val oos = new ObjectOutputStream(new GZIPOutputStream(Channels.newOutputStream(output)))
    oos.writeObject(new SerializableDataProcessor(ssrd))
    oos.close()
  }

  /**
   * Here begins the parser runtime. Compiler-oriented mechanisms (OOLAG etc.) aren't used in the
   * runtime. Instead we deal with success and failure statuses.
   */
  def parse(input: DFDL.Input, lengthLimitInBits: Long = -1): DFDL.ParseResult = {
    Assert.usage(!this.isError)

    val rootERD = ssrd.elementRuntimeData

    val initialState =
      PState.createInitialPState(rootERD,
        input,
        this,
        bitOffset = 0,
        bitLengthLimit = lengthLimitInBits) // TODO also want to pass here the externally set variables, other flags/settings.
    parse(initialState)
  }

  def parse(file: File): DFDL.ParseResult = {
    Assert.usage(!this.isError)

    val initialState =
      PState.createInitialPState(ssrd.elementRuntimeData,
        FileChannel.open(file.toPath),
        this,
        bitOffset = 0,
        bitLengthLimit = file.length * 8) // TODO also want to pass here the externally set variables, other flags/settings.
    parse(initialState)
  }

  def parse(state: PState): ParseResult = {
    ExecutionMode.usingRuntimeMode {

      if (areDebugging) {
        Assert.invariant(optDebugger.isDefined)
        addEventHandler(debugger)
        state.notifyDebugging(true)
      }
      state.dataProc.get.init(ssrd.parser)
      doParse(ssrd.parser, state)
      val pr = new ParseResult(this, state)
      pr.validateResult(state)

      val s = state
      val dp = s.dataProc
      val ssrdParser = ssrd.parser
      if (dp.isDefined) dp.value.fini(ssrdParser)

      pr
    }
  }

  object dataInputStreamLimits extends DataStreamCommon.Limits {
    def maximumSimpleElementSizeInBytes: Long = DaffodilTunableParameters.maxFieldContentLengthInBytes
    def maximumSimpleElementSizeInCharacters: Long = DaffodilTunableParameters.maxFieldContentLengthInBytes
    def maximumForwardSpeculationLengthInBytes: Long = DaffodilTunableParameters.maxFieldContentLengthInBytes
    def maximumRegexMatchLengthInCharacters: Long = DaffodilTunableParameters.maxFieldContentLengthInBytes
    def defaultInitialRegexMatchLimitInChars: Long = DaffodilTunableParameters.defaultInitialRegexMatchLimitInChars
  }

  private def doParse(p: Parser, state: PState) {
    try {
      state.dataInputStream.setLimits(dataInputStreamLimits)
      this.startElement(state, p)
      p.parse1(state)
      this.endElement(state, p)
    } catch {
      // technically, runtime shouldn't throw. It's really too heavyweight a construct. And "failure"
      // when parsing isn't exceptional, it's routine behavior. So ought not be implemented via an
      // exception handling construct.
      //
      // But we might not catch everything inside...
      //
      case pe: ParseError => {
        // if we get one here, then someone threw instead of returning a status.
        Assert.invariantFailed("ParseError caught. ParseErrors should be returned as failed status, not thrown. Fix please.")
      }
      case procErr: ProcessingError => {
        val x = procErr
        state.setFailed(x.toParseError)
      }
      case sde: SchemaDefinitionError => {
        // A SDE was detected at runtime (perhaps due to a runtime-valued property like byteOrder or encoding)
        // These are fatal, and there's no notion of backtracking them, so they propagate to top level
        // here.
        state.setFailed(sde)
      }
      case rsde: RuntimeSchemaDefinitionError => {
        state.setFailed(rsde)
      }
      case e: ErrorAlreadyHandled => {
        state.setFailed(e.th)
      }
      case e: TunableLimitExceededError => {
        state.setFailed(e)
      }
    }

    state.dataInputStream.validateFinalStreamState
  }

  def unparse(output: DFDL.Output, xmlEventCursor: XMLEventCursor): DFDL.UnparseResult = {
    val rootERD = ssrd.elementRuntimeData
    val infosetCursor = InfosetCursor.fromXMLEventCursor(xmlEventCursor, rootERD)
    unparse(output, infosetCursor)
  }

  def unparse(output: DFDL.Output, infosetXML: scala.xml.Node): DFDL.UnparseResult = {
    val rootERD = ssrd.elementRuntimeData
    val is = InfosetCursor.fromXMLNode(infosetXML, rootERD)
    unparse(output, is)
  }

  def unparse(output: DFDL.Output, infosetCursor: InfosetCursor): DFDL.UnparseResult = {
    Assert.usage(!this.isError)
    val outStream = java.nio.channels.Channels.newOutputStream(output)
    val out = DirectOrBufferedDataOutputStream(outStream, null) // null means no other stream created this one.
    out.setBitOrder(BitOrder.MostSignificantBitFirst) // FIXME: derive from rootERD (doesn't have currently.) Note: only needed if starting bit position isn't 0
    val unparserState =
      UState.createInitialUState(
        out,
        this,
        infosetCursor) // TODO also want to pass here the externally set variables, other flags/settings.
    val res = try {
      if (areDebugging) {
        Assert.invariant(optDebugger.isDefined)
        addEventHandler(debugger)
        unparserState.notifyDebugging(true)
      }
      unparserState.dataProc.get.init(ssrd.unparser)
      unparse(unparserState)
      unparserState.evalSuspensions() // handles outputValueCalc that were suspended due to forward references.
      unparserState.unparseResult
    } catch {
      case ue: UnparseError => {
        unparserState.addUnparseError(ue)
        unparserState.unparseResult
      }
      case procErr: ProcessingError => {
        val x = procErr
        unparserState.setFailed(x.toUnparseError)
        unparserState.unparseResult
      }
      case sde: SchemaDefinitionError => {
        // A SDE was detected at runtime (perhaps due to a runtime-valued property like byteOrder or encoding)
        // These are fatal, and there's no notion of backtracking them, so they propagate to top level
        // here.
        unparserState.setFailed(sde)
        unparserState.unparseResult
      }
      case rsde: RuntimeSchemaDefinitionError => {
        unparserState.setFailed(rsde)
        unparserState.unparseResult
      }
      case e: ErrorAlreadyHandled => {
        unparserState.setFailed(e.th)
        unparserState.unparseResult
      }
      case e: TunableLimitExceededError => {
        unparserState.setFailed(e)
        unparserState.unparseResult
      }
      case se: org.xml.sax.SAXException => {
        unparserState.setFailed(new UnparseError(None, None, se.getMessage))
        unparserState.unparseResult
      }
      case e: scala.xml.parsing.FatalError => {
        unparserState.setFailed(new UnparseError(None, None, e.getMessage))
        unparserState.unparseResult
      }
    }

    if (unparserState.dataProc.isDefined) unparserState.dataProc.value.fini(ssrd.unparser)
    infosetCursor.fini

    res
  }

  def unparse(state: UState): Unit = {
    val rootUnparser = ssrd.unparser
    rootUnparser.unparse(state)
    if (!state.dataOutputStream.isFinished) state.dataOutputStream.setFinished()
  }

}

class ParseResult(dp: DataProcessor, override val resultState: PState)
    extends DFDL.ParseResult
    with WithDiagnosticsImpl {

  def toWriter(writer: java.io.Writer) = {
    resultState.infoset.toWriter(writer)
    writer.flush()
  }

  /**
   * Xerces validation.
   */
  private def validateWithXerces(state: PState): Unit = {
    if (state.status eq Success) {
      val schemaURIStrings = state.infoset.asInstanceOf[InfosetElement].runtimeData.schemaURIStringsForFullValidation
      Validator.validateXMLSources(schemaURIStrings, result)
    } else {
      Assert.abort(new IllegalStateException("There is no result. Should check by calling isError() first."))
    }
  }

  /**
   * To be successful here, we need to capture xerces parse/validation
   * errors and add them to the Diagnostics list in the PState.
   *
   * @param state the initial parse state.
   */
  def validateResult(state: PState) {
    if (dp.getValidationMode == ValidationMode.Full) {
      try {
        validateWithXerces(state)
      } catch {
        case (spe: SAXParseException) =>
          state.reportValidationErrorNoContext(spe.getMessage)

        case (se: SAXException) =>
          state.reportValidationErrorNoContext(se.getMessage)

        case (ve: ValidationException) =>
          state.reportValidationErrorNoContext(ve.getMessage)
      }
    }
  }

  lazy val isValidationSuccess = {
    val res = dp.getValidationMode match {
      case ValidationMode.Off => true
      case _ => {
        val res = resultState.diagnostics.exists { d =>
          d match {
            case ve: ValidationError => true
            case _ => false
          }
        }
        res
      }
    }
    res
  }

  lazy val result =
    if (resultState.status eq Success) {
      resultAsScalaXMLElement
    } else {
      Assert.abort(new IllegalStateException("There is no result. Should check by calling isError() first."))
    }

  lazy val resultAsScalaXMLElement =
    if (resultState.status eq Success) {
      val xmlClean = {
        val nodeSeq = resultState.infoset.toXML()
        val Seq(eNoHidden) = XMLUtils.removeHiddenElements(nodeSeq)
        //        val eNoAttribs = XMLUtils.removeAttributes(eNoHidden)
        //        eNoAttribs
        eNoHidden
      }
      xmlClean
    } else {
      Assert.abort(new IllegalStateException("There is no result. Should check by calling isError() first."))
    }
}

class UnparseResult(dp: DataProcessor, ustate: UState)
    extends DFDL.UnparseResult
    with WithDiagnosticsImpl {

  override def resultState = ustate

  private def maybeEncodingInfo =
    if (Maybe.WithNulls.isDefined(ustate.currentInfosetNode))
      One(ustate.currentInfosetNode.asInstanceOf[DIElement].runtimeData.encodingInfo)
    else
      Nope

  private def encodingInfo = maybeEncodingInfo.getOrElse(dp.ssrd.elementRuntimeData.encodingInfo)

  def summaryEncoding = encodingInfo.summaryEncoding

  override def isScannable = encodingInfo.isScannable
  override def encodingName = {
    Assert.invariant(encodingInfo.isKnownEncoding)
    // we're not supporting runtime-calculated encodings yet so not
    // capturing that information (what the actual runtime-value of encoding was
    encodingInfo.knownEncodingName
  }
}
