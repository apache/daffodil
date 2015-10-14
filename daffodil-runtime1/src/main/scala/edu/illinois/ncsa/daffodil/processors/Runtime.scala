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

import edu.illinois.ncsa.daffodil.api.WithDiagnostics
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.JDOMUtils
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.Implicits._; object INoWarn4 { ImplicitsSuppressUnusedImportWarning() }
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.api.WithDiagnostics
import org.jdom2.Namespace
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.dsom.ValidationError
import edu.illinois.ncsa.daffodil.util.Validator
import org.xml.sax.SAXParseException
import org.xml.sax.SAXException
import edu.illinois.ncsa.daffodil.util.ValidationException
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.externalvars.ExternalVariablesLoader
import scala.xml.Node
import java.io.File
import edu.illinois.ncsa.daffodil.externalvars.Binding
import edu.illinois.ncsa.daffodil.processors.charset.CharsetUtils
import java.nio.channels.Channels
import java.nio.charset.CodingErrorAction
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import java.nio.channels.FileChannel
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import java.io.ObjectOutputStream
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.PreSerialization
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters.TunableLimitExceededError
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.debugger.Debugger
import java.util.zip.GZIPOutputStream
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.processors.unparsers.InfosetSource
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.xml.scalaLib.PrettyPrinter
import edu.illinois.ncsa.daffodil.equality._; object EqualityNoWarn3 { EqualitySuppressUnusedImportWarning() }
import edu.illinois.ncsa.daffodil.processors.unparsers.UnparseError
import edu.illinois.ncsa.daffodil.dsom.oolag.ErrorAlreadyHandled
import edu.illinois.ncsa.daffodil.events.MultipleEventHandler
import edu.illinois.ncsa.daffodil.io.DataInputStream
import edu.illinois.ncsa.daffodil.io.DataStreamCommon
import edu.illinois.ncsa.daffodil.io.BasicDataOutputStream
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException

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
      try {
        if (areDebugging) {
          Assert.invariant(optDebugger.isDefined)
          addEventHandler(debugger)
          state.notifyDebugging(true)
        }
        state.dataProc.init(ssrd.parser)
        doParse(ssrd.parser, state)
        val pr = new ParseResult(this, state)
        pr.validateResult(state)
        return pr
      } catch {
        case s: scala.util.control.ControlThrowable => throw s
        case u: UnsuppressableException => throw u
        case th: Throwable =>
          System.err.println("Unexpected throw of " + th)
          throw th // place for a breakpoint
      } finally {
        val s = state
        val dp = s.dataProc
        val ssrdParser = ssrd.parser
        dp.fini(ssrdParser)
      }
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
        Assert.invariantFailed("got a processing error that was not a parse error: %s. This is the parser!".format(x))
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
    } finally {
      state.dataInputStream.validateFinalStreamState
    }
  }

  def unparse(output: DFDL.Output, xmlEventReader: Iterator[scala.xml.pull.XMLEvent]): DFDL.UnparseResult = {
    val rootERD = ssrd.elementRuntimeData
    val infosetSource = InfosetSource.fromXMLSource(xmlEventReader, rootERD)
    unparse(output, infosetSource)
  }

  def unparse(output: DFDL.Output, infosetXML: scala.xml.Node): DFDL.UnparseResult = {
    val rootERD = ssrd.elementRuntimeData
    val is = InfosetSource.fromXMLNode(infosetXML, rootERD)
    unparse(output, is)
  }

  def unparse(output: DFDL.Output, infosetSource: InfosetSource): DFDL.UnparseResult = {
    Assert.usage(!this.isError)
    val out = BasicDataOutputStream(output)
    out.setBitOrder(BitOrder.MostSignificantBitFirst) // FIXME: derive from rootERD (doesn't have currently.) Note: only needed if starting bit position isn't 0
    val unparserState =
      UState.createInitialUState(
        out,
        this,
        infosetSource) // TODO also want to pass here the externally set variables, other flags/settings.
    try {
      if (areDebugging) {
        Assert.invariant(optDebugger.isDefined)
        addEventHandler(debugger)
        unparserState.notifyDebugging(true)
      }
      unparserState.dataProc.init(ssrd.unparser)
      unparse(unparserState)
      unparserState.unparseResult
    } catch {
      case ue: UnparseError => {
        unparserState.addUnparseError(ue)
        unparserState.unparseResult
      }
    } finally {
      unparserState.dataProc.fini(ssrd.unparser)
    }
  }

  def unparse(state: UState): Unit = {
    val rootUnparser = ssrd.unparser
    rootUnparser.unparse(state)
  }

}

class ParseResult(dp: DataProcessor, override val resultState: PState)
  extends DFDL.ParseResult
  with WithDiagnosticsImpl {

  def toWriter(writer: java.io.Writer) = {
    if (resultState.infoset.totalElementCount < DaffodilTunableParameters.prettyPrintElementLimit) {
      // pretty print small infosets
      val pp = new PrettyPrinter(80, 2)
      writer.write(pp.format(resultState.infoset.toXML()(0)))
    } else {
      // direct write for larger infosets
      resultState.infoset.toWriter(writer)
    }
    writer.write("\n")
    writer.flush()
  }

  /**
   * Xerces validation.
   */
  private def validateWithXerces(state: PState): Unit = {
    if (state.status == Success) {
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
    if (resultState.status == Success) {
      resultAsScalaXMLElement
    } else {
      Assert.abort(new IllegalStateException("There is no result. Should check by calling isError() first."))
    }

  lazy val resultAsScalaXMLElement =
    if (resultState.status == Success) {
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
    if (Maybe.isDefined(ustate.currentInfosetNode))
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
