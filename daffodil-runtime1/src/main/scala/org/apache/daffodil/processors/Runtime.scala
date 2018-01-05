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
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.api.WithDiagnostics
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.util.Validator
import org.xml.sax.SAXParseException
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
import edu.illinois.ncsa.daffodil.debugger.Debugger
import java.util.zip.GZIPOutputStream
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.infoset.InfosetInputter
import edu.illinois.ncsa.daffodil.processors.unparsers.UnparseError
import edu.illinois.ncsa.daffodil.oolag.ErrorAlreadyHandled
import edu.illinois.ncsa.daffodil.events.MultipleEventHandler
import edu.illinois.ncsa.daffodil.io.DirectOrBufferedDataOutputStream
import edu.illinois.ncsa.daffodil.util.LogLevel
import org.xml.sax.ErrorHandler
import org.xml.sax.SAXException
import edu.illinois.ncsa.daffodil.io.BitOrderChangeException
import edu.illinois.ncsa.daffodil.infoset._
import edu.illinois.ncsa.daffodil.processors.parsers.ParseError
import edu.illinois.ncsa.daffodil.processors.parsers.Parser
import edu.illinois.ncsa.daffodil.processors.parsers.PState
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.api.TunableLimitExceededError
import edu.illinois.ncsa.daffodil.api.DaffodilTunables

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

  protected var tunablesObj = ssrd.tunable // Compiler-set tunables

  def setValidationMode(mode: ValidationMode.Type): Unit = { ssrd.validationMode = mode }
  def getValidationMode() = ssrd.validationMode
  def getVariables = ssrd.variables
  def getTunables = tunablesObj

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
    setTunable("allowExternalPathExpressions", flag.toString)
  }

  def setExternalVariables(extVars: Map[String, String]): Unit = {
    val bindings = ExternalVariablesLoader.getVariables(extVars)
    ExternalVariablesLoader.loadVariables(bindings, ssrd, ssrd.variables)
    ssrd.variables = ExternalVariablesLoader.loadVariables(extVars, ssrd, ssrd.variables)
  }
  def setExternalVariables(extVars: File): Unit = {
    ssrd.variables = ExternalVariablesLoader.loadVariables(extVars, ssrd, ssrd.variables, getTunables)
  }
  def setExternalVariables(extVars: File, tunable: DaffodilTunables): Unit = {
    ssrd.variables = ExternalVariablesLoader.loadVariables(extVars, ssrd, ssrd.variables, getTunables)
  }
  def setExternalVariables(extVars: Seq[Binding]): Unit = {
    ssrd.variables = ExternalVariablesLoader.loadVariables(extVars, ssrd, ssrd.variables)
  }

  def setTunable(tunable: String, value: String): Unit = tunablesObj = tunablesObj.setTunable(tunable, value)
  def setTunables(tunables: Map[String, String]): Unit = tunablesObj = tunablesObj.setTunables(tunables)
  def resetTunables(): Unit = tunablesObj = ssrd.tunable // Compiler-set values

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
  def parse(input: DFDL.Input, output: InfosetOutputter, lengthLimitInBits: Long = -1): DFDL.ParseResult = {
    Assert.usage(!this.isError)

    val rootERD = ssrd.elementRuntimeData

    val initialState =
      PState.createInitialPState(rootERD,
        input,
        output,
        this,
        bitOffset = 0,
        bitLengthLimit = lengthLimitInBits)
    parse(initialState)
  }

  def parse(file: File, output: InfosetOutputter): DFDL.ParseResult = {
    Assert.usage(!this.isError)

    val initialState =
      PState.createInitialPState(ssrd.elementRuntimeData,
        FileChannel.open(file.toPath),
        output,
        this,
        bitOffset = 0,
        bitLengthLimit = file.length * 8)
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
      if (!pr.isProcessingError) {
        pr.validateResult()

        // now that everything has succeeded, call the infoset outputter to
        // output the infoset
        //
        // TODO: eventually, we want infoset outputting to happen while parsing
        // so that we can start to throw away infoset nodes. When that happens
        // we can remove this line.
        state.infoset.visit(state.output)
      }
      val s = state
      val dp = s.dataProc
      val ssrdParser = ssrd.parser
      if (dp.isDefined) dp.value.fini(ssrdParser)

      pr
    }
  }

  private def doParse(p: Parser, state: PState) {
    try {
      state.dataInputStream.setLimits(state.tunable)
      this.startElement(state, p)
      p.parse1(state)
      this.endElement(state, p)
      //
      // After the end of all processing, we still call things that ask for the
      // ERD, and expect to find it on the processor.context. If we don't set
      // this, then the processor.context is Nope (because it is set on the
      // way into a parser, and unset back when a parser unwinds). We
      // want it to do this wind/unwind, but here at the ultimate top
      // level we want to defeat that final unwind
      // so that subsequent use of the state can generally work and have a context.
      //
      state.setMaybeProcessor(Maybe(p))

      /* Verify that all stacks are empty */
      Assert.invariant(state.mpstate.arrayIndexStack.length == 1)
      Assert.invariant(state.mpstate.groupIndexStack.length == 1)
      Assert.invariant(state.mpstate.childIndexStack.length == 1)
      Assert.invariant(state.mpstate.occursBoundsStack.length == 1)
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
      case us: UnsuppressableException => throw us
      case x: Throwable =>
        Assert.invariantFailed("Runtime.scala - Leaked exception: " + x)
    }

    state.dataInputStream.validateFinalStreamState
  }

  def unparse(inputter: InfosetInputter, output: DFDL.Output): DFDL.UnparseResult = {
    Assert.usage(!this.isError)
    val outStream = java.nio.channels.Channels.newOutputStream(output)
    val out = DirectOrBufferedDataOutputStream(outStream, null) // null means no other stream created this one.
    val unparserState =
      UState.createInitialUState(
        out,
        this,
        inputter)
    val res = try {
      if (areDebugging) {
        Assert.invariant(optDebugger.isDefined)
        addEventHandler(debugger)
        unparserState.notifyDebugging(true)
      }
      inputter.initialize(ssrd.elementRuntimeData, unparserState.tunable)
      unparserState.dataProc.get.init(ssrd.unparser)
      out.setPriorBitOrder(ssrd.elementRuntimeData.defaultBitOrder)
      doUnparse(unparserState)
      unparserState.evalSuspensions(unparserState) // handles outputValueCalc that were suspended due to forward references.
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
        unparserState.setFailed(new UnparseError(None, None, se))
        unparserState.unparseResult
      }
      case e: scala.xml.parsing.FatalError => {
        unparserState.setFailed(new UnparseError(None, None, e))
        unparserState.unparseResult
      }
      case ie: InfosetException => {
        unparserState.setFailed(new UnparseError(None, None, ie))
        unparserState.unparseResult
      }
      case th: Throwable => throw th
    }
    res
  }

  private def doUnparse(state: UState): Unit = {
    val rootUnparser = ssrd.unparser
    // LoggingDefaults.setLoggingLevel(LogLevel.Debug)
    rootUnparser.unparse1(state)

    /* Verify that all stacks are empty */
    Assert.invariant(state.arrayIndexStack.length == 1)
    Assert.invariant(state.groupIndexStack.length == 1)
    Assert.invariant(state.childIndexStack.length == 1)
    Assert.invariant(state.occursBoundsStack.length == 1)
    Assert.invariant(state.currentInfosetNodeStack.isEmpty)
    Assert.invariant(state.escapeSchemeEVCache.isEmpty)
    //
    // All the DOS that precede the last one
    // will get setFinished by the suspension that created them. The last one after the
    // final suspension gets setFinished here.
    //
    // TODO: revisit this in light of API changes that allow a series of calls
    // that unparse to the same output stream. Sort of like a cursor.
    // The idea here is that I'd have an incoming stream of Infoset events, and
    // an outgoing java.io.outputStream, and each unparse call would consume
    // events, and write to the java.io.outputStream, but neither would be closed.
    // An application could do this in a loop calling unparse repeatedly
    // without having to create a new infoset event stream or outputstream.
    //
    Assert.invariant(!state.dataOutputStream.isFinished)
    try {
      state.dataOutputStream.setFinished(state)
    } catch {
      case boc: BitOrderChangeException =>
        state.SDE(boc)
    }
    log(LogLevel.Debug, "%s final stream for %s finished.", this, state)

    val ev = state.advanceMaybe
    if (ev.isDefined) {
      UnparseError(Nope, One(state.currentLocation), "Expected no remaining events, but received %s.", ev.get)
    }
  }

}

class ParseResult(dp: DataProcessor, override val resultState: PState)
  extends DFDL.ParseResult
  with WithDiagnosticsImpl
  with ErrorHandler {

  /**
   * To be successful here, we need to capture xerces parse/validation
   * errors and add them to the Diagnostics list in the PState.
   *
   * @param state the initial parse state.
   */
  def validateResult(): Unit = {
    Assert.usage(resultState.processorStatus eq Success)
    if (dp.getValidationMode == ValidationMode.Full) {
      val schemaURIStrings = resultState.infoset.asInstanceOf[InfosetElement].runtimeData.schemaURIStringsForFullValidation
      try {
        val sw = new java.io.StringWriter()
        val xml = new XMLTextInfosetOutputter(sw)
        resultState.infoset.visit(xml)
        Validator.validateXMLSources(schemaURIStrings, sw.toString, this)
      } catch {
        //
        // Some SAX Parse errors are thrown even if you specify an error handler to the
        // validator.
        //
        // So we also need this catch
        //
        case e: SAXException =>
          resultState.validationErrorNoContext(e)
      }
    }
  }

  override def warning(spe: SAXParseException): Unit = {
    resultState.validationErrorNoContext(spe)
  }
  override def error(spe: SAXParseException): Unit = {
    resultState.validationErrorNoContext(spe)
  }
  override def fatalError(spe: SAXParseException): Unit = {
    resultState.validationErrorNoContext(spe)
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

  private def encodingInfo = if (maybeEncodingInfo.isDefined) maybeEncodingInfo.get else dp.ssrd.elementRuntimeData.encodingInfo

  def summaryEncoding = encodingInfo.summaryEncoding

  override def isScannable = encodingInfo.isScannable
  override def encodingName = {
    Assert.invariant(encodingInfo.isKnownEncoding)
    // we're not supporting runtime-calculated encodings yet so not
    // capturing that information (what the actual runtime-value of encoding was
    encodingInfo.knownEncodingName
  }
}
