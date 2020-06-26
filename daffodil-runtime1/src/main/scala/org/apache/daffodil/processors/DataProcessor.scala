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

package org.apache.daffodil.processors

import java.io.File
import java.io.ObjectOutputStream
import java.nio.channels.Channels
import java.nio.CharBuffer
import java.nio.file.Files
import java.nio.LongBuffer
import java.util.zip.GZIPOutputStream

import org.xml.sax.ErrorHandler
import org.xml.sax.SAXException
import org.xml.sax.SAXParseException
import org.apache.daffodil.Implicits._

import scala.collection.immutable.Queue; object INoWarn4 { ImplicitsSuppressUnusedImportWarning() }
import org.apache.daffodil.equality._; object EqualityNoWarn3 { EqualitySuppressUnusedImportWarning() }
import org.apache.daffodil.api.WithDiagnostics
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.dsom._
import org.apache.daffodil.ExecutionMode
import org.apache.daffodil.api.DFDL
import org.apache.daffodil.api.WithDiagnostics
import org.apache.daffodil.util.Validator
import org.apache.daffodil.api.ValidationMode
import org.apache.daffodil.externalvars.ExternalVariablesLoader
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.util.Logging
import org.apache.daffodil.debugger.Debugger
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.infoset.InfosetInputter
import org.apache.daffodil.processors.unparsers.UnparseError
import org.apache.daffodil.oolag.ErrorAlreadyHandled
import org.apache.daffodil.events.MultipleEventHandler
import org.apache.daffodil.io.DirectOrBufferedDataOutputStream
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.io.BitOrderChangeException
import org.apache.daffodil.io.FileIOException
import org.apache.daffodil.infoset._
import org.apache.daffodil.processors.parsers.ParseError
import org.apache.daffodil.processors.parsers.Parser
import org.apache.daffodil.processors.parsers.PState
import org.apache.daffodil.exceptions.UnsuppressableException
import org.apache.daffodil.dsom.TunableLimitExceededError
import org.apache.daffodil.api.DaffodilTunables
import java.io.IOException
import org.apache.daffodil.util.Misc

/**
 * Implementation mixin - provides simple helper methods
 */
trait WithDiagnosticsImpl extends WithDiagnostics {

  //  final lazy val hasDiagnostics = {
  //    getDiagnostics.size > 0
  //  }
}

class InvalidUsageException(msg: String, cause: Throwable = null) extends Exception(msg, cause)

object DataProcessor {

  /**
   * This is the DataProcessor constructed from a saved processor.
   *
   * It enables us to implement restrictions on what you can/cannot do with a reloaded
   * processor versus an original one.
   *
   * When we create one of these, it will have default values for everything
   * settable like debuggers, debug mode.
   *
   * Note that this does preserve the externalVars and validationMode. That is because
   * those may be needed by serializations other than our own save/reload (e.g., Apache Spark which
   * serializes to move things for remote execution).
   *
   * Hence, we're depending on the save method to explicitly reset validationMode and
   * externalVars to initial values.
   */
  private class SerializableDataProcessor(
    val data: SchemaSetRuntimeData,
    tunable: DaffodilTunables,
    externalVars: Queue[Binding], // must be explicitly set to empty by save method
    validationModeArg: ValidationMode.Type) // must be explicitly set from Full to Limited by save method.
    extends DataProcessor(data, tunable, externalVars, validationModeArg) {

    override def withValidationMode(mode: ValidationMode.Type): DataProcessor = {
      if (mode == ValidationMode.Full) {
        throw new InvalidUsageException("'Full' validation not allowed when using a restored parser.")
      }
      super.withValidationMode(mode)
    }

    @deprecated("Use withValidationMode.", "2.6.0")
    override def setValidationMode(mode: ValidationMode.Type): Unit = {
      if (mode == ValidationMode.Full) {
        throw new InvalidUsageException("'Full' validation not allowed when using a restored parser.")
      }
      validationMode = mode
    }
  }
}

/**
 * The very last aspects of compilation, and the start of the
 * back-end runtime.
 */
class DataProcessor private (
  //
  // Once the deprecated API goes away, these var can become val.
  //
  var ssrd: SchemaSetRuntimeData,
  var tunables: DaffodilTunables, // Compiler-set tunables
  var variableMap: VariableMap,
  //
  // The below do not need to be transient
  // because this object itself isn't serialized. A SerializableDataProcessor is.
  // The values these will have (since this is a base class) are the correct default values that we want
  // back when the object is re-initialized.
  //
  protected var areDebugging : Boolean,
  protected var optDebugger : Option[Debugger],
  var validationMode: ValidationMode.Type,
  private var externalVars: Queue[Binding])
  extends DFDL.DataProcessor with Logging
  with HasSetDebugger
  with Serializable
  with MultipleEventHandler {

  import DataProcessor.SerializableDataProcessor

  /**
   * In order to make this serializable, without serializing the unwanted current state of
   * debugger, external var settings, etc. we replace, at serialization time, this object
   * with a [[SerializableDataProcessor]] which is a private derived class that
   * sets all these troublesome var slots back to the default values.
   *
   * But note: there is serialization for us to save/reload, and there is serialization
   * in other contexts like Apache Spark, which may serialize objects without notifying us.
   *
   * So we preserve everything that something like Spark might need preserved (validation modes, external vars)
   * and reinitialize things that are *always* reinitialized e.g., debugger, areDebugging.
   *
   * That means when we save for reloading, we must explicitly clobber validationMode and externalVars to
   * initialized values.
   *
   * @throws java.io.ObjectStreamException
   * @return the serializable object
   */
  @throws(classOf[java.io.ObjectStreamException])
  private def writeReplace() : Object =
    new SerializableDataProcessor(ssrd, tunables, externalVars, validationMode)

  /**
   * The compilerExternalVars argument supports the deprecated feature to assign external var bindings
   * on the compiler object.
   *
   * These are just incorporated into the initial variable map of the data processor.
   */

  def this(
    ssrd: SchemaSetRuntimeData,
    tunables:DaffodilTunables,
    compilerExternalVars: Queue[Binding] = Queue.empty,
    validationMode: ValidationMode.Type = ValidationMode.Off) =
    this(ssrd, tunables, ExternalVariablesLoader.loadVariables(compilerExternalVars, ssrd, ssrd.originalVariables),
      false, None, validationMode, compilerExternalVars)

  private def copy(
    ssrd: SchemaSetRuntimeData = ssrd,
    tunables: DaffodilTunables = tunables,
    areDebugging : Boolean = areDebugging,
    optDebugger : Option[Debugger] = optDebugger,
    validationMode: ValidationMode.Type = validationMode,
    variableMap : VariableMap = variableMap.copy,
    externalVars: Queue[Binding] = externalVars) =
    new DataProcessor(ssrd, tunables, variableMap, areDebugging, optDebugger, validationMode,  externalVars)

  // This thread local state is used by the PState when it needs buffers for
  // regex matching. This cannot be in PState because a PState does not last
  // beyond a single parse, but we want to share this among different parses to
  // avoid large memory allocations. The alternative is to use a ThreadLocal
  // companion object, but that would have not access to tunables, so one could
  // not configure the size of the regex match buffers.
  @transient lazy val regexMatchState = new ThreadLocal[(CharBuffer, LongBuffer)] {
    override def initialValue = {
      val cb = CharBuffer.allocate(tunables.maximumRegexMatchLengthInCharacters)
      val lb = LongBuffer.allocate(tunables.maximumRegexMatchLengthInCharacters)
      (cb, lb)
    }
  }

  /**
   * Returns a data processor with the same state.
   */
  override def clone(): DataProcessor = copy()

  /**
   * Returns a data processor with all the same state, but the validation mode changed to that of the argument.
   *
   * Note that the default validation mode is "off", that is, no validation is performed.
   */
  @deprecated("Use withValidationMode.", "2.6.0")
  def setValidationMode(mode: ValidationMode.Type): Unit = { validationMode = mode }

  def withValidationMode(mode:ValidationMode.Type): DataProcessor = copy(validationMode = mode)

  // TODO Deprecate and replace usages with just tunables.
  def getTunables: DaffodilTunables = tunables

  def debugger = {
    Assert.invariant(areDebugging)
    optDebugger.get
  }

  @deprecated("Use withDebugger.", "2.6.0")
  def setDebugger(dbg: AnyRef): Unit = {
    val optDbg = if (dbg eq null) None else Some(dbg.asInstanceOf[Debugger])
    optDebugger = optDbg
  }

  def withDebugger(dbg:AnyRef) = {
    val optDbg = if (dbg eq null) None else Some(dbg.asInstanceOf[Debugger])
    copy(optDebugger = optDbg)
  }

  @deprecated("Use withDebugging.", "2.6.0")
  def setDebugging(flag: Boolean): Unit = {
    areDebugging = flag
    tunables = tunables.setTunable("allowExternalPathExpressions", flag.toString)
  }

  def withDebugging(flag: Boolean): DataProcessor = {
    val newTunables = tunables.setTunable("allowExternalPathExpressions", flag.toString)
    copy(areDebugging = flag, tunables = newTunables)
  }

  private def loadExternalVariables(extVars: Map[String, String]): Queue[Binding] = {
    val bindings = ExternalVariablesLoader.mapToBindings(extVars)
    val newVars = externalVars ++ bindings
    ExternalVariablesLoader.loadVariables(bindings, ssrd, variableMap)
    newVars
  }

  private def loadExternalVariables(extVars: File): Queue[Binding]  = {
    val bindings = ExternalVariablesLoader.fileToBindings(extVars)
    val newVars = externalVars ++ bindings
    ExternalVariablesLoader.loadVariables(bindings, ssrd, variableMap)
    newVars
  }

  private def loadExternalVariables(bindings: Seq[Binding]): Queue[Binding]  = {
    val newVars = externalVars ++ bindings
    ExternalVariablesLoader.loadVariables(bindings, ssrd, variableMap)
    newVars
  }

  @deprecated("Use withExternalVariables.", "2.6.0")
  def setExternalVariables(extVars: Map[String, String]): Unit = {
    val newBindings = loadExternalVariables(extVars)
    externalVars = newBindings
  }

  def withExternalVariables(extVars: Map[String, String]): DataProcessor = {
    val newBindings = loadExternalVariables(extVars)
    copy(externalVars = newBindings)
  }

  @deprecated("Use withExternalVariables.", "2.6.0")
  def setExternalVariables(extVars: File): Unit = {
    val  newBindings = loadExternalVariables(extVars)
    externalVars = newBindings
  }

  def withExternalVariables(extVars: File): DataProcessor = {
    val newBindings = loadExternalVariables(extVars)
    copy(externalVars = newBindings)
  }

  /**
   * Note that tunables is not used. So this method is equivalent to
   * the other similar method that doesn't take that parameter.
   *
   * @param extVars File containing configuration with external variable bindings in it.
   * @param tunable This is ignored.
   */
  @deprecated("Use withExternalVariables.", "2.6.0")
  def setExternalVariables(extVars: File, tunable: DaffodilTunables): Unit = {
    val newBindings = loadExternalVariables(extVars)
    externalVars = newBindings
  }

  @deprecated("Use withExternalVariables.", "2.6.0")
  def setExternalVariables(extVars: Seq[Binding]): Unit = {
    val newBindings = loadExternalVariables(extVars)
    externalVars = newBindings
  }

  def withExternalVariables(extVars: Seq[Binding]): DataProcessor = {
    val newBindings = loadExternalVariables(extVars)
    copy(externalVars = newBindings)
  }

  @deprecated("Use withTunables.", "2.6.0")
  def setTunable(tunable: String, value: String): Unit = tunables = tunables.setTunable(tunable, value)

  def withTunable(tunable: String, value: String): DataProcessor = copy(tunables = tunables.setTunable(tunable, value))

  @deprecated("Use withTunables.", "2.6.0")
  def setTunables(tunablesArg: Map[String, String]): Unit = tunables = tunables.setTunables(tunablesArg)

  def withTunables(tunablesArg: Map[String, String]): DataProcessor = copy(tunables = tunables.setTunables(tunablesArg))

  override def isError = false // really there is no compiling at all currently, so there can be no errors.

  override def getDiagnostics = ssrd.diagnostics

  def save(output: DFDL.Output): Unit = {

    externalVars.foreach{ ev =>
      log(LogLevel.Warning, "External variable %s is not saved as part of saved DFDL processor.", ev.varQName)
    }

    val oos = new ObjectOutputStream(new GZIPOutputStream(Channels.newOutputStream(output)))

    //
    // Make a copy of this object, so that our state mods below don't side-effect the user's object.
    // Saving shouldn't have side-effects on the state of the object.
    //
    val dpToSave = this.copy()
    //
    // Note that the serialization system *does* preserve these two settings. This is for general serialization
    // that may be required by other software (e.g., Apache Spark)
    //
    // But for our save/reload purposes, we don't want them preserved.
    //
    dpToSave.externalVars = Queue.empty[Binding] // explicitly set these to empty so restored processor won't have them.
    dpToSave.validationMode = ValidationMode.Off // explicitly turn off, so restored processor won't be validating.

    try {
      //
      // The serialization system encapsulates exceptions or any throw really
      // inside an IOException.
      //
      // We catch and undo this, and throw the original exception,
      // which won't give us a breakpoint at the right place, but at least
      // it will give us the right exception, and if it is a regular exception that
      // contains a stack copy, a backtrace.
      //
      // Note that getting an SDE here is a bug, i.e., a mistake in Daffodil's code base.
      //
      // SDEs should all be detected before we start saving the processor definition, at the very latest by
      // calling preSerialization methods explicitly first so any final computations that only
      // get done before serialization do get done.
      //
      oos.writeObject(dpToSave)
    } catch {
      case e: IOException => {
        val Some(cause) = Misc.getSomeCause(e)
        throw cause
      }
    }
    oos.close()
  }

  /**
   * Here begins the parser runtime. Compiler-oriented mechanisms (OOLAG etc.) aren't used in the
   * runtime. Instead we deal with success and failure statuses.
   */
  def parse(input: InputSourceDataInputStream, output: InfosetOutputter): DFDL.ParseResult = {
    Assert.usage(!this.isError)

    val rootERD = ssrd.elementRuntimeData
    val initialState = PState.createInitialPState(rootERD, input, output, this)
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
        state.output.setBlobPaths(state.blobPaths)
      } else {
        // failed, so delete all blobs that were created
        state.blobPaths.foreach { path =>
          Files.delete(path)
        }
        // ensure the blob paths are empty in case of outputter reuse
        state.output.setBlobPaths(Seq.empty)
      }
      val s = state
      val dp = s.dataProc
      val ssrdParser = ssrd.parser
      if (dp.isDefined) dp.value.fini(ssrdParser)

      pr
    }
  }

  private def doParse(p: Parser, state: PState): Unit = {
    var optThrown: Maybe[Throwable] = None
    try {
      try {
        Assert.usageErrorUnless(state.dataInputStreamIsValid, "Attempted to use an invalid input source. This can happen due to our position in the input source not being properly reset after failed parse could not backtrack to its original position")

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
      } catch {
        //We will actually be handling all errors in the outer loop
        //However, there is a chance that our finally block will itself throw.
        //In such a case, it is useful to include the original error.
        case e: Throwable => {
          optThrown = Some(e)
          throw e
        }
      } finally { state.verifyFinalState(optThrown) }

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
        state.dataInputStream.inputSource.setInvalid
        state.setFailed(sde)
      }
      case rsde: RuntimeSchemaDefinitionError => {
        state.dataInputStream.inputSource.setInvalid
        state.setFailed(rsde)
      }
      case e: ErrorAlreadyHandled => {
        state.setFailed(e.th)
      }
      case e: TunableLimitExceededError => {
        state.setFailed(e)
      }
      case us: UnsuppressableException => throw us
      case x: Throwable => {
        val sw = new java.io.StringWriter()
        val pw = new java.io.PrintWriter(sw)
        x.printStackTrace(pw)
        Assert.invariantFailed("Runtime.scala - Leaked exception: " + x + "\n" + sw.toString)
      }
    }

  }

  def unparse(inputter: InfosetInputter, output: DFDL.Output): DFDL.UnparseResult = {
    Assert.usage(!this.isError)
    val outStream = java.nio.channels.Channels.newOutputStream(output)
    unparse(inputter, outStream)
  }

  def unparse(inputter: InfosetInputter, outStream: java.io.OutputStream) = {
    val out = DirectOrBufferedDataOutputStream(
      outStream,
      null, // null means no other stream created this one.
      isLayer = false,
      tunables.outputStreamChunkSizeInBytes,
      tunables.maxByteArrayOutputStreamBufferSizeInBytes,
      tunables.tempFilePath)

    inputter.initialize(ssrd.elementRuntimeData, getTunables())
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
    } finally {
      unparserState.dataOutputStream.cleanUp
    }
    res
  }

  private def doUnparse(state: UState): Unit = {
    val rootUnparser = ssrd.unparser

    Assert.invariant {
      // rootERD is pushed when the state is constructed and initialized.
      val mtrd = state.maybeTopTRD()
      mtrd.isDefined &&
        (mtrd.get eq rootUnparser.context)
    }
    rootUnparser.unparse1(state)
    state.popTRD(rootUnparser.context.asInstanceOf[TermRuntimeData])

    // Restore invariant that there is always a processor.
    // Later when suspensions get evaluated, there are still times when
    // properties are read, and those require a processor to be set
    // for model groups so that the RuntimeData can be found.
    //
    // For elements, the infoset node enables you to find the
    // ElementRuntimeData, but for model groups there is no infoset node,
    // so we need the Processor, which has a context which is the RD.
    //
    state.setProcessor(rootUnparser)

    // Verify that all stacks are empty
    Assert.invariant(state.arrayIndexStack.length == 1)
    Assert.invariant(state.groupIndexStack.length == 1)
    Assert.invariant(state.childIndexStack.length == 1)
    Assert.invariant(state.currentInfosetNodeMaybe.isEmpty)
    Assert.invariant(state.escapeSchemeEVCache.isEmpty)
    Assert.invariant(state.maybeTopTRD().isEmpty) // dynamic TRD stack is empty
    Assert.invariant(!state.withinHiddenNest) //ensure we are not in hidden nest

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
      case fio: FileIOException =>
        state.SDE(fio)
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
    if (dp.validationMode == ValidationMode.Full) {
      val schemaURIStrings = resultState.infoset.asInstanceOf[InfosetElement].runtimeData.schemaURIStringsForFullValidation
      try {
        val bos = new java.io.ByteArrayOutputStream()
        val xml = new XMLTextInfosetOutputter(bos, false)
        resultState.infoset.visit(xml)
        val bis = new java.io.ByteArrayInputStream(bos.toByteArray)
        Validator.validateXMLSources(schemaURIStrings, bis, this)
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
