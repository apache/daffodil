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

package org.apache.daffodil.runtime1.processors

import java.io.File
import java.io.IOException
import java.io.ObjectOutputStream
import java.net.URI
import java.nio.CharBuffer
import java.nio.LongBuffer
import java.nio.channels.Channels
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.zip.GZIPOutputStream

import org.apache.daffodil.api
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.iapi.DataLocation
import org.apache.daffodil.lib.iapi.Diagnostic
import org.apache.daffodil.lib.validation.DaffodilLimitedValidator
import org.apache.daffodil.lib.validation.XercesValidator
import org.apache.daffodil.runtime1.layers.LayerFatalException

object INoWarn4 {
  ImplicitsSuppressUnusedImportWarning()
}
import org.apache.daffodil.lib.equality._
import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.lib.iapi.WithDiagnostics
import org.apache.daffodil.runtime1.dsom._
import org.apache.daffodil.runtime1.iapi.DFDL
object EqualityNoWarn3 {
  EqualitySuppressUnusedImportWarning()
}
import org.apache.daffodil.io.BitOrderChangeException
import org.apache.daffodil.io.FileIOException
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.UnsuppressableException
import org.apache.daffodil.lib.externalvars.Binding
import org.apache.daffodil.lib.oolag.ErrorAlreadyHandled
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.runtime1.events.MultipleEventHandler
import org.apache.daffodil.runtime1.externalvars.ExternalVariablesLoader
import org.apache.daffodil.runtime1.infoset.DIElement
import org.apache.daffodil.runtime1.infoset.InfosetException
import org.apache.daffodil.runtime1.infoset.InfosetInputter
import org.apache.daffodil.runtime1.infoset.TeeInfosetOutputter
import org.apache.daffodil.runtime1.infoset.XMLTextInfosetOutputter
import org.apache.daffodil.runtime1.processors.parsers.PState
import org.apache.daffodil.runtime1.processors.parsers.ParseError
import org.apache.daffodil.runtime1.processors.parsers.Parser
import org.apache.daffodil.runtime1.processors.unparsers.UState
import org.apache.daffodil.runtime1.processors.unparsers.UnparseError

/**
 * Implementation mixin - provides simple helper methods
 */
trait WithDiagnosticsImpl extends WithDiagnostics {

  //  final lazy val hasDiagnostics = {
  //    getDiagnostics.size > 0
  //  }
}

class InvalidUsageException(msg: String, cause: Throwable = null)
  extends api.exceptions.InvalidUsageException(msg, cause)

object DataProcessor {

  /**
   * This is the SerializableDataProcessor constructed from a saved processor.
   *
   * It enables us to implement restrictions on what you can/cannot do with a reloaded
   * processor versus an original one.
   *
   * When we reload a processor, we want it to have default values for everything settable
   * like debug mode and debugger.
   *
   * Note that this class does preserve variableMap. That is because
   * serializations other than our own save/reload may need such settings (e.g., Apache Spark
   * which serializes to move objects for remote execution).
   *
   * Hence, we're depending on the save method to explicitly reset them to default values.
   */
  private class SerializableDataProcessor(
    ssrd: SchemaSetRuntimeData,
    tunables: DaffodilTunables,
    variableMap: VariableMap // must be explicitly reset by save method
  ) extends DataProcessor(ssrd, tunables, variableMap) {

    override def getMainSchemaURIForFullValidation: URI = {
      throw new InvalidUsageException(
        "'Full' validation not allowed when using a restored parser."
      )
    }

    override def withValidator(validator: api.validation.Validator): api.DataProcessor = {
      if (validator.isInstanceOf[XercesValidator]) {
        throw new InvalidUsageException(
          "'Full' validation not allowed when using a restored parser."
        )
      }
      super.withValidator(validator)
    }
  }

}

/**
 * The very last aspects of compilation, and the start of the
 * back-end runtime.
 */
class DataProcessor(
  val ssrd: SchemaSetRuntimeData,
  val tunables: DaffodilTunables, // Compiler-set tunables
  val variableMap: VariableMap,
  // The Validator API requires this to be thread-safe so this is safe to share among different
  // DataProcessors
  val validator: api.validation.Validator = null,
  protected val areDebugging: Boolean = false,
  protected val optDebugger: Option[api.debugger.Debugger] = None,
  protected val diagnostics: Seq[Diagnostic] = Seq.empty
) extends DFDL.DataProcessor
  with Serializable
  with MultipleEventHandler {

  import DataProcessor.SerializableDataProcessor

  override def getMainSchemaURIForFullValidation: URI = ssrd.mainSchemaUriForFullValidation

  /**
   * In order to make this serializable, without serializing the unwanted current state of
   * debugging mode, debugger, etc. we replace, at serialization time, this
   * object with a [[SerializableDataProcessor]] which is a private derived class that
   * sets all these troublesome slots back to the default values.
   *
   * But note: there is serialization for us to save/reload, and there is serialization
   * in other contexts like Apache Spark, which may serialize objects without notifying us.
   *
   * So we preserve everything that something like Spark might need preserved
   * and reinitialize things that are *always* reinitialized e.g., debugger, areDebugging.
   *
   * @throws java.io.ObjectStreamException Must be part of writeReplace's API
   * @return the serializable object
   */
  @throws(classOf[java.io.ObjectStreamException])
  private def writeReplace(): Object =
    new SerializableDataProcessor(ssrd, tunables, variableMap.copy())

  def copy(
    ssrd: SchemaSetRuntimeData = ssrd,
    tunables: DaffodilTunables = tunables,
    variableMap: VariableMap = variableMap.copy(),
    validator: api.validation.Validator = validator,
    areDebugging: Boolean = areDebugging,
    optDebugger: Option[api.debugger.Debugger] = optDebugger,
    diagnostics: Seq[Diagnostic] = diagnostics
  ) = new DataProcessor(
    ssrd,
    tunables,
    variableMap,
    validator,
    areDebugging,
    optDebugger,
    diagnostics
  )

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

  override def withValidator(validator: api.validation.Validator) = {
    copy(validator = validator).asInstanceOf[api.DataProcessor]
  }

  def debugger = {
    Assert.invariant(areDebugging)
    optDebugger.get
  }

  override def withDebugger(dbg: api.debugger.Debugger): DataProcessor = {
    val optDbg = if (dbg eq null) None else Some(dbg)
    copy(areDebugging = optDbg.isDefined, optDebugger = optDbg)
  }

  def withDebugging(flag: Boolean): DataProcessor = {
    val newTunables = tunables.withTunable("allowExternalPathExpressions", flag.toString)
    copy(areDebugging = flag, tunables = newTunables)
  }

  def withExternalVariables(extVars: java.util.Map[String, String]): DataProcessor = {
    val bindings = ExternalVariablesLoader.mapToBindings(extVars)
    val newVariableMap =
      ExternalVariablesLoader.loadVariables(bindings, ssrd, variableMap.copy())
    copy(variableMap = newVariableMap)
  }

  def withExternalVariables(extVars: File): DataProcessor = {
    val bindings = ExternalVariablesLoader.fileToBindings(extVars)
    val newVariableMap =
      ExternalVariablesLoader.loadVariables(bindings, ssrd, variableMap.copy())
    copy(variableMap = newVariableMap)
  }

  def withExternalVariables(bindings: Seq[Binding]): DataProcessor = {
    val newVariableMap =
      ExternalVariablesLoader.loadVariables(bindings, ssrd, variableMap.copy())
    copy(variableMap = newVariableMap)
  }

  def withTunable(tunable: String, value: String): DataProcessor = {
    val newTunables = tunables.withTunable(tunable, value)
    copy(tunables = newTunables)
  }

  def withTunables(tunablesArg: Map[String, String]): DataProcessor = {
    val newTunables = tunables.withTunables(tunablesArg)
    copy(tunables = newTunables)
  }

  override def isError = false

  override def getDiagnostics = diagnostics.asInstanceOf[Seq[api.Diagnostic]]

  override def newXMLReaderInstance: DFDL.DaffodilParseXMLReader = {
    val xrdr = new DaffodilParseXMLReader(this)
    xrdr
  }

  override def newContentHandlerInstance(
    output: DFDL.Output
  ): DFDL.DaffodilUnparseContentHandler =
    new DaffodilUnparseContentHandlerImpl(this, output)

  def save(output: DFDL.Output): Unit = {

    val os = Channels.newOutputStream(output)

    // write a null-terminated UTF-8 string as a simple version identifier
    val headerString = "DAFFODIL " + Misc.getDaffodilVersion + "\u0000"
    os.write(headerString.getBytes(StandardCharsets.UTF_8))

    // serialize and compress the data processor to the outputstream
    val oos = new ObjectOutputStream(new GZIPOutputStream(os))

    //
    // Make a copy of this object so that we can make its saved state
    // different than its original state.  Note other software like
    // Apache Spark may require variableMap to be
    // preserved.  But for our save/reload purposes, we want to reset
    // them back to their original values.
    //
    val dpToSave = this.copy(
      // reset to original variables defined in schema
      variableMap = ssrd.originalVariables,
      validator = null,
      // don't save any warnings that were generated
      diagnostics = Seq.empty
    )

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

  def walkMetadata(handler: api.MetadataHandler): Unit = {
    val walker = new MetadataWalker(this)
    walker.walk(handler)
  }

  /**
   * Here begins the parser runtime. Compiler-oriented mechanisms (OOLAG etc.) aren't used in the
   * runtime. Instead we deal with success and failure statuses.
   */
  def parse(
    input: InputSourceDataInputStream,
    output: api.infoset.InfosetOutputter
  ): DFDL.ParseResult = {
    checkNotError()
    // If full validation is enabled, tee all the infoset events to a second
    // infoset outputter that writes the infoset to a byte array, and then
    // we'll validate that byte array upon a successful parse.
    //
    // TODO: ideally we could create a validator that validates using only SAX
    // events from a ContentHandler. Then we could just validate as parse
    // events are created rather than writing the entire infoset in memory and
    // then validating at the end of the parse. See DAFFODIL-2386
    //
    val (outputter: api.infoset.InfosetOutputter, maybeValidationBytes) = {
      validator match {
        case DaffodilLimitedValidator | null =>
          (output, Nope)
        case _ =>
          val bos = new java.io.ByteArrayOutputStream()
          val xmlOutputter = new XMLTextInfosetOutputter(bos, false)
          val teeOutputter = new TeeInfosetOutputter(output, xmlOutputter)
          // copy the blob attributes from the users outputter to the tee infoset outputter
          // since Daffodil will now use that to get blob attributes
          teeOutputter.setBlobAttributes(
            output.getBlobDirectory,
            output.getBlobPrefix,
            output.getBlobSuffix
          )
          (teeOutputter, One(bos))
      }
    }

    val rootERD = ssrd.elementRuntimeData
    val state = PState.createInitialPState(rootERD, input, outputter, this, areDebugging)

    if (areDebugging) {
      Assert.invariant(optDebugger.isDefined)
      addEventHandler(debugger)
      state.notifyDebugging(true)
    }
    state.dataProc.get.init(state, ssrd.parser)
    doParse(ssrd.parser, state)

    val pr = if (state.processorStatus == Success) {
      // validate infoset, errors are added to the PState diagnostics
      maybeValidationBytes.toOption.foreach { bytes =>
        val bis = new java.io.ByteArrayInputStream(bytes.toByteArray)
        validator.validateXML(bis, state)
      }
      // copy the blob paths we created to the users infoset outputter
      output.setBlobPaths(state.blobPaths)
      new ParseResult(state)
    } else {
      // failed, so delete all blobs that were created
      state.blobPaths.foreach { path =>
        Files.delete(path)
      }
      // ensure the blob paths on the users infoset outputter are empty in case of reuse
      output.setBlobPaths(Seq.empty)
      new ParseResult(state)
    }
    val s = state
    val dp = s.dataProc
    val ssrdParser = ssrd.parser
    if (dp.isDefined) dp.value.fini(ssrdParser)

    pr
  }

  private def doParse(p: Parser, state: PState): Unit = {
    var optThrown: Maybe[Throwable] = None
    try {
      try {
        Assert.usage(
          state.dataInputStreamIsValid,
          "Attempted to use an invalid input source. This can happen due to our position in the input source not being properly reset after failed parse could not backtrack to its original position"
        )

        // Force the evaluation of any defineVariable's with non-constant default
        // value expressions
        state.initializeVariables()

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

        if (state.processorStatus == Success) {
          // At this point all infoset nodes have been set final, all infoset
          // walker blocks released, and all elements walked. The one exception
          // is the root node has not been set final because isFinal is handled
          // by the sequence parser and there is no sequence around the root
          // node. So mark it final and do one last walk to end the document.
          state.infoset.child(0).isFinal = true
          state.walker.walk(lastWalk = true)
          Assert.invariant(state.walker.isFinished)
        }
      } catch {
        // We will actually be handling all errors in the outer loop
        // However, there is a chance that our finally block will itself throw.
        // In such a case, it is useful to include the original error.
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
        Assert.invariantFailed(
          "ParseError caught. ParseErrors should be returned as failed status, not thrown. Fix please."
        )
      }
      case procErr: ProcessingError => {
        val x = procErr
        state.setFailed(x.toParseError)
      }
      case sde: SchemaDefinitionError => {
        // A SDE was detected at runtime (perhaps due to a runtime-valued property like byteOrder or encoding)
        // These are fatal, and there's no notion of backtracking them, so they propagate to top level
        // here.
        state.dataInputStream.inputSource.setInvalid()
        state.setFailed(sde)
      }
      case sdefw: SchemaDefinitionErrorFromWarning => {
        state.dataInputStream.inputSource.setInvalid()
        state.setFailed(sdefw)
      }
      case e: ErrorAlreadyHandled => {
        state.setFailed(e.th)
      }
      case e: TunableLimitExceededError => {
        state.setFailed(e)
      }
      case us: UnsuppressableException => throw us
      case lre: LayerFatalException => throw lre
      case x: Throwable => {
        val sw = new java.io.StringWriter()
        val pw = new java.io.PrintWriter(sw)
        x.printStackTrace(pw)
        Assert.invariantFailed("Runtime.scala - Leaked exception: " + x + "\n" + sw.toString)
      }
    }

  }

  def unparse(
    actualInputter: api.infoset.InfosetInputter,
    output: DFDL.Output
  ): UnparseResult = {
    checkNotError()
    val outStream = java.nio.channels.Channels.newOutputStream(output)
    unparse(actualInputter, outStream)
  }

  def unparse(actualInputter: api.infoset.InfosetInputter, outStream: java.io.OutputStream) = {
    val inputter = new InfosetInputter(actualInputter)
    inputter.initialize(ssrd.elementRuntimeData, tunables)
    val unparserState =
      UState.createInitialUState(outStream, this, inputter, areDebugging)
    val res =
      try {
        if (areDebugging) {
          Assert.invariant(optDebugger.isDefined)
          addEventHandler(debugger)
          unparserState.notifyDebugging(true)
        }
        unparserState.dataProc.get.init(unparserState, ssrd.unparser)
        unparserState.getDataOutputStream.setPriorBitOrder(
          ssrd.elementRuntimeData.defaultBitOrder
        )
        doUnparse(unparserState)
        unparserState.evalSuspensions(isFinal = true)
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
        case sdefw: SchemaDefinitionErrorFromWarning => {
          unparserState.setFailed(sdefw)
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
        unparserState.getDataOutputStream.cleanUp()
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

    // Force the evaluation of any defineVariable's with non-constant default
    // value expressions
    state.initializeVariables()

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
    Assert.invariant(state.arrayIterationIndexStack.length == 1)
    Assert.invariant(state.occursIndexStack.length == 1)
    Assert.invariant(state.groupIndexStack.length == 1)
    Assert.invariant(state.childIndexStack.length == 1)
    Assert.invariant(state.currentInfosetNodeMaybe.isEmpty)
    Assert.invariant(state.escapeSchemeEVCache.isEmpty)
    Assert.invariant(state.maybeTopTRD().isEmpty) // dynamic TRD stack is empty
    Assert.invariant(!state.withinHiddenNest) // ensure we are not in hidden nest

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
    Assert.invariant(!state.getDataOutputStream.isFinished)
    try {
      state.getDataOutputStream.setFinished(state)
    } catch {
      case boc: BitOrderChangeException =>
        state.SDE(boc)
      case fio: FileIOException =>
        state.SDE(fio)
    }

    val ev = state.advanceMaybe
    if (ev.isDefined) {
      UnparseError(
        Nope,
        One(state.currentLocation),
        "Expected no remaining events, but received %s.",
        ev.get
      )
    }
  }

  override def parse(
    input: api.InputSourceDataInputStream,
    output: api.infoset.InfosetOutputter
  ): DFDL.ParseResult = {
    parse(input.asInstanceOf[InputSourceDataInputStream], output)
  }
}

class ParseResult(
  override val resultState: PState
) extends DFDL.ParseResult
  with WithDiagnosticsImpl {
  override def location(): DataLocation = resultState.currentLocation
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

  private def encodingInfo = if (maybeEncodingInfo.isDefined) maybeEncodingInfo.get
  else dp.ssrd.elementRuntimeData.encodingInfo

  override def isScannable = encodingInfo.isScannable

  override def encodingName = {
    Assert.invariant(encodingInfo.isKnownEncoding)
    // we're not supporting runtime-calculated encodings yet so not
    // capturing that information (what the actual runtime-value of encoding was
    encodingInfo.knownEncodingName
  }
}
