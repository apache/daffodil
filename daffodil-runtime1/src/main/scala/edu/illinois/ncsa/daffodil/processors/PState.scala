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

import scala.xml.Node
import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.api.LocationInSchemaFile
import edu.illinois.ncsa.daffodil.dsom.DiagnosticImplMixin
import edu.illinois.ncsa.daffodil.dsom.RuntimeSchemaDefinitionError
import edu.illinois.ncsa.daffodil.dsom.RuntimeSchemaDefinitionWarning
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocatable
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.exceptions.SavesErrorsAndWarnings
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.api._
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.dsom.ValidationError
import edu.illinois.ncsa.daffodil.externalvars.ExternalVariablesLoader
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.dpath.DState
import edu.illinois.ncsa.daffodil.processors.unparsers.UnparseError
import edu.illinois.ncsa.daffodil.xml.GlobalQName
import java.io.StringReader
import org.apache.commons.io.input.ReaderInputStream
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import java.nio.charset.CharsetDecoder
import java.nio.charset.Charset
import scala.collection.mutable
import java.nio.charset.CharsetEncoder
import edu.illinois.ncsa.daffodil.io.ByteBufferDataInputStream
import edu.illinois.ncsa.daffodil.io.DataInputStream
import edu.illinois.ncsa.daffodil.io.DataStreamCommon
import edu.illinois.ncsa.daffodil.util.MStack
import edu.illinois.ncsa.daffodil.util.Pool
import edu.illinois.ncsa.daffodil.dpath.ParseMode
import edu.illinois.ncsa.daffodil.io.LocalBufferMixin
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.processors.dfa.DFAField
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParserBase

object MPState {
  class Mark {

    var arrayIndexStackMark: MStack.Mark = _
    var groupIndexStackMark: MStack.Mark = _
    var childIndexStackMark: MStack.Mark = _
    var occursBoundsStackMark: MStack.Mark = _
    var delimiterStackMark: MStack.Mark = _
    var currentEscapeScheme: Maybe[EscapeSchemeParserHelper] = Nope

    clear()

    def clear() {
      arrayIndexStackMark = MStack.nullMark
      groupIndexStackMark = MStack.nullMark
      childIndexStackMark = MStack.nullMark
      occursBoundsStackMark = MStack.nullMark
      delimiterStackMark = MStack.nullMark
      currentEscapeScheme = Nope
    }

    def setMarkFrom(mp: MPState) {
      arrayIndexStackMark = mp.arrayIndexStack.mark
      groupIndexStackMark = mp.groupIndexStack.mark
      childIndexStackMark = mp.childIndexStack.mark
      occursBoundsStackMark = mp.occursBoundsStack.mark
      delimiterStackMark = mp.delimiterStack.mark
      currentEscapeScheme = mp.currentEscapeScheme
    }
    def resetFromMark(mp: MPState) {
      mp.arrayIndexStack.reset(this.arrayIndexStackMark)
      mp.groupIndexStack.reset(this.groupIndexStackMark)
      mp.childIndexStack.reset(this.childIndexStackMark)
      mp.occursBoundsStack.reset(this.occursBoundsStackMark)
      mp.delimiterStack.reset(this.delimiterStackMark)
      mp.currentEscapeScheme = this.currentEscapeScheme
    }
  }
}

case class MPState() {

  val dstate: DState = new DState
  val arrayIndexStack = new MStack.OfLong
  arrayIndexStack.push(1L)
  def moveOverOneArrayIndexOnly() = arrayIndexStack.push(arrayIndexStack.pop + 1)
  def arrayPos = arrayIndexStack.top

  val groupIndexStack = new MStack.OfLong
  groupIndexStack.push(1L)
  def moveOverOneGroupIndexOnly() = groupIndexStack.push(groupIndexStack.pop + 1)
  def groupPos = groupIndexStack.top

  // TODO: it doesn't look anything is actually reading the value of childindex
  // stack. Can we get rid of it?
  val childIndexStack = new MStack.OfLong
  childIndexStack.push(1L)
  def moveOverOneElementChildOnly() = childIndexStack.push(childIndexStack.pop + 1)
  def childPos = childIndexStack.top

  val occursBoundsStack = new MStack.OfLong
  def updateBoundsHead(ob: Long) = {
    occursBoundsStack.pop()
    occursBoundsStack.push(ob)
  }
  def occursBounds = occursBoundsStack.top

  var currentEscapeScheme: Maybe[EscapeSchemeParserHelper] = Nope
  var currentDelimsCooked : Maybe[List[String]] = Nope
  var currentFieldDFA: Maybe[DFAField] = Nope
  var currentParser: Maybe[TextDelimitedParserBase] = Nope

  val delimiterStack = new MStack.Of[DelimiterStackNode]
  def pushDelimiters(node: DelimiterStackNode) = {
    Assert.usage(node != null)
    delimiterStack.push(node)
  }
  def popDelimiters() = {
    val res = delimiterStack.pop
    Assert.invariant(res != null)
    res
  }
  def localDelimiters = delimiterStack.top
  def remoteDelimiters = {
    val iter = delimiterStack.iterator // PERFORMANCE: Allocates an iterator object
    if (iter.hasNext) iter.next // skip top element. We want the "tail" of the stack
    iter
  }
  def getAllTerminatingMarkup = delimiterStack.iterator.flatMap {
    dsNode =>
      {
        Assert.invariant(dsNode != null)
        dsNode.getTerminatingMarkup
      }
  }.toArray

  def getAllDelimitersWithPos = delimiterStack.iterator.flatMap {
    dsNode =>
      {
        Assert.invariant(dsNode != null)
        dsNode.getDelimitersWithPos
      }
  }.toArray
}

/**
 * Trait mixed into the PState.Mark object class and the ParseOrUnparseState
 *
 * contains member functions for everything the debugger needs to be able to observe.
 */
trait StateForDebugger {
  def bytePos: Long
  def childPos: Long
  def groupPos: Long
  def currentLocation: DataLocation
  def arrayPos: Long
  def bitLimit0b: MaybeULong
  def discriminator: Boolean = false
}

case class TupleForDebugger(
  val bytePos: Long,
  val childPos: Long,
  val groupPos: Long,
  val currentLocation: DataLocation,
  val arrayPos: Long,
  val bitLimit0b: MaybeULong,
  override val discriminator: Boolean) extends StateForDebugger

/**
 * A parser takes a state, and returns an updated state
 *
 * The fact that there are side-effects/mutations on parts of the state
 * enables us to reuse low-level java primitives that mutate streams.
 *
 * The goal however, is to hide that fact so that the only places that have to
 * know are the places doing the mutation, and the places rolling them back
 * which should be isolated to the alternative parser, and repParsers, i.e.,
 * places where points-of-uncertainty are handled.
 */
abstract class ParseOrUnparseState(
  dstateArg: DState,
  var variableMap: VariableMap,
  var diagnostics: List[Diagnostic],
  var dataProc: DataProcessor) extends DFDL.State
  with StateForDebugger
  with ThrowsSDE with SavesErrorsAndWarnings
  with LocalBufferMixin {

  val dstate = {
    setMode(dstateArg)
    dstateArg
  }

  def setMode(dstate: DState): Unit

  def copyStateForDebugger = {
    TupleForDebugger(
      bytePos,
      childPos,
      groupPos,
      currentLocation,
      arrayPos,
      bitLimit0b,
      discriminator)
  }

  override def schemaFileLocation = getContext().schemaFileLocation

  def dataStream: DataStreamCommon

  def bitPos0b: Long
  def bitLimit0b: MaybeULong
  final def bytePos0b = bitPos0b >> 3
  final def bytePos1b = (bitPos0b >> 3) + 1
  final def bitPos1b = bitPos0b + 1
  final def bitLimit1b = if (bitLimit0b.isDefined) MaybeULong(bitLimit0b.get + 1) else MaybeULong.Nope
  final def whichBit0b = bitPos0b % 8

  // TODO: many off-by-one errors due to not keeping strong separation of
  // one-based and zero-based indexes.
  //
  // We could separate these with the type system.
  //
  // So implement a OneBasedBitPos and ZeroBasedBitPos value class with
  // operations that convert between them, allow adding & subtracting only
  // in sensible ways, etc.
  final def bitPos = bitPos0b
  final def bytePos = bytePos0b
  // def charPos: Long

  def groupPos: Long
  def arrayPos: Long
  def childPos: Long
  def occursBoundsStack: MStack.OfLong

  def hasInfoset: Boolean
  def infoset: InfosetItem

  def thisElement: InfosetElement

  def getContext(): ElementRuntimeData = {
    // threadCheck()
    val currentElement = infoset.asInstanceOf[InfosetElement]
    val res = currentElement.runtimeData
    res
  }

  /**
   * The User API sets the debugger and debug on/off flag on the DataProcessor object.
   * When a PState or UState is created by the DataProcessor, the DataProcessor
   * sets notifies the state object so that it can setup any debug-specific behaviors.
   */
  def notifyDebugging(flag: Boolean): Unit

  def SDE(str: String, args: Any*) = {
    ExecutionMode.requireRuntimeMode
    val ctxt = getContext()
    val rsde = new RuntimeSchemaDefinitionError(ctxt.schemaFileLocation, this, str, args: _*)
    ctxt.toss(rsde)
  }

  def SDEButContinue(str: String, args: Any*) = {
    ExecutionMode.requireRuntimeMode
    val ctxt = getContext()
    val rsde = new RuntimeSchemaDefinitionError(ctxt.schemaFileLocation, this, str, args: _*)
    diagnostics = rsde :: diagnostics
  }

  def SDW(str: String, args: Any*) = {
    ExecutionMode.requireRuntimeMode
    val ctxt = getContext()
    val rsdw = new RuntimeSchemaDefinitionWarning(ctxt.schemaFileLocation, this, str, args: _*)
    diagnostics = rsdw :: diagnostics
  }

  private val decoderCache = new mutable.HashMap[Charset, CharsetDecoder]()
  private val encoderCache = new mutable.HashMap[Charset, CharsetEncoder]()

  def getDecoder(charset: Charset): CharsetDecoder = {
    // threadCheck()
    var optDecoder = decoderCache.get(charset)
    if (optDecoder.isEmpty) {
      val decoder = charset.newDecoder()
      decoderCache.put(charset, decoder)
      optDecoder = Option(decoder)
    }
    optDecoder.get
  }

  def getEncoder(charset: Charset): CharsetEncoder = {
    // threadCheck()
    var optEncoder = encoderCache.get(charset)
    if (optEncoder.isEmpty) {
      val encoder = charset.newEncoder()
      encoderCache.put(charset, encoder)
      optEncoder = Option(encoder)
    }
    optEncoder.get
  }

}

final class PState private (
  var infoset: InfosetItem,
  var dataInputStream: DataInputStream,
  vmap: VariableMap,
  var status: ProcessorResult,
  diagnosticsArg: List[Diagnostic],
  val mpstate: MPState,
  dataProcArg: DataProcessor,
  var foundDelimiter: Maybe[FoundDelimiterText])
  extends ParseOrUnparseState(mpstate.dstate, vmap, diagnosticsArg, dataProcArg) {

  val discriminatorStack = new MStack.OfBoolean
  discriminatorStack.push(false)

  def setMode(dstate: DState) = dstate.setMode(ParseMode)

  override def dataStream: DataStreamCommon = dataInputStream

  def saveDelimitedText(foundText: String, originalRepresentation: String) {
    // threadCheck()
    val newDelimiter = new FoundDelimiterText(foundText, originalRepresentation) // TODO: Performance - allocates every time
    this.foundDelimiter = One(newDelimiter)
  }

  def clearDelimitedText() {
    // threadCheck()
    this.foundDelimiter = Nope
  }

  override def hasInfoset = true
  def thisElement = infoset.asInstanceOf[InfosetElement]

  override def groupPos = mpstate.groupPos
  override def arrayPos = mpstate.arrayPos
  override def childPos = mpstate.childPos
  override def occursBoundsStack = mpstate.occursBoundsStack

  private val markPool = new PState.MarkPool

  def mark: PState.Mark = {
    // threadCheck()
    val m = markPool.getFromPool
    m.assignFromPState(this)
    m
  }

  def reset(m: PState.Mark) {
    // threadCheck()
    m.resetOntoPState(this)
    m.clear()
    markPool.returnToPool(m)
  }

  def discard(m: PState.Mark) {
    dataInputStream.discard(m.disMark)
    m.clear()
    markPool.returnToPool(m)
  }

  override def toString() = {
    // threadCheck()
    "PState( bitPos=%s status=%s )".format(bitPos0b, status)
  }

  def currentLocation: DataLocation =
    new DataLoc(bitPos1b, bitLimit1b, Right(dataInputStream),
      Maybe(thisElement.runtimeData))

  override def discriminator = discriminatorStack.top
  def bitPos0b = dataInputStream.bitPos0b
  def bitLimit0b = dataInputStream.bitLimit0b
  //  def charLimit = inStream.charLimit0b

  def simpleElement: DISimple = {
    val res = infoset match {
      case s: DISimple => s
      case _ => Assert.usageError("not a simple element")
    }
    res
  }

  def complexElement: DIComplex = {
    val res = infoset match {
      case c: DIComplex => c
      case _ => Assert.usageError("not a complex element.")
    }
    res
  }
  def parentDocument = infoset.asInstanceOf[InfosetDocument]

  def setEndBitLimit(bitLimit0b: Long) {
    dataInputStream.setBitLimit0b(MaybeULong(bitLimit0b))
  }

  def setParent(newParent: InfosetItem) {
    this.infoset = newParent
  }

  def setVariable(varQName: GlobalQName, newValue: Any, referringContext: RuntimeData, pstate: PState) {
    this.variableMap = variableMap.setVariable(varQName, newValue, referringContext, pstate)
  }

  def reportValidationError(msg: String, args: Any*) {
    val ctxt = getContext()
    val vde = new ValidationError(Some(ctxt.schemaFileLocation), this, msg, args: _*)
    diagnostics = vde :: diagnostics
  }

  def reportValidationErrorNoContext(msg: String, args: Any*) {
    val vde = new ValidationError(None, this, msg, args: _*)
    diagnostics = vde :: diagnostics
  }

  def setFailed(failureDiagnostic: Diagnostic) {
    // threadCheck()
    status = new Failure(failureDiagnostic)
    diagnostics = failureDiagnostic :: diagnostics
  }

  def pushDiscriminator {
    // threadCheck()
    discriminatorStack.push(false)
  }

  def popDiscriminator {
    // threadCheck()
    discriminatorStack.pop
  }

  def setDiscriminator(disc: Boolean) {
    // threadCheck()
    discriminatorStack.pop()
    discriminatorStack.push(disc)
  }

  //  def setPos(bitPos: Long) {
  ////    val newInStream = inStream.withPos(bitPos)
  ////    this.inStream = newInStream
  //    dataInputStream.asInstanceOf[ByteBufferDataInputStream].setBitPos0b(bitPos)
  //  }

  def captureInfosetElementState = {
    // threadCheck()
    thisElement.captureState()
  }

  def restoreInfosetElementState(st: InfosetElementState) = {
    // threadCheck()
    thisElement.restoreState(st)
  }

  final def notifyDebugging(flag: Boolean) {
    // threadCheck()
    dataInputStream.setDebugging(flag)
  }
}

object PState {

  /**
   * A Mark for PState is a container for Marks for all the
   * things a PState contains that have their own mark/reset protocol,
   * and is a copy of everything else in PState.
   */
  class Mark {

    def bitPos0b = disMark.bitPos0b

    var infosetState: InfosetElementState = _
    var disMark: DataInputStream.Mark = _
    var variableMap: VariableMap = _
    var status: ProcessorResult = _
    var diagnostics: List[Diagnostic] = _
    var discriminatorStackMark: MStack.Mark = _
    var foundDelimiter: Maybe[FoundDelimiterText] = Nope

    val mpStateMark = new MPState.Mark

    def clear() {
      infosetState = null
      disMark = null
      variableMap = null
      status = null
      diagnostics = null
      discriminatorStackMark = MStack.nullMark
      foundDelimiter = Nope
      mpStateMark.clear()
    }

    def assignFromPState(ps: PState) {
      this.infosetState = ps.captureInfosetElementState
      this.disMark = ps.dataInputStream.mark
      this.variableMap = ps.variableMap
      this.status = ps.status
      this.diagnostics = ps.diagnostics
      this.discriminatorStackMark = ps.discriminatorStack.mark
      this.mpStateMark.setMarkFrom(ps.mpstate)
    }

    def resetOntoPState(ps: PState) {
      ps.restoreInfosetElementState(this.infosetState)
      ps.dataInputStream.reset(this.disMark)
      ps.variableMap = this.variableMap
      ps.status = this.status
      ps.diagnostics = this.diagnostics
      ps.discriminatorStack.reset(this.discriminatorStackMark)
      ps.foundDelimiter = this.foundDelimiter
      mpStateMark.resetFromMark(ps.mpstate)
    }

  }

  private class MarkPool extends Pool[Mark] {
    override def allocate = new Mark
  }

  /**
   * Initialize the state block given our InStream and a root element declaration.
   */
  def createInitialPState(
    root: ElementRuntimeData,
    dis: DataInputStream,
    dataProc: DFDL.DataProcessor): PState = {

    val doc = Infoset.newDocument(root)
    val variables = dataProc.getVariables
    val status = Success
    val diagnostics = Nil
    val mutablePState = new MPState
    val newState = new PState(doc, dis, variables, status, diagnostics, mutablePState,
      dataProc.asInstanceOf[DataProcessor], Nope)
    newState
  }

  /**
   * For testing, we can pass in the Infoset pre-constructed.
   */
  def createInitialPState(
    doc: InfosetDocument,
    root: ElementRuntimeData,
    dis: DataInputStream,
    dataProc: DFDL.DataProcessor): PState = {

    val variables = dataProc.getVariables
    val status = Success
    val diagnostics = Nil
    val mutablePState = new MPState

    val newState = new PState(doc, dis, variables, status, diagnostics, mutablePState,
      dataProc.asInstanceOf[DataProcessor], Nope)
    newState
  }

  /**
   * For testing it is convenient to just hand it strings for data.
   */
  def createInitialPState(
    root: ElementRuntimeData,
    data: String,
    bitOffset: Long,
    dataProc: DFDL.DataProcessor): PState = {
    val in = Misc.stringToReadableByteChannel(data)
    createInitialPState(root, in, dataProc, data.length, bitOffset)
  }

  /**
   * Construct our InStream object and initialize the state block.
   */
  def createInitialPState(
    root: ElementRuntimeData,
    input: DFDL.Input,
    dataProc: DFDL.DataProcessor,
    bitOffset: Long = 0,
    bitLengthLimit: Long = -1): PState = {
    val bitOrder = root.defaultBitOrder
    val dis =
      ByteBufferDataInputStream.fromByteChannel(input, bitOffset, bitLengthLimit, bitOrder)
    createInitialPState(root, dis, dataProc)
  }

}
