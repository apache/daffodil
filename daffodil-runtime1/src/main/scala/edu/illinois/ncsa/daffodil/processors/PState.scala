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

import scala.Right
import scala.collection.mutable

import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.dpath.DState
import edu.illinois.ncsa.daffodil.dsom.RuntimeSchemaDefinitionError
import edu.illinois.ncsa.daffodil.dsom.RuntimeSchemaDefinitionWarning
import edu.illinois.ncsa.daffodil.dsom.ValidationError
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.SavesErrorsAndWarnings
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.io.ByteBufferDataInputStream
import edu.illinois.ncsa.daffodil.io.DataInputStream
import edu.illinois.ncsa.daffodil.io.DataStreamCommon
import edu.illinois.ncsa.daffodil.io.LocalBufferMixin
import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter
import edu.illinois.ncsa.daffodil.util.MStack
import edu.illinois.ncsa.daffodil.util.MStackOfBoolean
import edu.illinois.ncsa.daffodil.util.MStackOfInt
import edu.illinois.ncsa.daffodil.util.MStackOfLong
import edu.illinois.ncsa.daffodil.util.MStackOfMaybe
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.Pool
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.processors.charset.EncoderDecoderMixin

object MPState {

  def apply() = {
    val obj = new MPState()
    obj.init
    obj
  }

  class Mark {

    var arrayIndexStackMark: MStack.Mark = _
    var groupIndexStackMark: MStack.Mark = _
    var childIndexStackMark: MStack.Mark = _
    var occursBoundsStackMark: MStack.Mark = _

    clear()

    def clear() {
      arrayIndexStackMark = MStack.nullMark
      groupIndexStackMark = MStack.nullMark
      childIndexStackMark = MStack.nullMark
      occursBoundsStackMark = MStack.nullMark
    }

    def setMarkFrom(mp: MPState) {
      arrayIndexStackMark = mp.arrayIndexStack.mark
      groupIndexStackMark = mp.groupIndexStack.mark
      childIndexStackMark = mp.childIndexStack.mark
      occursBoundsStackMark = mp.occursBoundsStack.mark
    }
    def resetFromMark(mp: MPState) {
      mp.arrayIndexStack.reset(this.arrayIndexStackMark)
      mp.groupIndexStack.reset(this.groupIndexStackMark)
      mp.childIndexStack.reset(this.childIndexStackMark)
      mp.occursBoundsStack.reset(this.occursBoundsStackMark)
    }
  }
}

class MPState private () {

  val arrayIndexStack = MStackOfLong()
  def moveOverOneArrayIndexOnly() = arrayIndexStack.push(arrayIndexStack.pop + 1)
  def arrayPos = arrayIndexStack.top

  val groupIndexStack = MStackOfLong()
  def moveOverOneGroupIndexOnly() = groupIndexStack.push(groupIndexStack.pop + 1)
  def groupPos = groupIndexStack.top

  // TODO: it doesn't look anything is actually reading the value of childindex
  // stack. Can we get rid of it?
  val childIndexStack = MStackOfLong()
  def moveOverOneElementChildOnly() = childIndexStack.push(childIndexStack.pop + 1)
  def childPos = childIndexStack.top

  val occursBoundsStack = MStackOfLong()
  def updateBoundsHead(ob: Long) = {
    occursBoundsStack.pop()
    occursBoundsStack.push(ob)
  }
  def occursBounds = occursBoundsStack.top

  val delimiters = new mutable.ArrayBuffer[DFADelimiter]
  val delimitersLocalIndexStack = MStackOfInt()

  val escapeSchemeEVCache = new MStackOfMaybe[EscapeSchemeParserHelper]

  private def init {
    arrayIndexStack.push(1L)
    groupIndexStack.push(1L)
    childIndexStack.push(1L)
    childIndexStack.push(1L)
    delimitersLocalIndexStack.push(-1)
  }
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
abstract class ParseOrUnparseState protected (
  protected var variableBox: VariableBox,
  var diagnostics: List[Diagnostic],
  var dataProc: Maybe[DataProcessor],
  protected var status_ : ProcessorResult) extends DFDL.State
  with StateForDebugger
  with ThrowsSDE with SavesErrorsAndWarnings
  with LocalBufferMixin
  with EncoderDecoderMixin
  with Logging {

  def this(vmap: VariableMap, diags: List[Diagnostic], dataProc: Maybe[DataProcessor], status: ProcessorResult = Success) =
    this(new VariableBox(vmap), diags, dataProc, status)

  def variableMap = variableBox.vmap
  def setVariableMap(newMap: VariableMap) {
    variableBox.setVMap(newMap)
  }

  def status = status_

  final def setFailed(failureDiagnostic: Diagnostic) {
    // threadCheck()
    status_ = new Failure(failureDiagnostic)
    diagnostics = failureDiagnostic :: diagnostics
  }

  /**
   * Important: If an error is being suppressed, you must call this to reset the state
   * back so that the prior failure doesn't "last forever" past the point where it is being suppressed.
   *
   * This happens, for example, in the debugger when it is evaluating expressions.
   */
  def setSuccess() {
    status_ = Success
  }

  def currentNode: Maybe[DINode]

  private val _dState = new DState

  /**
   * Used when evaluating expressions. Holds state of expression
   * during evaluation.
   *
   * Doesn't hold every bit of state - that is to say there's still the
   * regular execution call stack, which
   * keeps track of exactly where in the expression evaluation we are.
   */
  def dState = _dState

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

  def dataStream: Maybe[DataStreamCommon]

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
  def occursBoundsStack: MStackOfLong

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
    // ExecutionMode.requireRuntimeMode // not any more. More code is shared between compile and runtime now, so these requirements gotta go
    val ctxt = getContext()
    val rsde = new RuntimeSchemaDefinitionError(ctxt.schemaFileLocation, this, str, args: _*)
    ctxt.toss(rsde)
  }

  def SDEButContinue(str: String, args: Any*) = {
    // ExecutionMode.requireRuntimeMode
    val ctxt = getContext()
    val rsde = new RuntimeSchemaDefinitionError(ctxt.schemaFileLocation, this, str, args: _*)
    diagnostics = rsde :: diagnostics
  }

  def SDW(str: String, args: Any*) = {
    // ExecutionMode.requireRuntimeMode
    val ctxt = getContext()
    val rsdw = new RuntimeSchemaDefinitionWarning(ctxt.schemaFileLocation, this, str, args: _*)
    diagnostics = rsdw :: diagnostics
  }

}

/**
 * State used when compiling Evaluatable[T] objects
 *  So they don't require a "real" state.
 *
 *  This serves two purposes. First it lets us obey the regular API for evaluation, so we don't need
 *  one way to evaluate and another very similar thing for analyzing expressions to see if they are constnat.
 *
 *  Second, it serves as a detector of when an expression is non-constant by blowing up when things
 *  inconsistent with constant-value are attempted to be extracted from the state. By "blow up" it throws
 *  a structured set of exceptions, typically children of InfosetException or VariableException.
 */
class CompileState(trd: RuntimeData, maybeDataProc: Maybe[DataProcessor])
  extends ParseOrUnparseState(trd.variableMap, Nil, maybeDataProc) {
  /**
   * As seen from class CompileState, the missing signatures are as follows.
   *  *  For convenience, these are usable as stub implementations.
   */
  // Members declared in edu.illinois.ncsa.daffodil.processors.ParseOrUnparseState
  def arrayPos: Long = 1L
  def bitLimit0b: MaybeULong = MaybeULong.Nope
  def bitPos0b: Long = 0L
  def childPos: Long = 0L
  def dataStream = Nope
  def groupPos: Long = 0L
  def hasInfoset: Boolean = infoset_.isDefined

  private lazy val infoset_ : Maybe[DIElement] = Nope

  def infoset: InfosetItem =
    if (infoset_.isDefined)
      infoset_.value
    else
      throw new InfosetNoInfosetException(One(trd)) // for expressions evaluated in debugger, default expressions for top-level variable decls.

  def currentNode = Maybe(infoset.asInstanceOf[DINode])

  def notifyDebugging(flag: Boolean): Unit = {}
  private val occursBoundsStack_ = MStackOfLong()
  def occursBoundsStack: MStackOfLong = occursBoundsStack_

  def thisElement: InfosetElement = infoset.asInstanceOf[InfosetElement]

  // Members declared in edu.illinois.ncsa.daffodil.processors.StateForDebugger
  def currentLocation: DataLocation = Assert.usageError("Not to be used.")
}

final class PState private (
  var infoset: InfosetItem,
  var dataInputStream: DataInputStream,
  vmap: VariableMap,
  status: ProcessorResult,
  diagnosticsArg: List[Diagnostic],
  val mpstate: MPState,
  dataProcArg: DataProcessor,
  var delimitedParseResult: Maybe[dfa.ParseResult])
  extends ParseOrUnparseState(vmap, diagnosticsArg, One(dataProcArg), status) {

  override def currentNode = Maybe(infoset.asInstanceOf[DINode])

  val discriminatorStack = MStackOfBoolean()
  discriminatorStack.push(false)

  override def dataStream = One(dataInputStream)

  def saveDelimitedParseResult(result: Maybe[dfa.ParseResult]) {
    // threadCheck()
    this.delimitedParseResult = result
  }

  def clearDelimitedParseResult() {
    // threadCheck()
    this.delimitedParseResult = Nope
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

  def setVariable(vrd: VariableRuntimeData, newValue: Any, referringContext: RuntimeData, pstate: PState) {
    this.setVariableMap(variableMap.setVariable(vrd, newValue, referringContext, pstate))
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
    var delimitedParseResult: Maybe[dfa.ParseResult] = Nope

    val mpStateMark = new MPState.Mark

    def clear() {
      infosetState = null
      disMark = null
      variableMap = null
      status = null
      diagnostics = null
      discriminatorStackMark = MStack.nullMark
      delimitedParseResult = Nope
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
      ps.setVariableMap(this.variableMap)
      ps.status_ = this.status
      ps.diagnostics = this.diagnostics
      ps.discriminatorStack.reset(this.discriminatorStackMark)
      ps.delimitedParseResult = this.delimitedParseResult
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
    val mutablePState = MPState()
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
    val mutablePState = MPState()

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
