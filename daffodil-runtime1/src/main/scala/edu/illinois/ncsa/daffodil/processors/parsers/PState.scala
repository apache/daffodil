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

package edu.illinois.ncsa.daffodil.processors.parsers

import scala.Right
import scala.collection.mutable

import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.api.DaffodilTunables
import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.infoset.DIComplex
import edu.illinois.ncsa.daffodil.infoset.DIComplexState
import edu.illinois.ncsa.daffodil.infoset.DIElement
import edu.illinois.ncsa.daffodil.infoset.DISimple
import edu.illinois.ncsa.daffodil.infoset.DISimpleState
import edu.illinois.ncsa.daffodil.infoset.Infoset
import edu.illinois.ncsa.daffodil.infoset.InfosetDocument
import edu.illinois.ncsa.daffodil.infoset.InfosetOutputter
import edu.illinois.ncsa.daffodil.io.ByteBufferDataInputStream
import edu.illinois.ncsa.daffodil.io.DataInputStream
import edu.illinois.ncsa.daffodil.processors.DataLoc
import edu.illinois.ncsa.daffodil.processors.DataProcessor
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeParserHelper
import edu.illinois.ncsa.daffodil.processors.NonTermRuntimeData
import edu.illinois.ncsa.daffodil.processors.ParseOrUnparseState
import edu.illinois.ncsa.daffodil.processors.ProcessorResult
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.processors.VariableRuntimeData
import edu.illinois.ncsa.daffodil.processors.dfa
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
import edu.illinois.ncsa.daffodil.util.Poolable

object MPState {

  def apply() = {
    val obj = new MPState()
    obj.init
    obj
  }

  final class Mark {

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

    def captureFrom(mp: MPState) {
      arrayIndexStackMark = mp.arrayIndexStack.mark
      groupIndexStackMark = mp.groupIndexStack.mark
      childIndexStackMark = mp.childIndexStack.mark
      occursBoundsStackMark = mp.occursBoundsStack.mark
    }
    def restoreInto(mp: MPState) {
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
    occursBoundsStack.push(1L)
    delimitersLocalIndexStack.push(-1)
  }
}

final class PState private (
  var infoset: DIElement,
  var dataInputStream: ByteBufferDataInputStream,
  val output: InfosetOutputter,
  vmap: VariableMap,
  diagnosticsArg: List[Diagnostic],
  val mpstate: MPState,
  dataProcArg: DataProcessor,
  var delimitedParseResult: Maybe[dfa.ParseResult],
  tunable: DaffodilTunables) // Runtime tunables obtained from DataProcessor)
  extends ParseOrUnparseState(vmap, diagnosticsArg, One(dataProcArg), tunable) {

  override def currentNode = Maybe(infoset)

  private val discriminatorStack = MStackOfBoolean()
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
  def thisElement = infoset

  override def groupPos = mpstate.groupPos
  override def arrayPos = mpstate.arrayPos
  override def childPos = mpstate.childPos
  override def occursBoundsStack = mpstate.occursBoundsStack

  private val markPool = new PState.MarkPool

  def mark(requestorID: String): PState.Mark = {
    // threadCheck()
    val m = markPool.getFromPool(requestorID)
    m.captureFrom(this, requestorID)
    m
  }

  def reset(m: PState.Mark) {
    // threadCheck()
    m.restoreInto(this)
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
    "PState( bitPos=%s status=%s )".format(bitPos0b, processorStatus)
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

  /**
   * This takes newParent as DIElement, not DIComplex, because there is code
   * where we don't know whether the node is simple or complex but we set it as
   * the parent anyway. If simple children will simply not be appended.
   *
   * But this invariant that there is always a parent we could append a child into
   * is being maintained. THis invariant starts at the very top as there is a
   * Document which is the parent of the root element. So there's no time when there
   * isn't a parent there.
   */
  def setParent(newParent: DIElement) {
    this.infoset = newParent
  }

  def setVariable(vrd: VariableRuntimeData, newValue: Any, referringContext: VariableRuntimeData, pstate: PState) {
    this.setVariableMap(variableMap.setVariable(vrd, newValue, referringContext, pstate))
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
  class Mark extends Poolable {

    def bitPos0b = disMark.bitPos0b

    val simpleElementState = DISimpleState()
    val complexElementState = DIComplexState()
    var disMark: DataInputStream.Mark = _
    var variableMap: VariableMap = _
    var processorStatus: ProcessorResult = _
    var validationStatus: Boolean = _
    var diagnostics: List[Diagnostic] = _
    var delimitedParseResult: Maybe[dfa.ParseResult] = Nope

    val mpStateMark = new MPState.Mark

    def clear() {
      simpleElementState.clear()
      complexElementState.clear()
      disMark = null
      variableMap = null
      processorStatus = null
      validationStatus = true
      diagnostics = null
      delimitedParseResult = Nope
      mpStateMark.clear()
    }

    def captureFrom(ps: PState, requestorID: String) {
      val e = ps.thisElement
      if (e.isSimple)
        simpleElementState.captureFrom(e)
      else
        complexElementState.captureFrom(e)
      this.disMark = ps.dataInputStream.mark(requestorID)
      this.variableMap = ps.variableMap
      this.processorStatus = ps.processorStatus
      this.validationStatus = ps.validationStatus
      this.diagnostics = ps.diagnostics
      this.mpStateMark.captureFrom(ps.mpstate)
    }

    def restoreInto(ps: PState) {
      val e = ps.thisElement
      e match {
        case s: DISimple => simpleElementState.restoreInto(e)
        case c: DIComplex => complexElementState.restoreInto(e)
      }
      ps.dataInputStream.reset(this.disMark)
      ps.setVariableMap(this.variableMap)
      ps._processorStatus = this.processorStatus
      ps._validationStatus = this.validationStatus
      ps.diagnostics = this.diagnostics
      ps.delimitedParseResult = this.delimitedParseResult
      mpStateMark.restoreInto(ps.mpstate)
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
    dis: ByteBufferDataInputStream,
    output: InfosetOutputter,
    dataProc: DFDL.DataProcessor): PState = {

    val tunables = dataProc.getTunables()

    val doc = Infoset.newDocument(root, tunables).asInstanceOf[DIElement]
    val variables = dataProc.getVariables
    val diagnostics = Nil
    val mutablePState = MPState()
    val newState = new PState(doc, dis, output, variables, diagnostics, mutablePState,
      dataProc.asInstanceOf[DataProcessor], Nope, tunables)
    newState
  }

  /**
   * For testing, we can pass in the Infoset pre-constructed.
   */
  def createInitialPState(
    doc: InfosetDocument,
    root: ElementRuntimeData,
    dis: ByteBufferDataInputStream,
    output: InfosetOutputter,
    dataProc: DFDL.DataProcessor): PState = {

    val variables = dataProc.getVariables
    val diagnostics = Nil
    val mutablePState = MPState()

    val newState = new PState(doc.asInstanceOf[DIElement], dis, output, variables, diagnostics, mutablePState,
      dataProc.asInstanceOf[DataProcessor], Nope, dataProc.getTunables())
    newState
  }

  /**
   * For testing it is convenient to just hand it strings for data.
   */
  def createInitialPState(
    root: ElementRuntimeData,
    data: String,
    output: InfosetOutputter,
    bitOffset: Long,
    dataProc: DFDL.DataProcessor): PState = {
    val in = Misc.stringToReadableByteChannel(data)
    createInitialPState(root, in, output, dataProc, data.length, bitOffset)
  }

  /**
   * Construct our InStream object and initialize the state block.
   */
  def createInitialPState(
    root: ElementRuntimeData,
    input: DFDL.Input,
    output: InfosetOutputter,
    dataProc: DFDL.DataProcessor,
    bitOffset: Long = 0,
    bitLengthLimit: Long = -1): PState = {
    val dis =
      ByteBufferDataInputStream.fromByteChannel(input, bitOffset, bitLengthLimit)
    dis.cst.setPriorBitOrder(root.defaultBitOrder)
    createInitialPState(root, dis, output, dataProc)
  }
}

object ParserBitOrderChecks {
  /**
   * Checks for bit order change. If the bit order is changing, checks if we're
   * on a proper byte boundary.
   */
  final def checkParseBitOrder(pstate: PState) = {
    //
    // TODO: This looks like a lot of overhead for every single parse call.
    //
    // We need to check for bitOrder change. If it is changing, we
    // need to know if it is on a proper byte boundary.
    //
    val dis = pstate.dataInputStream
    val isChanging = isParseBitOrderChanging(dis, pstate)
    if (isChanging) {
      //
      // the bit order is changing. Let's be sure
      // that it's legal to do so w.r.t. other properties
      // These checks will have been evaluated at compile time if
      // all the properties are static, so this is really just
      // in case the charset or byteOrder are runtime-valued.
      //
      pstate.processor.context match {
        case trd: TermRuntimeData => {
          val mcboc = trd.maybeCheckBitOrderAndCharsetEv
          val mcbbo = trd.maybeCheckByteAndBitOrderEv
          if (mcboc.isDefined) mcboc.get.evaluate(pstate) // Expressions must be evaluated on the element, not before it is created.
          if (mcbbo.isDefined) mcbbo.get.evaluate(pstate)
        }
        case _ => // ok
      }

      dis.st.setPriorBitOrder(pstate.bitOrder)
      if (!dis.isAligned(8))
        pstate.SDE("Can only change dfdl:bitOrder on a byte boundary. Bit pos (1b) was %s.", dis.bitPos1b)
    }
  }

  private def isParseBitOrderChanging(dis: ByteBufferDataInputStream, pstate: PState): Boolean = {
    pstate.processor.context match {
      case ntrd: NonTermRuntimeData => false
      case _ => {
        val priorBitOrder = dis.st.priorBitOrder
        val newBitOrder = pstate.bitOrder
        priorBitOrder ne newBitOrder
      }
    }
  }
}
