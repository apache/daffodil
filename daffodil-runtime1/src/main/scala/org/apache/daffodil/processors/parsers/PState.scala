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

package org.apache.daffodil.processors.parsers

import scala.Right
import scala.collection.mutable

import org.apache.daffodil.api.DFDL
import org.apache.daffodil.api.DaffodilTunables
import org.apache.daffodil.api.DataLocation
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.infoset.DIComplex
import org.apache.daffodil.infoset.DIComplexState
import org.apache.daffodil.infoset.DIElement
import org.apache.daffodil.infoset.DISimple
import org.apache.daffodil.infoset.DISimpleState
import org.apache.daffodil.infoset.Infoset
import org.apache.daffodil.infoset.InfosetDocument
import org.apache.daffodil.infoset.InfosetOutputter
import org.apache.daffodil.io.ByteBufferDataInputStream
import org.apache.daffodil.io.DataInputStream
import org.apache.daffodil.processors.DataLoc
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.EscapeSchemeParserHelper
import org.apache.daffodil.processors.NonTermRuntimeData
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.processors.ProcessorResult
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.processors.VariableMap
import org.apache.daffodil.processors.VariableRuntimeData
import org.apache.daffodil.processors.dfa
import org.apache.daffodil.processors.dfa.DFADelimiter
import org.apache.daffodil.util.MStack
import org.apache.daffodil.util.MStackOfBoolean
import org.apache.daffodil.util.MStackOfInt
import org.apache.daffodil.util.MStackOfLong
import org.apache.daffodil.util.MStackOfMaybe
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe.Nope
import org.apache.daffodil.util.Maybe.One
import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.util.Misc
import org.apache.daffodil.util.Pool
import org.apache.daffodil.util.Poolable

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

  var wasAnyArrayElementNonZeroLength = false
  var wasLastArrayElementZeroLength = true

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

  /**
   * Checks for bit order change. If the bit order is changing, checks if we're
   * on a proper byte boundary.
   */
  final override protected def checkBitOrder(): Unit = {

    //
    // TODO: This looks like a lot of overhead for every single parse call.
    //
    // We need to check for bitOrder change. If it is changing, we
    // need to know if it is on a proper byte boundary.
    //
    val dis = this.dataInputStream
    val isChanging = isParseBitOrderChanging(dis)
    if (isChanging) {
      //
      // the bit order is changing. Let's be sure
      // that it's legal to do so w.r.t. other properties
      // These checks will have been evaluated at compile time if
      // all the properties are static, so this is really just
      // in case the charset or byteOrder are runtime-valued.
      //
      this.processor.context match {
        case trd: TermRuntimeData => {
          val mcboc = trd.maybeCheckBitOrderAndCharsetEv
          val mcbbo = trd.maybeCheckByteAndBitOrderEv
          if (mcboc.isDefined) mcboc.get.evaluate(this) // Expressions must be evaluated on the element, not before it is created.
          if (mcbbo.isDefined) mcbbo.get.evaluate(this)
        }
        case _ => // ok
      }

      dis.st.setPriorBitOrder(this.bitOrder)
      if (!dis.isAligned(8))
        SDE("Can only change dfdl:bitOrder on a byte boundary. Bit pos (1b) was %s.", dis.bitPos1b)
    }
  }

  private def isParseBitOrderChanging(dis: ByteBufferDataInputStream): Boolean = {
    this.processor.context match {
      case ntrd: NonTermRuntimeData => false
      case _ => {
        val priorBitOrder = dis.st.priorBitOrder
        val newBitOrder = this.bitOrder
        priorBitOrder ne newBitOrder
      }
    }
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
