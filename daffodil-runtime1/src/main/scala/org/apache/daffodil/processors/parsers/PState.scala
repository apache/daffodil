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

import java.nio.channels.Channels
import java.nio.file.Files
import java.nio.file.Path

import scala.Right
import scala.collection.mutable

import org.apache.daffodil.api.DFDL
import org.apache.daffodil.api.DaffodilTunables
import org.apache.daffodil.api.DataLocation
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.infoset.DIComplex
import org.apache.daffodil.infoset.DIElement
import org.apache.daffodil.infoset.DISimple
import org.apache.daffodil.infoset.Infoset
import org.apache.daffodil.infoset.InfosetDocument
import org.apache.daffodil.infoset.InfosetOutputter
import org.apache.daffodil.io.InputSourceDataInputStream
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
import org.apache.daffodil.util.MStackOf
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe.Nope
import org.apache.daffodil.util.Maybe.One
import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.util.Pool
import org.apache.daffodil.util.Poolable
import org.apache.daffodil.infoset.DIComplexState
import org.apache.daffodil.infoset.DISimpleState
import org.apache.daffodil.exceptions.Abort
import org.apache.daffodil.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.xml.GlobalQName

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

    def clear(): Unit = {
      arrayIndexStackMark = MStack.nullMark
      groupIndexStackMark = MStack.nullMark
      childIndexStackMark = MStack.nullMark
      occursBoundsStackMark = MStack.nullMark
    }

    def captureFrom(mp: MPState): Unit = {
      arrayIndexStackMark = mp.arrayIndexStack.mark
      groupIndexStackMark = mp.groupIndexStack.mark
      childIndexStackMark = mp.childIndexStack.mark
    }
    def restoreInto(mp: MPState): Unit = {
      mp.arrayIndexStack.reset(this.arrayIndexStackMark)
      mp.groupIndexStack.reset(this.groupIndexStackMark)
      mp.childIndexStack.reset(this.childIndexStackMark)
    }
  }
}

class MPState private () {

  val arrayIndexStack = MStackOfLong()
  def moveOverOneArrayIndexOnly() = arrayIndexStack.push(arrayIndexStack.pop + 1)
  def moveBackOneArrayIndexOnly() = arrayIndexStack.push(arrayIndexStack.pop - 1)

  def arrayPos = arrayIndexStack.top

  val groupIndexStack = MStackOfLong()
  def moveOverOneGroupIndexOnly() = groupIndexStack.push(groupIndexStack.pop + 1)
  def groupPos = groupIndexStack.top

  // TODO: it doesn't look anything is actually reading the value of childindex
  // stack. Can we get rid of it?
  val childIndexStack = MStackOfLong()
  def moveOverOneElementChildOnly() = childIndexStack.push(childIndexStack.pop + 1)
  def childPos = {
    val res = childIndexStack.top
    Assert.invariant(res >= 1)
    res
  }

  val delimiters = new mutable.ArrayBuffer[DFADelimiter]
  val delimitersLocalIndexStack = MStackOfInt()

  val escapeSchemeEVCache = new MStackOfMaybe[EscapeSchemeParserHelper]

  private def init: Unit = {
    arrayIndexStack.push(1L)
    groupIndexStack.push(1L)
    childIndexStack.push(1L)
    delimitersLocalIndexStack.push(-1)
  }

  def verifyFinalState(): Unit = {
    // The current values of the top of these stacks might have
    // changed, but the fact that they are just 1 deep should be restored.
    Assert.invariant(arrayIndexStack.length == 1)
    Assert.invariant(groupIndexStack.length == 1)
    Assert.invariant(childIndexStack.length == 1)
    Assert.invariant(delimitersLocalIndexStack.length == 1)
  }
}

final class PState private (
  var infoset: DIElement,
  var dataInputStream: InputSourceDataInputStream,
  val output: InfosetOutputter,
  vmap: VariableMap,
  diagnosticsArg: List[Diagnostic],
  val mpstate: MPState,
  dataProcArg: DataProcessor,
  var delimitedParseResult: Maybe[dfa.ParseResult],
  var blobPaths: Seq[Path],
  tunable: DaffodilTunables) // Runtime tunables obtained from DataProcessor)
  extends ParseOrUnparseState(vmap, diagnosticsArg, One(dataProcArg), tunable) {

  override def currentNode = Maybe(infoset)

  private val discriminatorStack = MStackOfBoolean()
  discriminatorStack.push(false)

  /**
   * This stack tracks variables that have changed within the current point of
   * uncertainty. This tracking is necessary to revert changes made to variables
   * when the parser needs to backtrack.
   */
  private val changedVariablesStack = new MStackOf[mutable.MutableList[GlobalQName]]()
  changedVariablesStack.push(mutable.MutableList[GlobalQName]())

  override def dataStream = One(dataInputStream)

  def saveDelimitedParseResult(result: Maybe[dfa.ParseResult]): Unit = {
    // threadCheck()
    this.delimitedParseResult = result
  }

  def clearDelimitedParseResult(): Unit = {
    // threadCheck()
    this.delimitedParseResult = Nope
  }

  override def hasInfoset = true
  def thisElement = infoset

  override def groupPos = mpstate.groupPos
  override def arrayPos = mpstate.arrayPos
  override def childPos = mpstate.childPos

  private val markPool = new PState.MarkPool

  def mark(requestorID: String): PState.Mark = {
    // threadCheck()
    val m = markPool.getFromPool(requestorID)
    m.captureFrom(this, requestorID)
    m
  }

  def isInUse(m: PState.Mark) = {
    markPool.isInUse(m)
  }

  def dataInputStreamIsValid = dataInputStream.inputSource.isValid

  def reset(m: PState.Mark): Unit = {
    // threadCheck()
    m.restoreInto(this)
    m.clear()
    markPool.returnToPool(m)
    changedVariablesStack.top.foreach { v => {
      val variable = variableMap.find(v)
      if (variable.isDefined)
        variable.get.reset
    }}
    /* When parsing choices or unordered sequences it is necessary to clear the
     * list at the top of the stack because it is possible for multiple
     * PState.reset calls to occur withing the same point of uncertainty. If we
     * do not clear the list, the changes made in failed branches of the choice
     * will accumulate even though their effects have already been reset.
     */
    changedVariablesStack.top.clear
  }

  def discard(m: PState.Mark): Unit = {
    dataInputStream.discard(m.disMark)
    m.clear()
    markPool.returnToPool(m)
  }

  override def toString() = {
    // threadCheck()
    val hidden = if (withinHiddenNest) "hidden " else ""
    "PState( bitPos=%s status=%s %s)".format(bitPos0b, processorStatus, hidden)
  }

  def currentLocation: DataLocation = {
    val isAtEnd = !dataInputStream.isDefinedForLength(1)
    new DataLoc(bitPos1b, bitLimit1b, isAtEnd, Right(dataInputStream), Maybe(thisElement.runtimeData))
  }

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

  def setEndBitLimit(bitLimit0b: Long): Unit = {
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
  def setParent(newParent: DIElement): Unit = {
    this.infoset = newParent
  }

  def setVariable(vrd: VariableRuntimeData, newValue: DataValuePrimitive, referringContext: VariableRuntimeData, pstate: PState): Unit = {
    variableMap.setVariable(vrd, newValue, referringContext, pstate)
    changedVariablesStack.top += vrd.globalQName
  }

  /**
   * Note that this function does not actually read the variable, it is used
   * just to track that the variable was read in case we need to backtrack.
   */
  def markVariableRead(vrd: VariableRuntimeData): Unit = {
    changedVariablesStack.top += vrd.globalQName
  }

  def newVariableInstance(vrd: VariableRuntimeData): Unit = {
    variableMap.newVariableInstance(vrd)
    changedVariablesStack.top += vrd.globalQName
  }

  def removeVariableInstance(vrd: VariableRuntimeData): Unit = {
    variableMap.removeVariableInstance(vrd)
  }

  def pushPointOfUncertainty: Unit = {
    // threadCheck()
    discriminatorStack.push(false)
    changedVariablesStack.push(mutable.MutableList[GlobalQName]())
  }

  def popPointOfUncertainty: Unit = {
    // threadCheck()
    discriminatorStack.pop
    changedVariablesStack.pop
  }

  def setDiscriminator(disc: Boolean): Unit = {
    // threadCheck()
    discriminatorStack.pop()
    discriminatorStack.push(disc)
  }

  def addBlobPath(path: Path): Unit = {
    blobPaths = path +: blobPaths
  }

  final def notifyDebugging(flag: Boolean): Unit = {
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

      dis.cst.setPriorBitOrder(this.bitOrder)
      if (!dis.isAligned(8))
        SDE("Can only change dfdl:bitOrder on a byte boundary. Bit pos (1b) was %s.", dis.bitPos1b)
    }
  }

  private def isParseBitOrderChanging(dis: InputSourceDataInputStream): Boolean = {
    this.processor.context match {
      case ntrd: NonTermRuntimeData => false
      case _ => {
        val priorBitOrder = dis.cst.priorBitOrder
        val newBitOrder = this.bitOrder
        priorBitOrder ne newBitOrder
      }
    }
  }

  override lazy val (regexMatchBuffer, regexMatchBitPositionBuffer) = dataProcArg.regexMatchState.get

  /**
   * Verify that the state is left where we expect it to be after
   * a normal parse. I.e., stacks have been popped back to their original state,
   * pools - all items returned, etc.
   *
   * If for some reason parsing ends with a throw (not supposed to, but just if)
   * then all bets are off, so most checks are disabled.
   * Some checks are still done. If those fail, we include the original error (if present) as the cause.
   *
   * verifyFinalState may be called from within another try..catch block for which certain excepetions
   * are expected as control flow.
   * To avoid accidently creating such an exception, we wrap our generated exception in UnsuppressableException
   */
  def verifyFinalState(optThrown: Maybe[Throwable]): Unit = {
    try {
      if (optThrown.isEmpty) {
        Assert.invariant(this.discriminatorStack.length == 1)
        Assert.invariant(!this.withinHiddenNest) //ensure we are not in hidden nest
        mpstate.verifyFinalState()
      }
      // These we check regardless of throw or not.
      markPool.finalCheck
      dataInputStream.inputSource.compact // discard any storage that can be freed.
      dataInputStream.validateFinalStreamState
    } catch {
      case e: Throwable => {
        if (optThrown.isDefined) e.addSuppressed(optThrown.get)
        val toThrow = new Abort(e)
        toThrow.addSuppressed(e)
        throw toThrow
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

    override def toString() = {
      if (disMark ne null)
        "Mark(bitPos0b = " + bitPos0b + " requestorId = " + poolDebugLabel + ")"
      else
        "Mark(uninitialized" + " requestorId = " + poolDebugLabel + ")"
    }

    def bitPos0b = disMark.bitPos0b

    val simpleElementState = DISimpleState()
    val complexElementState = DIComplexState()
    var disMark: DataInputStream.Mark = _
    var variableMap: VariableMap = _
    var processorStatus: ProcessorResult = _
    var validationStatus: Boolean = _
    var diagnostics: List[Diagnostic] = _
    var delimitedParseResult: Maybe[dfa.ParseResult] = Nope
    var blobPaths: Seq[Path] = Seq.empty

    val mpStateMark = new MPState.Mark

    def clear(): Unit = {
      simpleElementState.clear()
      complexElementState.clear()
      disMark = null
      variableMap = null
      processorStatus = null
      validationStatus = true
      diagnostics = null
      delimitedParseResult = Nope
      mpStateMark.clear()
      blobPaths = Seq.empty
      // DO NOT clear requestorId. It is there to help us debug if we try to repeatedly reset/discard a mark already discarded.
    }

    def captureFrom(ps: PState, requestorID: String): Unit = {
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
      this.blobPaths = ps.blobPaths
    }

    def restoreInfoset(ps: PState) = {
      val e = ps.thisElement
      e match {
        case s: DISimple => simpleElementState.restoreInto(e)
        case c: DIComplex => complexElementState.restoreInto(e)
      }
    }

    def restoreInto(ps: PState): Unit = {
      restoreInfoset(ps)
      ps.dataInputStream.reset(this.disMark)
      ps.setVariableMap(this.variableMap)
      ps._processorStatus = this.processorStatus
      ps._validationStatus = this.validationStatus
      ps.diagnostics = this.diagnostics
      ps.delimitedParseResult = this.delimitedParseResult
      mpStateMark.restoreInto(ps.mpstate)

      // We are backtracking here, potentially past blob files that have
      // already been written, so we must delete them. Since we always prepend
      // blobs to the blobPaths Seq as they are created, we can delete them by
      // deleting all blob files that are in front of the blobsToKeep Seq. This
      // also lets us do fast reference equality comparisons for determining
      // when to stop deleting.
      val blobsToKeep = this.blobPaths
      var currentBlobs = ps.blobPaths
      while (currentBlobs ne blobsToKeep) {
        Files.delete(currentBlobs.head)
        currentBlobs = currentBlobs.tail
      }
      ps.blobPaths = blobsToKeep
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
    dis: InputSourceDataInputStream,
    output: InfosetOutputter,
    dataProc: DFDL.DataProcessor): PState = {

    val tunables = dataProc.getTunables()
    val doc = Infoset.newDocument(root).asInstanceOf[DIElement]
    createInitialPState(doc.asInstanceOf[InfosetDocument], root, dis, output, dataProc)
  }

  /**
   * For testing, we can pass in the Infoset pre-constructed.
   */
  def createInitialPState(
    doc: InfosetDocument,
    root: ElementRuntimeData,
    dis: InputSourceDataInputStream,
    output: InfosetOutputter,
    dataProc: DFDL.DataProcessor): PState = {

    /**
     * This is a full deep copy as variableMap is mutable. Reusing
     * dataProc.VariableMap without a copy would not be thread safe.
     */
    val variables = dataProc.variableMap.copy

    val diagnostics = Nil
    val mutablePState = MPState()
    val tunables = dataProc.getTunables()

    dis.cst.setPriorBitOrder(root.defaultBitOrder)
    val newState = new PState(doc.asInstanceOf[DIElement], dis, output, variables, diagnostics, mutablePState,
      dataProc.asInstanceOf[DataProcessor], Nope, Seq.empty, tunables)
    newState
  }

  /**
   * For testing it is convenient to just hand it strings for data.
   */
  def createInitialPState(
    root: ElementRuntimeData,
    data: String,
    output: InfosetOutputter,
    dataProc: DFDL.DataProcessor): PState = {
    val in = InputSourceDataInputStream(data.getBytes("utf-8"))
    createInitialPState(root, in, output, dataProc)
  }

  /**
   * Construct our InStream object and initialize the state block.
   */
  def createInitialPState(
    root: ElementRuntimeData,
    input: java.nio.channels.ReadableByteChannel,
    output: InfosetOutputter,
    dataProc: DFDL.DataProcessor): PState = {
    val dis = InputSourceDataInputStream(Channels.newInputStream(input))
    createInitialPState(root, dis, output, dataProc)
  }
}
