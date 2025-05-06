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

package org.apache.daffodil.runtime1.processors.parsers

import java.nio.channels.Channels
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.util.Collections
import scala.collection.mutable.ArrayBuffer

import org.apache.daffodil.api
import org.apache.daffodil.io.DataInputStream
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.lib.exceptions.Abort
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.lib.iapi.DataLocation
import org.apache.daffodil.lib.util.MStack
import org.apache.daffodil.lib.util.MStackOf
import org.apache.daffodil.lib.util.MStackOfInt
import org.apache.daffodil.lib.util.MStackOfLong
import org.apache.daffodil.lib.util.MStackOfMaybe
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.lib.util.Pool
import org.apache.daffodil.lib.util.Poolable
import org.apache.daffodil.runtime1.iapi.DFDL
import org.apache.daffodil.runtime1.infoset.DIComplex
import org.apache.daffodil.runtime1.infoset.DIComplexState
import org.apache.daffodil.runtime1.infoset.DIElement
import org.apache.daffodil.runtime1.infoset.DISimple
import org.apache.daffodil.runtime1.infoset.DISimpleState
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.infoset.Infoset
import org.apache.daffodil.runtime1.infoset.InfosetWalker
import org.apache.daffodil.runtime1.processors.DataLoc
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.EscapeSchemeParserHelper
import org.apache.daffodil.runtime1.processors.NonTermRuntimeData
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.ProcessorResult
import org.apache.daffodil.runtime1.processors.RuntimeData
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.VariableInstance
import org.apache.daffodil.runtime1.processors.VariableMap
import org.apache.daffodil.runtime1.processors.VariableRuntimeData
import org.apache.daffodil.runtime1.processors.dfa
import org.apache.daffodil.runtime1.processors.dfa.DFADelimiter

object MPState {

  def apply() = {
    val obj = new MPState()
    obj.init()
    obj
  }

  final class Mark {

    var arrayIterationIndexStackMark: MStack.Mark = _
    var occursIndexStackMark: MStack.Mark = _
    var groupIndexStackMark: MStack.Mark = _
    var childIndexStackMark: MStack.Mark = _
    var occursBoundsStackMark: MStack.Mark = _

    clear()

    def clear(): Unit = {
      arrayIterationIndexStackMark = MStack.nullMark
      occursIndexStackMark = MStack.nullMark
      groupIndexStackMark = MStack.nullMark
      childIndexStackMark = MStack.nullMark
      occursBoundsStackMark = MStack.nullMark
    }

    def captureFrom(mp: MPState): Unit = {
      arrayIterationIndexStackMark = mp.arrayIterationIndexStack.mark
      occursIndexStackMark = mp.occursIndexStack.mark
      groupIndexStackMark = mp.groupIndexStack.mark
      childIndexStackMark = mp.childIndexStack.mark
    }

    def restoreInto(mp: MPState): Unit = {
      mp.arrayIterationIndexStack.reset(this.arrayIterationIndexStackMark)
      mp.occursIndexStack.reset(this.occursIndexStackMark)
      mp.groupIndexStack.reset(this.groupIndexStackMark)
      mp.childIndexStack.reset(this.childIndexStackMark)
    }
  }
}

class MPState private () {

  val arrayIterationIndexStack = MStackOfLong()
  val occursIndexStack = MStackOfLong()

  def moveOverOneArrayIterationIndexOnly() =
    arrayIterationIndexStack.push(arrayIterationIndexStack.pop() + 1)

  def arrayIterationPos = arrayIterationIndexStack.top

  def moveOverOneOccursIndexOnly() = occursIndexStack.push(occursIndexStack.pop() + 1)
  def occursPos = occursIndexStack.top

  val groupIndexStack = MStackOfLong()
  def moveOverOneGroupIndexOnly() = groupIndexStack.push(groupIndexStack.pop() + 1)
  def groupPos = groupIndexStack.top

  // TODO: it doesn't look anything is actually reading the value of childindex
  // stack. Can we get rid of it?
  val childIndexStack = MStackOfLong()
  def moveOverOneElementChildOnly() = childIndexStack.push(childIndexStack.pop() + 1)
  def childPos = {
    val res = childIndexStack.top
    Assert.invariant(res >= 1)
    res
  }
  val delimiters = new ArrayBuffer[DFADelimiter]
  val delimitersLocalIndexStack = MStackOfInt()

  val escapeSchemeEVCache = new MStackOfMaybe[EscapeSchemeParserHelper]

  private def init(): Unit = {
    arrayIterationIndexStack.push(1L)
    occursIndexStack.push(1L)
    groupIndexStack.push(1L)
    childIndexStack.push(1L)
    delimitersLocalIndexStack.push(-1)
  }

  def verifyFinalState(): Unit = {
    // The current values of the top of these stacks might have
    // changed, but the fact that they are just 1 deep should be restored.
    Assert.invariant(arrayIterationIndexStack.length == 1)
    Assert.invariant(occursIndexStack.length == 1)
    Assert.invariant(groupIndexStack.length == 1)
    Assert.invariant(childIndexStack.length == 1)
    Assert.invariant(delimitersLocalIndexStack.length == 1)
  }
}

final class PState private (
  var infoset: DIElement,
  var infosetLastChild: Maybe[DIElement],
  var dataInputStream: InputSourceDataInputStream,
  val walker: InfosetWalker,
  vmap: VariableMap,
  diagnosticsArg: java.util.List[api.Diagnostic],
  val mpstate: MPState,
  dataProcArg: DataProcessor,
  var delimitedParseResult: Maybe[dfa.ParseResult],
  var blobPaths: Seq[Path],
  tunable: DaffodilTunables
) // Runtime tunables obtained from DataProcessor)
  extends ParseOrUnparseState(vmap, diagnosticsArg, One(dataProcArg), tunable)
  with PStateUsingMacrosMixin {

  override def currentNode = Maybe(infoset)

  def output = walker.outputter

  /**
   * This stack is used to track points of uncertainty during a parse. When a
   * parser determines a PoU should exist, it should call withPointOfUncertainty
   * to create a new PoU (represented by a Mark) and perform all the logic that
   * should exist while that pou should remain in scope (such as calling child
   * parsers). For example:
   *
   *   pstate.withPointOfUncertainty { pou =>
   *     // call child parsers
   *   }
   *
   * Calling withPointOfUncertainty pushes the new Mark onto the top of this
   * stack. Thus, the top of the stack always represents the current in-scope
   * PoU.
   *
   * Discriminators/initiated content/etc. can resolve the current in-scope PoU
   * by calling resolvePointOfUncertainty(). This simply pops the in-scope PoU
   * off the top of the stack and discards it, allowing associated memory to be
   * freed. By immediately popping off the stack, this allows multiple
   * discriminators to resolve multiple PoU's up the stack.
   *
   * Upon returning from any child parsers, the parser that called
   * withPointOfUncertainty must be able to determine if some other child
   * parser resolved its PoU. To do so, it only needs to inspect the top value
   * of this stack. There are only two potential values:
   *
   * 1. The top of the stack is the same as the PoU that was provided by
   *    withPointOfUncertainty. This implies nothing popped it off the stack,
   *    and thus nothing resolved it. This pou was not resolved by a
   *    discriminator. In this case the parser may choose to discard or reset
   *    to the PoU.
   *
   * 2. The top of the stack is something different than the PoU that was
   *    provided by withPointOfUncertainty. This implies that something did pop
   *    it off the stack, which means something must have resolved it. This PoU
   *    was resolved by a discriminator. In this case, it is an error if the
   *    parser attempts to discard or reset to the pou.
   *
   * The isPointOfUncertaintyResolved() function performs this logic so parsers
   * do not need to worry about this implementation detail. Also provided are
   * discard/resetTo/resolvePointOfUncertainty functions to simplify this
   * logic.
   */
  val pointsOfUncertainty = new MStackOf[PState.Mark]()

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
  def thisElement: DIElement = infoset

  override def groupPos = mpstate.groupPos
  override def arrayIterationPos = mpstate.arrayIterationPos
  override def occursPos = mpstate.occursPos
  override def childPos = mpstate.childPos

  private val markPool = new PState.MarkPool

  private def mark(requestorID: String, context: RuntimeData): PState.Mark = {
    // threadCheck()
    val m = markPool.getFromPool(requestorID)
    m.captureFrom(this, requestorID, context)
    m.element.infosetWalkerBlockCount += 1
    m
  }

  def isInUse(m: PState.Mark) = {
    markPool.isInUse(m)
  }

  def dataInputStreamIsValid = dataInputStream.inputSource.isValid

  private def reset(m: PState.Mark): Unit = {
    // threadCheck()
    m.element.infosetWalkerBlockCount -= 1
    m.restoreInto(this)
    m.clear()
    markPool.returnToPool(m)
  }

  private def discard(m: PState.Mark): Unit = {
    m.element.infosetWalkerBlockCount -= 1
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
    new DataLoc(bitPos1b, bitLimit1b, Right(dataInputStream), Maybe(thisElement.runtimeData))
  }

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
  def parentDocument = infoset.asInstanceOf[api.InfosetDocument]

  def setEndBitLimit(bitLimit0b: Long): Unit = {
    dataInputStream.setBitLimit0b(MaybeULong(bitLimit0b))
  }

  /**
   * Change the current infoset element that we are modifying and the last
   * child of that element that has been added to that infoset element.
   *
   * This takes newInfoset as a DIElement, not DIComplex, because 'infoset'
   * represents the current DIElement that Daffodil is working on, which could
   * be either a complex or simple element.
   *
   * This also expects the last element that has been added as a child of
   * newInfoset, sometimes needed for logic related to separated content.
   *
   * When starting a new infoset element, this function should be called with
   * the new element passed as newInfoset (since it is the current infoset
   * element being modified) and newInfosetLastChild set to Nope (because this
   * new element does not have any children yet).
   *
   * When ending an element, this function should be called with the elements
   * parent as newInfoset (because we are now modifying the parent, e.g.
   * adding more children), and newInfosetLastChild should be set to the element
   * we just ended (because it is the last child added to newInfoset).
   *
   * Note that we must keep track of and store infosetLastChild because by the
   * time Daffodil needs information about the last child added, the child
   * could have been released by the InfosetWalker and no longer actually be
   * part of the infoset. So we cannot query the infoset for this information,
   * but must instead store it in, and retrieve it from, the PState.
   */
  def setInfoset(newInfoset: DIElement, newInfosetLastChild: Maybe[DIElement]): Unit = {
    this.infoset = newInfoset
    this.infosetLastChild = newInfosetLastChild
  }

  /**
   * When we take marks of this PState when we enter a PoU, we intentionally do
   * not take a deep copy of the variable map because that is too expensive of
   * an operation for a data structure that rarely changes. So taking a mark
   * just makes a shallow copy of the PState variable map.
   *
   * But this means modifications of the variable map could potentially affect
   * PoU marks, which means resetting back to a mark does not work as intended.
   * To resolve this issue, anytime a function is called that will change the
   * state of a variable, we must first call this function. This will take a
   * deep copy of the variable map and set that copy as the variable map for
   * this PState. This way, the state already captured in the mark will not be
   * modified and we can correctly reset back to that state when resetting the
   * PoU. Note that we only do this if we have not already done a deep copy in
   * this PoU--if we've already done a deep copy, we don't need to do it again
   * since the PoU copy can't be modified.
   */
  @inline
  private def changingVariable(): Unit = {
    if (!pointsOfUncertainty.isEmpty) {
      val curPoU = pointsOfUncertainty.top
      if (curPoU.variableMap eq this.variableMap) {
        this.setVariableMap(this.variableMap.copy())
      }
    }
  }

  override def setVariable(
    vrd: VariableRuntimeData,
    newValue: DataValuePrimitive,
    referringContext: ThrowsSDE
  ): Unit = {
    changingVariable()
    variableMap.setVariable(vrd, newValue, referringContext, this)
  }

  override def getVariable(
    vrd: VariableRuntimeData,
    referringContext: ThrowsSDE
  ): DataValuePrimitive = {
    // Skip the call to changingVariable if this variable has already been
    // read, which means another read will not actually change the state. This
    // potentially avoids an expensive deep copy if we've read a variable,
    // entered a PoU, and then read that variable again
    if (variableMap.readVariableWillChangeState(vrd)) {
      changingVariable()
    }
    variableMap.readVariable(vrd, referringContext, this)
  }

  def newVariableInstance(vrd: VariableRuntimeData): VariableInstance = {
    changingVariable()
    variableMap.newVariableInstance(vrd)
  }

  def removeVariableInstance(vrd: VariableRuntimeData): Unit = {
    // we do not need to call changingVariable() here even though this changes
    // variable state, because newVariableInstance would have already called it
    variableMap.removeVariableInstance(vrd)
  }

  /**
   * This function creates a mark, which represents a point of uncertainty. This
   * function should never be used, and is only public so that the
   * withPointOfUncertainty function can access it. You should almost certainly
   * be using withPointOfUncertainty instead.
   */
  def createPointOfUncertainty(pouID: String, context: RuntimeData): PState.Mark = {
    val pou = mark(pouID, context)
    pointsOfUncertainty.push(pou)
    pou
  }

  /**
   * Reset to the point of uncertainty created by withPointOfUncertainty. This
   * also discards the point of uncertainty. Once called, the pou variable
   * should no longer be used. If it is possible a child parser resolved this
   * pou, one must first check the result of isPointOfUncertaintResolved--it is
   * an error to call this function if the PoU has been resolved.
   */
  def resetToPointOfUncertainty(pou: PState.Mark): Unit = {
    Assert.usage(!isPointOfUncertaintyResolved(pou))
    val pouPop = pointsOfUncertainty.pop
    Assert.invariant(pou == pouPop)
    reset(pouPop)
  }

  /**
   * Discard the point of uncertainty created by withPointOfUncertainty. Once
   * called, the pou variable should no longer be used. If the pou is not
   * reset, resolved, or discarded, this function is automatically called at
   * the end of func block in withPointOfUncertainty. If it is possible child
   * parser resolved this pou, one must first check the result of
   * isPointOfUncertaintResolved--it is an error to call this function if the
   * PoU has been resolved.
   */
  def discardPointOfUncertainty(pou: PState.Mark): Unit = {
    Assert.usage(!isPointOfUncertaintyResolved(pou))
    val pouPop = pointsOfUncertainty.pop
    Assert.invariant(pou == pouPop)
    discard(pou)
  }

  /**
   * Resolve the current in-scope point of uncertainty. Should only be called
   * by discriminators/initated content/etc.
   */
  def resolvePointOfUncertainty(): Unit = {
    if (!pointsOfUncertainty.isEmpty) {
      val pouPop = pointsOfUncertainty.pop
      discard(pouPop)
    }
  }

  /**
   * Determine if the PoU created in withPointOfUncertainty has been resolved by
   * a discrminator.
   */
  def isPointOfUncertaintyResolved(pou: PState.Mark): Boolean = {
    pointsOfUncertainty.isEmpty || pointsOfUncertainty.top != pou
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
          if (mcboc.isDefined)
            mcboc.get.evaluate(
              this
            ) // Expressions must be evaluated on the element, not before it is created.
          if (mcbbo.isDefined) mcbbo.get.evaluate(this)
        }
        case _ => // ok
      }

      dis.cst.setPriorBitOrder(this.bitOrder)
      if (!dis.isAligned(8))
        SDE(
          "Can only change dfdl:bitOrder on a byte boundary. Bit pos (1b) was %s.",
          dis.bitPos1b
        )
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

  override lazy val (regexMatchBuffer, regexMatchBitPositionBuffer) =
    dataProcArg.regexMatchState.get

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
        Assert.invariant(this.pointsOfUncertainty.length == 0)
        Assert.invariant(!this.withinHiddenNest) // ensure we are not in hidden nest
        mpstate.verifyFinalState()
      }
      // These we check regardless of throw or not.
      markPool.finalCheck()
      dataInputStream.inputSource.compact() // discard any storage that can be freed.
      dataInputStream.validateFinalStreamState()
    } catch {
      case e: Throwable => {
        if (optThrown.isDefined) e.addSuppressed(optThrown.get)
        val toThrow = new Abort(e)
        toThrow.addSuppressed(e)
        throw toThrow
      }
    }
  }

  def suspensions = Seq.empty
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
        "bitPos: %s, context: %s [%s] (%s)".format(
          bitPos0b.toString,
          context.toString,
          context.locationDescription,
          poolDebugLabel
        )
      else
        "bitPos: (uninitialized), context: %s [%s] (%s)".format(
          context.toString,
          context.locationDescription,
          poolDebugLabel
        )
    }

    def bitPos0b = disMark.bitPos0b

    val simpleElementState = DISimpleState()
    val complexElementState = DIComplexState()
    var element: DIElement = _
    var elementLastChild: Maybe[DIElement] = _
    var disMark: DataInputStream.Mark = _
    var variableMap: VariableMap = _
    var processorStatus: ProcessorResult = _
    var validationStatus: Boolean = _
    var diagnostics: java.util.List[api.Diagnostic] = _
    var delimitedParseResult: Maybe[dfa.ParseResult] = Nope
    var blobPaths: Seq[Path] = Seq.empty
    var context: RuntimeData = _

    val mpStateMark = new MPState.Mark

    def clear(): Unit = {
      elementLastChild = Nope
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
      // DO NOT clear requestorId/context. It is there to help us debug if we try to repeatedly reset/discard a mark already discarded.
    }

    def captureFrom(ps: PState, requestorID: String, context: RuntimeData): Unit = {
      this.element = ps.thisElement
      this.elementLastChild = ps.infosetLastChild
      if (element.isSimple)
        simpleElementState.captureFrom(element)
      else
        complexElementState.captureFrom(element)
      this.disMark = ps.dataInputStream.mark(requestorID)
      this.processorStatus = ps.processorStatus
      this.validationStatus = ps.validationStatus
      this.diagnostics = ps.diagnostics
      this.mpStateMark.captureFrom(ps.mpstate)
      this.blobPaths = ps.blobPaths
      this.context = context

      // Note that this is intentionally a shallow copy. This normally would
      // not work because the variable map is mutable so other state changes
      // could mutate this snapshot. This is avoided by carefully changing the
      // PState variable map to a deep copy of this variable map right before a
      // change is made. This essentially makes the PState variable map behave
      // as copy-on-write.
      this.variableMap = ps.variableMap
    }

    def restoreInfoset(ps: PState) = {
      Assert.invariant(this.element eq ps.thisElement)
      this.element match {
        case s: DISimple => simpleElementState.restoreInto(this.element)
        case c: DIComplex => complexElementState.restoreInto(this.element)
      }
    }

    def restoreInto(ps: PState): Unit = {
      restoreInfoset(ps)
      ps.infosetLastChild = this.elementLastChild
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
    output: api.infoset.InfosetOutputter,
    dataProc: DFDL.DataProcessor,
    areDebugging: Boolean
  ): PState = {

    val tunables = dataProc.tunables
    val doc = Infoset.newDocument(root).asInstanceOf[DIElement]
    createInitialPState(
      doc.asInstanceOf[api.InfosetDocument],
      root,
      dis,
      output,
      dataProc,
      areDebugging
    )
  }

  /**
   * For testing, we can pass in the Infoset pre-constructed.
   */
  def createInitialPState(
    doc: api.InfosetDocument,
    root: ElementRuntimeData,
    dis: InputSourceDataInputStream,
    output: api.infoset.InfosetOutputter,
    dataProc: DFDL.DataProcessor,
    areDebugging: Boolean
  ): PState = {

    /**
     * This is a full deep copy as variableMap is mutable. Reusing
     * dataProc.VariableMap without a copy would not be thread safe.
     */
    val variables = dataProc.variableMap.copy()

    val diagnostics = Collections.emptyList[api.Diagnostic]()
    val mutablePState = MPState()
    val tunables = dataProc.tunables
    val infosetWalker = InfosetWalker(
      doc.asInstanceOf[DIElement],
      output,
      walkHidden = false,
      ignoreBlocks = false,
      releaseUnneededInfoset = !areDebugging && tunables.releaseUnneededInfoset,
      walkSkipMin = tunables.infosetWalkerSkipMin,
      walkSkipMax = tunables.infosetWalkerSkipMax
    )

    dis.cst.setPriorBitOrder(root.defaultBitOrder)
    val newState = new PState(
      doc.asInstanceOf[DIElement],
      Nope,
      dis,
      infosetWalker,
      variables,
      diagnostics,
      mutablePState,
      dataProc.asInstanceOf[DataProcessor],
      Nope,
      Seq.empty,
      tunables
    )
    newState
  }

  /**
   * For testing it is convenient to just hand it strings for data.
   */
  def createInitialPState(
    root: ElementRuntimeData,
    data: String,
    output: api.infoset.InfosetOutputter,
    dataProc: DFDL.DataProcessor,
    areDebugging: Boolean
  ): PState = {
    val in = InputSourceDataInputStream(data.getBytes(StandardCharsets.UTF_8))
    createInitialPState(root, in, output, dataProc, areDebugging)
  }

  /**
   * Construct our InStream object and initialize the state block.
   */
  def createInitialPState(
    root: ElementRuntimeData,
    input: java.nio.channels.ReadableByteChannel,
    output: api.infoset.InfosetOutputter,
    dataProc: DFDL.DataProcessor,
    areDebugging: Boolean
  ): PState = {
    val dis = InputSourceDataInputStream(Channels.newInputStream(input))
    createInitialPState(root, dis, output, dataProc, areDebugging)
  }
}
