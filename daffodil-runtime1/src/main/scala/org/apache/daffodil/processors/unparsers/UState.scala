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

package org.apache.daffodil.processors.unparsers

import java.io.ByteArrayOutputStream
import java.nio.CharBuffer
import java.nio.LongBuffer

import scala.Left
import scala.collection.mutable

import org.apache.daffodil.api.DFDL
import org.apache.daffodil.api.DaffodilTunables
import org.apache.daffodil.api.DataLocation
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.dpath.UnparserBlocking
import org.apache.daffodil.dsom.RuntimeSchemaDefinitionError
import org.apache.daffodil.equality.EqualitySuppressUnusedImportWarning
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.exceptions.SavesErrorsAndWarnings
import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.infoset.DIArray
import org.apache.daffodil.infoset.DIDocument
import org.apache.daffodil.infoset.DIElement
import org.apache.daffodil.infoset.DINode
import org.apache.daffodil.infoset.InfosetAccessor
import org.apache.daffodil.infoset.InfosetInputter
import org.apache.daffodil.io.DirectOrBufferedDataOutputStream
import org.apache.daffodil.io.StringDataInputStreamForUnparse
import org.apache.daffodil.processors.DataLoc
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.processors.DelimiterStackUnparseNode
import org.apache.daffodil.processors.EscapeSchemeUnparserHelper
import org.apache.daffodil.processors.Failure
import org.apache.daffodil.processors.NonTermRuntimeData
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.processors.Suspension
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.processors.UnparseResult
import org.apache.daffodil.processors.VariableBox
import org.apache.daffodil.processors.VariableMap
import org.apache.daffodil.processors.VariableRuntimeData
import org.apache.daffodil.processors.charset.BitsCharset
import org.apache.daffodil.processors.charset.BitsCharsetDecoder
import org.apache.daffodil.processors.charset.BitsCharsetEncoder
import org.apache.daffodil.processors.dfa.DFADelimiter
import org.apache.daffodil.util.Cursor
import org.apache.daffodil.util.LocalStack
import org.apache.daffodil.util.MStackOf
import org.apache.daffodil.util.MStackOfLong
import org.apache.daffodil.util.MStackOfMaybe
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe.Nope
import org.apache.daffodil.util.Maybe.One

object ENoWarn { EqualitySuppressUnusedImportWarning() }

abstract class UState(
  dos: DirectOrBufferedDataOutputStream,
  vbox: VariableBox,
  diagnosticsArg: List[Diagnostic],
  dataProcArg: Maybe[DataProcessor],
  tunable: DaffodilTunables)
  extends ParseOrUnparseState(vbox, diagnosticsArg, dataProcArg, tunable)
  with Cursor[InfosetAccessor] with ThrowsSDE with SavesErrorsAndWarnings {

  /**
   * Push onto the dynamic TRD context stack
   */
  def pushTRD(trd: TermRuntimeData): Unit

  /**
   * Returns the top of the stack if it exists. No state change to stack contents.
   */
  def maybeTopTRD(): Maybe[TermRuntimeData]

  /**
   * Pop the dynamic TRD context stack. The popped TRD should be the same as the argument rd.
   * The popped TRD is returned.
   */
  def popTRD(trd: TermRuntimeData): TermRuntimeData

  // def unparse1(unparser: Unparser): Unit

  override def toString = {
    val elt = if (this.currentInfosetNodeMaybe.isDefined) "node=" + this.currentInfosetNode.toString else ""
    "UState(" + elt + " DOS=" + dataOutputStream.toString() + ")"
  }

  var dataOutputStream: DirectOrBufferedDataOutputStream = dos

  def prior: UStateForSuspension
  def currentInfosetNode: DINode
  def currentInfosetNodeMaybe: Maybe[DINode]
  def escapeSchemeEVCache: MStackOfMaybe[EscapeSchemeUnparserHelper]
  def setVariables(newVariableMap: VariableMap): Unit

  // def charBufferDataOutputStream: LocalStack[CharBufferDataOutputStream]
  def withUnparserDataInputStream: LocalStack[StringDataInputStreamForUnparse]
  def withByteArrayOutputStream: LocalStack[(ByteArrayOutputStream, DirectOrBufferedDataOutputStream)]

  def allTerminatingMarkup: List[DFADelimiter]
  def localDelimiters: DelimiterStackUnparseNode
  def pushDelimiters(node: DelimiterStackUnparseNode): Unit
  def popDelimiters(): Unit

  def currentInfosetNodeStack: MStackOfMaybe[DINode]
  def arrayIndexStack: MStackOfLong
  def childIndexStack: MStackOfLong
  def groupIndexStack: MStackOfLong
  def moveOverOneArrayIndexOnly(): Unit
  def moveOverOneGroupIndexOnly(): Unit
  def moveOverOneElementChildOnly(): Unit

  def inspectOrError: InfosetAccessor
  def advanceOrError: InfosetAccessor
  def isInspectArrayEnd: Boolean

  override def dataStream = Maybe(dataOutputStream)

  override def currentNode = currentInfosetNodeMaybe

  override def hasInfoset = currentInfosetNodeMaybe.isDefined

  override def infoset = {
    Assert.invariant(Maybe.WithNulls.isDefined(currentInfosetNode))
    currentInfosetNode match {
      case a: DIArray => {
        a.getOccurrence(arrayPos)
      }
      case e: DIElement => thisElement
    }
  }

  override def thisElement: DIElement = {
    Assert.usage(Maybe.WithNulls.isDefined(currentInfosetNode))
    val curNode = currentInfosetNode
    curNode match {
      case e: DIElement => e
      case a: DIArray => a.parent
    }
  }

  private def maybeCurrentInfosetElement: Maybe[DIElement] = {
    if (!Maybe.WithNulls.isDefined(currentInfosetNode)) Nope
    else {
      currentInfosetNode match {
        case e: DIElement => One(e)
        case a: DIArray => Nope
      }
    }
  }

  def currentLocation: DataLocation = {
    val m = maybeCurrentInfosetElement
    val mrd = if (m.isDefined) Maybe(m.value.runtimeData) else Nope
    val isAtEnd = false // TODO: this isn't right, but what does it mean to be at the end? Nothing appears to use this value when unparsing
    new DataLoc(bitPos1b, bitLimit1b, isAtEnd, Left(dataOutputStream), mrd)
  }

  lazy val unparseResult = new UnparseResult(dataProc.get, this)

  def bitPos0b = if (dataOutputStream.maybeAbsBitPos0b.isDefined) dataOutputStream.maybeAbsBitPos0b.get else 0L

  def bitLimit0b = dataOutputStream.maybeRelBitLimit0b

  def charPos = -1L

  final def notifyDebugging(flag: Boolean): Unit = {
    dataOutputStream.setDebugging(flag)
  }

  def addUnparseError(ue: UnparseError): Unit = {
    diagnostics = ue :: diagnostics
    _processorStatus = new Failure(ue)
  }

  /**
   * Checks for legal bitOrder change (byte boundary required), or splits the
   * DOS so that the check will occur later when they are collapsed back together.
   *
   * If you think about it, the only way we could not have the absoluteBitPos is
   * because something variable-length preceded us, and couldn't be computed due
   * to suspended computation (forward referencing expression somewhere prior).
   *
   * At some point, that suspension will get resolved, and forward collapsing of
   * the DataOutputStreams will occur. When it encounters a split created here,
   * we already know that the bit orders are different (or we wouldn't have put in
   * the split), so we just have to see if we're on a byte boundary. That could happen
   * if the original DOS ended in a frag byte, but previous to it, was something
   * that was variable bits wide (all bits shift such that original DOS's frag byte
   * becomes a whole byte.)
   *
   * The invariant here is that the original DOS will get collapsed together with
   * DOS preceding it. After that collapsing, it has to end at a byte boundary (no
   * frag byte). If it doesn't then it's a bit order-change error. Otherwise
   * we're ok.
   *
   * This is why we can always proceed with a new buffered DOS, knowing we're
   * going to be on a byte boundary with the bit order needed.
   */
  final override protected def checkBitOrder(): Unit = {
    //
    // Check for bitOrder change. If yes, then unless we know we're byte aligned
    // we must split the DOS until we find out. That way the new buffered DOS
    // can be assumed to be byte aligned (which will be checked on combining),
    // and the bytes in it will actually start out byte aligned.
    //
    val dos = this.dataOutputStream
    val isChanging = isUnparseBitOrderChanging(dos)
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
          if (mcboc.isDefined) mcboc.get.evaluate(this)
          if (mcbbo.isDefined) mcbbo.get.evaluate(this)
        }
        case _ => // ok
      }

      // TODO: Figure out why this setPriorBitOrder is needed here.
      // If we remove it, then test_ep2 (an envelope-payload test with
      // bigEndian MSBF envelope and littleEndian LSBF payload)
      // fails with Assert.invariant(isWritable)
      // when writing a long. The buffered DOS it is writing to is finished.
      //
      // It's unclear why setting the prior bit order here affects whether
      // a DOS is active or finished elsewhere, but it does.
      //
      val bo = this.bitOrder // will NOT recurse back to here. It *will* hit cache.
      dos.setPriorBitOrder(bo)

      // If we can't check right now because we don't have absolute bit position
      // then split the DOS so it gets checked later.
      //
      splitOnUknownByteAlignmentBitOrderChange(dos)
    }
  }

  private def isUnparseBitOrderChanging(dos: DirectOrBufferedDataOutputStream): Boolean = {
    val ctxt = this.processor.context
    ctxt match {
      case ntrd: NonTermRuntimeData => false
      case _ => {
        val priorBitOrder = dos.priorBitOrder
        val newBitOrder = this.bitOrder
        priorBitOrder ne newBitOrder
      }
    }
  }

  /**
   *  If necessary, split DOS so bitOrder proper byte boundary is checked later.
   *
   *  If we can't check because of unknown absolute bit position,
   *  then we split the DOS, start a new buffering one (assumed to be
   *  byte aligned, with the new bitOrder).
   *
   *  The bit order would not be unknown except that something of
   *  variable length precedes us and is suspended.
   *  When that eventually is resolved, then the DOS will collapse forward
   *  and the boundary between the original (dos here), and the buffered
   *  one will be checked as part of the collapsing logic.
   *
   *  That is, this split does NOT queue a suspension object, it
   *  Just inserts a split in the DOS. This gets put together later when
   *  the DOS are collapsed together, and the check for byte boundary occurs
   *  at that time.
   */
  private def splitOnUknownByteAlignmentBitOrderChange(dos: DirectOrBufferedDataOutputStream): Unit = {
    val mabp = dos.maybeAbsBitPos0b
    val mabpDefined = mabp.isDefined
    val isSplitNeeded: Boolean = {
      if (mabpDefined && dos.isAligned(8)) {
        //
        // Not only do we have to be logically aligned, we also have
        // to be physically aligned in the buffered stream, otherwise we
        // cannot switch bit orders, and we have to split off a new
        // stream to start the accumulation of the new bit-order material.
        //
        // fragmentLastByteLimit == 0 means there is no fragment byte,
        // which only happens if we're on a byte boundary in the implementation.
        //
        if (dos.fragmentLastByteLimit == 0) false
        else true
      } else if (!mabpDefined) true
      else {
        // mabp is defined, and we're not on a byte boundary
        // and the bit order is changing.
        // Error: bit order change on non-byte boundary
        val bp1b = mabp.get + 1
        SDE("Can only change dfdl:bitOrder on a byte boundary. Bit pos (1b) was %s. Should be 1 mod 8, was %s (mod 8)", bp1b, bp1b % 8)
      }
    }
    if (isSplitNeeded) {
      Assert.invariant(dos.isBuffering) // Direct DOS always has absolute position, so has to be buffering.
      val newDOS = dos.addBuffered
      dataOutputStream = newDOS
      //
      // Just splitting to start a new bitOrder on a byte boundary in a new
      // buffered DOS
      // So the prior DOS can be finished. Nothing else will be added to it.
      //
      // Note: unlike a suspension, in this case, we're not going to write anything
      // more to the end of that DOS. A bitOrder change occurs before we get to
      // any such content being unparsed, or suspended. So after a bitOrder change,
      // the unparsing occurs, possibly buffered, and works as if the
      // bitOrder change was legal and happened, even though we cannot know yet
      // if that is the case, and it will get checked later.
      //
      // Finished means you won't add data to the end of it any more.
      // It does NOT prevent information like the absoluteBitPos to
      // propagate.
      dos.setFinished(this)
    }
  }

  def regexMatchBuffer: CharBuffer = Assert.usageError("Not to be used.")
  def regexMatchBitPositionBuffer: LongBuffer = Assert.usageError("Not to be used.")

  def documentElement: DIDocument

  def newVariableInstance(vrd: VariableRuntimeData): Unit = {
    variableMap.newVariableInstance(vrd)
  }

  def removeVariableInstance(vrd: VariableRuntimeData): Unit = {
    variableMap.removeVariableInstance(vrd)
  }
}

/**
 * When we create a suspension during unparse, we need to clone the UStateMain
 * for when the suspension is later resumed. However, we do not need nearly as
 * much information for these cloned ustates as the main unparse. Either we can
 * access the necessary information directly from the main UState, or the
 * information isn't used and there's no need to copy it/take up valuable
 * memory.
 */
final class UStateForSuspension(
  val mainUState: UStateMain,
  dos: DirectOrBufferedDataOutputStream,
  vbox: VariableBox,
  override val currentInfosetNode: DINode,
  occursIndex: Long,
  escapeSchemeEVCacheMaybe: Maybe[MStackOfMaybe[EscapeSchemeUnparserHelper]],
  delimiterStackMaybe: Maybe[MStackOf[DelimiterStackUnparseNode]],
  override val prior: UStateForSuspension,
  tunable: DaffodilTunables)
  extends UState(dos, vbox, mainUState.diagnostics, mainUState.dataProc, tunable) {

  dState.setMode(UnparserBlocking)
  dState.setCurrentNode(thisElement.asInstanceOf[DINode])
  dState.setContextNode(thisElement.asInstanceOf[DINode])
  dState.setVBox(vbox)
  dState.setErrorOrWarn(this)

  private def die = Assert.invariantFailed("Function should never be needed in UStateForSuspension")

  override def getDecoder(cs: BitsCharset): BitsCharsetDecoder = mainUState.getDecoder(cs)
  override def getEncoder(cs: BitsCharset): BitsCharsetEncoder = mainUState.getEncoder(cs)

  // override def charBufferDataOutputStream = mainUState.charBufferDataOutputStream
  override def withUnparserDataInputStream = mainUState.withUnparserDataInputStream
  override def withByteArrayOutputStream = mainUState.withByteArrayOutputStream

  override def advance: Boolean = die
  override def advanceAccessor: InfosetAccessor = die
  override def inspect: Boolean = die
  override def inspectAccessor: InfosetAccessor = die
  override def fini: Unit = {
    //do nothing
  }
  override def inspectOrError = die
  override def advanceOrError = die
  override def isInspectArrayEnd = die

  override def currentInfosetNodeStack = die
  override def currentInfosetNodeMaybe = Maybe(currentInfosetNode)

  override def arrayIndexStack = die
  override def moveOverOneArrayIndexOnly() = die
  override def arrayPos = occursIndex

  override def groupIndexStack = die
  override def moveOverOneGroupIndexOnly() = die
  override def groupPos = 0 // was die, but this is called when copying state during debugging

  override def childIndexStack = die
  override def moveOverOneElementChildOnly() = die
  override def childPos = 0 // was die, but this is called when copying state during debugging.

  override def pushDelimiters(node: DelimiterStackUnparseNode) = die
  override def popDelimiters() = die
  override def localDelimiters = delimiterStackMaybe.get.top
  override def allTerminatingMarkup = {
    delimiterStackMaybe.get.iterator.flatMap { dnode =>
      dnode.separator ++ dnode.terminator
    }.toList
  }

  override def escapeSchemeEVCache: MStackOfMaybe[EscapeSchemeUnparserHelper] = escapeSchemeEVCacheMaybe.get

  override def setVariables(newVariableMap: VariableMap) = die

  override def pushTRD(trd: TermRuntimeData): Unit = die
  override def maybeTopTRD() = die
  override def popTRD(trd: TermRuntimeData): TermRuntimeData = die

  override def documentElement = mainUState.documentElement

  override def incrementHiddenDef = Assert.usageError("Unparser suspended UStates need not be aware of hidden contexts")
  override def decrementHiddenDef = Assert.usageError("Unparser suspended UStates need not be aware of hidden contexts")
  override def withinHiddenNest = Assert.usageError("Unparser suspended UStates need not be aware of hidden contexts")

}

final class UStateMain private (
  private val inputter: InfosetInputter,
  vbox: VariableBox,
  diagnosticsArg: List[Diagnostic],
  dataProcArg: DataProcessor,
  dos: DirectOrBufferedDataOutputStream,
  initialSuspendedExpressions: mutable.Queue[Suspension],
  tunable: DaffodilTunables)
  extends UState(dos, vbox, diagnosticsArg, One(dataProcArg), tunable) {

  dState.setMode(UnparserBlocking)

  def this(
    inputter: InfosetInputter,
    vmap: VariableMap,
    diagnosticsArg: List[Diagnostic],
    dataProcArg: DataProcessor,
    dataOutputStream: DirectOrBufferedDataOutputStream,
    initialSuspendedExpressions: mutable.Queue[Suspension],
    tunable: DaffodilTunables) =
    this(inputter, new VariableBox(vmap), diagnosticsArg, dataProcArg,
      dataOutputStream, initialSuspendedExpressions, tunable)

  private var _prior: UStateForSuspension = null
  override def prior = _prior

  def cloneForSuspension(suspendedDOS: DirectOrBufferedDataOutputStream): UState = {
    val es =
      if (!escapeSchemeEVCache.isEmpty) {
        // If there are any escape schemes, then we need to clone the whole
        // MStack, since the escape scheme cache logic requires an MStack. We
        // reallyjust need the top for cloning for suspensions, but that
        // requires changes to how the escape schema cache is accessed, which
        // isn't a trivial change.
        val esClone = new MStackOfMaybe[EscapeSchemeUnparserHelper]()
        esClone.copyFrom(escapeSchemeEVCache)
        Maybe(esClone)
      } else {
        Nope
      }
    val ds =
      if (!delimiterStack.isEmpty) {
        // If there are any delimiters, then we need to clone them all since
        // they may be needed for escaping
        val dsClone = new MStackOf[DelimiterStackUnparseNode]()
        dsClone.copyFrom(delimiterStack)
        Maybe(dsClone)
      } else {
        Nope
      }

    val clone = new UStateForSuspension(
      this,
      suspendedDOS,
      variableBox,
      currentInfosetNodeStack.top.get, // only need the to of the stack, not the whole thing
      arrayIndexStack.top, // only need the top of the stack, not the whole thing
      es,
      ds,
      prior,
      tunable)

    clone.setProcessor(processor)

    this._prior = clone
    clone
  }

  override lazy val withUnparserDataInputStream = new LocalStack[StringDataInputStreamForUnparse](new StringDataInputStreamForUnparse)
  override lazy val withByteArrayOutputStream = new LocalStack[(ByteArrayOutputStream, DirectOrBufferedDataOutputStream)](
    {
      val baos = new ByteArrayOutputStream() // TODO: PERFORMANCE: Allocates new object. Can reuse one from an onStack/pool via reset()
      val dos = DirectOrBufferedDataOutputStream(
        baos,
        null,
        false,
        tunable.outputStreamChunkSizeInBytes,
        tunable.maxByteArrayOutputStreamBufferSizeInBytes,
        tunable.tempFilePath)
      (baos, dos)
    },
    pair => pair match {
      case (baos, dos) =>
        baos.reset()
        dos.resetAllBitPos()
    })

  override def advance: Boolean = inputter.advance
  override def advanceAccessor: InfosetAccessor = inputter.advanceAccessor
  override def inspect: Boolean = inputter.inspect
  override def inspectAccessor: InfosetAccessor = inputter.inspectAccessor
  override def fini: Unit = { inputter.fini }

  /**
   * Use this so if there isn't an event we get a clean diagnostic message saying
   * that is what has gone wrong.
   */
  override def inspectOrError = {
    val m = inspectMaybe
    if (m.isEmpty) Assert.invariantFailed("An InfosetEvent was required for unparsing, but no InfosetEvent was available.")
    m.get
  }

  override def advanceOrError = {
    val m = advanceMaybe
    if (m.isEmpty) Assert.invariantFailed("An InfosetEvent was required for unparsing, but no InfosetEvent was available.")
    m.get
  }

  override def isInspectArrayEnd = {
    if (!inspect) false
    else {
      val p = inspectAccessor
      val res = p match {
        case e if e.isEnd && e.isArray => true
        case _ => false
      }
      res
    }
  }

  def currentInfosetNode: DINode =
    if (currentInfosetNodeMaybe.isEmpty) null
    else currentInfosetNodeMaybe.get

  def currentInfosetNodeMaybe: Maybe[DINode] =
    if (currentInfosetNodeStack.isEmpty) Nope
    else currentInfosetNodeStack.top

  override val currentInfosetNodeStack = new MStackOfMaybe[DINode]

  override val arrayIndexStack = MStackOfLong()
  arrayIndexStack.push(1L)
  override def moveOverOneArrayIndexOnly() = arrayIndexStack.push(arrayIndexStack.pop + 1)
  override def arrayPos = arrayIndexStack.top

  override val groupIndexStack = MStackOfLong()
  groupIndexStack.push(1L)
  override def moveOverOneGroupIndexOnly() = groupIndexStack.push(groupIndexStack.pop + 1)
  override def groupPos = groupIndexStack.top

  // TODO: it doesn't look anything is actually reading the value of childindex
  // stack. Can we get rid of it?
  override val childIndexStack = MStackOfLong()
  childIndexStack.push(1L)
  override def moveOverOneElementChildOnly() = childIndexStack.push(childIndexStack.pop + 1)
  override def childPos = childIndexStack.top

  override lazy val escapeSchemeEVCache = new MStackOfMaybe[EscapeSchemeUnparserHelper]

  val delimiterStack = new MStackOf[DelimiterStackUnparseNode]()
  override def pushDelimiters(node: DelimiterStackUnparseNode) = delimiterStack.push(node)
  override def popDelimiters() = delimiterStack.pop
  override def localDelimiters = delimiterStack.top
  override def allTerminatingMarkup = {
    delimiterStack.iterator.flatMap { dnode =>
      dnode.separator ++ dnode.terminator
    }.toList
  }

  override def setVariables(newVariableMap: VariableMap) = {
    setVariableMap(newVariableMap)
  }

  /**
   * For outputValueCalc we accumulate the suspendables here.
   *
   * Note: only the primary UState (the initial one) will use this.
   * All the other clones used for outputValueCalc, those never
   * need to add any.
   */
  private val suspensions = initialSuspendedExpressions

  def addSuspension(se: Suspension): Unit = {
    suspensions.enqueue(se)
  }

  def evalSuspensions(ustate: UState): Unit = {
    var countOfNotMakingProgress = 0
    while (!suspensions.isEmpty &&
      countOfNotMakingProgress < suspensions.length) {
      val se = suspensions.dequeue
      se.runSuspension()
      if (!se.isDone) suspensions.enqueue(se)
      if (se.isDone || se.isMakingProgress)
        countOfNotMakingProgress = 0
      else
        countOfNotMakingProgress += 1
    }
    // after the loop, did we terminate
    // with some expressions still unevaluated?
    if (suspensions.length > 1) {
      // unable to evaluate all the expressions
      suspensions.map { sus =>
        sus.runSuspension() // good place for a breakpoint so we can debug why things are locked up.
        sus.explain()
      }
      System.err.println("Dump of UStates")
      var us = ustate
      while (us ne null) {
        System.err.println(us)
        us = us.prior
      }

      throw new SuspensionDeadlockException(suspensions.seq)
    } else if (suspensions.length == 1) {
      Assert.invariantFailed("Single suspended expression making no forward progress. " + suspensions(0))
    }
  }

  final override def pushTRD(trd: TermRuntimeData) =
    inputter.pushTRD(trd)

  final override def maybeTopTRD(): Maybe[TermRuntimeData] =
    inputter.maybeTopTRD()

  final override def popTRD(trd: TermRuntimeData) = {
    val poppedTRD = inputter.popTRD()
    if (poppedTRD ne trd)
      Assert.invariantFailed("TRDs do not match. Expected: " + trd + " got " + poppedTRD)
    poppedTRD
  }

  final override def documentElement = inputter.documentElement

  override def toString = {
    val elt = if (this.currentInfosetNodeMaybe.isDefined) "node=" + this.currentInfosetNode.toString else ""
    val hidden = if (withinHiddenNest) " hidden" else ""
    "UState(" + elt +  hidden + " DOS=" + dataOutputStream.toString() + ")"
  }
}

class SuspensionDeadlockException(suspExprs: Seq[Suspension])
  extends RuntimeSchemaDefinitionError(
    suspExprs(0).rd.schemaFileLocation,
    suspExprs(0).savedUstate,
    "Expressions/Unparsers are circularly deadlocked (mutually defined):\n%s",
    suspExprs.groupBy { _.rd }.mapValues { _(0) }.values.mkString(" - ", "\n - ", ""))

object UState {

  def createInitialUState(
    out: DirectOrBufferedDataOutputStream,
    dataProc: DFDL.DataProcessor,
    inputter: InfosetInputter): UStateMain = {
    Assert.invariant(inputter.isInitialized)

    /**
     * This is a full deep copy as variableMap is mutable. Reusing
     * dataProc.VariableMap without a copy would not be thread safe.
     */
    val variables = dataProc.variableMap.copy

    val diagnostics = Nil
    val newState = new UStateMain(inputter, variables, diagnostics, dataProc.asInstanceOf[DataProcessor], out,
      new mutable.Queue[Suspension], dataProc.getTunables()) // null means no prior UState
    newState
  }
}
