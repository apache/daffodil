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

package org.apache.daffodil.runtime1.processors.unparsers

import java.io.ByteArrayOutputStream
import java.nio.CharBuffer
import java.nio.LongBuffer

import org.apache.daffodil.io.DirectOrBufferedDataOutputStream
import org.apache.daffodil.io.StringDataInputStreamForUnparse
import org.apache.daffodil.io.processors.charset.BitsCharset
import org.apache.daffodil.io.processors.charset.BitsCharsetDecoder
import org.apache.daffodil.io.processors.charset.BitsCharsetEncoder
import org.apache.daffodil.lib.api.DaffodilTunables
import org.apache.daffodil.lib.api.DataLocation
import org.apache.daffodil.lib.api.Diagnostic
import org.apache.daffodil.lib.equality.EqualitySuppressUnusedImportWarning
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.SavesErrorsAndWarnings
import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.util.Cursor
import org.apache.daffodil.lib.util.LocalStack
import org.apache.daffodil.lib.util.MStackOf
import org.apache.daffodil.lib.util.MStackOfLong
import org.apache.daffodil.lib.util.MStackOfMaybe
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.runtime1.api.DFDL
import org.apache.daffodil.runtime1.dpath.UnparserBlocking
import org.apache.daffodil.runtime1.infoset.DIArray
import org.apache.daffodil.runtime1.infoset.DIDocument
import org.apache.daffodil.runtime1.infoset.DIElement
import org.apache.daffodil.runtime1.infoset.DINode
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.infoset.InfosetAccessor
import org.apache.daffodil.runtime1.infoset.InfosetInputter
import org.apache.daffodil.runtime1.processors.DataLoc
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.DelimiterStackUnparseNode
import org.apache.daffodil.runtime1.processors.EscapeSchemeUnparserHelper
import org.apache.daffodil.runtime1.processors.Failure
import org.apache.daffodil.runtime1.processors.NonTermRuntimeData
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.Suspension
import org.apache.daffodil.runtime1.processors.SuspensionTracker
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.UnparseResult
import org.apache.daffodil.runtime1.processors.VariableBox
import org.apache.daffodil.runtime1.processors.VariableInstance
import org.apache.daffodil.runtime1.processors.VariableMap
import org.apache.daffodil.runtime1.processors.VariableRuntimeData
import org.apache.daffodil.runtime1.processors.dfa.DFADelimiter

object ENoWarn { EqualitySuppressUnusedImportWarning() }

abstract class UState(
  vbox: VariableBox,
  diagnosticsArg: List[Diagnostic],
  dataProcArg: Maybe[DataProcessor],
  tunable: DaffodilTunables,
  areDebugging: Boolean
) extends ParseOrUnparseState(vbox, diagnosticsArg, dataProcArg, tunable)
  with Cursor[InfosetAccessor]
  with ThrowsSDE
  with SavesErrorsAndWarnings {

  final override def setVariable(
    vrd: VariableRuntimeData,
    newValue: DataValuePrimitive,
    referringContext: ThrowsSDE
  ) =
    vbox.vmap.setVariable(vrd, newValue, referringContext, this)

  /**
   * For unparsing, this throws a RetryableException in the case where the variable cannot (yet) be read.
   *
   * @param vrd Identifies the variable to read.
   * @param referringContext Where to place blame if there is an error.
   * @return The data value of the variable, or throws exceptions if there is no value.
   */
  final override def getVariable(
    vrd: VariableRuntimeData,
    referringContext: ThrowsSDE
  ): DataValuePrimitive =
    vbox.vmap.readVariable(vrd, referringContext, this)

  final override def newVariableInstance(vrd: VariableRuntimeData): VariableInstance =
    variableMap.newVariableInstance(vrd)

  final override def removeVariableInstance(vrd: VariableRuntimeData): Unit =
    variableMap.removeVariableInstance(vrd)

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

  override def toString = {
    val elt =
      if (this.currentInfosetNodeMaybe.isDefined) "node=" + this.currentInfosetNode.toString
      else ""
    "UState(" + elt + " DOS=" + dataOutputStream.toString() + ")"
  }

  var dataOutputStream: DirectOrBufferedDataOutputStream

  def currentInfosetNode: DINode
  def currentInfosetNodeMaybe: Maybe[DINode]
  def escapeSchemeEVCache: MStackOfMaybe[EscapeSchemeUnparserHelper]

  def withUnparserDataInputStream: LocalStack[StringDataInputStreamForUnparse]
  def withByteArrayOutputStream
    : LocalStack[(ByteArrayOutputStream, DirectOrBufferedDataOutputStream)]

  def allTerminatingMarkup: List[DFADelimiter]
  def localDelimiters: DelimiterStackUnparseNode
  def pushDelimiters(node: DelimiterStackUnparseNode): Unit
  def popDelimiters(): Unit

  def currentInfosetNodeStack: MStackOfMaybe[DINode]
  def arrayIterationIndexStack: MStackOfLong
  def occursIndexStack: MStackOfLong
  def childIndexStack: MStackOfLong
  def groupIndexStack: MStackOfLong
  def moveOverOneArrayIterationIndexOnly(): Unit
  def moveOverOneOccursIndexOnly(): Unit
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
        a(arrayIterationPos)
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
    new DataLoc(bitPos1b, bitLimit1b, Left(dataOutputStream), mrd)
  }

  lazy val unparseResult = new UnparseResult(dataProc.get, this)

  def bitPos0b = if (dataOutputStream.maybeAbsBitPos0b.isDefined)
    dataOutputStream.maybeAbsBitPos0b.get
  else 0L

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
  private def splitOnUknownByteAlignmentBitOrderChange(
    dos: DirectOrBufferedDataOutputStream
  ): Unit = {
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
        SDE(
          "Can only change dfdl:bitOrder on a byte boundary. Bit pos (1b) was %s. Should be 1 mod 8, was %s (mod 8)",
          bp1b,
          bp1b % 8
        )
      }
    }
    if (isSplitNeeded) {
      Assert.invariant(
        dos.isBuffering
      ) // Direct DOS always has absolute position, so has to be buffering.
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
      //
      // When setFinished is called the DOS is going to store the state that we
      // pass into it in finishedFormatInfo. Eventually this DOS will become a
      // direct DOS that may be delivered to a following buffered DOS. When that
      // happens this saved finishedFormatInfo will be used. However, this
      // requires that the UState does not change while we are waiting for this
      // DOS to become direct. If the state does change, it will become
      // incorrect and can lead to undefined behavior. To prevent this, we must
      // clone the UState so it can no longer change, and pass that clone into
      // setFinished.
      val finfo = this match {
        case m: UStateMain => m.cloneForSuspension(dos)
        case _ =>
          Assert.invariantFailed(
            "State must be a UStateMain when splitting for bit order change"
          )
      }

      val newDOS = dos.addBuffered()
      dataOutputStream = newDOS
      dos.setFinished(finfo)
    }
  }

  def regexMatchBuffer: CharBuffer = Assert.usageError("Not to be used.")
  def regexMatchBitPositionBuffer: LongBuffer = Assert.usageError("Not to be used.")

  def documentElement: DIDocument

  final val releaseUnneededInfoset: Boolean = !areDebugging && tunable.releaseUnneededInfoset

  def delimitedParseResult = Nope
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
  override var dataOutputStream: DirectOrBufferedDataOutputStream,
  vbox: VariableBox,
  override val currentInfosetNode: DINode,
  arrayIterationIndex: Long,
  occursIndex: Long,
  escapeSchemeEVCacheMaybe: Maybe[MStackOfMaybe[EscapeSchemeUnparserHelper]],
  delimiterStackMaybe: Maybe[MStackOf[DelimiterStackUnparseNode]],
  tunable: DaffodilTunables,
  areDebugging: Boolean
) extends UState(vbox, mainUState.diagnostics, mainUState.dataProc, tunable, areDebugging) {

  dState.setMode(UnparserBlocking)
  dState.setCurrentNode(thisElement.asInstanceOf[DINode])
  dState.setContextNode(thisElement.asInstanceOf[DINode])
  dState.setErrorOrWarn(this)

  private def die =
    Assert.invariantFailed("Function should never be needed in UStateForSuspension")

  override def getDecoder(cs: BitsCharset): BitsCharsetDecoder = mainUState.getDecoder(cs)
  override def getEncoder(cs: BitsCharset): BitsCharsetEncoder = mainUState.getEncoder(cs)

  override def suspensions = mainUState.suspensions

  // override def charBufferDataOutputStream = mainUState.charBufferDataOutputStream
  override def withUnparserDataInputStream = mainUState.withUnparserDataInputStream
  override def withByteArrayOutputStream = mainUState.withByteArrayOutputStream

  // $COVERAGE-OFF$
  override def advance: Boolean = die
  override def advanceAccessor: InfosetAccessor = die
  override def inspect: Boolean = die
  override def inspectAccessor: InfosetAccessor = die
  override def fini(): Unit = die
  override def inspectOrError = die
  override def advanceOrError = die
  override def isInspectArrayEnd = die
  override def currentInfosetNodeStack = die
  override def arrayIterationIndexStack = die
  override def moveOverOneArrayIterationIndexOnly() = die
  override def occursIndexStack = die
  override def moveOverOneOccursIndexOnly() = die
  override def groupIndexStack = die
  override def moveOverOneGroupIndexOnly() = die
  override def childIndexStack = die
  override def moveOverOneElementChildOnly() = die
  override def pushDelimiters(node: DelimiterStackUnparseNode) = die
  override def popDelimiters() = die
  // $COVERAGE-ON$

  override def groupPos = 0 // was die, but this is called when copying state during debugging
  override def currentInfosetNodeMaybe = Maybe(currentInfosetNode)
  override def arrayIterationPos = arrayIterationIndex
  override def occursPos = occursIndex
  override def childPos = 0 // was die, but this is called when copying state during debugging.

  override def localDelimiters = delimiterStackMaybe.get.top
  override def allTerminatingMarkup = {
    delimiterStackMaybe.get.iterator.flatMap { dnode =>
      dnode.separator ++ dnode.terminator
    }.toList
  }

  override def escapeSchemeEVCache: MStackOfMaybe[EscapeSchemeUnparserHelper] =
    escapeSchemeEVCacheMaybe.get

  override def pushTRD(trd: TermRuntimeData): Unit = die
  override def maybeTopTRD() = die
  override def popTRD(trd: TermRuntimeData): TermRuntimeData = die

  override def documentElement = mainUState.documentElement

  override def incrementHiddenDef() =
    Assert.usageError("Unparser suspended UStates need not be aware of hidden contexts")
  override def decrementHiddenDef() =
    Assert.usageError("Unparser suspended UStates need not be aware of hidden contexts")
  override def withinHiddenNest =
    Assert.usageError("Unparser suspended UStates need not be aware of hidden contexts")
}

final class UStateMain private (
  private val inputter: InfosetInputter,
  outStream: java.io.OutputStream,
  vbox: VariableBox,
  diagnosticsArg: List[Diagnostic],
  dataProcArg: DataProcessor,
  tunable: DaffodilTunables,
  areDebugging: Boolean
) extends UState(vbox, diagnosticsArg, One(dataProcArg), tunable, areDebugging) {

  dState.setMode(UnparserBlocking)

  def this(
    inputter: InfosetInputter,
    outputStream: java.io.OutputStream,
    vmap: VariableMap,
    diagnosticsArg: List[Diagnostic],
    dataProcArg: DataProcessor,
    tunable: DaffodilTunables,
    areDebugging: Boolean
  ) =
    this(
      inputter,
      outputStream,
      new VariableBox(vmap),
      diagnosticsArg,
      dataProcArg,
      tunable,
      areDebugging
    )

  override var dataOutputStream: DirectOrBufferedDataOutputStream = {
    val out = DirectOrBufferedDataOutputStream(
      outStream,
      null, // null means no other stream created this one.
      isLayer = false,
      tunable.outputStreamChunkSizeInBytes,
      tunable.maxByteArrayOutputStreamBufferSizeInBytes,
      tunable.tempFilePath
    )
    out
  }

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
      variableBox.cloneForSuspension(),
      currentInfosetNodeStack.top.get, // only need the to of the stack, not the whole thing
      arrayIterationIndexStack.top, // only need the top of the stack, not the whole thing
      occursIndexStack.top,
      es,
      ds,
      tunable,
      areDebugging
    )

    clone.setProcessor(processor)

    clone
  }

  override lazy val withUnparserDataInputStream =
    new LocalStack[StringDataInputStreamForUnparse](new StringDataInputStreamForUnparse)
  override lazy val withByteArrayOutputStream =
    new LocalStack[(ByteArrayOutputStream, DirectOrBufferedDataOutputStream)](
      {
        val baos =
          new ByteArrayOutputStream() // TODO: PERFORMANCE: Allocates new object. Can reuse one from an onStack/pool via reset()
        val dos = DirectOrBufferedDataOutputStream(
          baos,
          null,
          false,
          tunable.outputStreamChunkSizeInBytes,
          tunable.maxByteArrayOutputStreamBufferSizeInBytes,
          tunable.tempFilePath
        )
        (baos, dos)
      },
      pair =>
        pair match {
          case (baos, dos) =>
            baos.reset()
            dos.resetAllBitPos()
        }
    )

  override def advance: Boolean = inputter.advance
  override def advanceAccessor: InfosetAccessor = inputter.advanceAccessor
  override def inspect: Boolean = inputter.inspect
  override def inspectAccessor: InfosetAccessor = inputter.inspectAccessor
  // $COVERAGE-OFF$ // unused, but necessary to meet requirements of Cursor[T]
  override def fini() = Assert.usageError("Not to be used on UState")
  // $COVERAGE-ON$
  /**
   * Use this so if there isn't an event we get a clean diagnostic message saying
   * that is what has gone wrong.
   */
  override def inspectOrError = {
    val m = inspectMaybe
    if (m.isEmpty)
      Assert.invariantFailed(
        "An InfosetEvent was required for unparsing, but no InfosetEvent was available."
      )
    m.get
  }

  override def advanceOrError = {
    val m = advanceMaybe
    if (m.isEmpty)
      Assert.invariantFailed(
        "An InfosetEvent was required for unparsing, but no InfosetEvent was available."
      )
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

  override val arrayIterationIndexStack = MStackOfLong()
  arrayIterationIndexStack.push(1L)
  override def moveOverOneArrayIterationIndexOnly() =
    arrayIterationIndexStack.push(arrayIterationIndexStack.pop() + 1)
  override def arrayIterationPos = arrayIterationIndexStack.top

  override val occursIndexStack = MStackOfLong()
  occursIndexStack.push(1L)
  override def moveOverOneOccursIndexOnly() = occursIndexStack.push(occursIndexStack.pop() + 1)
  override def occursPos = occursIndexStack.top

  override val groupIndexStack = MStackOfLong()
  groupIndexStack.push(1L)
  override def moveOverOneGroupIndexOnly() = groupIndexStack.push(groupIndexStack.pop() + 1)
  override def groupPos = groupIndexStack.top

  // TODO: it doesn't look anything is actually reading the value of childindex
  // stack. Can we get rid of it?
  override val childIndexStack = MStackOfLong()
  childIndexStack.push(1L)
  override def moveOverOneElementChildOnly() = childIndexStack.push(childIndexStack.pop() + 1)
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

  /**
   * For outputValueCalc we accumulate the suspendables here.
   *
   * Note: only the primary UState (the initial one) will use this.
   * All the other clones used for outputValueCalc, those never
   * need to add any.
   */
  private val suspensionTracker =
    new SuspensionTracker(tunable.unparseSuspensionWaitYoung, tunable.unparseSuspensionWaitOld)

  def addSuspension(se: Suspension): Unit = {
    suspensionTracker.trackSuspension(se)
  }

  def evalSuspensions(isFinal: Boolean): Unit = {
    suspensionTracker.evalSuspensions()
    if (isFinal) suspensionTracker.requireFinal()
  }

  def suspensions = suspensionTracker.suspensions

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
    val elt =
      if (this.currentInfosetNodeMaybe.isDefined) "node=" + this.currentInfosetNode.toString
      else ""
    val hidden = if (withinHiddenNest) " hidden" else ""
    "UState(" + elt + hidden + " DOS=" + dataOutputStream.toString() + ")"
  }
}

object UState {

  def createInitialUState(
    outStream: java.io.OutputStream,
    dataProc: DFDL.DataProcessor,
    inputter: InfosetInputter,
    areDebugging: Boolean
  ): UStateMain = {
    Assert.invariant(inputter.isInitialized)

    /**
     * This is a full deep copy as variableMap is mutable. Reusing
     * dataProc.VariableMap without a copy would not be thread safe.
     */
    val variables = dataProc.variableMap.copy()

    val diagnostics = Nil
    val newState = new UStateMain(
      inputter,
      outStream,
      variables,
      diagnostics,
      dataProc.asInstanceOf[DataProcessor],
      dataProc.tunables,
      areDebugging
    )
    newState
  }
}
