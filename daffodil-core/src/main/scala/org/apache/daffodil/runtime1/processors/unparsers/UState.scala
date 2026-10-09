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

import org.apache.daffodil.api
import org.apache.daffodil.api.DataLocation
import org.apache.daffodil.io.DirectOrBufferedDataOutputStream
import org.apache.daffodil.io.StringDataInputStreamForUnparse
import org.apache.daffodil.io.processors.charset.BitsCharset
import org.apache.daffodil.io.processors.charset.BitsCharsetDecoder
import org.apache.daffodil.io.processors.charset.BitsCharsetEncoder
import org.apache.daffodil.lib.equality.EqualitySuppressUnusedImportWarning
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.SavesErrorsAndWarnings
import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.lib.util.Cursor
import org.apache.daffodil.lib.util.LocalStack
import org.apache.daffodil.lib.util.MStackOf
import org.apache.daffodil.lib.util.MStackOfLong
import org.apache.daffodil.lib.util.MStackOfMaybe
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.lib.util.ThreadSafePool
import org.apache.daffodil.runtime1.dpath.UnparserBlocking
import org.apache.daffodil.runtime1.iapi.DFDL
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
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
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
  diagnosticsArg: Seq[api.Diagnostic],
  dataProcArg: Maybe[DataProcessor],
  tunable: DaffodilTunables,
  areDebugging: Boolean,
  eventState: InfosetEventState,
  delimiterEscapePosition: DelimiterEscapePositionState
) extends ParseOrUnparseState(vbox, diagnosticsArg, dataProcArg, tunable)
  with InfosetTreeState
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
  final def pushTRD(trd: TermRuntimeData): Unit = eventState.pushTRD(trd)

  /**
   * Returns the top of the stack if it exists. No state change to stack contents.
   */
  final def maybeTopTRD(): Maybe[TermRuntimeData] = eventState.maybeTopTRD()

  /**
   * Pop the dynamic TRD context stack. The popped TRD should be the same as the argument rd.
   * The popped TRD is returned.
   */
  final def popTRD(trd: TermRuntimeData): TermRuntimeData = eventState.popTRD(trd)

  override def toString = {
    val elt =
      if (this.currentInfosetNodeMaybe.isDefined) "node=" + this.currentInfosetNode.toString
      else ""
    "UState(" + elt + " DOS=" + getDataOutputStream.toString() + ")"
  }

  protected var _dataOutputStream: DirectOrBufferedDataOutputStream = _

  def getDataOutputStream: DirectOrBufferedDataOutputStream = _dataOutputStream

  def setDataOutputStream(dos: DirectOrBufferedDataOutputStream): Unit = {
    _dataOutputStream = dos
  }

  final def escapeSchemeEVCache: MStackOfMaybe[EscapeSchemeUnparserHelper] =
    delimiterEscapePosition.escapeSchemeEVCache

  final def withUnparserDataInputStream: LocalStack[StringDataInputStreamForUnparse] =
    delimiterEscapePosition.withUnparserDataInputStream
  final def withByteArrayOutputStream
    : LocalStack[(ByteArrayOutputStream, DirectOrBufferedDataOutputStream)] =
    delimiterEscapePosition.withByteArrayOutputStream

  final def allTerminatingMarkup: List[DFADelimiter] =
    delimiterEscapePosition.allTerminatingMarkup
  final def localDelimiters: DelimiterStackUnparseNode = delimiterEscapePosition.localDelimiters
  final def pushDelimiters(node: DelimiterStackUnparseNode): Unit =
    delimiterEscapePosition.pushDelimiters(node)
  final def popDelimiters(): Unit = delimiterEscapePosition.popDelimiters()

  final def childIndexStack: MStackOfLong = delimiterEscapePosition.childIndexStack
  final def moveOverOneElementChildOnly(): Unit =
    delimiterEscapePosition.moveOverOneElementChildOnly()
  final override def childPos: Long = delimiterEscapePosition.childPos

  final override def advance: Boolean = eventState.advance
  final override def advanceAccessor: InfosetAccessor = eventState.advanceAccessor
  final override def inspect: Boolean = eventState.inspect
  final override def inspectAccessor: InfosetAccessor = eventState.inspectAccessor

  /**
   * Use these so if there isn't an event we get a clean diagnostic message saying
   * that is what has gone wrong.
   */
  final def inspectOrError: InfosetAccessor = eventState.inspectOrError
  final def advanceOrError: InfosetAccessor = eventState.advanceOrError
  final def isInspectArrayEnd: Boolean = eventState.isInspectArrayEnd

  override def dataStream = Maybe(getDataOutputStream)

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
    new DataLoc(bitPos1b, bitLimit1b, Left(getDataOutputStream), mrd)
  }

  lazy val unparseResult = new UnparseResult(dataProc.get, this)

  def bitPos0b = if (getDataOutputStream.maybeAbsBitPos0b.isDefined)
    getDataOutputStream.maybeAbsBitPos0b.get
  else 0L

  def bitLimit0b = getDataOutputStream.maybeRelBitLimit0b

  def charPos = -1L

  final def notifyDebugging(flag: Boolean): Unit = {
    getDataOutputStream.setDebugging(flag)
  }

  def addUnparseError(ue: UnparseError): Unit = {
    diagnostics = ue +: diagnostics
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
    val dos = this.getDataOutputStream
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
      setDataOutputStream(newDOS)
      dos.setFinished(finfo)
    }
  }

  def regexMatchStatePool: ThreadSafePool[(CharBuffer, LongBuffer)] =
    Assert.usageError("Not to be used.")

  def documentElement: DIDocument

  final val releaseUnneededInfoset: Boolean = !areDebugging && tunable.releaseUnneededInfoset

  final def freeChildIfNoLongerNeeded(parent: DINode, index: Int): Unit =
    parent.freeChildIfNoLongerNeeded(index, releaseUnneededInfoset)

  def delimitedParseResult = Nope
}

/**
 * The state of the infoset tree as unparsing makes it from events: the event
 * cursor, the TRD and node stacks, and the position within the current group,
 * array and occurrence. The unparsers that create and finish the nodes of
 * elements work through it.
 */
trait InfosetTreeState extends Cursor[InfosetAccessor] {
  def tunable: DaffodilTunables

  // How the infoset's nodes are made and kept as unparsing consumes events.
  // InfosetFromEvents does it for the events of an inputter.

  // The node of an element in a hidden group, which has no events.
  def getHiddenElement(erd: ElementRuntimeData): DIElement

  // The node of an outputValueCalc element whose start event was just consumed.
  def getOvcElement(startEvent: InfosetAccessor, erd: ElementRuntimeData): DIElement

  // Adds a node whose start was just reached to the infoset.
  def attachElement(newElem: DIElement): Unit

  // Finishes a node whose end was just reached.
  def finishElement(cur: DINode, erd: ElementRuntimeData): Unit
  def finishOvcElement(cur: DINode): Unit

  def inspectOrError: InfosetAccessor
  def advanceOrError: InfosetAccessor
  def isInspectArrayEnd: Boolean

  def pushTRD(trd: TermRuntimeData): Unit
  def maybeTopTRD(): Maybe[TermRuntimeData]
  def popTRD(trd: TermRuntimeData): TermRuntimeData

  def currentInfosetNode: DINode
  def currentInfosetNodeMaybe: Maybe[DINode]
  def currentInfosetNodeStack: MStackOfMaybe[DINode]
  def documentElement: DIDocument

  def arrayIterationIndexStack: MStackOfLong
  def occursIndexStack: MStackOfLong
  def groupIndexStack: MStackOfLong
  def moveOverOneArrayIterationIndexOnly(): Unit
  def moveOverOneOccursIndexOnly(): Unit
  def moveOverOneGroupIndexOnly(): Unit
  def arrayIterationPos: Long
  def occursPos: Long
  def groupPos: Long

  def withinHiddenNest: Boolean

  def freeChildIfNoLongerNeeded(parent: DINode, index: Int): Unit
}

/**
 * The part of a UState that consumes infoset events from an InfosetInputter:
 * the event cursor and the TRD stack. Only the UStates that read the
 * infoset events hold a real one.
 */
trait InfosetEventState {
  def advance: Boolean
  def advanceAccessor: InfosetAccessor
  def inspect: Boolean
  def inspectAccessor: InfosetAccessor
  def inspectOrError: InfosetAccessor
  def advanceOrError: InfosetAccessor
  def isInspectArrayEnd: Boolean
  def pushTRD(trd: TermRuntimeData): Unit
  def maybeTopTRD(): Maybe[TermRuntimeData]
  def popTRD(trd: TermRuntimeData): TermRuntimeData
}

/**
 * Events read from an InfosetInputter. The purpose names what the caller is
 * doing, for the diagnostic when an event is required but none is available.
 */
final class InputterEventState(inputter: InfosetInputter, purpose: String)
  extends InfosetEventState {

  override def advance: Boolean = inputter.advance
  override def advanceAccessor: InfosetAccessor = inputter.advanceAccessor
  override def inspect: Boolean = inputter.inspect
  override def inspectAccessor: InfosetAccessor = inputter.inspectAccessor

  override def inspectOrError: InfosetAccessor = {
    if (inspect) {
      inspectAccessor
    } else {
      Assert.invariantFailed(
        "An InfosetEvent was required for " + purpose + ", but no InfosetEvent was available."
      )
    }
  }

  override def advanceOrError: InfosetAccessor = {
    if (advance) {
      advanceAccessor
    } else {
      Assert.invariantFailed(
        "An InfosetEvent was required for " + purpose + ", but no InfosetEvent was available."
      )
    }
  }

  override def isInspectArrayEnd: Boolean = {
    if (!inspect) {
      false
    } else {
      inspectAccessor match {
        case e if e.isEnd && e.isArray => true
        case _ => false
      }
    }
  }

  override def pushTRD(trd: TermRuntimeData): Unit = inputter.pushTRD(trd)
  override def maybeTopTRD(): Maybe[TermRuntimeData] = inputter.maybeTopTRD()
  override def popTRD(trd: TermRuntimeData): TermRuntimeData = {
    val poppedTRD = inputter.popTRD()
    if (poppedTRD ne trd) {
      Assert.invariantFailed("TRDs do not match. Expected: " + trd + " got " + poppedTRD)
    }
    poppedTRD
  }
}

/**
 * For a UState that never reads infoset events: a clone made to resume a
 * suspension.
 */
object NoInfosetEventState extends InfosetEventState {
  private def die =
    Assert.invariantFailed("Function should never be needed in UStateForSuspension")

  override def advance: Boolean = die
  override def advanceAccessor: InfosetAccessor = die
  override def inspect: Boolean = die
  override def inspectAccessor: InfosetAccessor = die
  override def inspectOrError: InfosetAccessor = die
  override def advanceOrError: InfosetAccessor = die
  override def isInspectArrayEnd: Boolean = die
  override def pushTRD(trd: TermRuntimeData): Unit = die
  override def maybeTopTRD(): Maybe[TermRuntimeData] = die
  override def popTRD(trd: TermRuntimeData): TermRuntimeData = die
}

/**
 * The part of a UState that only emitting delimited, escaped text uses: the
 * delimiter stack, the escape scheme cache, the scratch buffers for measuring
 * and escaping text, and the position within the current sequence or choice.
 *
 * The main unparse owns all of it. The clone that resumes a suspension holds
 * only copies of the escape scheme and delimiter stacks, which are never
 * pushed onto, and shares the scratch buffers of the state it came from.
 */
final class DelimiterEscapePositionState private (
  tunable: DaffodilTunables,
  // The state a suspension's clone came from; Nope for the main unparse.
  clonedFrom: Maybe[DelimiterEscapePositionState],
  private var escapeSchemeEVCacheMaybe: Maybe[MStackOfMaybe[EscapeSchemeUnparserHelper]],
  delimiterStackMaybe: Maybe[MStackOf[DelimiterStackUnparseNode]]
) {

  def escapeSchemeEVCache: MStackOfMaybe[EscapeSchemeUnparserHelper] = {
    if (escapeSchemeEVCacheMaybe.isEmpty) {
      Assert.invariant(clonedFrom.isEmpty)
      escapeSchemeEVCacheMaybe = Maybe(new MStackOfMaybe[EscapeSchemeUnparserHelper](8))
    }
    escapeSchemeEVCacheMaybe.get
  }

  private lazy val unparserDataInputStream =
    new LocalStack[StringDataInputStreamForUnparse](new StringDataInputStreamForUnparse)

  def withUnparserDataInputStream: LocalStack[StringDataInputStreamForUnparse] = {
    if (clonedFrom.isEmpty) {
      unparserDataInputStream
    } else {
      clonedFrom.get.withUnparserDataInputStream
    }
  }

  private lazy val byteArrayOutputStream =
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

  def withByteArrayOutputStream
    : LocalStack[(ByteArrayOutputStream, DirectOrBufferedDataOutputStream)] = {
    if (clonedFrom.isEmpty) {
      byteArrayOutputStream
    } else {
      clonedFrom.get.withByteArrayOutputStream
    }
  }

  def pushDelimiters(node: DelimiterStackUnparseNode): Unit = {
    Assert.invariant(clonedFrom.isEmpty)
    delimiterStackMaybe.get.push(node)
  }

  def popDelimiters(): Unit = {
    Assert.invariant(clonedFrom.isEmpty)
    delimiterStackMaybe.get.pop
  }

  def localDelimiters: DelimiterStackUnparseNode = delimiterStackMaybe.get.top

  def allTerminatingMarkup: List[DFADelimiter] = {
    delimiterStackMaybe.get.iterator.flatMap { dnode =>
      dnode.separator ++ dnode.terminator
    }.toList
  }

  // Sequence and choice unparsers read it to find their current child; build
  // tracks position in its own frames instead. A clone has none.
  private val childIndexStackMaybe: Maybe[MStackOfLong] = {
    if (clonedFrom.isEmpty) {
      val stack = MStackOfLong(16)
      stack.push(1L)
      Maybe(stack)
    } else {
      Nope
    }
  }

  def childIndexStack: MStackOfLong = childIndexStackMaybe.get

  def moveOverOneElementChildOnly(): Unit = {
    val stack = childIndexStack
    stack.setTop(stack.top + 1)
  }

  // A clone reports 0, which is read when copying state during debugging.
  def childPos: Long = {
    if (childIndexStackMaybe.isEmpty) {
      0L
    } else {
      childIndexStackMaybe.get.top
    }
  }

  /**
   * The surface for a clone that resumes a suspension: it needs only the
   * current escape scheme and delimiters, and shares the scratch buffers.
   */
  def cloneForSuspension(): DelimiterEscapePositionState = {
    val es =
      if (escapeSchemeEVCacheMaybe.isDefined && !escapeSchemeEVCacheMaybe.get.isEmpty) {
        // If there are any escape schemes, clone the whole MStack, since the
        // escape scheme cache logic requires one (only the top is really
        // needed, but changing the cache access isn't trivial). Sized to the
        // source's depth: nothing pushes onto the clone afterward.
        val source = escapeSchemeEVCacheMaybe.get
        val esClone = new MStackOfMaybe[EscapeSchemeUnparserHelper](source.length)
        esClone.copyFrom(source)
        Maybe(esClone)
      } else {
        Nope
      }
    val ds =
      if (!delimiterStackMaybe.get.isEmpty) {
        // If there are any delimiters, clone them all since they may be
        // needed for escaping. Sized to the source's depth: push and pop
        // both die on this clone, so it never grows past that depth.
        val source = delimiterStackMaybe.get
        val dsClone = new MStackOf[DelimiterStackUnparseNode](source.length)
        dsClone.copyFrom(source)
        Maybe(dsClone)
      } else {
        Nope
      }
    new DelimiterEscapePositionState(tunable, Maybe(this), es, ds)
  }
}

object DelimiterEscapePositionState {
  def apply(tunable: DaffodilTunables): DelimiterEscapePositionState =
    new DelimiterEscapePositionState(
      tunable,
      Nope,
      Nope,
      Maybe(new MStackOf[DelimiterStackUnparseNode]())
    )
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
  val dataOutputStream: DirectOrBufferedDataOutputStream,
  vbox: VariableBox,
  override val currentInfosetNode: DINode,
  arrayIterationIndex: Long,
  occursIndex: Long,
  delimiterEscapePosition: DelimiterEscapePositionState,
  tunable: DaffodilTunables,
  areDebugging: Boolean
) extends UState(
    vbox,
    mainUState.diagnostics,
    mainUState.dataProc,
    tunable,
    areDebugging,
    NoInfosetEventState,
    delimiterEscapePosition
  ) {

  _dataOutputStream = dataOutputStream
  dState.setMode(UnparserBlocking)
  dState.setCurrentNode(thisElement.asInstanceOf[DINode])
  dState.setContextNode(thisElement.asInstanceOf[DINode])
  dState.setErrorOrWarn(this)

  private def die =
    Assert.invariantFailed("Function should never be needed in UStateForSuspension")

  override def getDecoder(cs: BitsCharset): BitsCharsetDecoder = mainUState.getDecoder(cs)
  override def getEncoder(cs: BitsCharset): BitsCharsetEncoder = mainUState.getEncoder(cs)

  override def suspensions = mainUState.suspensions

  // $COVERAGE-OFF$
  override def fini(): Unit = die
  override def currentInfosetNodeStack = die
  override def arrayIterationIndexStack = die
  override def moveOverOneArrayIterationIndexOnly() = die
  override def occursIndexStack = die
  override def moveOverOneOccursIndexOnly() = die
  override def groupIndexStack = die
  override def moveOverOneGroupIndexOnly() = die
  override def getHiddenElement(erd: ElementRuntimeData) = die
  override def getOvcElement(startEvent: InfosetAccessor, erd: ElementRuntimeData) = die
  override def attachElement(newElem: DIElement) = die
  override def finishElement(cur: DINode, erd: ElementRuntimeData) = die
  override def finishOvcElement(cur: DINode) = die
  // $COVERAGE-ON$

  override def groupPos = 0 // was die, but this is called when copying state during debugging
  override def currentInfosetNodeMaybe = Maybe(currentInfosetNode)
  override def arrayIterationPos = arrayIterationIndex
  override def occursPos = occursIndex

  override def documentElement = mainUState.documentElement

  override def incrementHiddenDef() =
    Assert.usageError("Unparser suspended UStates need not be aware of hidden contexts")
  override def decrementHiddenDef() =
    Assert.usageError("Unparser suspended UStates need not be aware of hidden contexts")
  override def withinHiddenNest =
    Assert.usageError("Unparser suspended UStates need not be aware of hidden contexts")
  override def setDataOutputStream(value: DirectOrBufferedDataOutputStream) = {
    Assert.invariantFailed("Should never change dataOutputStream on a suspension")
  }
}

final class UStateMain private (
  private val inputter: InfosetInputter,
  outStream: java.io.OutputStream,
  vbox: VariableBox,
  diagnosticsArg: Seq[api.Diagnostic],
  dataProcArg: DataProcessor,
  tunable: DaffodilTunables,
  areDebugging: Boolean,
  delimiterEscapePosition: DelimiterEscapePositionState
) extends UState(
    vbox,
    diagnosticsArg,
    One(dataProcArg),
    tunable,
    areDebugging,
    new InputterEventState(inputter, "unparsing"),
    delimiterEscapePosition
  )
  with InfosetFromEvents {

  dState.setMode(UnparserBlocking)

  def this(
    inputter: InfosetInputter,
    outputStream: java.io.OutputStream,
    vmap: VariableMap,
    diagnosticsArg: Seq[api.Diagnostic],
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
      areDebugging,
      DelimiterEscapePositionState(tunable)
    )

  setDataOutputStream({
    val out = DirectOrBufferedDataOutputStream(
      outStream,
      null, // null means no other stream created this one.
      isLayer = false,
      tunable.outputStreamChunkSizeInBytes,
      tunable.maxByteArrayOutputStreamBufferSizeInBytes,
      tunable.tempFilePath
    )
    out
  })

  def cloneForSuspension(suspendedDOS: DirectOrBufferedDataOutputStream): UState = {
    val clone = new UStateForSuspension(
      this,
      suspendedDOS,
      variableBox.cloneForSuspension(),
      currentInfosetNodeStack.top.get, // only need the to of the stack, not the whole thing
      arrayIterationIndexStack.top, // only need the top of the stack, not the whole thing
      occursIndexStack.top,
      delimiterEscapePosition.cloneForSuspension(),
      tunable,
      areDebugging
    )

    clone.setProcessor(processor)

    clone
  }

  // $COVERAGE-OFF$ // unused, but necessary to meet requirements of Cursor[T]
  override def fini() = Assert.usageError("Not to be used on UState")
  // $COVERAGE-ON$

  def currentInfosetNode: DINode =
    if (currentInfosetNodeMaybe.isEmpty) null
    else currentInfosetNodeMaybe.get

  def currentInfosetNodeMaybe: Maybe[DINode] =
    if (currentInfosetNodeStack.isEmpty) Nope
    else currentInfosetNodeStack.top

  override val currentInfosetNodeStack = new MStackOfMaybe[DINode](16)

  override val arrayIterationIndexStack = MStackOfLong(16)
  arrayIterationIndexStack.push(1L)
  override def moveOverOneArrayIterationIndexOnly() =
    arrayIterationIndexStack.setTop(arrayIterationIndexStack.top + 1)
  override def arrayIterationPos = arrayIterationIndexStack.top

  override val occursIndexStack = MStackOfLong(16)
  occursIndexStack.push(1L)
  override def moveOverOneOccursIndexOnly() = occursIndexStack.setTop(occursIndexStack.top + 1)
  override def occursPos = occursIndexStack.top

  override val groupIndexStack = MStackOfLong()
  groupIndexStack.push(1L)
  override def moveOverOneGroupIndexOnly() = groupIndexStack.setTop(groupIndexStack.top + 1)
  override def groupPos = groupIndexStack.top

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

  final override def documentElement = inputter.documentElement

  override def toString = {
    val elt =
      if (this.currentInfosetNodeMaybe.isDefined) "node=" + this.currentInfosetNode.toString
      else ""
    val hidden = if (withinHiddenNest) " hidden" else ""
    "UState(" + elt + hidden + " DOS=" + getDataOutputStream.toString() + ")"
  }
}

object UState {

  def createInitialUState(
    outStream: java.io.OutputStream,
    dataProc: DFDL.DataProcessor,
    inputter: InfosetInputter,
    areDebugging: Boolean
  ): UStateMain = {

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
