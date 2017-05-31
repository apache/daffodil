/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors.unparsers

import java.io.ByteArrayOutputStream
import java.nio.charset.Charset
import java.nio.charset.CharsetDecoder
import java.nio.charset.CharsetEncoder

import scala.Left
import scala.collection.mutable

import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.dpath.UnparserBlocking
import edu.illinois.ncsa.daffodil.dsom.RuntimeSchemaDefinitionError
import edu.illinois.ncsa.daffodil.equality.EqualitySuppressUnusedImportWarning
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.SavesErrorsAndWarnings
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.infoset.DIArray
import edu.illinois.ncsa.daffodil.infoset.DIElement
import edu.illinois.ncsa.daffodil.infoset.DINode
import edu.illinois.ncsa.daffodil.io.DirectOrBufferedDataOutputStream
import edu.illinois.ncsa.daffodil.io.StringDataInputStreamForUnparse
import edu.illinois.ncsa.daffodil.processors.DataLoc
import edu.illinois.ncsa.daffodil.processors.DataProcessor
import edu.illinois.ncsa.daffodil.processors.DelimiterStackUnparseNode
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeUnparserHelper
import edu.illinois.ncsa.daffodil.processors.Failure
import edu.illinois.ncsa.daffodil.processors.ParseOrUnparseState
import edu.illinois.ncsa.daffodil.processors.Suspension
import edu.illinois.ncsa.daffodil.processors.UnparseResult
import edu.illinois.ncsa.daffodil.processors.VariableBox
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter
import edu.illinois.ncsa.daffodil.util.Cursor
import edu.illinois.ncsa.daffodil.util.LocalStack
import edu.illinois.ncsa.daffodil.util.MStackOf
import edu.illinois.ncsa.daffodil.util.MStackOfLong
import edu.illinois.ncsa.daffodil.util.MStackOfMaybe
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.infoset.InfosetAccessor
import edu.illinois.ncsa.daffodil.infoset.InfosetInputter
import edu.illinois.ncsa.daffodil.processors.ParseOrUnparseState
import edu.illinois.ncsa.daffodil.api.DaffodilTunables
import edu.illinois.ncsa.daffodil.processors.NonTermRuntimeData
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData

object ENoWarn { EqualitySuppressUnusedImportWarning() }

abstract class UState(
  dos: DirectOrBufferedDataOutputStream,
  vbox: VariableBox,
  diagnosticsArg: List[Diagnostic],
  dataProcArg: Maybe[DataProcessor],
  tunable: DaffodilTunables)
  extends ParseOrUnparseState(vbox, diagnosticsArg, dataProcArg, tunable)
  with Cursor[InfosetAccessor] with ThrowsSDE with SavesErrorsAndWarnings {

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
    new DataLoc(bitPos1b, bitLimit1b, Left(dataOutputStream),
      if (m.isDefined) Maybe(m.value.runtimeData) else Nope)
  }

  lazy val unparseResult = new UnparseResult(dataProc.get, this)

  def bitPos0b = if (dataOutputStream.maybeAbsBitPos0b.isDefined) dataOutputStream.maybeAbsBitPos0b.get else 0L

  def bitLimit0b = dataOutputStream.maybeRelBitLimit0b

  def charPos = -1L

  final def notifyDebugging(flag: Boolean) {
    dataOutputStream.setDebugging(flag)
  }

  def addUnparseError(ue: UnparseError) {
    diagnostics = ue :: diagnostics
    _processorStatus = new Failure(ue)
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
class UStateForSuspension(
  val mainUState: UStateMain,
  dos: DirectOrBufferedDataOutputStream,
  vbox: VariableBox,
  override val currentInfosetNode: DINode,
  arrayIndex: Long,
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

  override def getDecoder(cs: Charset): CharsetDecoder = mainUState.getDecoder(cs)
  override def getEncoder(cs: Charset): CharsetEncoder = mainUState.getEncoder(cs)

  // override def charBufferDataOutputStream = mainUState.charBufferDataOutputStream
  override def withUnparserDataInputStream = mainUState.withUnparserDataInputStream
  override def withByteArrayOutputStream = mainUState.withByteArrayOutputStream

  override def advance: Boolean = die
  override def advanceAccessor: InfosetAccessor = die
  override def inspect: Boolean = die
  override def inspectAccessor: InfosetAccessor = die
  override def fini: Unit = {}
  override def inspectOrError = die
  override def advanceOrError = die
  override def isInspectArrayEnd = die

  override def currentInfosetNodeStack = die
  override def currentInfosetNodeMaybe = Maybe(currentInfosetNode)

  override def arrayIndexStack = die
  override def moveOverOneArrayIndexOnly() = die
  override def arrayPos = arrayIndex

  override def groupIndexStack = die
  override def moveOverOneGroupIndexOnly() = die
  override def groupPos = 0 // was die, but this is called when copying state during debugging

  override def childIndexStack = die
  override def moveOverOneElementChildOnly() = die
  override def childPos = 0 // was die, but this is called when copying state during debugging.

  override def occursBoundsStack = die

  override def pushDelimiters(node: DelimiterStackUnparseNode) = die
  override def popDelimiters() = die
  override def localDelimiters = delimiterStackMaybe.get.top
  override def allTerminatingMarkup = {
    delimiterStackMaybe.get.iterator.flatMap { dnode =>
      (dnode.separator.toList ++ dnode.terminator.toList)
    }.toList
  }

  override def escapeSchemeEVCache: MStackOfMaybe[EscapeSchemeUnparserHelper] = escapeSchemeEVCacheMaybe.get

  override def setVariables(newVariableMap: VariableMap) = die
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
      arrayIndexStack.top, // only need the to of the stack, not the whole thing
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
      val dos = DirectOrBufferedDataOutputStream(baos, null)
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

  val occursBoundsStack = MStackOfLong()
  occursBoundsStack.push(1L)

  override lazy val escapeSchemeEVCache = new MStackOfMaybe[EscapeSchemeUnparserHelper]

  val delimiterStack = new MStackOf[DelimiterStackUnparseNode]()
  override def pushDelimiters(node: DelimiterStackUnparseNode) = delimiterStack.push(node)
  override def popDelimiters() = delimiterStack.pop
  override def localDelimiters = delimiterStack.top
  override def allTerminatingMarkup = {
    delimiterStack.iterator.flatMap { dnode =>
      (dnode.separator.toList ++ dnode.terminator.toList)
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

  def addSuspension(se: Suspension) {
    suspensions.enqueue(se)
  }

  def evalSuspensions(ustate: UState) {
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

    val variables = dataProc.getVariables
    val diagnostics = Nil
    val newState = new UStateMain(inputter, variables, diagnostics, dataProc.asInstanceOf[DataProcessor], out,
      new mutable.Queue[Suspension], dataProc.getTunables()) // null means no prior UState
    newState
  }
}

object UnparserBitOrderChecks {

  final def checkUnparseBitOrder(ustate: UState) = {
    //
    // Check for bitOrder change. If yes, then unless we know we're byte aligned
    // we must split the DOS until we find out. That way the new buffered DOS
    // can be assumed to be byte aligned (which will be checked on combining),
    // and the bytes in it will actually start out byte aligned.
    //
    val dos = ustate.dataOutputStream
    val isChanging = isUnparseBitOrderChanging(dos, ustate)
    if (isChanging) {
      //
      // the bit order is changing. Let's be sure
      // that it's legal to do so w.r.t. other properties
      // These checks will have been evaluated at compile time if
      // all the properties are static, so this is really just
      // in case the charset or byteOrder are runtime-valued.
      //
      ustate.processor.context match {
        case trd: TermRuntimeData => {
          val mcboc = trd.maybeCheckBitOrderAndCharsetEv
          val mcbbo = trd.maybeCheckByteAndBitOrderEv
          if (mcboc.isDefined) mcboc.get.evaluate(ustate)
          if (mcbbo.isDefined) mcbbo.get.evaluate(ustate)
        }
        case _ => // ok
      }

      dos.setPriorBitOrder(ustate.bitOrder)
      splitOnUnalignedBitOrderChange(dos, ustate)
    }
  }

  private def isUnparseBitOrderChanging(dos: DirectOrBufferedDataOutputStream, ustate: UState): Boolean = {
    val ctxt = ustate.processor.context
    ctxt match {
      case ntrd: NonTermRuntimeData => false
      case _ => {
        val priorBitOrder = dos.priorBitOrder
        val newBitOrder = ustate.bitOrder
        priorBitOrder ne newBitOrder
      }
    }
  }

  private def splitOnUnalignedBitOrderChange(dos: DirectOrBufferedDataOutputStream, ustate: UState): Unit = {
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
        ustate.SDE("Can only change dfdl:bitOrder on a byte boundary. Bit pos (1b) was %s.", bp1b)
      }
    }
    if (isSplitNeeded) {
      val newDOS = dos.addBuffered
      ustate.dataOutputStream = newDOS
      //
      // Just splitting to start a new bitOrder on a byte boundary in a new
      // buffered DOS
      // So the prior DOS can be finished. Nothing else will be added to it.
      //
      dos.setFinished(ustate)
    }
  }
}
