package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.util.Cursor
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.DINode
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.ParseOrUnparseState
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.exceptions.SavesErrorsAndWarnings
import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.processors.DataLoc
import edu.illinois.ncsa.daffodil.processors.DataProcessor
import edu.illinois.ncsa.daffodil.processors.UnparseResult
import edu.illinois.ncsa.daffodil.processors.Success
import edu.illinois.ncsa.daffodil.processors.Failure
import edu.illinois.ncsa.daffodil.processors.InfosetElement
import edu.illinois.ncsa.daffodil.processors.DIArray
import edu.illinois.ncsa.daffodil.dsom.ValidationError
import edu.illinois.ncsa.daffodil.dsom.RuntimeSchemaDefinitionError
import edu.illinois.ncsa.daffodil.processors.DelimiterStackUnparseNode
import edu.illinois.ncsa.daffodil.processors.Failure
import edu.illinois.ncsa.daffodil.processors.DIElement
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeUnparserHelper
import edu.illinois.ncsa.daffodil.io.DataOutputStream
import edu.illinois.ncsa.daffodil.io.DirectOrBufferedDataOutputStream
import edu.illinois.ncsa.daffodil.equality._; object ENoWarn { EqualitySuppressUnusedImportWarning() }
import scala.collection.mutable
import edu.illinois.ncsa.daffodil.util.MStack
import edu.illinois.ncsa.daffodil.util.LocalStack
import edu.illinois.ncsa.daffodil.io.CharBufferDataOutputStream
import edu.illinois.ncsa.daffodil.io.StringDataInputStreamForUnparse
import java.io.ByteArrayOutputStream
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.dpath.SuspendableExpression
import edu.illinois.ncsa.daffodil.dpath.Blocking
import passera.unsigned.ULong

class UState(
  private val infosetCursor: InfosetCursor,
  vmap: VariableMap,
  diagnosticsArg: List[Diagnostic],
  dataProcArg: DataProcessor,
  var dataOutputStream: DataOutputStream,
  initialSuspendedExpressions: mutable.Queue[SuspendableExpression] = new mutable.Queue[SuspendableExpression])
  extends ParseOrUnparseState(vmap, diagnosticsArg, One(dataProcArg), Success)
  with Cursor[InfosetAccessor] with ThrowsSDE with SavesErrorsAndWarnings {

  override def toString = {
    "UState(" + dataOutputStream.toString() + ")"
  }

  def cloneForOVC(newDOS: DataOutputStream): UState = {
    val clone = new UState(
      NonUsableInfosetCursor,
      this.variableMap,
      Nil, // no diagnostics for now. Any that accumulate here must eventually be output.
      dataProcArg, // same data proc.
      newDOS,
      this.suspendedExpressions // inherit same place to put these OVC suspensions from original.
      )
    Assert.invariant(currentInfosetNodeMaybe.isDefined)
    clone.currentInfosetNodeStack.push(this.currentInfosetNodeStack.top)
    val dstate = clone.dState
    dstate.setCurrentNode(thisElement.asInstanceOf[DINode])
    dstate.setVMap(variableMap)
    dstate.setContextNode(thisElement.asInstanceOf[DINode]) // used for diagnostics
    dstate.setArrayPos(arrayPos)
    dstate.setLocationInfo(bitPos1b, bitLimit1b, dataStream)
    dstate.setErrorOrWarn(this)
    dstate.resetValue
    dstate.setMode(Blocking)
    clone
  }

  //  private val dstateTable = new NonAllocatingMap[CompiledExpression, DState](
  //    new java.util.LinkedHashMap[CompiledExpression, DState])
  //  /**
  //   * Every expression has a DState that can be used for that expression.
  //   */
  //  def dState(expr: CompiledExpression): DState = {
  //    val maybeDstate = dstateTable.get(expr)
  //    if (maybeDstate.isDefined) maybeDstate.get
  //    else {
  //      val newDstate = new DState
  //      dstateTable.put(expr, newDstate)
  //      newDstate
  //    }
  //  }
  override def dataStream = Maybe(dataOutputStream)

  final val charBufferDataOutputStream = new LocalStack[CharBufferDataOutputStream](new CharBufferDataOutputStream)
  final val withUnparserDataInputStream = new LocalStack[StringDataInputStreamForUnparse](new StringDataInputStreamForUnparse)
  final val withByteArrayOutputStream = new LocalStack[(ByteArrayOutputStream, DirectOrBufferedDataOutputStream)](
    {
      val baos = new ByteArrayOutputStream() // PERFORMANCE: Allocates new object. Can reuse one from an onStack/pool via reset()
      val dos = DirectOrBufferedDataOutputStream(baos).asInstanceOf[DirectOrBufferedDataOutputStream]
      (baos, dos)
    },
    pair => pair match {
      case (baos, dos) =>
        baos.reset()
        dos.setMaybeRelBitLimit0b(MaybeULong.Nope)
        dos.setRelBitPos0b(ULong(0L))
    })

  @inline final def withTemporaryDataOutputStream[T](temp: DataOutputStream)(body: => T): T = {
    val savedDOS = dataOutputStream
    try {
      dataOutputStream = temp
      body
    } finally {
      dataOutputStream = savedDOS
    }
  }

  def addUnparseError(ue: UnparseError) {
    diagnostics = ue :: diagnostics
    status_ = new Failure(ue)
  }

  override def advance: Boolean = infosetCursor.advance
  override def advanceAccessor: InfosetAccessor = infosetCursor.advanceAccessor
  override def inspect: Boolean = infosetCursor.inspect
  override def inspectAccessor: InfosetAccessor = infosetCursor.inspectAccessor

  /**
   * Use this so if there isn't an event we get a clean diagnostic message saying
   * that is what has gone wrong.
   */
  def inspectOrError = {
    val m = inspectMaybe
    if (m.isEmpty) Assert.invariantFailed("An InfosetEvent was required for unparsing, but no InfosetEvent was available.")
    m.get
  }

  def advanceOrError = {
    val m = advanceMaybe
    if (m.isEmpty) Assert.invariantFailed("An InfosetEvent was required for unparsing, but no InfosetEvent was available.")
    m.get
  }

  def isInspectArrayEnd = {
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

  private var currentInfosetEvent_ : Maybe[InfosetAccessor] = Nope
  def currentInfosetNode: DINode =
    if (currentInfosetNodeMaybe.isEmpty) null
    else currentInfosetNodeMaybe.get

  override def currentNode = currentInfosetNodeMaybe

  def currentInfosetNodeMaybe: Maybe[DINode] =
    if (currentInfosetNodeStack.isEmpty) Nope
    else currentInfosetNodeStack.top

  def currentInfosetEvent = currentInfosetEvent_

  def setCurrentInfosetEvent(ev: Maybe[InfosetAccessor]) {
    currentInfosetEvent_ = ev
  }

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

  override def thisElement: InfosetElement = {
    Assert.usage(Maybe.WithNulls.isDefined(currentInfosetNode))
    val curNode = currentInfosetNode
    curNode match {
      case e: DIElement => e
      case a: DIArray => a.parent
    }
  }

  val unparseResult = new UnparseResult(dataProcArg, this)

  private def maybeCurrentInfosetElement: Maybe[DIElement] = {
    if (!Maybe.WithNulls.isDefined(currentInfosetNode)) Nope
    else {
      currentInfosetNode match {
        case e: DIElement => One(e)
        case a: DIArray => Nope
      }
    }
  }

  /**
   * flag indicates that the caller of the unparser does NOT care that the infoset
   * elements are retained; hence, the unparser is free to delete them, clobber
   * them or whatever it wants.
   */
  var removeUnneededInfosetElements = false // set from API of top level Unparser call.

  def currentLocation: DataLocation = {
    val m = maybeCurrentInfosetElement
    new DataLoc(bitPos1b, bitLimit1b, Left(dataOutputStream),
      if (m.isDefined) Maybe(m.value.runtimeData) else Nope)
  }

  val currentInfosetNodeStack = new MStack.OfMaybe[DINode]

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

  val escapeSchemeEVCache = new MStack.OfMaybe[EscapeSchemeUnparserHelper]

  val delimiterStack = new MStack.Of[DelimiterStackUnparseNode]()
  def pushDelimiters(node: DelimiterStackUnparseNode) = delimiterStack.push(node)
  def popDelimiters() = delimiterStack.pop
  def localDelimiters = delimiterStack.top
  def allTerminatingMarkup = {
    delimiterStack.iterator.flatMap { dnode =>
      (dnode.separator.toList ++ dnode.terminator.toList)
    }.toList
  }

  def bitPos0b = dataOutputStream.relBitPos0b.toLong
  def bitLimit0b = dataOutputStream.maybeRelBitLimit0b

  def charPos = -1L

  def validationError(msg: String, args: Any*) {
    val ctxt = getContext()
    val vde = new ValidationError(Some(ctxt.schemaFileLocation), this, msg, args: _*)
    status_ = new Failure(vde)
    diagnostics = vde :: diagnostics
  }

  def setVariables(newVariableMap: VariableMap) = {
    this.variableMap = newVariableMap
  }

  final def notifyDebugging(flag: Boolean) {
    dataOutputStream.setDebugging(flag)
  }

  /**
   * For outputValueCalc we accumulate the suspendables here.
   *
   * Note: only the primary UState (the initial one) will use this.
   * All the other clones used for outputValueCalc, those never
   * need to add any.
   */
  private val suspendedExpressions = initialSuspendedExpressions

  def addSuspendedExpression(se: SuspendableExpression) {
    suspendedExpressions.enqueue(se)
  }

  def evalSuspendedExpressions() {
    var countOfNotMakingProgress = 0
    while (!suspendedExpressions.isEmpty &&
      countOfNotMakingProgress < suspendedExpressions.length) {
      val se = suspendedExpressions.dequeue
      se.evaluate()
      if (!se.isDone) suspendedExpressions.enqueue(se)
      if (!se.isMakingProgress)
        countOfNotMakingProgress += 1
      else
        countOfNotMakingProgress = 0
    }
    // after the loop, did we terminate
    // with some expressions still unevaluated?
    if (suspendedExpressions.length > 0) {
      // unable to evaluate all the expressions
      throw new SuspendedExpressionsDeadlockException(suspendedExpressions.seq)
    }
  }

}

class SuspendedExpressionsDeadlockException(suspExprs: Seq[SuspendableExpression])
  extends RuntimeSchemaDefinitionError(
    suspExprs(0).diSimple.erd.schemaFileLocation,
    suspExprs(0).ustate,
    "Expressions are circularly deadlocked (mutually defined): %s",
    suspExprs)

object UState {

  def createInitialUState(
    out: DataOutputStream,
    dataProc: DFDL.DataProcessor,
    docSource: InfosetCursor): UState = {

    val variables = dataProc.getVariables
    val diagnostics = Nil
    val newState = new UState(docSource, variables, diagnostics, dataProc.asInstanceOf[DataProcessor], out)
    newState
  }
}
