package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.processors.ProcessorResult
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.DINode
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.DISimple
import edu.illinois.ncsa.daffodil.processors.ParseOrUnparseState
import edu.illinois.ncsa.daffodil.processors.InfosetItem
import edu.illinois.ncsa.daffodil.dpath.DState
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.exceptions.SavesErrorsAndWarnings
import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.processors.DataLoc
import edu.illinois.ncsa.daffodil.processors.DataProcessor
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.UnparseResult
import edu.illinois.ncsa.daffodil.processors.Success
import edu.illinois.ncsa.daffodil.processors.Failure
import edu.illinois.ncsa.daffodil.processors.InfosetElement
import edu.illinois.ncsa.daffodil.processors.DIArray
import edu.illinois.ncsa.daffodil.dsom.ValidationError
import edu.illinois.ncsa.daffodil.processors.DelimiterStackNode
import edu.illinois.ncsa.daffodil.processors.DelimiterStackUnparseNode
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeUnparserHelper
import edu.illinois.ncsa.daffodil.processors.Failure
import edu.illinois.ncsa.daffodil.processors.DIElement
import edu.illinois.ncsa.daffodil.events.MultipleEventHandler
import edu.illinois.ncsa.daffodil.io.DataOutputStream
import edu.illinois.ncsa.daffodil.io.BasicDataOutputStream
import edu.illinois.ncsa.daffodil.io.DataStreamCommon
import scala.collection.mutable

sealed trait UnparserMode
case object UnparseMode extends UnparserMode
case object AccumulateNodesMode extends UnparserMode

class UState(
  infosetSource: InfosetSource,
  vmap: VariableMap,
  diagnosticsArg: List[Diagnostic],
  dataProcArg: DataProcessor,
  var dataOutputStream: DataOutputStream)
  extends ParseOrUnparseState(new DState, vmap, diagnosticsArg, dataProcArg)
  with Iterator[InfosetEvent] with ThrowsSDE with SavesErrorsAndWarnings {

  override def dataStream: DataStreamCommon = dataOutputStream

  def withTemporaryDataOutputStream[T](temp: DataOutputStream)(body: => T): T = {
    val savedDOS = dataOutputStream
    try {
      dataOutputStream = temp
      body
    } finally {
      dataOutputStream = savedDOS
    }
  }

  def copyUState() = {
    val res = new UState(infosetSource, variableMap, diagnostics, dataProc, dataOutputStream)
    res.dataOutputStream = dataOutputStream.asInstanceOf[BasicDataOutputStream].copyOutStream
    res
  }

  def addUnparseError(ue: UnparseError) {
    diagnostics = ue :: diagnostics
    status_ = new Failure(ue)
  }

  def peekArrayEnd = {
    val p = peek
    val res = p match {
      case End(a: DIArray) => true
      case _ => false
    }
    res
  }

  private var currentInfosetEvent_ : Maybe[InfosetEvent] = Nope
  def currentInfosetNode = if (currentInfosetNodeStack.isEmpty) Nope else currentInfosetNodeStack.top
  def currentInfosetEvent = currentInfosetEvent_

  def hasNext = infosetSource.hasNext

  def peek = infosetSource.peek // we depend on the infosetSource.peek method to check hasNext.

  def next = {
    Assert.usage(hasNext)
    val ev = infosetSource.next
    currentInfosetEvent_ = One(ev)
    if (!currentInfosetNodeStack.isEmpty) currentInfosetNodeStack.pop
    currentInfosetNodeStack.push(One(ev.node))
    ev
  }

  var mode: UnparserMode = UnparseMode

  override def hasInfoset = currentInfosetNode.isDefined

  override def infoset = {
    Assert.invariant(currentInfosetNode.isDefined)
    currentInfosetNode.get match {
      case a: DIArray => {
        a.getOccurrence(arrayPos)
      }
      case e: DIElement => thisElement
    }
  }

  override def thisElement: InfosetElement = currentInfosetNode.get.asInstanceOf[InfosetElement]

  private var status_ : ProcessorResult = Success

  override def status = status_

  val unparseResult = new UnparseResult(dataProcArg, this)

  def addDeferredElement(elt: DINode) {
    Assert.usage(elt.isInstanceOf[DISimple]) // only simple types for outputValueCalc

    //TODO: Implement something here. For now, we're going to have a really 
    //limited implementation - we know we can evaluate the deferred element
    //expressions when we have the ENTIRE infoset.
  }

  private def maybeCurrentInfosetElement: Maybe[DIElement] = {
    if (!currentInfosetNode.isDefined) Nope
    else {
      currentInfosetNode.get match {
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
    new DataLoc(bitPos1b, bitLimit1b, Left(dataOutputStream),
      maybeCurrentInfosetElement.map { _.runtimeData })
  }

  val currentInfosetNodeStack = mutable.ArrayStack[Maybe[DINode]]()

  val arrayIndexStack = mutable.ArrayStack[Long](1L)
  def moveOverOneArrayIndexOnly() = arrayIndexStack.push(arrayIndexStack.pop + 1)
  def arrayPos = arrayIndexStack.top

  val groupIndexStack = mutable.ArrayStack[Long](1L)
  def moveOverOneGroupIndexOnly() = groupIndexStack.push(groupIndexStack.pop + 1)
  def groupPos = groupIndexStack.top

  // TODO: it doesn't look anything is actually reading the value of childindex
  // stack. Can we get rid of it?
  val childIndexStack = mutable.ArrayStack[Long](1L)
  def moveOverOneElementChildOnly() = childIndexStack.push(childIndexStack.pop + 1)
  def childPos = childIndexStack.top

  val occursBoundsStack = new mutable.ArrayStack[Long]
  def updateBoundsHead(ob: Long) = {
    occursBoundsStack.pop()
    occursBoundsStack.push(ob)
  }

  def occursBounds = occursBoundsStack.top

  var currentEscapeScheme: Maybe[EscapeSchemeUnparserHelper] = Nope

  val delimiterStack = new mutable.ArrayStack[DelimiterStackUnparseNode]()
  def pushDelimiters(node: DelimiterStackUnparseNode) = delimiterStack.push(node)
  def popDelimiters() = delimiterStack.pop
  def localDelimiters = delimiterStack.top

  def bitPos0b = dataOutputStream.bitPos0b
  def bitLimit0b = dataOutputStream.bitLimit0b

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

}

object UState {

  def createInitialUState(
    out: DataOutputStream,
    dataProc: DFDL.DataProcessor,
    docSource: InfosetSource): UState = {

    val variables = dataProc.getVariables
    val diagnostics = Nil
    val newState = new UState(docSource, variables, diagnostics, dataProc.asInstanceOf[DataProcessor], out)
    newState
  }
}
