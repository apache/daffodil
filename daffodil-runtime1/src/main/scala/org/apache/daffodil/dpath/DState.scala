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

package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.processors._ ; import edu.illinois.ncsa.daffodil.infoset._
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.calendar.DFDLCalendar
import edu.illinois.ncsa.daffodil.equality._; object EqualityNoWarn2 { EqualitySuppressUnusedImportWarning() }
import edu.illinois.ncsa.daffodil.api.DataLocation
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInt }

/**
 * Modes for expression evaluation.
 */
sealed trait EvalMode

/**
 * Parsing always uses this mode, for everything except discriminators.
 *
 * Unparsing never uses this mode for anything.
 *
 * In this mode, evaluation is simple. If a child doesn't exist
 * but is needed to be traversed, an exception is thrown. If a value doesn't exist
 * an exception is thrown. fn:exists and fn:count return the answers
 * immediately based on the current state of the infoset object.
 *
 * In this mode, if an infoset node or value that is needed for evaluation is not found,
 * then it is a Runtime Schema Definition Error, i.e., a fatal error.
 */
sealed trait ParserMode extends EvalMode
case object ParserNonBlocking extends ParserMode

/**
 * Used when evaluating discriminators, which only occurs during parsing.
 *
 * In this mode, if a failure occurs in that an infoset node or value is not found
 * then it is a Processing error.
 */
case object ParserDiscriminatorNonBlocking extends ParserMode

/**
 * Unparsing uses this mode for outputValueCalc, variables,
 * and length expressions, when it wants to actually suspend a co-routine that
 * can be resumed.
 *
 * In this mode, we must be running on a co-routine which can be blocked.
 *
 * In this mode, evaluation can suspend waiting for a child
 * node to be added/appended, or for a value to be set.
 *
 * fn:count can block if the array isn't final yet.
 * fn:exists can block if the array or element isn't final yet.
 *
 * Once the situation leading to the suspension changes, evaluation is
 * retried.
 */
case object UnparserBlocking extends EvalMode

/**
 *  Unparsing uses this mode for evaluation to determine whether or not
 *  something will block.
 *
 *  In this mode, we are running on the main co-routine, so we can't actually block.
 *
 *  However, unlike ParserNonBlocking mode, in this mode fn:count of a non-final
 *  array will error, indicating to the caller that the expression would have
 *  blocked waiting for that array to be finalized. Similarly fn:exists can
 *  error, as can many other things. Anything that would have blocked in UnparserBlocking
 *  mode will instead error.
 */
case object UnparserNonBlocking extends EvalMode

/**
 * expression evaluation side-effects this state block.
 *
 * For DPath expressions, all infoset content must be obtained via methods on this object so that
 * if used in a forward referencing expression the expression can block until the information
 * becomes available.
 */
case class DState() {
  import edu.illinois.ncsa.daffodil.util.Numbers._

  var isCompile = false

  var opIndex: Int = 0

  /**
   * The currentValue is used when we have a value that is not
   * associated with an element of simple type. E.g., If I have
   * the expression 5 + $x, then none of those literals, nor their
   * nor variable value, nor their sum, has an element associated with it.
   */
  private var _currentValue: AnyRef = null

  private var _mode: EvalMode = ParserNonBlocking

  /**
   * Set to UnparserBlocking for forward-referencing expressions during unparsing.
   */
  def setMode(m: EvalMode) {
    _mode = m
  }
  def mode = _mode

  //
  //  The purpose of this commented out code was to retry expressions, not
  //  from the begining, but closer to where the expression failed. That is
  //  for a big complex expression, we would not redo the whole thing, but would
  //  block the co-routine right where it was unable to proceed e.g., in a call to
  //  node.dataValue, such that when it was resumed, it would resume right there.
  //  retry the call to node.dataValue, and then carry on with the rest of the
  //  expression.
  //
  //  If we don't do this, then we're not taking advantage of that optimization
  //  Then we don't really need coroutines at all to implement blocking. We just
  //  keep the saved/cloned state around, and retry whatever needed to be done, from scratch.
  //
  //  The vast bulk of expressions are going to be fairly small, so this extra
  //  overhead vs. the complexity of coroutines?? I think we should move away
  //  From coroutines.
  //
  //  However, if expressions were to block on the infoset, by queuing themselves
  //  adjacent to what they need, then... well you can still just retry the whole
  //  expression again, this time knowing it won't block in the same place.
  //
  //  private var thisExpressionCoroutine_ : Maybe[Coroutine[AnyRef]] = Nope
  //
  //  def setThisExpressionCoroutine(co: Maybe[Coroutine[AnyRef]]) {
  //    Assert.usage(mode eq UnparserBlocking)
  //    thisExpressionCoroutine_ = co
  //  }
  //
  //  def thisExpressionCoroutine = {
  //    Assert.usage(mode eq UnparserBlocking)
  //    thisExpressionCoroutine_
  //  }
  //
  //  private var coroutineToResumeIfBlocked_ : Maybe[Coroutine[AnyRef]] = Nope
  //
  //  def setCoroutineToResumeIfBlocked(co: Maybe[Coroutine[AnyRef]]) {
  //    Assert.usage(mode eq UnparserBlocking)
  //    coroutineToResumeIfBlocked_ = co
  //  }
  //
  //  def coroutineToResumeIfBlocked = {
  //    Assert.usage(mode eq UnparserBlocking)
  //    coroutineToResumeIfBlocked_
  //  }

  def resetValue() {
    _currentValue = null
  }

  def currentValue: AnyRef = {
    if (_currentValue eq null)
      withRetryIfBlocking {
        currentSimple.dataValue
      }
    else _currentValue
  }

  def setCurrentValue(v: Any) {
    Assert.invariant(!v.isInstanceOf[(Any, Any)])
    _currentValue = asAnyRef(v)
    _currentNode = null
  }

  def booleanValue: Boolean = currentValue.asInstanceOf[Boolean]

  def longValue: Long = asLong(currentValue)
  def intValue: Int = longValue.toInt
  def doubleValue: Double = asDouble(currentValue)

  def integerValue: JBigInt = asBigInt(currentValue)
  def decimalValue: JBigDecimal = asBigDecimal(currentValue)
  def stringValue: String = currentValue.asInstanceOf[String]

  def isNilled: Boolean = currentElement.isNilled

  private def isAnArray(): Boolean = {
    if (!currentNode.isInstanceOf[DIArray]) {
      Assert.invariant(errorOrWarn.isDefined)
      if (currentNode.isInstanceOf[DIElement]) {
        errorOrWarn.get.SDW("The specified path to element %s is not to an array. Suggest using fn:exists instead.", currentElement.name)
      } else {
        errorOrWarn.get.SDW("The specified path is not to an array. Suggest using fn:exists instead.")
      }
      false
    } else {
      true
    }
  }

  def arrayLength: Long =
    if (isAnArray()) currentArray.length
    else 1L

  def finalArrayLength: Long =
    if (isAnArray()) {
      currentArray.requireFinal
      currentArray.length
    } else 1L

  def exists: Boolean = true // we're at a node, so it must exist.

  def dateValue: DFDLCalendar = currentValue.asInstanceOf[DFDLCalendar]
  def timeValue: DFDLCalendar = currentValue.asInstanceOf[DFDLCalendar]
  def dateTimeValue: DFDLCalendar = currentValue.asInstanceOf[DFDLCalendar]

  /**
   * Array index calculations (that is [expr], what XPath
   * calls 'predicate')
   */
  def index: Int = longValue.toInt

  private var _currentNode: DINode = null

  def currentNode = _currentNode
  def setCurrentNode(n: DINode) {
    _currentNode = n
    _currentValue = null
  }

  def currentSimple = {
    Assert.usage(currentNode != null)
    Assert.usage(mode != null)
    val cs = currentNode.asSimple
    //
    // If this is a computed value (for unparsing for dfdl:outputValueCalc property)
    // then if it is not yet computed, compute it.
    //
    // TODO: remove this code perhaps? This is demanded elsewhere....
    //
    //    if (mode =:= UnparseMode && !cs.hasValue && cs.runtimeData.outputValueCalcExpr.isDefined) {
    //      val expr = cs.runtimeData.outputValueCalcExpr.get
    //      val expressionValue = expr.evaluate(ustate)
    //      cs.setDataValue(expressionValue)
    //    }
    cs
  }

  def currentElement = currentNode.asInstanceOf[DIElement]
  def currentArray = currentNode.asInstanceOf[DIArray]
  def currentComplex = currentNode.asComplex

  private var _vbox: VariableBox = null

  def vmap = {
    Assert.usage(_vbox ne null)
    _vbox.vmap
  }

  /**
   * Used by PState and parser, where we want to isolate the modifications
   * to the vmap per expression evaluation. This isolate makes backtracking
   * changes to the vmap easy. Just don't copy the vmap back from the DState
   * into the PState, and the changes are gone.
   */
  def setVMap(m: VariableMap) {
    if (_vbox eq null) {
      _vbox = new VariableBox(m)
    } else {
      _vbox.setVMap(m)
    }
  }

  /**
   * Used by UState, where we want to shared the vmap as modified by the
   * expression evaluations that use the DState.
   */
  def setVBox(box: VariableBox) {
    _vbox = box
  }

  def runtimeData = {
    if (contextNode.isDefined) One(contextNode.get.erd)
    else Nope
  }

  private var _contextNode: Maybe[DINode] = Nope
  def contextNode = _contextNode
  def setContextNode(node: DINode) {
    _contextNode = One(node)
  }

  //  private var _bitPos1b: MaybeULong = MaybeULong.Nope
  //  private var _bitLimit1b: MaybeULong = MaybeULong.Nope
  //  private var _dataStream: Maybe[DataStreamCommon] = Nope
  //
  //  def setLocationInfo(bitPos1b: Long, bitLimit1b: MaybeULong, dataStream: Maybe[DataStreamCommon]) {
  //    _bitPos1b = MaybeULong(bitPos1b)
  //    _bitLimit1b = bitLimit1b
  //    _dataStream = dataStream
  //  }
  //
  //  def setLocationInfo() {
  //    _bitPos1b = MaybeULong.Nope
  //    _bitLimit1b = MaybeULong.Nope
  //    _dataStream = Nope
  //  }

  def contextLocation: Maybe[DataLocation] = {
    Nope
  }

  private var _savesErrorsAndWarnings: Maybe[SavesErrorsAndWarnings] = Nope
  def errorOrWarn = _savesErrorsAndWarnings
  def setErrorOrWarn(s: SavesErrorsAndWarnings) {
    _savesErrorsAndWarnings = One(s)
  }

  private var _arrayPos: Long = -1L // init to -1L so that we must set before use.
  def arrayPos = _arrayPos
  def setArrayPos(arrayPos1b: Long) {
    _arrayPos = arrayPos1b
  }

  def SDE(formatString: String, args: Any*) = {
    Assert.usage(runtimeData.isDefined)
    errorOrWarn.get.SDE(formatString, args: _*)
  }

  // These exists so we can override it in our fake DState we use when
  // checking expressions to see if they are constants. SelfMove
  // for real is a no-op, but when we're evaluating an expression to see if
  // it is a constant, the expression "." aka self, isn't constant.
  // Similarly, if you call fn:exists(....) and the contents are not gong to
  // exist at constant fold time. But we don't want fn:exists to say the result
  // is always a constant (false) because at constant folding time, hey, nothing
  // exists... this hook lets us change behavior for constant folding to throw.
  def selfMove(): Unit = {}
  def fnExists(): Unit = {}

  // @inline // TODO: Performance maybe this won't allocate a closure if this is inline? If not replace with macro
  final def withRetryIfBlocking[T](body: => T): T =
    DState.withRetryIfBlocking(this)(body)
}

object DState {

  // private object ToBeIgnored

  // @inline
  final def withRetryIfBlocking[T](ds: DState)(body: => T): T = { // TODO: Performance maybe this won't allocate a closure if this is inline? If not replace with macro
    ds.mode match {
      case _: ParserMode => body
      case UnparserNonBlocking => body
      case UnparserBlocking => {
        var isDone = false
        var res: T = null.asInstanceOf[T]
        while (!isDone) {
          try {
            res = body
            isDone = true
          } catch {
            case e: RetryableException => {
              // we're to block here, and retry subsequently.
              isDone = false
              //              if (ds.thisExpressionCoroutine.isDefined && ds.coroutineToResumeIfBlocked.isDefined) {
              //                ds.thisExpressionCoroutine.get.resume(ds.coroutineToResumeIfBlocked.get, ToBeIgnored)
              //              } else {
              throw e
              //              }
            }
          }
        }
        res
      }
    }
  }
}

class DStateForConstantFolding extends DState {
  private def die = throw new java.lang.IllegalStateException("No infoset at compile time.")

  override def currentSimple = currentNode.asInstanceOf[DISimple]
  override def currentElement = die
  override def currentArray = die
  override def currentComplex = die
  override def currentNode = new FakeDINode
  override def runtimeData = die
  override def vmap = die
  override def selfMove() = die
  override def fnExists() = die
  override def arrayPos = die
  override def arrayLength = die
}
