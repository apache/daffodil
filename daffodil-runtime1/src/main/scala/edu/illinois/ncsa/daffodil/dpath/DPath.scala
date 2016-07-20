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

import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.xml.NamedQName
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.calendar.DFDLCalendar
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.processors.unparsers.UnparseError
import edu.illinois.ncsa.daffodil.equality._; object EqualityNoWarn { EqualitySuppressUnusedImportWarning() }
import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.api.Diagnostic
import AsIntConverters._

trait WhereBlockedLocation {

  private var priorNodeOrVar: Maybe[AnyRef] = Nope
  private var priorInfo: Maybe[AnyRef] = Nope
  private var priorIndex: MaybeInt = MaybeInt.Nope
  private var priorExc: Maybe[AnyRef] = Nope

  private var maybeNodeOrVar: Maybe[AnyRef] = Nope
  private var maybeInfo: Maybe[AnyRef] = Nope
  private var maybeIndex: MaybeInt = MaybeInt.Nope
  private var maybeExc: Maybe[AnyRef] = Nope

  private var done_ : Boolean = false

  final def setDone {
    done_ = true
  }

  final def isDone = done_

  final def block(nodeOrVar: AnyRef, info: AnyRef, index: Long, exc: AnyRef) {
    Assert.usage(nodeOrVar ne null)
    Assert.usage(info ne null)
    Assert.usage(exc ne null)
    priorNodeOrVar = maybeNodeOrVar
    priorInfo = maybeInfo
    priorIndex = maybeIndex
    priorExc = maybeExc
    maybeNodeOrVar = One(nodeOrVar)
    maybeInfo = One(info)
    maybeIndex = MaybeInt(index.toInt)
    maybeExc = One(exc)
    done_ = false
  }

  final def isBlocked: Boolean = !done_ && maybeNodeOrVar.isDefined

  final def isBlockedFirstTime: Boolean = {
    isBlocked &&
      priorNodeOrVar.isEmpty
  }

  final def isBlockedSameLocation: Boolean = {
    val res = isBlocked &&
      {
        if (priorNodeOrVar.isEmpty) false
        else {
          Assert.invariant(maybeNodeOrVar.isDefined)
          val res =
            maybeNodeOrVar.get == priorNodeOrVar.get &&
              maybeInfo.get == priorInfo.get &&
              maybeIndex.get == priorIndex.get &&
              maybeExc.get == priorExc.get
          res
        }
      }
    res
  }
}

class ExpressionEvaluationException(e: Throwable, s: ParseOrUnparseState)
  extends ProcessingError("Expression Error",
    One(s.schemaFileLocation),
    Nope,
    "Expression evaluation failed: %s",
    DiagnosticUtils.getSomeMessage(e).get)

class RuntimeExpressionDPath[T <: AnyRef](qn: NamedQName, tt: NodeInfo.Kind, recipe: CompiledDPath,
  dpathText: String,
  ci: DPathCompileInfo,
  isEvaluatedAbove: Boolean)
    extends CompiledExpression[T](qn, dpathText) {

  override def targetType = tt

  // TODO: fix this check below. There is a unierse of target types which is
  // muuch smaller than the set of all types, so some check is useful to be sure
  // we stay within the subset of types that are actually used as target types.
  //  Assert.usage(targetType == NodeInfo.AnyType // used by debugger eval stmt
  //    || targetType == NodeInfo.NonEmptyString // string-valued properties
  //    || targetType == NodeInfo.Long // length and occurs expressions
  //    || targetType == NodeInfo.Boolean // assert/discriminator test expressions
  //    || targetType.isInstanceOf[NodeInfo.AnyAtomic.Kind] // inputValueCalc, outputValueCalc,
  //    // setVariable, defineVariable (default expressions), newVariableInstance
  //    , "not an accepted targetType")

  override lazy val prettyExpr = dpathText

  def isKnownNonEmpty = true // expressions are not allowed to return empty string

  private def UE(msg: String, maybeCL: Maybe[DataLocation]) = UnparseError(One(ci.schemaFileLocation), maybeCL, msg)

  private def doPE(e: Throwable, state: ParseOrUnparseState) = {
    val msg = "Expression evaluation failed due to: %s.".format(DiagnosticUtils.getSomeMessage(e).get)
    state match {
      case null => Assert.usageError("state cannot be null")
      case ustate: UState => UE(msg, One(ustate.currentLocation))
      case pstate: PState => {
        val pe = new ExpressionEvaluationException(e, state) // One(ci.schemaFileLocation), One(pstate.currentLocation), msg)
        pstate.setFailed(pe)
        null
      }
      case compState: CompileState => {
        val d = e match {
          case d: Diagnostic => d
          case _ =>
            new ExpressionEvaluationException(e, state)
        }
        compState.setFailed(d)
        null
      }
    }
  }

  private def handleCompileState(e: Diagnostic, state: ParseOrUnparseState) = {
    state match {
      case cs: CompileState => {
        state.setFailed(e)
        null
      }
      /**
       * Almost anything that can go wrong in an expression maps to a SDE.
       *
       * E.g., node doesn't exist? Your expression is supposed to test for that
       * using fn:exists(...) rather than just use the location and expect backtracking
       * if it doesn't exist. An SDE isn't backtracked, so enforces this provision.
       */
      case _ => throw e
    }
  }

  // This was a bug: you can't have state here. These things are shared across threads.
  // private var blockLocation = MaybeULong.Nope

  /**
   * For unparsing of forward-referencing (outputValueCalc) expressions
   *
   * Depends on the dstate havng been properly initialized.
   * E.g., the variable map, current node, mode, etc.
   *
   * The whereBlockedInfo is a return data structure that is side-effected to indicate
   * the block location. I.e., where in the infoset are we blocked. Used for forward-progress-checking
   * so as to detect deadlocks.
   */
  def evaluateForwardReferencing(state: ParseOrUnparseState, whereBlockedInfo: WhereBlockedLocation): Maybe[T] = {
    var value: Maybe[AnyRef] = Nope
    try {
      // TODO: This assumes a distinct state object (with its own dState) for every expression that
      // can be in evaluation simultaneously.
      val dstate = evaluateExpression(state, state.dState)
      value = Maybe(processForwardExpressionResults(dstate))
      whereBlockedInfo.setDone
    } catch {
      case noChild: InfosetNoSuchChildElementException =>
        whereBlockedInfo.block(noChild.diComplex, noChild.info, 0, noChild)
      case noArrayIndex: InfosetArrayIndexOutOfBoundsException =>
        whereBlockedInfo.block(noArrayIndex.diArray, noArrayIndex.diArray.erd.dpathElementCompileInfo, noArrayIndex.index, noArrayIndex)
      case nd: InfosetNoDataException if nd.erd.outputValueCalcExpr.isDefined => {
        // we got a no-data exception from an element with outputValueCalc
        // that is, some OVC element requested the value of another OVC element
        val ovc = new OutputValueCalcEvaluationException(nd.diSimple)
        whereBlockedInfo.block(ovc.diSimple, ovc.diSimple.erd.dpathElementCompileInfo, 0, ovc)
      }
      case ve: VariableException =>
        whereBlockedInfo.block(ve.qname, ve.context, 0, ve)
      case noLength: InfosetLengthUnknownException =>
        whereBlockedInfo.block(noLength.diElement, noLength.erd, 0, noLength)
      case th: Throwable => handleThrow(th, state)
    }
    val value1 = postProcess(value, state)
    value1
  }

  private def processForwardExpressionResults(dstate: DState): AnyRef = {
    val v: AnyRef = {
      dstate.currentNode match {
        case null => {
          // there is no element. Can happen if one evaluates say, 5 + 6 or 5 + $variable
          Assert.invariant(dstate.currentValue != null)
          dstate.currentValue
        }
        case n: DIElement if n.isNilled => n
        case c: DIComplex => {
          Assert.invariant(!targetType.isInstanceOf[NodeInfo.AnyAtomic.Kind])
          c
        }
        case s: DISimple => {
          s.dataValue
        }
        case _ => Assert.invariantFailed("must be an element, simple or complex.")
      }
    }
    v
  }

  private def evaluateExpression(state: ParseOrUnparseState, dstate: DState): DState = {
    recipe.runExpression(state, dstate) // initializes dstate from state, then runs
    state.variableMap = dstate.vmap
    dstate
  }

  /**
   * For parsing or unparsing of backward-referencing expressions.
   * That is, not outputValueCalc.
   */
  private def evaluateMaybe(state: ParseOrUnparseState): Maybe[T] = {
    val value =
      try {
        val dstate = evaluateExpression(state, state.dState)
        processExpressionResults(dstate)
      } catch {
        case th: Throwable => handleThrow(th, state)
      }
    val value1 = postProcess(Maybe(value), state)
    value1
  }

  final def evaluate(state: ParseOrUnparseState): T = {
    val maybeRes = evaluateMaybe(state)
    if (maybeRes.isDefined) {
      maybeRes.value
    } else {
      Assert.invariant(state.status ne Success)
      throw state.status.asInstanceOf[Failure].cause
    }
  }

  def isConstant = false
  def constant: T = Assert.usageError("Not a constant.")

  private def processExpressionResults(dstate: DState): AnyRef = {
    val v = {
      dstate.currentNode match {
        case null => {
          // there is no element. Can happen if one evaluates say, 5 + 6 in the debugger in which case
          // there is a value, but no node.
          dstate.currentValue
        }
        case n: DIElement if n.isNilled => n
        case c: DIComplex => {
          Assert.invariant(!targetType.isInstanceOf[NodeInfo.AnyAtomic.Kind])
          c
        }
        case s: DISimple => {
          try {
            s.dataValue
          } catch {
            case ovc: OutputValueCalcEvaluationException => {
              Assert.invariantFailed("OVC should always have a data value by the time it reaches here.")
            }
          }
        }
        case _ => Assert.invariantFailed("must be an element, simple or complex.")
      }
    }
    v
  }

  private def handleThrow(th: Throwable, state: ParseOrUnparseState): Null = {
    th match {
      //
      // Here we catch exceptions that indicate something went wrong with the
      // expression, but by that we mean legal evaluation of a compiled expression
      // produced an error such as divide by zero or string having wrong format for
      // conversion to another type. I.e., things the DFDL schema author could get
      // wrong that some data inputs would exacerbate.
      //
      // This should not catch things that indicate a Daffodil code problem e.g.,
      // class cast exceptions.
      //
      // Of course some things that are daffodil code bugs can hide - if for example
      // there is an arithmetic error in daffodil code, this catch can't distinguish
      // that error (which should be an abort, from an arithmetic exception
      // due to an expression dividing by zero say.
      case e: InfosetException => handleCompileState(e, state)
      case e: VariableException => handleCompileState(e, state)
      case e: IllegalStateException => doPE(e, state)
      case e: NumberFormatException => doPE(e, state)
      case e: IllegalArgumentException => doPE(e, state)
      case e: ArithmeticException => doPE(e, state)
      case th => throw th
    }
  }

  /**
   * Accepts a value or null to mean there is no value.
   *
   * Note: Can't use a Maybe[T] here because T is required to be an AnyRef, and that would exclude
   * Long, Int, etc. from being used as values.
   */
  private def postProcess(v: Maybe[AnyRef], state: ParseOrUnparseState): Maybe[T] = {
    if (v.isEmpty) Nope
    else {
      val value = v.get
      val value1 =
        if (!value.isInstanceOf[DIElement]) {
          targetType match {
            case NodeInfo.AnyType => value // ok
            case NodeInfo.Long => asLong(value)
            case NodeInfo.UnsignedLong => asLong(value)
            case NodeInfo.NonEmptyString => {
              Assert.invariant(value.isInstanceOf[String])
              ci.schemaDefinitionUnless(value.asInstanceOf[String].length > 0,
                "Non-empty string required.")
              value
            }
            case NodeInfo.DateTime | NodeInfo.Date | NodeInfo.Time => {
              Assert.invariant(value.isInstanceOf[DFDLCalendar])
              value
            }
            case _: NodeInfo.String.Kind => {
              Assert.invariant(value.isInstanceOf[String])
              value
            }
            case NodeInfo.Boolean => {
              Assert.invariant(value.isInstanceOf[Boolean])
              value
            }
            case NodeInfo.HexBinary => {
              Assert.invariant(value.isInstanceOf[Array[Byte]])
              value
            }
            case _ => // TODO: add more checks. E.g., that proper type matching occurred for all the number
              // and date types as well.
              value
          }
        } else {
          value
        }
      One(value1.asInstanceOf[T])
    }
  }

}
