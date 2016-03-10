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
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.xml.{ NamedQName, XMLUtils }
import edu.illinois.ncsa.daffodil.processors._
import scala.xml.Node
import scala.collection.immutable.Queue
import edu.illinois.ncsa.daffodil.dsom._
import com.ibm.icu.util.Calendar
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.calendar.DFDLCalendar
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.processors.unparsers.UnparseError
import edu.illinois.ncsa.daffodil.equality._; object EqualityNoWarn { EqualitySuppressUnusedImportWarning() }
import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.api.Diagnostic
import java.lang.{ Byte => JByte, Short => JShort, Integer => JInt, Long => JLong, Float => JFloat, Double => JDouble, Boolean => JBoolean }
import AsIntConverters._

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
      case _ => throw e // doSDE(e, state)
    }
  }

  private var blockLocation = MaybeULong.Nope

  /**
   * For unparsing of forward-referencing (outputValueCalc) expressions
   *
   * Depends on the dstate havng been properly initialized.
   * E.g., the variable map, current node, mode, etc.
   */
  def evaluateForwardReferencing(state: ParseOrUnparseState): Maybe[T] = {
    val value =
      try {
        val dstate = evaluateExpression(state, true)
        processForwardExpressionResults(dstate)
      } catch {
        case noChild: InfosetNoSuchChildElementException => { block(noChild); null }
        case noArrayIndex: InfosetArrayIndexOutOfBoundsException => { block(noArrayIndex); null }
        case nd: InfosetNoDataException if nd.erd.outputValueCalcExpr.isDefined => {
          // we got a no-data exception from an element with outputValueCalc
          val ovc = new OutputValueCalcEvaluationException(nd.diSimple)
          block(ovc); null
        }
        case ve: VariableException => { block(ve); null }
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

  private def block(noChild: InfosetNoSuchChildElementException) {
    uniqueULongPerBlockLocation(noChild.diComplex, noChild.info, 0, noChild)
  }

  private def block(noArrayIndex: InfosetArrayIndexOutOfBoundsException) {
    uniqueULongPerBlockLocation(noArrayIndex.diArray, noArrayIndex.diArray.erd.dpathElementCompileInfo, noArrayIndex.index, noArrayIndex)
  }

  private def block(ovc: OutputValueCalcEvaluationException) {
    uniqueULongPerBlockLocation(ovc.diSimple, ovc.diSimple.erd.dpathElementCompileInfo, 0, ovc)
  }

  private def block(ovc: VariableException) {
    uniqueULongPerBlockLocation(ovc.qname, ovc.context, 0, ovc)
  }

  /**
   * This is a Long that is unique for each location where expression evaluation can block
   * on forward reference during unparsing. The purpose of this is to detect if an expression
   * is blocking on different locations successively, but gradually making forward progress, or
   * if the expression is stuck and keeps blocking on the exact same missing information,
   * which means there is a cyclic dependency of some kind in the expression.
   */
  private def uniqueULongPerBlockLocation(diNode: AnyRef, info: AnyRef, index: Long, exc: Exception) {
    //
    // Not quite correct - this is based on hash codes, and since hash codes can overlap, there is a
    // slight possibility that this will produce the same value for distinct block locations.
    // That would make the caller think no forward progress was being made when in fact there is.
    //
    val num: Long = scala.math.abs(diNode.hashCode() + info.hashCode() + index + exc.getClass().hashCode())
    blockLocation = MaybeULong(num)
  }

  def expressionEvaluationBlockLocation = blockLocation

  private def evaluateExpression(state: ParseOrUnparseState, restart: Boolean): DState = {
    recipe.runExpression(state, restart) // initializes dstate from state, then runs
    val dstate = state.dState
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
        val dstate = evaluateExpression(state, false)
        processExpressionResults(dstate)
      } catch {
        case th: Throwable => handleThrow(th, state)
      }
    val value1 = postProcess(value, state)
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
  private def postProcess(v: AnyRef, state: ParseOrUnparseState): Maybe[T] = {
    val value = v
    value match {
      case null => {
        // there is no element. Can happen if one evaluates say, 5 + 6 in the debugger in which case
        // there is a value, but no node.
        return Nope
      }
      // case m: Maybe[AnyRef] => Assert.invariantFailed("should be null or value, not Maybe object.")
      case _ => // fall through
    }
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
