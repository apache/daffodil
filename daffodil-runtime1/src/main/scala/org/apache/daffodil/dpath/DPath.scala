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

package org.apache.daffodil.dpath

import org.apache.daffodil.exceptions._
import org.apache.daffodil.util._
import org.apache.daffodil.xml.NamedQName
import org.apache.daffodil.processors._
import org.apache.daffodil.infoset._
import org.apache.daffodil.dsom._
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.calendar.DFDLCalendar
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.processors.unparsers.UnparseError
import org.apache.daffodil.equality._; object EqualityNoWarn { EqualitySuppressUnusedImportWarning() }
import org.apache.daffodil.api.DataLocation
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.util.Numbers._
import org.apache.daffodil.processors.parsers.DoSDEMixin
import org.apache.daffodil.processors.parsers.PState

class ExpressionEvaluationException(e: Throwable, s: ParseOrUnparseState)
  extends ProcessingError(
    "Expression Evaluation",
    One(s.schemaFileLocation),
    Nope,
    Maybe(e),
    Nope)
// "Expression evaluation failed: %s",
// Misc.getSomeMessage(e).get

final class RuntimeExpressionDPath[T <: AnyRef](qn: NamedQName, tt: NodeInfo.Kind, recipe: CompiledDPath,
  dpathText: String,
  ci: DPathCompileInfo,
  isEvaluatedAbove: Boolean,
  override val contentReferencedElementInfos: Set[DPathElementCompileInfo],
  override val valueReferencedElementInfos: Set[DPathElementCompileInfo])
  extends CompiledExpression[T](qn, dpathText) with DoSDEMixin {

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

  private def UE(e: Throwable, maybeCL: Maybe[DataLocation]) =
    throw new UnparseError(One(ci.schemaFileLocation), maybeCL, e)

  private def doPE(e: Throwable, state: ParseOrUnparseState): Null = {
    state match {
      case null => Assert.usageError("state cannot be null")
      case ustate: UState => UE(e, One(ustate.currentLocation))
      case pstate: PState => {
        val pe = new ExpressionEvaluationException(e, state) // One(ci.schemaFileLocation), One(pstate.currentLocation), msg)
        pstate.setFailed(pe.toParseError)
      }
      case compState: CompileState => {
        val d = e match {
          case d: Diagnostic => d
          case _ =>
            new ExpressionEvaluationException(e, state)
        }
        compState.setFailed(d)
      }
    }
    null
  }

  private def handleCompileState(e: Diagnostic, state: ParseOrUnparseState): Null = {
    state match {
      //
      // Something went wrong when evaluting an expression at compilation time
      // so we suppress it and just indicate the failure.
      case cs: CompileState => {
        state.setFailed(e)
        null
      }
      /**
       * Almost anything that can go wrong in an expression maps to a SDE.
       * Except when we're evaluating a discriminator.
       *
       * E.g., node doesn't exist? Your expression is supposed to test for that
       * using fn:exists(...) rather than just use the location and expect backtracking
       * if it doesn't exist. An SDE isn't backtracked, so enforces this provision.
       *
       * But when in a discriminator, a node doesn't exist is a PE, not an SDE.
       */
      case ps: PState => ps.dState.mode match {
        case ParserDiscriminatorNonBlocking => {
          e match {
            //
            // In discriminators, these specific infoset exceptions are
            // processing errors, not RSDE
            //
            case nc: InfosetNoSuchChildElementException => doPE(e, ps)

            case ni: InfosetNoInfosetException => doPE(e, ps)

            case nd: InfosetNoDataException => doPE(e, ps)

            case ai: InfosetArrayIndexOutOfBoundsException => doPE(e, ps)

            // TBD: what about InfosetLengthUnknownException ?? That' more for
            // getting unparsing to block until the length information becomes
            // known. When parsing, this can only happen if..... you ask about the
            // content length of an IVC element ??

            //
            // outside discriminators they're all RSDE.
            //
            case _ => doSDE(e, ps)
          }
        }
        case _ => doSDE(e, ps)
      }
      case us: UState => throw e
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
  def evaluateForwardReferencing(state: ParseOrUnparseState, whereBlockedInfo: Suspension): Maybe[T] = {
    var value: Maybe[AnyRef] = Nope
    try {
      // TODO: This assumes a distinct state object (with its own dState) for every expression that
      // can be in evaluation simultaneously.
      val dstate = evaluateExpression(state, state.dState)
      value = Maybe(processForwardExpressionResults(dstate))
      whereBlockedInfo.setDone
    } catch {
      case unfin: InfosetNodeNotFinalException =>
        whereBlockedInfo.block(unfin.node, unfin.node.erd.dpathElementCompileInfo, 0, unfin)
      case noChild: InfosetNoSuchChildElementException =>
        whereBlockedInfo.block(noChild.diComplex, noChild.nqn, 0, noChild)
      case noSibling: InfosetNoNextSiblingException =>
        whereBlockedInfo.block(noSibling.diSimple, noSibling.info, 0, noSibling)
      case noArrayIndex: InfosetArrayIndexOutOfBoundsException =>
        whereBlockedInfo.block(noArrayIndex.diArray, noArrayIndex.diArray.erd.dpathElementCompileInfo, noArrayIndex.index, noArrayIndex)
      case nd: InfosetNoDataException if nd.erd.outputValueCalcExpr.isDefined => {
        // we got a no-data exception from an element with outputValueCalc
        // that is, some OVC element requested the value of another OVC element
        val ovc = new OutputValueCalcEvaluationException(nd)
        whereBlockedInfo.block(nd.diElement, nd.erd.dpathElementCompileInfo, 0, ovc)
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

  override def run(dstate: DState) {
    recipe.run(dstate)
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
    state match {
      case ustate: UState => // nothing. These are shared already
      case pstate: PState => pstate.setVariableMap(dstate.vmap) // TODO: if we're always copying this back, then no point in trying to isolate. Just share just like in UState case.
    }
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
      Assert.invariant(state.processorStatus ne Success)
      val cause = state.processorStatus.asInstanceOf[Failure].cause
      throw cause /* try-catch exists in Parser.parse1 to catch these */
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
      case e: InfosetException => handleCompileState(e.asDiagnostic, state)
      case e: VariableException => handleCompileState(e, state)
      case e: ExpressionEvaluationException => {
        state match {
          case cs: CompileState => handleCompileState(e, state)
          case _ => doPE(e, state)
        }
      }
      case e: IllegalStateException => doPE(e, state)
      case e: NumberFormatException => doPE(e, state)
      case e: IllegalArgumentException => doPE(e, state)
      case e: ArithmeticException => doPE(e, state)
      case e: FNErrorException => doPE(e, state)
      case e: UserDefinedFunctionProcessingErrorException => doPE(e, state)
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
              if (value.asInstanceOf[String].length == 0) {
                val e = new RuntimeSchemaDefinitionError(ci.schemaFileLocation, state, "Non-empty string required.")
                doSDE(e, state)
              }
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
