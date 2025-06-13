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

package org.apache.daffodil.runtime1.dpath

import java.lang.{
  Byte => JByte,
  Double => JDouble,
  Float => JFloat,
  Integer => JInt,
  Long => JLong,
  Short => JShort
}
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInt }
import java.net.URI

import org.apache.daffodil.lib.calendar.DFDLCalendar
import org.apache.daffodil.lib.equality.EqualitySuppressUnusedImportWarning
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.iapi.DataLocation
import org.apache.daffodil.lib.iapi.Diagnostic
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.lib.xml.NamedQName
import org.apache.daffodil.runtime1.dsom.CompiledExpression
import org.apache.daffodil.runtime1.dsom.DPathCompileInfo
import org.apache.daffodil.runtime1.dsom.DPathElementCompileInfo
import org.apache.daffodil.runtime1.dsom.RuntimeSchemaDefinitionError
import org.apache.daffodil.runtime1.infoset.DIComplex
import org.apache.daffodil.runtime1.infoset.DIElement
import org.apache.daffodil.runtime1.infoset.DISimple
import org.apache.daffodil.runtime1.infoset.InfosetArrayIndexOutOfBoundsException
import org.apache.daffodil.runtime1.infoset.InfosetException
import org.apache.daffodil.runtime1.infoset.InfosetLengthUnknownException
import org.apache.daffodil.runtime1.infoset.InfosetNoDataExceptionBase
import org.apache.daffodil.runtime1.infoset.InfosetNoInfosetException
import org.apache.daffodil.runtime1.infoset.InfosetNoNextSiblingException
import org.apache.daffodil.runtime1.infoset.InfosetNoSuchChildElementException
import org.apache.daffodil.runtime1.infoset.InfosetNodeNotFinalException
import org.apache.daffodil.runtime1.infoset.InfosetSelfReferencingException
import org.apache.daffodil.runtime1.infoset.OutputValueCalcEvaluationException
import org.apache.daffodil.runtime1.processors.CompileState
import org.apache.daffodil.runtime1.processors.Failure
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.ProcessingError
import org.apache.daffodil.runtime1.processors.Success
import org.apache.daffodil.runtime1.processors.Suspension
import org.apache.daffodil.runtime1.processors.VariableException
import org.apache.daffodil.runtime1.processors.parsers.DoSDEMixin
import org.apache.daffodil.runtime1.processors.parsers.PState
import org.apache.daffodil.runtime1.processors.unparsers.UState
import org.apache.daffodil.runtime1.processors.unparsers.UnparseError
import org.apache.daffodil.runtime1.udf.UserDefinedFunctionProcessingErrorException;
object EqualityNoWarn { EqualitySuppressUnusedImportWarning() }

class ExpressionEvaluationException(e: Throwable, s: ParseOrUnparseState)
  extends ProcessingError(
    "Expression Evaluation",
    One(s.schemaFileLocation),
    Nope,
    Maybe(e),
    Nope
  )
// "Expression evaluation failed: %s",
// Misc.getSomeMessage(e).get

final class RuntimeExpressionDPath[T <: AnyRef](
  qn: NamedQName,
  tt: NodeInfo.Kind,
  recipe: CompiledDPath,
  dpathText: String,
  ci: DPathCompileInfo,
  isEvaluatedAbove: Boolean,
  override val contentReferencedElementInfos: Set[DPathElementCompileInfo],
  override val valueReferencedElementInfos: Set[DPathElementCompileInfo]
) extends CompiledExpression[T](qn, dpathText)
  with DoSDEMixin {

  override def targetType = tt

  // TODO: fix this check below. There is a universe of target types which is
  // much smaller than the set of all types, so some check is useful to be sure
  // we stay within the subset of types that are actually used as target types.
  //  Assert.usage(targetType == NodeInfo.AnyType // used by debugger eval stmt
  //    || targetType == NodeInfo.NonEmptyString // string-valued properties
  //    || targetType == NodeInfo.Long // length and occurs expressions
  //    || targetType == NodeInfo.Boolean // assert/discriminator test expressions
  //    || targetType.isInstanceOf[NodeInfo.AnyAtomic.Kind] // inputValueCalc, outputValueCalc,
  //    // setVariable, defineVariable (default expressions), newVariableInstance
  //    , "not an accepted targetType")

  override lazy val prettyExpr = dpathText

  private def UE(e: Throwable, maybeCL: Maybe[DataLocation]) =
    throw new UnparseError(One(ci.schemaFileLocation), maybeCL, e)

  private def doPE(e: Throwable, state: ParseOrUnparseState): Null = {
    state match {
      case null => Assert.usageError("state cannot be null")
      case ustate: UState => UE(e, One(ustate.currentLocation))
      case pstate: PState => {
        val pe =
          new ExpressionEvaluationException(
            e,
            state
          ) // One(ci.schemaFileLocation), One(pstate.currentLocation), msg)
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
      case ps: PState =>
        ps.dState.mode match {
          case ParserDiscriminatorNonBlocking => {
            e match {
              //
              // In discriminators, these specific infoset exceptions are
              // processing errors, not RSDE
              //
              case nc: InfosetNoSuchChildElementException => doPE(e, ps)

              case ni: InfosetNoInfosetException => doPE(e, ps)

              case nd: InfosetNoDataExceptionBase => doPE(e, ps)

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
          case ParserNonBlocking => {
            e match {
              case nd: InfosetNoDataExceptionBase =>
                doSDE(
                  new InfosetSelfReferencingException(
                    state.dState.currentNode.asElement,
                    state.dState.currentNode.erd
                  ),
                  ps
                )
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
  def evaluateForwardReferencing(
    state: ParseOrUnparseState,
    whereBlockedInfo: Suspension
  ): Maybe[T] = {
    var value: Maybe[T] = Nope
    try {
      // TODO: This assumes a distinct state object (with its own dState) for every expression that
      // can be in evaluation simultaneously.
      val dstate = evaluateExpression(state, state.dState)
      value = One(processForwardExpressionResults(dstate))
      whereBlockedInfo.setDone()
    } catch {
      case unfin: InfosetNodeNotFinalException =>
        whereBlockedInfo.block(unfin.node, unfin.node.erd.dpathElementCompileInfo, 0, unfin)
      case noChild: InfosetNoSuchChildElementException =>
        whereBlockedInfo.block(noChild.diComplex, noChild.nqn, 0, noChild)
      case noSibling: InfosetNoNextSiblingException =>
        whereBlockedInfo.block(noSibling.diSimple, noSibling.info, 0, noSibling)
      case noArrayIndex: InfosetArrayIndexOutOfBoundsException =>
        whereBlockedInfo.block(
          noArrayIndex.diArray,
          noArrayIndex.diArray.erd.dpathElementCompileInfo,
          noArrayIndex.index,
          noArrayIndex
        )
      case nd: InfosetNoDataExceptionBase
          if nd.erd.dpathElementCompileInfo.isOutputValueCalc => {
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
    validateType(value, state)
    value
  }

  override def run(dstate: DState): Unit = {
    recipe.run(dstate)
  }

  private def processForwardExpressionResults(dstate: DState): T = {
    val v = {
      dstate.currentNode match {
        case null => {
          // there is no element. Can happen if one evaluates say, 5 + 6 or 5 + $variable
          Assert.invariant(dstate.currentValue.isDefined)
          dstate.currentValue.getAnyRef
        }
        case n: DIElement if n.isNilled => One(n)
        case c: DIComplex => {
          Assert.invariant(!targetType.isInstanceOf[NodeInfo.AnyAtomic.Kind])
          c
        }
        case s: DISimple => {
          s.dataValue.getAnyRef
        }
        case _ => Assert.invariantFailed("must be an element, simple or complex.")
      }
    }
    v.asInstanceOf[T]
  }

  private def evaluateExpression(state: ParseOrUnparseState, dstate: DState): DState = {
    recipe.runExpression(state, dstate) // initializes dstate from state, then runs
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
        One(processExpressionResults(dstate))
      } catch {
        case th: Throwable => {
          handleThrow(th, state)
          Nope
        }
      }
    validateType(value, state)
    value
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

  private def processExpressionResults(dstate: DState): T = {
    val v = {
      dstate.currentNode match {
        case null => {
          // there is no element. Can happen if one evaluates say, 5 + 6 in the debugger in which case
          // there is a value, but no node.
          dstate.currentValue.getAnyRef
        }
        case n: DIElement if n.isNilled => One
        case c: DIComplex => {
          Assert.invariant(!targetType.isInstanceOf[NodeInfo.AnyAtomic.Kind])
          c
        }
        case s: DISimple => {
          try {
            s.dataValue.getAnyRef
          } catch {
            case ovc: OutputValueCalcEvaluationException => {
              Assert.invariantFailed(
                "OVC should always have a data value by the time it reaches here."
              )
            }
          }
        }
        case _ => Assert.invariantFailed("must be an element, simple or complex.")
      }
    }
    v.asInstanceOf[T]
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
   * Verifies that the resulting type of an expression matches the target type
   */
  private def validateType(v: Maybe[T], state: ParseOrUnparseState): Unit = {
    if (v.isDefined) {
      val value = v.get
      if (!value.isInstanceOf[DIElement]) {
        targetType match {
          case NodeInfo.NonEmptyString => {
            Assert.invariant(value.isInstanceOf[String])
            if (value.asInstanceOf[String].length == 0) {
              val e = new RuntimeSchemaDefinitionError(
                ci.schemaFileLocation,
                "Non-empty string required."
              )
              doSDE(e, state)
            }
          }
          case _: NodeInfo.String.Kind => Assert.invariant(value.isInstanceOf[String])
          case NodeInfo.DateTime => Assert.invariant(value.isInstanceOf[DFDLCalendar])
          case NodeInfo.Date => Assert.invariant(value.isInstanceOf[DFDLCalendar])
          case NodeInfo.Time => Assert.invariant(value.isInstanceOf[DFDLCalendar])
          case NodeInfo.Boolean => Assert.invariant(value.isInstanceOf[Boolean])
          case NodeInfo.Byte => Assert.invariant(value.isInstanceOf[JByte])
          case NodeInfo.UnsignedByte => Assert.invariant(value.isInstanceOf[JShort])
          case NodeInfo.Short => Assert.invariant(value.isInstanceOf[JShort])
          case NodeInfo.UnsignedShort => Assert.invariant(value.isInstanceOf[JInt])
          case NodeInfo.Int => Assert.invariant(value.isInstanceOf[JInt])
          case NodeInfo.UnsignedInt => Assert.invariant(value.isInstanceOf[JLong])
          case NodeInfo.Long => Assert.invariant(value.isInstanceOf[JLong])
          case NodeInfo.UnsignedLong => Assert.invariant(value.isInstanceOf[JBigInt])
          case NodeInfo.Integer => Assert.invariant(value.isInstanceOf[JBigInt])
          case NodeInfo.NonNegativeInteger => Assert.invariant(value.isInstanceOf[JBigInt])
          case NodeInfo.Float => Assert.invariant(value.isInstanceOf[JFloat])
          case NodeInfo.Double => Assert.invariant(value.isInstanceOf[JDouble])
          case NodeInfo.Decimal => Assert.invariant(value.isInstanceOf[JBigDecimal])
          case NodeInfo.HexBinary => Assert.invariant(value.isInstanceOf[Array[Byte]])
          case NodeInfo.AnyURI => Assert.invariant(value.isInstanceOf[URI])
          case _ => {
            // All other NodeInfo.* types are non-primitives which can have
            // many different kinds of types, not worth checking
          }
        }
      }
    }
  }

}
