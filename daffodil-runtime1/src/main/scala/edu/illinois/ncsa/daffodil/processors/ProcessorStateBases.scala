/* Copyright (c) 2017 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.dpath.DState
import edu.illinois.ncsa.daffodil.dsom.RuntimeSchemaDefinitionError
import edu.illinois.ncsa.daffodil.dsom.RuntimeSchemaDefinitionWarning
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.SavesErrorsAndWarnings
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.io.DataStreamCommon
import edu.illinois.ncsa.daffodil.io.LocalBufferMixin
import edu.illinois.ncsa.daffodil.util.MStackOfLong
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.processors.charset.EncoderDecoderMixin
import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.exceptions.SavesErrorsAndWarnings
import edu.illinois.ncsa.daffodil.api.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.api.DaffodilTunableParameters.WarnID
import edu.illinois.ncsa.daffodil.infoset._

/**
 * Trait mixed into the PState.Mark object class and the ParseOrUnparseState
 *
 * contains member functions for everything the debugger needs to be able to observe.
 */
trait StateForDebugger {
  def bytePos: Long
  def childPos: Long
  def groupPos: Long
  def currentLocation: DataLocation
  def arrayPos: Long
  def bitLimit0b: MaybeULong
  def discriminator: Boolean = false
}

case class TupleForDebugger(
  val bytePos: Long,
  val childPos: Long,
  val groupPos: Long,
  val currentLocation: DataLocation,
  val arrayPos: Long,
  val bitLimit0b: MaybeULong,
  override val discriminator: Boolean) extends StateForDebugger

/**
 * A parser takes a state, and returns an updated state
 *
 * The fact that there are side-effects/mutations on parts of the state
 * enables us to reuse low-level java primitives that mutate streams.
 *
 * The goal however, is to hide that fact so that the only places that have to
 * know are the places doing the mutation, and the places rolling them back
 * which should be isolated to the alternative parser, and repParsers, i.e.,
 * places where points-of-uncertainty are handled.
 */
abstract class ParseOrUnparseState protected (
  protected var variableBox: VariableBox,
  var diagnostics: List[Diagnostic],
  var dataProc: Maybe[DataProcessor],
  protected var status_ : ProcessorResult) extends DFDL.State
  with StateForDebugger
  with ThrowsSDE with SavesErrorsAndWarnings
  with LocalBufferMixin
  with EncoderDecoderMixin
  with Logging {

  def this(vmap: VariableMap, diags: List[Diagnostic], dataProc: Maybe[DataProcessor], status: ProcessorResult = Success) =
    this(new VariableBox(vmap), diags, dataProc, status)

  def variableMap = variableBox.vmap
  def setVariableMap(newMap: VariableMap) {
    variableBox.setVMap(newMap)
  }

  def status = status_

  final def setFailed(failureDiagnostic: Diagnostic) {
    // threadCheck()
    if (!diagnostics.contains(failureDiagnostic)) {
      status_ = new Failure(failureDiagnostic)
      diagnostics = failureDiagnostic :: diagnostics
    } else {
      Assert.invariant(status ne Success)
    }
  }

  /**
   * Important: If an error is being suppressed, you must call this to reset the state
   * back so that the prior failure doesn't "last forever" past the point where it is being suppressed.
   *
   * This happens, for example, in the debugger when it is evaluating expressions.
   */
  def setSuccess() {
    status_ = Success
  }

  def currentNode: Maybe[DINode]

  private val _dState = new DState

  /**
   * Used when evaluating expressions. Holds state of expression
   * during evaluation.
   *
   * Doesn't hold every bit of state - that is to say there's still the
   * regular execution call stack, which
   * keeps track of exactly where in the expression evaluation we are.
   */
  def dState = _dState

  def copyStateForDebugger = {
    TupleForDebugger(
      bytePos,
      childPos,
      groupPos,
      currentLocation,
      arrayPos,
      bitLimit0b,
      discriminator)
  }

  override def schemaFileLocation = getContext().schemaFileLocation

  def dataStream: Maybe[DataStreamCommon]

  def bitPos0b: Long
  def bitLimit0b: MaybeULong
  final def bytePos0b = bitPos0b >> 3
  final def bytePos1b = (bitPos0b >> 3) + 1
  final def bitPos1b = bitPos0b + 1
  final def bitLimit1b = if (bitLimit0b.isDefined) MaybeULong(bitLimit0b.get + 1) else MaybeULong.Nope
  final def whichBit0b = bitPos0b % 8

  // TODO: many off-by-one errors due to not keeping strong separation of
  // one-based and zero-based indexes.
  //
  // We could separate these with the type system.
  //
  // So implement a OneBasedBitPos and ZeroBasedBitPos value class with
  // operations that convert between them, allow adding & subtracting only
  // in sensible ways, etc.
  final def bitPos = bitPos0b
  final def bytePos = bytePos0b
  // def charPos: Long

  def groupPos: Long
  def arrayPos: Long
  def childPos: Long
  def occursBoundsStack: MStackOfLong

  def hasInfoset: Boolean
  def infoset: InfosetItem

  def maybeERD = {
    if (hasInfoset)
      Maybe(getContext())
    else
      Nope
  }

  def thisElement: InfosetElement

  def getContext(): ElementRuntimeData = {
    // threadCheck()
    val currentElement = infoset.asInstanceOf[InfosetElement]
    val res = currentElement.runtimeData
    res
  }

  /**
   * The User API sets the debugger and debug on/off flag on the DataProcessor object.
   * When a PState or UState is created by the DataProcessor, the DataProcessor
   * sets notifies the state object so that it can setup any debug-specific behaviors.
   */
  def notifyDebugging(flag: Boolean): Unit

  def SDE(str: String, args: Any*) = {
    // ExecutionMode.requireRuntimeMode // not any more. More code is shared between compile and runtime now, so these requirements gotta go
    val ctxt = getContext()
    val rsde = new RuntimeSchemaDefinitionError(ctxt.schemaFileLocation, this, str, args: _*)
    ctxt.toss(rsde)
  }

  def SDEButContinue(str: String, args: Any*) = {
    // ExecutionMode.requireRuntimeMode
    val ctxt = getContext()
    val rsde = new RuntimeSchemaDefinitionError(ctxt.schemaFileLocation, this, str, args: _*)
    diagnostics = rsde :: diagnostics
  }

  def SDW(str: String, args: Any*) = {
    // ExecutionMode.requireRuntimeMode
    val ctxt = getContext()
    val rsdw = new RuntimeSchemaDefinitionWarning(ctxt.schemaFileLocation, this, str, args: _*)
    diagnostics = rsdw :: diagnostics
  }

  def SDW(warnID: WarnID, str: String, args: Any*) = {
    // ExecutionMode.requireRuntimeMode
    if (DaffodilTunableParameters.notSuppressedWarning(warnID)) {
      val ctxt = getContext()
      val rsdw = new RuntimeSchemaDefinitionWarning(ctxt.schemaFileLocation, this, str, args: _*)
      diagnostics = rsdw :: diagnostics
    }
  }

}

/**
 * State used when compiling Evaluatable[T] objects
 *  So they don't require a "real" state.
 *
 *  This serves two purposes. First it lets us obey the regular API for evaluation, so we don't need
 *  one way to evaluate and another very similar thing for analyzing expressions to see if they are constnat.
 *
 *  Second, it serves as a detector of when an expression is non-constant by blowing up when things
 *  inconsistent with constant-value are attempted to be extracted from the state. By "blow up" it throws
 *  a structured set of exceptions, typically children of InfosetException or VariableException.
 */
class CompileState(trd: RuntimeData, maybeDataProc: Maybe[DataProcessor])
  extends ParseOrUnparseState(trd.variableMap, Nil, maybeDataProc) {
  /**
   * As seen from class CompileState, the missing signatures are as follows.
   *  *  For convenience, these are usable as stub implementations.
   */
  // Members declared in edu.illinois.ncsa.daffodil.processors.ParseOrUnparseState
  def arrayPos: Long = 1L
  def bitLimit0b: MaybeULong = MaybeULong.Nope
  def bitPos0b: Long = 0L
  def childPos: Long = 0L
  def dataStream = Nope
  def groupPos: Long = 0L
  def hasInfoset: Boolean = infoset_.isDefined

  private lazy val infoset_ : Maybe[DIElement] = Nope

  def infoset: DIElement =
    if (infoset_.isDefined)
      infoset_.value
    else
      throw new InfosetNoInfosetException(One(trd)) // for expressions evaluated in debugger, default expressions for top-level variable decls.

  def currentNode = Maybe(infoset.asInstanceOf[DINode])

  def notifyDebugging(flag: Boolean): Unit = {}
  private val occursBoundsStack_ = MStackOfLong()
  def occursBoundsStack: MStackOfLong = occursBoundsStack_

  def thisElement = infoset

  // Members declared in edu.illinois.ncsa.daffodil.processors.StateForDebugger
  def currentLocation: DataLocation = Assert.usageError("Not to be used.")
}
