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

package org.apache.daffodil.runtime1.processors.dfa

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.SchemaFileLocation
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.runtime1.processors.parsers.DelimiterTextType

/**
 * In order to be very fast, a DFA should run without allocating
 * any objects. Dispatch from state to state should be indexing
 * an array with an integer.
 * <p>
 * The state machine does things by side-effecting a
 * set of "registers", i.e., an instance of some class with mutable variables
 * in it. The whole runtime knows nothing of the registers however.
 */

/**
 * Some constants
 */
object DFA {
  def FinalState = -1
  def EndOfDataChar = -1.toChar // -1 means EndOfData
  def EndOfData = -3
}

trait DFA {

  /**
   * The state machine is an array of states.
   * So each State has a distinguished index.
   * 0 is reserved for the final state.
   */
  def states: Array[State]

  /**
   * Runs the DFA. Terminates when
   * some state has an action that transitions
   * to the final state.
   *
   * Results are "returned" by way of side-effecting the dfaStatus member
   * of the Registers object.
   * The results convey that we've either Failed, Succeeded,
   * encountered EndOfData or we need to Pause and make a determination of
   * what follows us before we can continue. It's needed because of this
   * whole idea of 'pausing' and 'resuming' for back tracking.
   */
  def run(r: Registers): Unit

  final protected def runLoop(
    r: Registers,
    terminateLoopOnState: Int,
    finalStatus: StateKind.StateKind
  ): Unit = {
    Assert.invariant(r.actionNum >= 0)
    r.status = StateKind.Parsing
    while (r.state != terminateLoopOnState) { // Terminates on FinalState
      val state = states(r.state)
      state.run(r)
      if (r.status != StateKind.Parsing) {
        return
      }
      r.actionNum = 0
      r.state = r.nextState
    }
    r.status = finalStatus
  }

}

final class DFADelimiterImpl(
  override val delimType: DelimiterTextType.Type,
  val states: Array[State],
  val lookingFor: String,
  override val location: SchemaFileLocation
) extends DFADelimiter
  with Serializable {

  def unparseValue: String = Assert.invariantFailed("Parser should not ask for unparseValue")

}
final class DFADelimiterImplUnparse(
  override val delimType: DelimiterTextType.Type,
  val states: Array[State],
  val lookingFor: String,
  val unparseValue: String,
  override val location: SchemaFileLocation
) extends DFADelimiter
  with Serializable {}

final class DFAFieldImpl(val states: Array[State]) extends DFAField with Serializable {}

/**
 * Reflects the status of the DFA's State.
 *
 * EndOfData - Reached end of data character
 * Failed - All rules within a DFA failed at a state
 * Succeeded - There was some combination of rules in the states that
 * succeeded and matched.
 * Paused - We encountered something that could be a delimiter
 * (only applicable to DFAField). We need to make a determination of
 * what comes next (the longest match of a whole delimiter?). If it's not a whole delimiter
 * then we will add the character to the field and continue/resume.
 */
object StateKind extends Enumeration {
  type StateKind = Value
  val EndOfData, Failed, Succeeded, Paused, Parsing = Value
}

trait DFAField extends DFA {

  /**
   * Runs the DFA. Terminates when
   * some state has an action that transitions
   * to the final state.
   */
  final override def run(r: Registers): Unit = runLoop(r, DFA.EndOfData, StateKind.EndOfData)
}

object DFADelimiter {
  private val controlOrWhitespace = "\\p{C}|\\p{Z}".r

  private def containsCtrlOrWS(s: String) = controlOrWhitespace.findFirstMatchIn(s).isDefined

  def strForDiagnostic(s: String) =
    if (containsCtrlOrWS(s))
      s"'$s' ('${Misc.remapStringToVisibleGlyphs(s)}')"
    else s"'$s'"
}

trait DFADelimiter extends DFA {
  def delimType: DelimiterTextType.Type
  def lookingFor: String
  def location: SchemaFileLocation
  override def toString(): String =
    "<DFA type='%s' lookingFor='%s' />".format(delimType, lookingFor)

  final override def run(r: Registers): Unit = runLoop(r, DFA.FinalState, StateKind.Succeeded)

  // We frequently want to know if a delimiter is ES or not when iterating over
  // delimiters, so cache the result of this comparison
  final val isES = lookingFor == "%ES;"

  def unparseValue: String

  lazy val strForDiagnostic: String = DFADelimiter.strForDiagnostic(lookingFor)

}

/**
 * An action transitions the state by modifying
 * information in the "registers", and returning
 * the integer corresponding to the next state.
 */

/**
 * A state is an ordered array of rules. Each rule
 * is a guard/test and an action. Only one
 * rule "fires" for each state, and the action
 * modifies the register state and returns the
 * identifying integer for the next state (in r.nextState), if successful.
 * Otherwise, a status is returned indicating whether or not
 * we Succeeded, Failed, reached EndOfData, or need to Pause
 * to gather further information.
 */
trait Rule extends Serializable {
  def test(r: Registers): Boolean // take this action?
  def act(r: Registers): Unit // modifies the registers
}
