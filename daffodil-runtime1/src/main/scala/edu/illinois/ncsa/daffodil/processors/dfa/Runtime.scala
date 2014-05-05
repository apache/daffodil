package edu.illinois.ncsa.daffodil.processors.dfa

import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.Reader
import edu.illinois.ncsa.daffodil.processors.DFDLCharReader
import edu.illinois.ncsa.daffodil.processors.dfa.StateKind.StateKind

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
   * Returning Right(num) means that this current state was satisfied, we
   * now need to check the next state.
   * Returning Left(status) means that we've either Failed, Succeeded,
   * encountered EndOfData or we need to Pause and make a determination of
   * what follows us before we can continue. It's needed because of this
   * whole idea of 'pausing' and 'resuming' for back tracking.
   */
  def run(initialState: Int, r: Registers, actionNum: Int = 0): Either[DFAStatus, Int] = {
    var stateNum = initialState
    while (stateNum != DFA.FinalState) {
      states(stateNum).run(actionNum, r) match {
        case Right(nextStateNum) => stateNum = nextStateNum
        case Left(status) => return Left(status)
      }
    }
    Left(new DFAStatus(stateNum, 0, StateKind.Succeeded))
  }
}

class DFADelimiterImpl(val states: Array[State], val lookingFor: String)
  extends DFADelimiter {

}

class DFAFieldImpl(val states: Array[State])
  extends DFAField {

}

/**
 * Reflects the status of the DFA's State.
 *
 * EndOfData - Reached end of data character
 * Failed - All rules within a DFA failed at a state
 * Succeeded - There was some combination of rules in the states that
 * succeeded and matched.
 * Paused - We encountered something that could be a delimiter
 * (only applicable to DFAField). We need to make a determination of
 * what comes next before we can continue/resume.
 */
object StateKind extends Enumeration {
  type StateKind = Value
  val EndOfData, Failed, Succeeded, Paused = Value
}

/**
 * DFAStatus contains information pertaining to the status of the DFA.
 *
 * currentStateNum - The current state index number at the time this status was returned.
 * actionNum - The action number we were executing at the time this status was returned.
 * status - Whether we encountered EndOfData, we Failed, Succeeded or Paused.
 *
 */
case class DFAStatus(val currentStateNum: Int, val actionNum: Int, val status: StateKind)

trait DFAField extends DFA {
  /**
   * Runs the DFA. Terminates when
   * some state has an action that transitions
   * to the final state.
   */
  override def run(initialState: Int, r: Registers, actionNum: Int = 0): Either[DFAStatus, Int] = {
    var stateNum = initialState
    var resume: Boolean = actionNum > 0
    while (stateNum != DFA.EndOfData) {
      val res = if (resume) {
        resume = false
        states(stateNum).run(actionNum, r)
      } else {
        states(stateNum).run(0, r)
      }
      res match {
        case Right(num) => stateNum = num
        case status => return status
      }
    }
    Left(new DFAStatus(stateNum, 0, StateKind.EndOfData))
  }

}

trait DFADelimiter extends DFA {
  def lookingFor: String
  override def toString(): String = "<DFA lookingFor='%s' />".format(lookingFor)
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
 * identifying integer for the next state, if successful.
 * Otherwise, a status is returned indicating whether or not
 * we Succeeded, Failed, reached EndOfData, or need to Pause
 * to gather further information.
 */
trait Rule {
  def test(r: Registers): Boolean // take this action?
  def act(r: Registers): Either[StateKind, Int] // modifies the registers and returns next state's index.
}

/**
 * Convenient thingy for hand-created rules
 * <p>
 * Can write things like `Rule { r.data0 == EC } { r.resultString.append(...)...}
 * Examples in other file.
 */
object Rule {
  def apply(testBody: Registers => Boolean)(actionBody: Registers => Either[StateKind, Int]) = {
    new Rule {
      override def test(r: Registers) = testBody(r)
      override def act(r: Registers) = actionBody(r)
    }
  }
}
