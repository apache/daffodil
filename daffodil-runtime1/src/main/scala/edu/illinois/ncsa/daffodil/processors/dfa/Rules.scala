package edu.illinois.ncsa.daffodil.processors.dfa

import scala.util.parsing.input.Reader
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import edu.illinois.ncsa.daffodil.processors.WSP
import edu.illinois.ncsa.daffodil.processors.NL
import edu.illinois.ncsa.daffodil.processors.DFDLCharReader
import edu.illinois.ncsa.daffodil.processors.DelimBase
import edu.illinois.ncsa.daffodil.exceptions.Assert

// This is the block of mutable things
// including the source of characters.
class Registers {

  var numCharsRead: Int = 0
  var numCharsReadUntilDelim: Int = 0
  var data0: Char = DFA.EndOfDataChar // current character
  var data1: Char = DFA.EndOfDataChar // next (lookahead 1) character

  // Very very loosely bind this whole system
  // to scala's Reader[Char] because that 
  // is too inefficient. (It allocates).
  // We really just need a 
  // get next char function.
  private var reader: DFDLCharReader = null;

  /**
   * Very important. We don't want to create these
   * over and over. We want to use one and reset it
   * and use it again.
   * <p>
   * So this is set up so you must construct it,
   * and then reset before first use. I.e.,
   * reset() is also init().
   */
  def reset(reader: DFDLCharReader) {
    this.reader = reader
    data0 = nextChar()
    data1 = nextChar()
    resultString.clear()
    delimString.clear()
    numCharsRead = 0
    numCharsReadUntilDelim = 0
  }

  /**
   * Use to set reader position to resume from.
   */
  def setResume(resumeReader: DFDLCharReader) {
    this.reader = resumeReader
  }

  /**
   * Copy an existing Registers' reader position information
   * to this Registers.
   *
   * This allows this Registers to pick-up where the other
   * left off.
   */
  def copy(that: Registers) {
    this.reader = that.reader
    this.data0 = that.data0
    this.data1 = that.data1
    resultString.clear
    delimString.clear
    numCharsRead = 0
    numCharsReadUntilDelim = 0
  }

  def getReader: DFDLCharReader = reader

  // returns -1 for EOF (or maybe 26.toChar aka ^Z)
  def nextChar() = {
    val res = reader.first
    reader = reader.rest
    res
  }

  val charsReadUntilDelim: StringBuilder = new StringBuilder()
  val resultString: StringBuilder = new StringBuilder()
  val delimString: StringBuilder = new StringBuilder()

  def advance() = {
    data0 = data1
    data1 = nextChar()
  }

  def appendToField(c: Char): Unit = {
    charsReadUntilDelim.append(c)
    resultString.append(c)
    incCharsRead
    incCharsReadUntilDelim
  }

  def appendToDelim(c: Char): Unit = {
    delimString.append(c)
    incCharsRead
  }

  def dropChar(c: Char): Unit = {
    charsReadUntilDelim.append(c)
    incCharsRead
    incCharsReadUntilDelim
  }

  def incCharsRead(): Unit = numCharsRead += 1
  def incCharsReadUntilDelim(): Unit = numCharsReadUntilDelim += 1

  // -1 means no matched delimiter
  def matchStartPos: Int = {
    if (!delimString.isEmpty) { resultString.length }
    else -1
  }

}
/**
 * This base class handles the connections from state to state (which form a directed graph)
 * through use of call-by-name here and lazy vals within.  This allows us to define the
 * state-to-state connections functionally.
 *
 * This object itself will exist and be a member of the states ArrayBuffer as well as the other
 * states before any access to the lazy val members.
 */
abstract class State(states: => ArrayBuffer[State], r: Registers) {

  def stateNum: Int
  def stateName: String
  def rules: ArrayBuffer[Rule]

  /**
   * A state executes by evaluating guarded actions.
   * if rules(n).test() evaluates to true, then take
   * action by rules(n).act().
   */
  def run(actNum: Int): Either[DFAStatus, Int] = {
    var actionNum = actNum //0
    while (actionNum < rules.length) {
      if (rules(actionNum).test()) {
        val res = rules(actionNum).act()
        res match {
          case Right(nextStateNum) => return Right(nextStateNum)
          case Left(status) => return Left(new DFAStatus(stateNum, actionNum, status))
        }
      }
      actionNum = actionNum + 1
    }
    Left(new DFAStatus(stateNum, actionNum, StateKind.Failed))
  }

  
  def reset(reader: DFDLCharReader) = r.reset(reader)
  def setResume(reader: DFDLCharReader) = r.setResume(reader)
  def register = r

  def findState(name: String): Int = {
    states.find(st => st.stateName == name).get.stateNum
  }
  // this is just one way to plumb the things together.
  // it depends on every state having a unique name
  //
  lazy val EECState = findState("EECState")
  lazy val ECState = findState("ECState")
  lazy val StartState = findState("StartState")
  lazy val PTERMState = findState("PTERM0")

  def printStr(): String = states.mkString
  override def toString(): String = stateName + "_" + stateNum
}

// The delimiter compiler still has to select and instantiate all the appropriate states
// Note that there is no reason why a state has to have a fixed static set of rules.
// The compiler could create rule objects and add them to the rules. This is what
// we would expect when compiling a delimiter, which can mean any terminating markup
// each of which is specified as a list of individual delimiters. Each of those
// can have char class entities like WSP+ in it, and so on. 
// 

/**
 * Ambiguity Truth Table
 * 0 means not same, 1 means same
 * ------------------------------------
 * EC		EEC		PTERM	Ambiguous?
 * 0		1		1		Y
 * 1		0		1		Y
 * 1		1		0		Y
 * 1		1		1		Y
 */
class StartStateUnambiguousEscapeChar(states: => ArrayBuffer[State], EEC: Option[Char], EC: Option[Char],
  r: Registers, compiledDelims: DelimsMatcher, val stateNum: Int)
  extends State(states, r) {

  val stateName: String = "StartState"
  val rules = ArrayBuffer(
    Rule { compiledDelims.couldBeFirstChar(r.data0) } { Left(StateKind.Paused) },
    Rule { EEC.isDefined && EC.isDefined && r.data0 == EEC.get } { Right(EECState) },
    Rule { EC.isDefined && r.data0 == EC.get } { Right(ECState) },
    Rule { r.data0 == DFA.EndOfDataChar } { Right(DFA.EndOfData) },
    Rule { true } {
      r.appendToField(r.data0)
      r.advance()
      Right(StartState)
    })

}

class StartState(states: => ArrayBuffer[State],
  r: Registers, compiledDelims: DelimsMatcher, val stateNum: Int)
  extends State(states, r) {

  val stateName: String = "StartState"
  val rules = ArrayBuffer(
    Rule { compiledDelims.couldBeFirstChar(r.data0) } { Left(StateKind.Paused) },
    Rule { r.data0 == DFA.EndOfDataChar } { Right(DFA.EndOfData) },
    Rule { true } {
      r.appendToField(r.data0)
      r.advance()
      Right(StartState)
    })

}

class StartStatePadding(states: => ArrayBuffer[State], r: Registers,
  val padChar: Char)
  extends State(states, r) {

  val stateName = "StartState"
  val stateNum: Int = 0
  val rules = ArrayBuffer(
    Rule { r.data0 == padChar } {
      r.appendToField(r.data0)
      r.advance()
      Right(StartState)
    },
    Rule { true } { Right(DFA.FinalState) })
}

/**
 * StartState for Field portion of EscapeBlock.
 * Here compiledDelims should only contain the endBlock DFADelimiter.
 */
class StartStateEscapeBlock(states: => ArrayBuffer[State], var EEC: Option[Char],
  r: Registers, compiledDelims: DelimsMatcher, val stateNum: Int)
  extends State(states, r) {
  val stateName: String = "StartState"

  val rules = ArrayBuffer(
    Rule { compiledDelims.couldBeFirstChar(r.data0) } { Left(StateKind.Paused) },
    Rule { EEC.isDefined && (r.data0 == EEC.get) } { Right(EECState) },
    Rule { true } {
      r.appendToField(r.data0)
      r.advance
      Right(StartState)
    })

}

class StartStateEscapeChar(states: => ArrayBuffer[State], var EEC: Option[Char], var EC: Option[Char],
  r: Registers, compiledDelims: DelimsMatcher, val stateNum: Int)
  extends State(states, r) {

  val stateName: String = "StartState"

  val rules_NO_EEC_BUT_EC_TERM_SAME = ArrayBuffer(
    Rule { compiledDelims.couldBeFirstChar(r.data0) } { Left(StateKind.Paused) },
    Rule { r.data0 == EC.get } { Right(ECState) },
    Rule { r.data0 == DFA.EndOfDataChar } { Right(DFA.EndOfData) },
    Rule { true } {
      r.appendToField(r.data0)
      r.advance
      Right(StartState)
    })

  val rules_EEC_EC_SAME_NOT_TERM = ArrayBuffer(
    Rule { compiledDelims.couldBeFirstChar(r.data0) } { Left(StateKind.Paused) },
    Rule { EEC.isDefined && r.data0 == EEC.get && r.data1 == EC.get } { Right(EECState) },
    Rule { EEC.isDefined && r.data0 == EEC.get && r.data1 != EC.get } { Right(ECState) },
    Rule { r.data0 == DFA.EndOfDataChar } { Right(DFA.EndOfData) },
    Rule { true } {
      r.appendToField(r.data0)
      r.advance
      Right(StartState)
    })

  val rules_EEC_TERM_SAME_NOT_EC = ArrayBuffer(
    Rule { compiledDelims.couldBeFirstChar(r.data0) } { Left(StateKind.Paused) },
    Rule { EEC.isDefined && r.data0 == EEC.get && r.data1 == EC.get } { Right(EECState) },
    Rule { r.data0 == EC.get } { Right(ECState) },
    Rule { r.data0 == DFA.EndOfDataChar } { Right(DFA.EndOfData) },
    Rule { true } {
      r.appendToField(r.data0)
      r.advance
      Right(StartState)
    })

  val rules_EC_TERM_SAME_NOT_EEC = ArrayBuffer(
    Rule { r.data0 == EC.get && r.data1 == EC.get } {
      // EC followed by EC, first EC gets dropped
      r.dropChar(r.data0)
      r.appendToField(r.data1)
      r.advance
      r.advance
      Right(StartState)
    },
    Rule { compiledDelims.couldBeFirstChar(r.data0) } { Left(StateKind.Paused) },
    Rule { EEC.isDefined && r.data0 == EEC.get && r.data1 == EC.get } { Right(EECState) },
    Rule { r.data0 == EC.get } { Right(ECState) },
    Rule { r.data0 == DFA.EndOfDataChar } { Right(DFA.EndOfData) },
    Rule { true } {
      r.appendToField(r.data0)
      r.advance
      Right(StartState)
    })

  val rules_EC_EEC_TERM_SAME = ArrayBuffer(
    Rule { compiledDelims.couldBeFirstChar(r.data0) } { Left(StateKind.Paused) },
    Rule { EEC.isDefined && r.data0 == EEC.get && r.data1 == EEC.get } {
      Right(EECState)
    },
    Rule { EEC.isDefined && r.data0 == EEC.get && r.data1 != DFA.EndOfDataChar } {
      // EEC followed by something not a PTERM[2]
      r.dropChar(r.data0)
      r.appendToField(r.data1)
      r.advance
      r.advance
      Right(StartState)
    },
    Rule { EEC.isDefined && r.data0 == EEC.get && r.data1 == DFA.EndOfDataChar } {
      Right(ECState)
    },
    Rule { r.data0 == DFA.EndOfDataChar } { Right(DFA.EndOfData) },
    Rule { true } {
      r.appendToField(r.data0)
      r.advance
      Right(StartState)
    })

  val rules_Unambiguous = ArrayBuffer(
    Rule { compiledDelims.couldBeFirstChar(r.data0) } { Left(StateKind.Paused) },
    Rule { EEC.isDefined && r.data0 == EEC.get } { Right(EECState) },
    Rule { EC.isDefined && r.data0 == EC.get } { Right(ECState) },
    Rule { r.data0 == DFA.EndOfDataChar } { Right(DFA.EndOfData) },
    Rule { true } {
      r.appendToField(r.data0)
      r.advance()
      Right(StartState)
    })

  val rules_NoEscaping = ArrayBuffer(
    Rule { compiledDelims.couldBeFirstChar(r.data0) } { Left(StateKind.Paused) },
    Rule { r.data0 == DFA.EndOfDataChar } { Right(DFA.EndOfData) },
    Rule { true } {
      r.appendToField(r.data0)
      r.advance()
      Right(StartState)
    })

  /**
   * Determines what rules to execute based on combinations of EC/EEC
   */
  def getRules: ArrayBuffer[Rule] = {

    val result = EEC match {
      case None if !EC.isDefined => {
        // No Escaping
        rules_NoEscaping
      }
      case None if EC.isDefined && compiledDelims.couldBeFirstChar(EC.get) => {
        // EC == PTERM0
        rules_NO_EEC_BUT_EC_TERM_SAME
      }
      case None if EC.isDefined && !compiledDelims.couldBeFirstChar(EC.get) => {
        // Unambiguous
        rules_Unambiguous
      }
      case Some(escEsc) if EC.isDefined && !compiledDelims.couldBeFirstChar(escEsc) && !compiledDelims.couldBeFirstChar(EC.get) && escEsc != EC.get => {
        // EC != EEC != PTERM0
        rules_Unambiguous
      }
      case Some(escEsc) if EC.isDefined && EC.get == escEsc && compiledDelims.couldBeFirstChar(escEsc) => {
        // EC == EEC == PTERM0
        rules_EC_EEC_TERM_SAME
      }
      case Some(escEsc) if EC.isDefined && compiledDelims.couldBeFirstChar(escEsc) && !compiledDelims.couldBeFirstChar(EC.get) => {
        // (EEC == PTERM0) != EC
        rules_EEC_TERM_SAME_NOT_EC
      }
      case Some(escEsc) if EC.isDefined && !compiledDelims.couldBeFirstChar(escEsc) && compiledDelims.couldBeFirstChar(EC.get) => {
        // (EC == PTERM0) != EEC
        rules_EC_TERM_SAME_NOT_EEC
      }
      case Some(escEsc) if EC.isDefined && EC.get == escEsc && !compiledDelims.couldBeFirstChar(escEsc) => {
        // (EC == EEC) != PTERM0
        rules_EEC_EC_SAME_NOT_TERM
      }
      case _ => throw new Exception("Unexpected case.")
    }
    result
  }

  lazy val rules = getRules

}

trait DelimsMatcher {
  def r: Registers
  def delims: Seq[DFADelimiter]

  def couldBeFirstChar(charIn: Char): Boolean = {
    // looking at data0
    delims.foreach(d => {
      val pTerm0 = d.states(0).asInstanceOf[DelimStateBase]
      if (pTerm0.checkMatch(charIn)) return true
    })
    false
  }
}

class DelimsMatcherImpl(val delims: Seq[DFADelimiter]) extends DelimsMatcher {
  val r: Registers = new Registers()
}

class ECState(states: => ArrayBuffer[State], var EC: Option[Char],
  r: Registers, compiledDelims: DelimsMatcher, val stateNum: Int)
  extends State(states, r) {

  val stateName = "ECState"
  val rules = ArrayBuffer(
    // ECState, means that data0 is EC
    //
    Rule { r.data1 == EC.get } {
      // next char is also EC, drop this EC
      // but do not treat next EC as char
      r.dropChar(r.data0)
      r.advance()
      Right(StartState)
    },
    Rule { r.data1 == DFA.EndOfDataChar } {
      r.dropChar(r.data0)
      Right(DFA.EndOfData)
    },
    Rule { true } {
      // constituent character
      r.dropChar(r.data0)
      r.appendToField(r.data1)
      r.advance()
      r.advance()
      Right(StartState)
    })
}

class EECState(states: => ArrayBuffer[State], var EEC: Option[Char], var EC: Option[Char],
  r: Registers, compiledDelims: DelimsMatcher, val stateNum: Int)
  extends State(states, r) {

  val stateName = "EECState"
  val rules = ArrayBuffer(
    // Because this is about EC and EEC we can 
    // just write these rules down statically. 
    // The real delim compiler would generate a flock of rules
    // based on what the actual delimiters are.
    //
    // We've already encountered EEC as data0 here
    //
    Rule { compiledDelims.couldBeFirstChar(r.data0) } {
      Left(StateKind.Paused) //PTERMState
    },
    Rule { EC.isDefined && r.data1 == EC.get } {
      // Drop EEC aka data0
      r.dropChar(r.data0)
      r.appendToField(r.data1)
      r.advance()
      r.advance()
      Right(StartState)
    },
    Rule { r.data1 == DFA.EndOfDataChar } {
      r.appendToField(r.data0)
      Right(DFA.EndOfData)
    },
    Rule { true } {
      // Not followed by an EC, therefore
      // it is an EEC and should remain in field.
      //
      r.appendToField(r.data0)
      r.advance()
      Right(StartState)
    })
}

class EECStateBlock(states: => ArrayBuffer[State], var EEC: Option[Char],
  r: Registers, compiledDelims: DelimsMatcher, val stateNum: Int)
  extends State(states, r) {

  val stateName = "EECState"
  val rules = ArrayBuffer(
    // Because this is about EC and EEC we can 
    // just write these rules down statically. 
    // The real delim compiler would generate a flock of rules
    // based on what the actual delimiters are.
    //
    // We've already encountered EEC as data0 here
    //
    Rule { compiledDelims.couldBeFirstChar(r.data0) } {
      Left(StateKind.Paused) //PTERMState
    },
    Rule { compiledDelims.couldBeFirstChar(r.data1) } {
      // EEC followed by possible blockEnd
      r.dropChar(r.data0)
      r.appendToField(r.data1)
      r.advance
      r.advance
      Right(StartState)
    },
    Rule { r.data1 == EEC.get } {
      // EEC followed by EEC, stays in field
      r.appendToField(r.data0)
      r.advance()
      Right(StartState)
    },
    Rule { r.data1 == DFA.EndOfDataChar } {
      r.appendToField(r.data0)
      Right(DFA.EndOfData)
    },
    Rule { true } {
      // EEC followed by normal character
      r.appendToField(r.data0)
      r.advance()
      Right(StartState)
    })
}

abstract class DelimStateBase(states: => ArrayBuffer[State], r: Registers)
  extends State(states, r) {
  var stateName: String = null
  def setStateName(name: String) = stateName = name
  def rules: ArrayBuffer[Rule]

  var nextState: Int

  def checkMatch(charIn: Char): Boolean

  override def toString(): String = {
    "<" + stateName + "_" + stateNum + "/>"
  }
}

class CharState(states: => ArrayBuffer[State], r: Registers, char: Char, var nextState: Int, val stateNum: Int)
  extends DelimStateBase(states, r) {

  stateName = "CharState(" + char + ")"
  val rulesToThisState = ArrayBuffer(
    Rule { r.data0 == char } {
      r.appendToDelim(r.data0)
      r.advance
      Right(stateNum)
    })

  val rules = ArrayBuffer(
    Rule { r.data0 == char } {
      r.appendToDelim(r.data0)
      r.advance
      Right(nextState)
    })

  def checkMatch(charIn: Char): Boolean = char == charIn
}

abstract class WSPBase(states: => ArrayBuffer[State], r: Registers)
  extends DelimStateBase(states, r) with WSP {

  def checkMatch(charIn: Char): Boolean = {
    val result = charIn match {
      case CTRL0 | CTRL1 | CTRL2 | CTRL3 | CTRL4 => true
      case SPACE | NEL | NBSP | OGHAM | MONG => true
      case SP0 | SP1 | SP2 | SP3 | SP4 | SP5 | SP6 | SP7 | SP8 | SP9 | SP10 => true
      case LSP | PSP | NARROW | MED | IDE => true
      case _ => false
    }
    result
  }
}

class WSPState(states: => ArrayBuffer[State], r: Registers, var nextState: Int, val stateNum: Int)
  extends WSPBase(states, r) {

  stateName = "WSPState"
  val rulesToThisState = ArrayBuffer(
    Rule { checkMatch(r.data0) } {
      r.appendToDelim(r.data0)
      r.advance
      Right(stateNum)
    })

  val rules = ArrayBuffer(
    Rule { checkMatch(r.data0) } {
      r.appendToDelim(r.data0)
      r.advance
      Right(nextState)
    })
}

abstract class WSPRepeats(states: => ArrayBuffer[State], r: Registers)
  extends WSPBase(states, r) {

}

class WSPPlusState(states: => ArrayBuffer[State], r: Registers, var nextState: Int, val stateNum: Int)
  extends WSPRepeats(states, r) {

  stateName = "WSPPlusState"
  val rulesToThisState = ArrayBuffer(
    Rule { checkMatch(r.data0) } {
      r.appendToDelim(r.data0)
      r.advance
      Right(stateNum) // This state
    })

  var matchedAtLeastOnce: Boolean = false
  val rules = ArrayBuffer(
    Rule { checkMatch(r.data0) } {
      r.appendToDelim(r.data0)
      r.advance
      matchedAtLeastOnce = true
      Right(stateNum) // This state
    },
    Rule { matchedAtLeastOnce } {
      Right(nextState)
    })
}

class WSPStarState(states: => ArrayBuffer[State], r: Registers, var nextState: Int, val stateNum: Int)
  extends WSPRepeats(states, r) {

  stateName = "WSPStarState"

  val rules = ArrayBuffer(
    Rule { checkMatch(r.data0) } {
      r.appendToDelim(r.data0)
      r.advance
      Right(stateNum) // This state
    },
    Rule { true } {
      Right(nextState)
    })
  var rulesToNextStateMinusUs: ArrayBuffer[Rule] = ArrayBuffer.empty
}

abstract class NLBase(states: => ArrayBuffer[State], r: Registers)
  extends DelimStateBase(states, r) with NL {

  def isNLNotCR(charIn: Char): Boolean = {
    charIn match {
      case LF | NEL | LS => true
      case _ => false
    }
  }

  def isCR(charIn: Char): Boolean = {
    charIn match {
      case CR => true
      case _ => false
    }
  }

  def isLF(charIn: Char): Boolean = {
    charIn match {
      case LF => true
      case _ => false
    }
  }
}

class NLState(states: => ArrayBuffer[State], r: Registers, var nextState: Int, val stateNum: Int)
  extends NLBase(states, r) {

  stateName = "NLState"

  val rules = ArrayBuffer(
    Rule { isCR(r.data0) && isLF(r.data1) } {
      r.appendToDelim(r.data0)
      r.appendToDelim(r.data1)
      r.advance
      r.advance
      Right(nextState)
    },
    Rule { isCR(r.data0) && !isLF(r.data1) } {
      r.appendToDelim(r.data0)
      r.advance
      Right(nextState)
    },
    Rule { isNLNotCR(r.data0) } {
      r.appendToDelim(r.data0)
      r.advance
      Right(nextState)
    })

  def checkMatch(charIn: Char): Boolean = isNLNotCR(charIn) || isCR(charIn)
}