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

import scala.collection.mutable.ArrayBuffer

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.MaybeChar
import org.apache.daffodil.runtime1.processors.DelimiterIterator
import org.apache.daffodil.runtime1.processors.NL
import org.apache.daffodil.runtime1.processors.WSP

/**
 * This base class handles the connections from state to state (which form a directed graph)
 * through use of call-by-name here and lazy vals within.  This allows us to define the
 * state-to-state connections functionally.
 *
 * This object itself will exist and be a member of the states ArrayBuffer as well as the other
 * states before any access to the lazy val members.
 */
abstract class State(states: Array[State]) extends Serializable {

  def stateNum: Int
  def stateName: String
  def rules: ArrayBuffer[Rule]

  /**
   * A state executes by evaluating guarded actions.
   * if rules(n).test() evaluates to true, then take
   * action by rules(n).act().
   */
  def run(r: Registers): Unit = {
    Assert.invariant(r.state == stateNum)
    runRules(rules, r)
  }

  final protected def runRules(rules: ArrayBuffer[Rule], r: Registers): Unit = {
    while (r.actionNum < rules.length) {
      val rule = rules(r.actionNum)
      val useThisRule = rule.test(r)
      if (useThisRule) {
        rule.act(r)
        // if (r.status == StateKind.Parsing) {
        return
        // }
      }
      r.actionNum = r.actionNum + 1
    }
    r.status = StateKind.Failed
  }

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

  def couldBeFirstChar(charIn: Char, delimIter: DelimiterIterator): Boolean = {
    delimIter.reset()
    while (delimIter.hasNext()) {
      if (couldBeFirstChar(charIn, delimIter.next())) {
        return true
      }
    }
    false
  }

  /**
   * Determines if the supplied character (charIn) could be
   * the start of this delimiter.
   *
   * This should only ever be called from DFAField.
   *
   * In the case of WSPStar, if charIn is not a whitespace then
   * we must compare charIn to the next character in the delimiter
   * if it's present.
   *
   */
  protected def couldBeFirstChar(charIn: Char, d: DFADelimiter): Boolean = {
    val states = d.states
    val pTerm0 = states(0)
    val res =
      if (pTerm0.isInstanceOf[WSPStarState]) {
        val wspStar = pTerm0.asInstanceOf[WSPStarState]
        if (wspStar.checkMatch(charIn)) {
          // Was a space
          true
        } else if (wspStar.nextState == DFA.FinalState) {
          // WSP* is not allowed to appear by itself as a terminator
          // or separator.
          //
          Assert.impossibleCase
        } else {
          // Wasn't a space, this is OK because it can be optional.
          // The character must match the next state in order for it to
          // be the start of a delimiter.
          //
          val pTerm1 = states(1).asInstanceOf[DelimStateBase]
          pTerm1.checkMatch(charIn)
        }
      } else {
        pTerm0.asInstanceOf[DelimStateBase].checkMatch(charIn)
      }
    res
  }

  object PauseRule extends Rule {
    override def test(r: Registers): Boolean = couldBeFirstChar(r.data0, r.delimitersIter)

    override def act(r: Registers): Unit = r.status = StateKind.Paused
  }

  object EndOfDataCharRule extends Rule {
    override def test(r: Registers): Boolean = r.data0 == DFA.EndOfDataChar

    override def act(r: Registers): Unit = r.nextState = DFA.EndOfData
  }

  object LookAheadEndOfDataCharRule extends Rule {
    override def test(r: Registers): Boolean = r.data1 == DFA.EndOfDataChar

    override def act(r: Registers): Unit = {
      r.appendToField(r.data0)
      r.nextState = DFA.EndOfData
    }
  }

  object StartStateRule extends Rule {
    override def test(r: Registers): Boolean = true

    override def act(r: Registers): Unit = {
      r.appendToField(r.data0)
      r.advance()
      r.nextState = StartState
    }
  }
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
class StartStateUnambiguousEscapeChar(
  states: Array[State],
  EEC: MaybeChar,
  EC: MaybeChar,
  val stateNum: Int
) extends State(states) {

  val stateName: String = "StartState"
  val rules = ArrayBuffer(
    PauseRule,
    new Rule {
      override def test(r: Registers): Boolean =
        EEC.isDefined && EC.isDefined && r.data0 == EEC.get

      override def act(r: Registers): Unit = r.nextState = EECState
    },
    new Rule {
      override def test(r: Registers): Boolean = EC.isDefined && r.data0 == EC.get

      override def act(r: Registers): Unit = r.nextState = ECState
    },
    EndOfDataCharRule,
    StartStateRule
  )
}

class StartState(states: Array[State], val stateNum: Int) extends State(states) {

  val stateName: String = "StartState"
  val rules = ArrayBuffer(PauseRule, EndOfDataCharRule, StartStateRule)
}

class StartStatePadding(states: Array[State], val padChar: Char) extends State(states) {

  val stateName = "StartState"
  val stateNum: Int = 0
  val rules = ArrayBuffer(
    new Rule {
      override def test(r: Registers): Boolean = r.data0 == padChar

      override def act(r: Registers): Unit = {
        r.appendToField(r.data0)
        r.advance()
        r.nextState = StartState
      }
    },
    new Rule {
      override def test(r: Registers): Boolean = true

      override def act(r: Registers): Unit = r.nextState = DFA.FinalState
    }
  )
}

/**
 * StartState for Field portion of EscapeBlock.
 * Here compiledDelims should only contain the endBlock DFADelimiter.
 */
class StartStateEscapeBlock(
  states: Array[State],
  val blockEnd: DFADelimiter,
  val EEC: MaybeChar,
  val stateNum: Int
) extends State(states) {
  val stateName: String = "StartState"

  val rules = ArrayBuffer(
    new Rule {
      override def test(r: Registers): Boolean = couldBeFirstChar(r.data0, blockEnd)

      override def act(r: Registers): Unit = {
        if (EEC.isDefined && (blockEnd.lookingFor.equals(EEC.get.toString))) { // EEC == escapeBlockEnd
          r.nextState = EECState
        } else {
          r.status = StateKind.Paused
        }
      }
    },
    new Rule {
      override def test(r: Registers): Boolean = EEC.isDefined && (r.data0 == EEC.get)

      override def act(r: Registers): Unit = r.nextState = EECState
    },
    EndOfDataCharRule,
    StartStateRule
  )
}

class StartStateEscapeChar(
  states: Array[State],
  val EEC: MaybeChar,
  val EC: Char,
  val stateNum: Int
) extends State(states) {
  val stateName: String = "StartState"

  object NextStateECRule extends Rule {
    override def test(r: Registers): Boolean = r.data0 == EC

    override def act(r: Registers): Unit = r.nextState = ECState
  }

  object NextStateEECRule extends Rule {
    override def test(r: Registers): Boolean =
      EEC.isDefined && r.data0 == EEC.get && r.data1 == EC

    override def act(r: Registers): Unit = r.nextState = EECState
  }

  val rules_NO_EEC_BUT_EC_TERM_SAME = ArrayBuffer(
    new Rule {
      override def test(r: Registers): Boolean =
        (r.data0 == EC) && couldBeFirstChar(r.data1, r.delimitersIter)

      override def act(r: Registers): Unit = r.nextState = ECState
    },
    PauseRule,
    NextStateECRule,
    EndOfDataCharRule,
    StartStateRule
  )

  val rules_EEC_EC_SAME_NOT_TERM = ArrayBuffer(
    PauseRule,
    NextStateEECRule,
    new Rule {
      override def test(r: Registers): Boolean =
        EEC.isDefined && r.data0 == EEC.get && r.data1 != EC

      override def act(r: Registers): Unit = r.nextState = ECState
    },
    EndOfDataCharRule,
    StartStateRule
  )

  val rules_EEC_TERM_SAME_NOT_EC =
    ArrayBuffer(PauseRule, NextStateEECRule, NextStateECRule, EndOfDataCharRule, StartStateRule)

  val rules_EC_TERM_SAME_NOT_EEC = ArrayBuffer(
    new Rule {
      override def test(r: Registers): Boolean = r.data0 == EC && r.data1 == EC

      override def act(r: Registers): Unit = {
        // EC followed by EC, first EC gets dropped
        r.dropChar(r.data0)
        r.appendToField(r.data1)
        r.advance()
        r.advance()
        r.nextState = StartState
      }
    },
    PauseRule,
    NextStateEECRule,
    NextStateECRule,
    EndOfDataCharRule,
    StartStateRule
  )

  val rules_EC_EEC_TERM_SAME = ArrayBuffer(
    PauseRule,
    new Rule {
      override def test(r: Registers): Boolean =
        EEC.isDefined && r.data0 == EEC.get && r.data1 == EEC.get

      override def act(r: Registers): Unit = r.nextState = EECState
    },
    new Rule {
      override def test(r: Registers): Boolean =
        EEC.isDefined && r.data0 == EEC.get && r.data1 != DFA.EndOfDataChar

      override def act(r: Registers): Unit = {
        // EEC followed by something not a PTERM[2]
        r.dropChar(r.data0)
        r.appendToField(r.data1)
        r.advance()
        r.advance()
        r.nextState = StartState
      }
    },
    new Rule {
      override def test(r: Registers): Boolean =
        EEC.isDefined && r.data0 == EEC.get && r.data1 == DFA.EndOfDataChar

      override def act(r: Registers): Unit = r.nextState = ECState
    },
    EndOfDataCharRule,
    StartStateRule
  )

  val rules_Unambiguous = ArrayBuffer(
    PauseRule,
    new Rule {
      override def test(r: Registers): Boolean = EEC.isDefined && r.data0 == EEC.get

      override def act(r: Registers): Unit = r.nextState = EECState
    },
    NextStateECRule,
    EndOfDataCharRule,
    StartStateRule
  )

  /**
   * A state executes by evaluating guarded actions.
   * if rules(n).test() evaluates to true, then take
   * action by rules(n).act().
   */
  override def run(r: Registers): Unit = {
    val rules = getRules(
      r.delimitersIter
    ) // TODO: Performance - this should be determined at schema-compilation time.
    // even though the specific delimiters may be runtime determined, there are guaranteed to be delimiters, escape chars etc.
    // such that we can choose the right DFA based on what is defined even if the specific character(s) we're using
    // come later.
    runRules(rules, r)
  }

  /**
   * Determines what rules to execute based on combinations of EC/EEC
   */
  def getRules(delimIter: DelimiterIterator): ArrayBuffer[Rule] = {

    val result = {
      if (!EEC.isDefined) {
        if (couldBeFirstChar(EC, delimIter)) {
          // EC == PTERM0
          rules_NO_EEC_BUT_EC_TERM_SAME
        } else {
          // Unambiguous
          rules_Unambiguous
        }
      } else if (EEC.isDefined) {
        val escEsc = EEC.get
        if (
          !couldBeFirstChar(escEsc, delimIter) && !couldBeFirstChar(
            EC,
            delimIter
          ) && escEsc != EC
        ) {
          // EC != EEC != PTERM0
          rules_Unambiguous
        } else if (EC == escEsc && couldBeFirstChar(escEsc, delimIter)) {
          // EC == EEC == PTERM0
          rules_EC_EEC_TERM_SAME
        } else if (couldBeFirstChar(escEsc, delimIter) && !couldBeFirstChar(EC, delimIter)) {
          // (EEC == PTERM0) != EC
          rules_EEC_TERM_SAME_NOT_EC
        } else if (!couldBeFirstChar(escEsc, delimIter) && couldBeFirstChar(EC, delimIter)) {
          // (EC == PTERM0) != EEC
          rules_EC_TERM_SAME_NOT_EEC
        } else if (EC == escEsc && !couldBeFirstChar(escEsc, delimIter)) {
          // (EC == EEC) != PTERM0
          rules_EEC_EC_SAME_NOT_TERM
        } else throw new Exception("Unexpected case.")
      } else throw new Exception("Unexpected case.")
    }
    result
  }

  var rules: ArrayBuffer[Rule] = ArrayBuffer.empty

}

class ECState(states: Array[State], val EC: Char, val stateNum: Int) extends State(states) {
  val stateName = "ECState"
  val rules = ArrayBuffer(
    // ECState, means that data0 is EC
    //
    new Rule {
      override def test(r: Registers): Boolean = couldBeFirstChar(r.data1, r.delimitersIter)

      override def act(r: Registers): Unit = {
        // constituent character
        r.dropChar(r.data0)
        r.appendToField(r.data1)
        r.advance()
        r.advance()
        r.nextState = StartState
      }
    },
    new Rule {
      override def test(r: Registers): Boolean = r.data1 == EC

      override def act(r: Registers): Unit = {
        // next char is also EC, drop this EC
        // but do not treat next EC as char
        r.dropChar(r.data0)
        r.advance()
        r.nextState = StartState
      }
    },
    new Rule {
      override def test(r: Registers): Boolean = r.data1 == DFA.EndOfDataChar

      override def act(r: Registers): Unit = {
        r.dropChar(r.data0)
        r.nextState = DFA.EndOfData
      }
    },
    new Rule {
      override def test(r: Registers): Boolean = true

      override def act(r: Registers): Unit = {
        // constituent character
        r.dropChar(r.data0)
        r.appendToField(r.data1)
        r.advance()
        r.advance()
        r.nextState = StartState
      }
    }
  )
}

class EECState(
  states: Array[State],
  val EEC: MaybeChar,
  val EC: Char,
  val stateNum: Int
) extends State(states) {
  val stateName = "EECState"
  val rules = ArrayBuffer(
    // Because this is about EC and EEC we can
    // just write these rules down statically.
    // The real delim compiler would generate a flock of rules
    // based on what the actual delimiters are.
    //
    // We've already encountered EEC as data0 here
    //
    PauseRule,
    new Rule {
      override def test(r: Registers): Boolean = r.data1 == EC

      override def act(r: Registers): Unit = {
        // Drop EEC aka data0
        r.dropChar(r.data0)
        r.appendToField(r.data1)
        r.advance()
        r.advance()
        r.nextState = StartState
      }
    },
    LookAheadEndOfDataCharRule,
    StartStateRule
  )
}

class EECStateBlock(
  states: Array[State],
  blockEnd: DFADelimiter,
  val EEC: MaybeChar,
  val stateNum: Int
) extends State(states) {
  val stateName = "EECState"
  val rules = ArrayBuffer(
    // Because this is about EC and EEC we can
    // just write these rules down statically.
    // The real delim compiler would generate a flock of rules
    // based on what the actual delimiters are.
    //
    // We've already encountered EEC as data0 here
    //
    new Rule {
      override def test(r: Registers): Boolean =
        couldBeFirstChar(r.data0, blockEnd) && !couldBeFirstChar(r.data1, blockEnd)

      override def act(r: Registers): Unit = r.status = StateKind.Paused
    },
    new Rule {
      override def test(r: Registers): Boolean = couldBeFirstChar(r.data1, blockEnd)

      override def act(r: Registers): Unit = {
        // EEC followed by possible blockEnd
        r.dropChar(r.data0)
        r.appendToField(r.data1)
        r.advance()
        r.advance()
        r.nextState = StartState
      }
    },
    new Rule {
      override def test(r: Registers): Boolean = r.data1 == EEC.get

      override def act(r: Registers): Unit = {
        // EEC followed by EEC, stays in field
        r.appendToField(r.data0)
        r.advance()
        r.nextState = StartState
      }
    },
    LookAheadEndOfDataCharRule,
    StartStateRule
  )
}

abstract class DelimStateBase(states: Array[State]) extends State(states) {
  var stateName: String = null

  def setStateName(name: String) = stateName = name

  def rules: ArrayBuffer[Rule]

  def nextState: Int

  def checkMatch(charIn: Char): Boolean

  override def toString(): String = {
    "<" + stateName + "_" + stateNum + "/>"
  }
}

class CharState(
  states: Array[State],
  char: Char,
  val nextState: Int,
  val stateNum: Int,
  ignoreCase: Boolean
) extends DelimStateBase(states) {
  stateName = "CharState(" + char + ")"
  val rulesToThisState = ArrayBuffer(new Rule {
    override def test(r: Registers): Boolean = checkMatch(r.data0)

    override def act(r: Registers): Unit = {
      r.appendToDelim(r.data0)
      r.advance()
      r.nextState = stateNum
    }
  })

  val rules = ArrayBuffer(new Rule {
    override def test(r: Registers): Boolean = checkMatch(r.data0)

    override def act(r: Registers): Unit = {
      r.appendToDelim(r.data0)
      r.advance()
      r.nextState = nextState
    }
  })

  @inline
  private def checkMatchIgnoreCase(charIn: Char): Boolean = {
    // note that we must check both toUpper and toLower. This is based on
    // String.java's equalsIgnoreCase method, which mentions that it is
    // possible for toUpper to not match but toLower to match due to the
    // behaviors of some alphabets (e.g. Gregorian)
    char.toUpper == charIn.toUpper || char.toLower == charIn.toLower
  }

  def checkMatch(charIn: Char): Boolean = {
    if (char == charIn) true
    else if (ignoreCase && checkMatchIgnoreCase(charIn)) true
    else false
  }
}

abstract class WSPBase(states: Array[State]) extends DelimStateBase(states) with WSP {
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

class WSPState(states: Array[State], val nextState: Int, val stateNum: Int)
  extends WSPBase(states) {
  stateName = "WSPState"
  val rulesToThisState = ArrayBuffer(new Rule {
    override def test(r: Registers): Boolean = checkMatch(r.data0)

    override def act(r: Registers): Unit = {
      r.appendToDelim(r.data0)
      r.advance()
      r.nextState = stateNum
    }
  })

  val rules = ArrayBuffer(new Rule {
    override def test(r: Registers): Boolean = checkMatch(r.data0)

    override def act(r: Registers): Unit = {
      r.appendToDelim(r.data0)
      r.advance()
      r.nextState = nextState
    }
  })
}

abstract class WSPRepeats(states: Array[State]) extends WSPBase(states) {}

class WSPPlusState(
  states: Array[State],
  val nextState: Int,
  val stateNum: Int
) extends WSPRepeats(states) {
  stateName = "WSPPlusState"
  val rulesToThisState = ArrayBuffer(new Rule {
    override def test(r: Registers): Boolean = checkMatch(r.data0)

    override def act(r: Registers): Unit = {
      r.appendToDelim(r.data0)
      r.advance()
      r.nextState = stateNum // This state
    }
  })
  val rules = ArrayBuffer(
    new Rule {
      override def test(r: Registers): Boolean = checkMatch(r.data0)

      override def act(r: Registers): Unit = {
        r.appendToDelim(r.data0)
        r.advance()
        r.matchedAtLeastOnce = true
        r.nextState = stateNum // This state
      }
    },
    new Rule {
      override def test(r: Registers): Boolean = r.matchedAtLeastOnce

      override def act(r: Registers): Unit = {
        r.matchedAtLeastOnce = false
        r.nextState = nextState
      }
    }
  )
}

class WSPStarState(
  states: Array[State],
  val nextState: Int,
  val stateNum: Int
) extends WSPRepeats(states) {
  stateName = "WSPStarState"

  val rules = ArrayBuffer(
    new Rule {
      override def test(r: Registers): Boolean = checkMatch(r.data0)

      override def act(r: Registers): Unit = {
        r.appendToDelim(r.data0)
        r.advance()
        r.nextState = stateNum // This state
      }
    },
    new Rule {
      override def test(r: Registers): Boolean = true

      override def act(r: Registers): Unit = r.nextState = nextState
    }
  )
}

abstract class NLBase(states: Array[State]) extends DelimStateBase(states) with NL {
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

class NLState(states: Array[State], val nextState: Int, val stateNum: Int)
  extends NLBase(states) {
  stateName = "NLState"

  val rules = ArrayBuffer(
    new Rule {
      override def test(r: Registers): Boolean = {
        isCR(r.data0) && isLF(r.data1)
      }

      override def act(r: Registers): Unit = {
        r.appendToDelim(r.data0)
        r.appendToDelim(r.data1)
        r.advance()
        r.advance()
        r.nextState = nextState
      }
    },
    new Rule {
      override def test(r: Registers): Boolean = {
        isCR(r.data0) && !isLF(r.data1)
      }

      override def act(r: Registers): Unit = {
        r.appendToDelim(r.data0)
        r.advance()
        r.nextState = nextState
      }
    },
    new Rule {
      override def test(r: Registers): Boolean = isNLNotCR(r.data0)

      override def act(r: Registers): Unit = {
        r.appendToDelim(r.data0)
        r.advance()
        r.nextState = nextState
      }
    }
  )

  def checkMatch(charIn: Char): Boolean = isNLNotCR(charIn) || isCR(charIn)
}

class ESState(states: Array[State], val nextState: Int, val stateNum: Int)
  extends NLBase(states) {
  stateName = "ESState"

  /**
   * This contains a sortof degenerate rule that always succeeds to handle the
   * empty-string corner case required by the %ES; character class. This will
   * immediately match while consuming no characters.
   */
  lazy val rules = ArrayBuffer(
    new Rule {
      override def test(r: Registers): Boolean = true

      override def act(r: Registers): Unit = r.status = StateKind.Succeeded
    }
  )

  def checkMatch(charIn: Char): Boolean =
    Assert.impossible("We should never ask if a character matches an %ES;")
}
