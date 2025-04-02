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

import org.apache.daffodil.lib.util.MaybeChar

/**
 * (12:12:21 PM) Mike Beckerle: I think I understand this. Let me explain why the 'backtrack" is ok.
 * (12:12:38 PM) Mike Beckerle: We have this DFA, but in our pictures there's this PTERM state.
 * (12:12:56 PM) Mike Beckerle: That PTERM state is a "macro" for a much more complicated DFA.
 * (12:13:54 PM) Mike Beckerle: The upshot is this "macro" accepts - having consumed a bunch of input, or it fails, and we are supposed to be back at the start of the delimiter.
 * (12:15:06 PM) Mike Beckerle: That "macro" can be expressed by exploding it into a big DFA, or by some claver logic that orchestrates part-specific matchers.
 * (12:16:46 PM) Mike Beckerle: The fastest thing would be to explode it into a big DFA, but pragmatically this might not be noticably faster than the other way.
 * (12:17:38 PM) Taylor: Why I was wondering if we should have two DFA types.  Field and delimiter.  Where the 'Field' can be 'paused' while we check the delimiters.
 * (12:17:47 PM) Taylor: If the delimiter match succeeds, we're done.
 * (12:18:06 PM) Taylor: if it fails, resume the 'field' DFA with the r.data0 as a char.
 * (12:18:37 PM) Taylor: all the field is doing is handling escape schemes if any
 * (12:18:39 PM) Mike Beckerle: That is, in principle, what the transition to the "macro" PTERM state is. It's suspending current state.  So yeah, I'm good with that.
 * (12:18:46 PM) Taylor: ok cool :D
 * (12:19:13 PM) Mike Beckerle: Cut paste this dialog into a comment in the code somewhere.
 */
object CreateFieldDFA {

  /**
   * Constructs a DFAField object without EscapeSchemeKind.None
   */
  def apply(): DFAField = {

    val allStates: Array[State] = new Array[State](1)

    val startState = new StartState(allStates, 0)

    allStates(0) = startState

    new DFAFieldImpl(allStates)
  }

  /**
   * Constructs a DFAField object with EscapeSchemeKind.Character
   */
  def apply(EC: Char, EEC: MaybeChar): DFAField = {

    val allStates: Array[State] = new Array(3)

    val ecState = new ECState(allStates, EC, 1)
    val eecState = new EECState(allStates, EEC, EC, 2)
    val startState = new StartStateEscapeChar(allStates, EEC, EC, 0)

    allStates(2) = eecState
    allStates(1) = ecState
    allStates(0) = startState

    new DFAFieldImpl(allStates)
  }

  /**
   * Constructs a DFAField object with EscpaeSchemeKind.Block
   */
  def apply(blockEnd: DFADelimiter, EEC: MaybeChar): DFAField = {

    val allStates: Array[State] = new Array[State](2)

    val eecState = new EECStateBlock(allStates, blockEnd, EEC, 1)
    val startState = new StartStateEscapeBlock(allStates, blockEnd, EEC, 0)

    allStates(1) = eecState
    allStates(0) = startState

    new DFAFieldImpl(allStates)
  }

}
