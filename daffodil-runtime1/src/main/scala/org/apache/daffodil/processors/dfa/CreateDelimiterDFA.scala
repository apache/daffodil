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

package org.apache.daffodil.processors.dfa

import scala.collection.mutable.ArrayBuffer

import org.apache.daffodil.processors.CharDelim
import org.apache.daffodil.processors.DelimBase
import org.apache.daffodil.processors.Delimiter
import org.apache.daffodil.processors.ESDelim
import org.apache.daffodil.processors.NLDelim
import org.apache.daffodil.processors.WSPDelim
import org.apache.daffodil.processors.WSPPlusDelim
import org.apache.daffodil.processors.WSPStarDelim
import org.apache.daffodil.processors.parsers.DelimiterTextType
import org.apache.daffodil.dsom.DPathCompileInfo

object CreateDelimiterDFA {

  /**
   * Constructs an Array of states reflecting the delimiters only.
   * StateNum is offset by stateOffset
   */
  protected def apply(delimType: DelimiterTextType.Type, ci: DPathCompileInfo, delimiter: Seq[DelimBase], delimiterStr: String, outputNewLine: String): DFADelimiter = {

    val allStates: ArrayBuffer[State] = ArrayBuffer.empty

    buildTransitions(delimiter, allStates, false)

    val unparseValue = delimiter.map { _.unparseValue(outputNewLine) }.mkString
    new DFADelimiterImplUnparse(delimType, allStates.reverse.toArray, delimiterStr, unparseValue, ci.schemaFileLocation)
  }

  /**
   * Constructs an Array of states reflecting the delimiters only.
   * StateNum is offset by stateOffset
   */
  protected def apply(delimType: DelimiterTextType.Type, ci: DPathCompileInfo, delimiter: Seq[DelimBase], delimiterStr: String, ignoreCase: Boolean): DFADelimiter = {

    val allStates: ArrayBuffer[State] = ArrayBuffer.empty

    buildTransitions(delimiter, allStates, ignoreCase)

    new DFADelimiterImpl(delimType, allStates.reverse.toArray, delimiterStr, ci.schemaFileLocation)
  }

  /**
   * Converts a String to a DFA representing
   * that string
   */
  def apply(delimType: DelimiterTextType.Type, ci: DPathCompileInfo, delimiterStr: String, ignoreCase: Boolean): DFADelimiter = {
    val d = new Delimiter()
    d.compileDelimiter(delimiterStr, ignoreCase)
    val db = d.delimBuf
    apply(delimType, ci, db, delimiterStr, ignoreCase)
  }

  /**
   * Converts a String to a DFA representing
   * that string
   */
  def apply(delimType: DelimiterTextType.Type, ci: DPathCompileInfo, delimiterStr: String, outputNewLine: String): DFADelimiter = {
    val d = new Delimiter()
    d.compileDelimiter(delimiterStr, false)
    val db = d.delimBuf
    apply(delimType, ci, db, delimiterStr, outputNewLine)
  }

  /**
   * Converts a Seq of String to a Seq of
   * DFA's representing each String with outputNewLine.
   */
  def apply(delimType: DelimiterTextType.Type, ci: DPathCompileInfo, delimiters: Seq[String], outputNewLine: String): Array[DFADelimiter] = {
    delimiters.map(d => apply(delimType, ci, d, outputNewLine)).toArray
  }

  /**
   * Converts a Seq of String to a Seq of
   * DFA's representing each String.
   */
  def apply(delimType: DelimiterTextType.Type, ci: DPathCompileInfo, delimiters: Seq[String], ignoreCase: Boolean): Array[DFADelimiter] = {
    delimiters.map(d => apply(delimType, ci, d, ignoreCase)).toArray
  }

  /**
   * Returns a state representing the DelimBase object.
   */
  protected def getState(d: DelimBase, nextState: Int, stateNum: Int,
    allStates: ArrayBuffer[State], ignoreCase: Boolean): DelimStateBase = {

    val theState = d match {
      case d: CharDelim => {
        new CharState(allStates, d.char, nextState, stateNum, ignoreCase)
      }
      case d: WSPDelim => {
        new WSPState(allStates, nextState, stateNum)
      }
      case d: WSPStarDelim => {
        new WSPStarState(allStates, nextState, stateNum)
      }
      case d: WSPPlusDelim => {
        new WSPPlusState(allStates, nextState, stateNum)
      }
      case d: NLDelim => {
        new NLState(allStates, nextState, stateNum)
      }
      case d: ESDelim => {
        new ESState(allStates, nextState, stateNum)
      }
    }
    theState
  }

  private def buildTransitions(
    delim: Seq[DelimBase],
    allStates: ArrayBuffer[State], ignoreCase: Boolean): State = {
    assert(!delim.isEmpty)
    buildTransitions(null, delim.reverse, allStates, ignoreCase)
  }

  private def buildTransitions(nextState: DelimStateBase, delim: Seq[DelimBase],
    allStates: ArrayBuffer[State], ignoreCase: Boolean): State = {

    if (delim.isEmpty && nextState != null) {
      // We are initial state
      nextState.stateName = "StartState" //"PTERM0"
      return nextState
    }

    val currentState = getState(
      delim(0),
      if (nextState == null) DFA.FinalState else nextState.stateNum,
      delim.length - 1, allStates, ignoreCase)
    val rest = delim.tail

    allStates += currentState
    return buildTransitions(currentState, rest, allStates, ignoreCase)
  }
}
