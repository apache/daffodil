/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.processors.dfa

import scala.collection.mutable.ArrayBuffer

import org.apache.daffodil.processors.CharDelim
import org.apache.daffodil.processors.DelimBase
import org.apache.daffodil.processors.Delimiter
import org.apache.daffodil.processors.NLDelim
import org.apache.daffodil.processors.WSPDelim
import org.apache.daffodil.processors.WSPPlusDelim
import org.apache.daffodil.processors.WSPStarDelim
import org.apache.daffodil.processors.RuntimeData
import org.apache.daffodil.processors.parsers.DelimiterTextType

object CreateDelimiterDFA {

  /**
   * Constructs an Array of states reflecting the delimiters only.
   * StateNum is offset by stateOffset
   */
  protected def apply(delimType: DelimiterTextType.Type, rd: RuntimeData, delimiter: Seq[DelimBase], delimiterStr: String, outputNewLine: String): DFADelimiter = {

    val allStates: ArrayBuffer[State] = ArrayBuffer.empty

    buildTransitions(delimiter, allStates, false)

    val unparseValue = delimiter.map { _.unparseValue(outputNewLine) }.mkString
    new DFADelimiterImplUnparse(delimType, allStates.reverse.toArray, delimiterStr, unparseValue, rd.schemaFileLocation)
  }

  /**
   * Constructs an Array of states reflecting the delimiters only.
   * StateNum is offset by stateOffset
   */
  protected def apply(delimType: DelimiterTextType.Type, rd: RuntimeData, delimiter: Seq[DelimBase], delimiterStr: String, ignoreCase: Boolean): DFADelimiter = {

    val allStates: ArrayBuffer[State] = ArrayBuffer.empty

    buildTransitions(delimiter, allStates, ignoreCase)

    new DFADelimiterImpl(delimType, allStates.reverse.toArray, delimiterStr, rd.schemaFileLocation)
  }

  /**
   * Converts a String to a DFA representing
   * that string
   */
  def apply(delimType: DelimiterTextType.Type, rd: RuntimeData, delimiterStr: String, ignoreCase: Boolean): DFADelimiter = {
    val d = new Delimiter()
    d.compileDelimiter(delimiterStr, ignoreCase)
    val db = d.delimBuf
    apply(delimType, rd, db, delimiterStr, ignoreCase)
  }

  /**
   * Converts a String to a DFA representing
   * that string
   */
  def apply(delimType: DelimiterTextType.Type, rd: RuntimeData, delimiterStr: String, outputNewLine: String): DFADelimiter = {
    val d = new Delimiter()
    d.compileDelimiter(delimiterStr, false)
    val db = d.delimBuf
    apply(delimType, rd, db, delimiterStr, outputNewLine)
  }

  /**
   * Converts a Seq of String to a Seq of
   * DFA's representing each String.
   */
  def apply(delimType: DelimiterTextType.Type, rd: RuntimeData, delimiters: Seq[String], ignoreCase: Boolean): Array[DFADelimiter] = {
    delimiters.map(d => apply(delimType, rd, d, ignoreCase)).toArray
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
    }
    theState
  }

  private def buildTransitions(delim: Seq[DelimBase],
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

    val currentState = getState(delim(0),
      if (nextState == null) DFA.FinalState else nextState.stateNum,
      delim.length - 1, allStates, ignoreCase)
    val rest = delim.tail

    allStates += currentState
    return buildTransitions(currentState, rest, allStates, ignoreCase)
  }
}
