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

import org.apache.daffodil.io.DataInputStream
import org.apache.daffodil.lib.equality._
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.lib.util.MaybeChar
import org.apache.daffodil.runtime1.processors.DelimiterIterator
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.TextJustificationType
import org.apache.daffodil.runtime1.processors.parsers.PState
import org.apache.daffodil.runtime1.processors.parsers.PaddingRuntimeMixin

abstract class TextDelimitedParserBase(
  override val justificationTrim: TextJustificationType.Type,
  override val parsingPadChar: MaybeChar,
  override val context: TermRuntimeData
) extends DFAParser
  with PaddingRuntimeMixin {

  private lazy val padCharInfo =
    if (parsingPadChar.isDefined) parsingPadChar.toString else "NONE"
  lazy val info: String =
    "justification='" + justificationTrim + "', padChar='" + padCharInfo + "'"

  final def parse(
    state: PState,
    input: DataInputStream,
    field: DFAField,
    delimIter: DelimiterIterator,
    isDelimRequired: Boolean
  ): Maybe[ParseResult] = {
    Assert.invariant(field != null)

    val lmt = new LongestMatchTracker()

    val fieldReg: Registers = state.dfaRegistersPool.getFromPool("TextDelimitedParserBase1")
    try { // to insure the fieldReg are returned to the pool even if there is an unexpected throw from the I/O layer
      fieldReg.reset(state, input, delimIter) // Initialization

      var stillSearching: Boolean = true
      var beforeDelimiter: DataInputStream.MarkPos = DataInputStream.MarkPos.NoMarkPos
      while (stillSearching) {

        Assert.invariant(beforeDelimiter =#= DataInputStream.MarkPos.NoMarkPos)
        field.run(fieldReg)
        beforeDelimiter = input.markPos

        fieldReg.status match {
          case StateKind.EndOfData => stillSearching = false
          case StateKind.Failed => stillSearching = false
          case StateKind.Paused => {

            delimIter.reset()
            while (delimIter.hasNext()) {
              val d = delimIter.next()
              input.resetPos(beforeDelimiter)
              beforeDelimiter = input.markPos
              val delimReg: Registers =
                state.dfaRegistersPool.getFromPool("TextDelimitedParserBase2")
              delimReg.reset(state, input, delimIter)
              d.run(delimReg)
              if (delimReg.status == StateKind.Succeeded) {
                lmt.successfulMatch(
                  delimReg.matchStartPos,
                  delimReg.delimString,
                  d,
                  delimIter.currentIndex
                )
              }
              state.dfaRegistersPool.returnToPool(delimReg)
            }
            if (!lmt.longestMatches.isEmpty) {
              stillSearching = false
            } else {
              // resume field parse
              // TODO: Please explain here why this is the way one resumes the field dfa?
              // TODO: The above assignment to the actionNum is the only reason that actionNum can't just
              // be a local variable in the run-the-rules loop.
              //
              // I'd like this code better if the flow was different.
              //
              // Right now: When scanning to isolate a field, when we hit a character that could be the first
              // character of some delimiter, we return with status PAUSED, which means PAUSE to see if there is
              // a complete delimiter.
              // If so then we're done with the field. If not, however, then we go around the loop and resume ...
              // and the rub is we resume in the middle of the rules for the current state. This is subtle, and
              // error prone.
              //
              // A better flow would (a) encapsulate all this redundant code better (b) on encountering the
              // first character of a delimiter, transition to a state that represents that we found a possible
              // first character of a delimiter. Then that
              // state's rules would be guarded by finding the longest match delimiter. If found transition to a
              // state indicating a field has been isolated. If the delimiter is not found, then accumulate the character
              // as a constituent of the field, and transition to the start state.
              //
              input.resetPos(
                beforeDelimiter
              ) // reposition input to where we were trying to find a delimiter (but did not)
              beforeDelimiter = DataInputStream.MarkPos.NoMarkPos
              fieldReg.actionNum =
                fieldReg.actionNum + 1 // but force it to goto next rule so it won't just retry what it just did.
              stillSearching = true
            }
          }
        }
      }
      Assert.invariant(beforeDelimiter != DataInputStream.MarkPos.NoMarkPos)
      input.resetPos(beforeDelimiter)
      val result = {
        if (lmt.longestMatches.isEmpty) {
          // there were no delimiter matches
          if (isDelimRequired) Nope
          else {
            val fieldValue: Maybe[String] = {
              val str = fieldReg.resultString.toString
              // TODO: Performance - avoid this copying of the string. We should be able to trim
              // on a CharSequence which is a base of both String and StringBuilder
              // Difficulty is the only common base to String and StringBuilder is CharSequence which is
              // pretty sparse.
              val fieldNoPadding = trimByJustification(str)
              One(fieldNoPadding)
            }
            One(new ParseResult(fieldValue, Nope, lmt.longestMatches))
          }
        } else {
          val fieldValue: Maybe[String] = {
            val str = fieldReg.resultString.toString // TODO: Performance see above.
            val fieldNoPadding = trimByJustification(str)
            One(fieldNoPadding)
          }
          val delim: Maybe[String] = {
            One(lmt.longestMatchedString)
          }

          One(new ParseResult(fieldValue, delim, lmt.longestMatches))
        }
      }
      result
    } finally {
      state.dfaRegistersPool.returnToPool(fieldReg)
      state.dfaRegistersPool.finalCheck()
    }
  }

}

/**
 * Assumes that the delims DFAs were constructed with the Esc
 * and EscEsc in mind.
 */
class TextDelimitedParser(
  justArg: TextJustificationType.Type,
  padCharArg: MaybeChar,
  context: TermRuntimeData
) extends TextDelimitedParserBase(justArg, padCharArg, context) {

  lazy val name: String = "TextDelimitedParser"

}

/**
 * Assumes that endBlock DFA was constructed with the
 * EscEsc in mind.
 */
class TextDelimitedParserWithEscapeBlock(
  justArg: TextJustificationType.Type,
  padCharArg: MaybeChar,
  context: TermRuntimeData
) extends TextDelimitedParserBase(justArg, padCharArg, context) {

  lazy val name: String = "TextDelimitedParserWithEscapeBlock"

  val leftPadding: DFADelimiter = {
    justificationTrim match {
      case TextJustificationType.Center | TextJustificationType.Right
          if parsingPadChar.isDefined =>
        CreatePaddingDFA(parsingPadChar.get, context)
      case _ => null
    }
  }

  val rightPadding: DFADelimiter = {
    justificationTrim match {
      case TextJustificationType.Center | TextJustificationType.Left
          if parsingPadChar.isDefined =>
        CreatePaddingDFA(parsingPadChar.get, context)
      case _ => null
    }
  }

  protected def removeLeftPadding(
    state: PState,
    input: DataInputStream,
    delimIter: DelimiterIterator
  ): Unit = {
    justificationTrim match {
      case TextJustificationType.Center | TextJustificationType.Right
          if parsingPadChar.isDefined => {
        val leftPaddingRegister = state.dfaRegistersPool.getFromPool("removeLeftPadding")
        leftPaddingRegister.reset(state, input, delimIter)
        leftPadding.run(leftPaddingRegister)
        state.dfaRegistersPool.returnToPool(leftPaddingRegister)
      }
      case _ => // No left padding
    }
  }

  protected def removeRightPadding(
    state: PState,
    input: DataInputStream,
    delimIter: DelimiterIterator
  ): Unit = {
    justificationTrim match {
      case TextJustificationType.Center | TextJustificationType.Left
          if parsingPadChar.isDefined => {
        val rightPaddingRegister = state.dfaRegistersPool.getFromPool("removeRightPadding")
        rightPaddingRegister.reset(state, input, delimIter)
        rightPadding.run(rightPaddingRegister)
        state.dfaRegistersPool.returnToPool(rightPaddingRegister)
      }
      case _ => // No right padding
    }
  }

  protected def parseStartBlock(
    state: PState,
    input: DataInputStream,
    startBlock: DFADelimiter,
    delimIter: DelimiterIterator
  ): Boolean = {
    val startBlockRegister = state.dfaRegistersPool.getFromPool("parseStartBlock")
    startBlockRegister.reset(state, input, delimIter)

    startBlock.run(startBlockRegister) // find the block start, fail otherwise
    val startStatus = startBlockRegister.status
    state.dfaRegistersPool.returnToPool(startBlockRegister)
    startStatus match {
      case StateKind.Succeeded => true // continue
      case _ => false // Failed
    }
  }

  /**
   * Called to parse the rest of the field until we reach a block end, but
   * beyond that, after we reach a block-end out until we reach the delimiter.
   */
  protected def parseRemainder(
    state: PState,
    input: DataInputStream,
    fieldEsc: DFAField,
    startBlock: DFADelimiter,
    endBlock: DFADelimiter,
    delimIter: DelimiterIterator,
    isDelimRequired: Boolean
  ): Maybe[ParseResult] = {

    val lmt = new LongestMatchTracker()

    val fieldRegister = state.dfaRegistersPool.getFromPool("parseRemainder")
    fieldRegister.reset(state, input, delimIter)

    var stillSearching: Boolean = true
    var foundBlockEnd: Boolean = false
    var beforeDelimiter: DataInputStream.MarkPos = DataInputStream.MarkPos.NoMarkPos
    while (stillSearching) {

      Assert.invariant(beforeDelimiter =#= DataInputStream.MarkPos.NoMarkPos)
      fieldEsc.run(fieldRegister)
      val dfaStatus = fieldRegister.status
      beforeDelimiter =
        input.markPos // at this point the input is one past the end of the field.
      fieldRegister.actionNum = 0

      dfaStatus match {
        case StateKind.EndOfData => stillSearching = false
        case StateKind.Failed => stillSearching = false
        case StateKind.Paused => {
          // Pick up where field left off, we are looking for the blockEnd.
          val endBlockRegister = state.dfaRegistersPool.getFromPool("parseRemainder2")
          endBlockRegister.reset(state, input, delimIter)
          endBlock.run(endBlockRegister)
          val endBlockStatus = endBlockRegister.status
          state.dfaRegistersPool.returnToPool(endBlockRegister)

          endBlockStatus match {
            case StateKind.Succeeded => {
              // Found the unescaped block end, now we need to
              // find any padding.
              this.removeRightPadding(state, input, delimIter)
              beforeDelimiter = input.markPos

              delimIter.reset()
              while (delimIter.hasNext()) {
                // Finally, we can look for the delimiter.
                val d = delimIter.next() // Pick up where end of block/padding left off
                val delimRegister = state.dfaRegistersPool.getFromPool("parseRemainder3")
                input.resetPos(beforeDelimiter)
                beforeDelimiter = input.markPos
                delimRegister.reset(state, input, delimIter)

                d.run(delimRegister)
                if (delimRegister.status == StateKind.Succeeded) {
                  lmt.successfulMatch(
                    delimRegister.matchStartPos,
                    delimRegister.delimString,
                    d,
                    delimIter.currentIndex
                  )
                }
                state.dfaRegistersPool.returnToPool(delimRegister)
              }
              foundBlockEnd = true
              stillSearching = false
            }
            case _ => {
              //
              // In this case we already found
              // a block start, (because we're in parseRemainder)
              // and we halted scanning because we found the start of a block end
              // However, it turns out not to be an entire block end.
              //
              // So we keep going. But we have to accumulate the
              // characters we were scrutinizing as the possible block end
              // into the field.
              //
              input.resetPos(beforeDelimiter)
              beforeDelimiter = DataInputStream.MarkPos.NoMarkPos
              fieldRegister.resetChars(state)

              // resume field parse
              //
              // This resumes the field dfa by moving it onto the next rule
              // This assumes that the field dfa will resume properly using
              // it's existing state, so long as the input is repositioned properly.
              //
              fieldRegister.actionNum = fieldRegister.actionNum + 1 // goto next rule
            }
          }
        }
      }
    } // End While
    Assert.invariant(beforeDelimiter !=#= DataInputStream.MarkPos.NoMarkPos)
    input.resetPos(beforeDelimiter)
    val result = {
      if (lmt.longestMatches.isEmpty) {
        //
        // No delimiter found
        // that may or may not be ok
        //
        if (foundBlockEnd && isDelimRequired) Nope
        else if (!foundBlockEnd) Nope
        else {
          //
          // In this case we found a block end, and no delimiter is required
          // so we have enough to be done with the field
          //
          val fieldValue: Maybe[String] = {
            One(fieldRegister.resultString.toString)
          }
          One(new ParseResult(fieldValue, Nope, lmt.longestMatches))
        }
      } else {
        //
        // A delimiter was found
        //
        val fieldValue: Maybe[String] = {
          One(fieldRegister.resultString.toString)
        }
        val delim: Maybe[String] = {
          One(lmt.longestMatchedString)
        }
        One(new ParseResult(fieldValue, delim, lmt.longestMatches))
      }
    }

    state.dfaRegistersPool.returnToPool(fieldRegister)
    result
  }

  def parse(
    state: PState,
    input: DataInputStream,
    field: DFAField,
    fieldEsc: DFAField,
    startBlock: DFADelimiter,
    endBlock: DFADelimiter,
    delimIter: DelimiterIterator,
    isDelimRequired: Boolean
  ): Maybe[ParseResult] = {
    Assert.invariant(fieldEsc != null)
    Assert.invariant(field != null)
    Assert.invariant(startBlock != null)
    Assert.invariant(endBlock != null)

    removeLeftPadding(state, input, delimIter)
    val foundStartBlock = parseStartBlock(state, input, startBlock, delimIter)
    val res = if (!foundStartBlock) {
      super.parse(state, input, field, delimIter, isDelimRequired)
    } else {
      parseRemainder(state, input, fieldEsc, startBlock, endBlock, delimIter, isDelimRequired)
    }
    state.dfaRegistersPool.finalCheck()

    res
  }

}
