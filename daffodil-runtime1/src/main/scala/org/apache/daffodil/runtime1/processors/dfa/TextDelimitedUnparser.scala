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

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

import org.apache.daffodil.io.DataInputStream
import org.apache.daffodil.lib.equality._
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.lib.util.MaybeChar
import org.apache.daffodil.runtime1.processors.AllDelimiterIterator
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.unparsers.UState
import org.apache.daffodil.runtime1.processors.unparsers.UnparseError

/**
 * When 'escapeCharacter': On unparsing a single character of the data
 * is escaped by adding an dfdl:escapeCharacter before it. The following
 * are escaped if they are in the data:
 *
 * - Any in-scope terminating delimiter by escaping its first
 * character.
 * - dfdl:escapeCharacter (escaped by
 * dfdl:escapeEscapeCharacter)
 * - Any dfdl:extraEscapedCharacters
 *
 * When 'escapeBlock': On unparsing the entire data are escaped by
 * adding dfdl:escapeBlockStart to the beginning and
 * dfdl:escapeBlockEnd to the end of the data. The data is either always
 * escaped or escaped when needed as specified by
 * dfdl:generateEscapeBlock. If the data is escaped and contains the
 * dfdl:escapeBlockEnd then first character of each appearance of the
 * dfdl:escapeBlockEnd is escaped by the dfdl:escapeEscapeCharacter.
 */
class TextDelimitedUnparser(override val context: TermRuntimeData) extends DelimitedUnparser {

  lazy val name: String = "TextDelimitedUnparser"
  lazy val info: String = ""

  def escape(
    input: DataInputStream,
    field: DFAField,
    delims: Array[DFADelimiter],
    blockEndDFA: DFADelimiter,
    escapeEscapeChar: MaybeChar,
    blockStart: String,
    blockEnd: String,
    generateEscapeBlock: Boolean,
    state: UState
  ): (String, Boolean) = {

    val (resultString, shouldGenerateEscapeBlock) =
      escapeBlock(input, field, delims, blockEndDFA, escapeEscapeChar, state)

    val result =
      if (generateEscapeBlock || shouldGenerateEscapeBlock) blockStart + resultString + blockEnd
      else resultString
    (result, shouldGenerateEscapeBlock)
  }

  /**
   * Performs escaping for escapeSchemeKind Block for unparsing.
   */
  final def escapeBlock(
    input: DataInputStream,
    field: DFAField,
    delims: Array[DFADelimiter],
    blockEnd: DFADelimiter,
    escapeEscapeChar: MaybeChar,
    state: UState
  ): (String, Boolean) = {
    Assert.invariant(delims != null)
    Assert.invariant(field != null)

    val blockEndDelimIter = new AllDelimiterIterator(ArrayBuffer(blockEnd))
    val successes: ArrayBuffer[(DFADelimiter, Registers)] = ArrayBuffer.empty

    // We need to recognize the blockEnd in addition to the other pieces of
    // text we should escape
    //
    val fieldReg: Registers = state.dfaRegistersPool.getFromPool("escapeBlock1")

    val fieldEscapesIter = {
      val ab = ArrayBuffer(ArraySeq.unsafeWrapArray(blockEnd +: delims): _*)
      new AllDelimiterIterator(ab)
    }
    val delimIter = new AllDelimiterIterator(ArrayBuffer(ArraySeq.unsafeWrapArray(delims): _*))

    fieldReg.reset(state, input, fieldEscapesIter)

    var stillSearching: Boolean = true
    var numCharsInserted: Int = 0

    var shouldGenerateEscapeBlock: Boolean = false
    var beforeDelimiter: DataInputStream.MarkPos = DataInputStream.MarkPos.NoMarkPos

    while (stillSearching) {

      // We want to examine each character and if it's not part of a
      // delimiter append it to the 'field' member.  If it is part of
      // a delimiter we want to perform a longest match.  We then
      // append the 'escape' character to the 'field' member followed
      // by the matched delimiter.  We then start the process again
      // starting with the character following that of the matched
      // delimiter until we reach end of data.
      //
      Assert.invariant(beforeDelimiter =#= DataInputStream.MarkPos.NoMarkPos)
      field.run(fieldReg)
      val dfaStatus = fieldReg.status
      fieldReg.actionNum = 0 // unnecessary?
      beforeDelimiter = input.markPos

      dfaStatus match {
        case StateKind.EndOfData => stillSearching = false
        case StateKind.Failed => stillSearching = false
        case StateKind.Paused => {
          // If we got here, that means we found a character that could be the
          // beginning of a delimiter. This could be many different things
          // (parent separator, block end, etc), so we must figure that out

          // We check for a blockEnd first, if it exists then we MUST
          // generate an escape block
          val blockEndReg: Registers = state.dfaRegistersPool.getFromPool("escapeBlock2")
          blockEndReg.reset(state, input, blockEndDelimIter)
          blockEnd.run(blockEndReg)
          val blockEndStatus = blockEndReg.status
          blockEndStatus match {
            case StateKind.Succeeded if (!escapeEscapeChar.isDefined) => {
              // Found an escapeEnd, which requires an escapeEscapeChar, but one was not provided
              beforeDelimiter = DataInputStream.MarkPos.NoMarkPos
              UnparseError(
                One(context.schemaFileLocation),
                One(state.currentLocation),
                "escapeEscapeCharacter was not defined but the escapeBlockEnd (%s) was present in the data.",
                blockEnd.lookingFor
              )
            }
            case StateKind.Succeeded => {
              // Found an escapeEnd, that means we must insert an escapeEscapeChar

              val afterBlockEnd =
                input.markPos // save position immediately after the blockEnd we found.
              //
              // note. The appendToField code assumes that a character needs to be read from
              // the input. However, the input has already been advanced past the blockEnd
              // (In call to blockEnd.run above.
              //
              // TODO: scrutinize DFA code. Why does appendToField call commitOneChar anyway?
              // It may not need to do that anymore, and that would allow us to get rid of the
              // input.markPos above, and input.resetPos below.
              //
              fieldReg.appendToField(escapeEscapeChar.get)
              numCharsInserted += 1 // this is how many escape characters we've inserted to escape the delims/blockEnds in the data
              val blockEnd = blockEndReg.delimString
              fieldReg.appendToField(blockEnd)
              input.resetPos(afterBlockEnd) // we want to resume scanning after the blockEnd.
              shouldGenerateEscapeBlock = true
              beforeDelimiter = DataInputStream.MarkPos.NoMarkPos
              fieldReg.actionNum = 0
              fieldReg.state = 0
              // now go around the while loop again
            }
            case _ => {
              // We did not find a block end, so check for the other pieces
              // of text we should generate an escape block for (e.g. separators, terminators)
              delimIter.reset()
              while (delimIter.hasNext()) {
                val d = delimIter.next()
                val delimReg: Registers = state.dfaRegistersPool.getFromPool("escapeBlock3")
                input.resetPos(beforeDelimiter)
                beforeDelimiter = input.markPos
                delimReg.reset(state, input, delimIter)
                d.run(delimReg)
                val delimStatus = delimReg.status
                delimStatus match {
                  case StateKind.Succeeded => {
                    // found a matching delmiter that may need escaping. It is
                    // possible that there is another delimiter that is a
                    // longer match or is matched earlier, so add it to a list
                    // and we will determine that later.
                    successes += (d -> delimReg)
                  }
                  case _ => {
                    // this delim did not match, ignore it and discard its register
                    state.dfaRegistersPool.returnToPool(delimReg)
                  }
                }
              }
              input.resetPos(beforeDelimiter)
              beforeDelimiter = DataInputStream.MarkPos.NoMarkPos
              fieldReg.resetChars(state)
              if (successes.isEmpty) {
                // did not match any delimiters, go to the next rule in the
                // field DFA, effectively resuming the field parse. This is possible
                // if the field.run() call found a character that could
                // potentially start a delimiter, but it ended up not matching
                // any delimiters.
                fieldReg.actionNum = fieldReg.actionNum + 1
              } else {
                // matched a delimiter, need to handle escaping it
                val (_, matchedReg) = longestMatch(successes).get
                val delim = matchedReg.delimString
                fieldReg.appendToField(
                  delim
                ) // the delim just becomes field content, because we already had an escape block start.
                successes.foreach { case (d, r) => state.dfaRegistersPool.returnToPool(r) }
                successes.clear()
                shouldGenerateEscapeBlock = true
                fieldReg.actionNum = 0
                fieldReg.state = 0
              }
              // now go around the while loop again
            } // end case StateKind.Failed for finding the block end.
          } // end blockEndStatus.status match
          state.dfaRegistersPool.returnToPool(blockEndReg)
        } // end case StateKind.Paused for finding any of block end or a delimiter
      } // end dfaStatus match
    } // end while stillSearching
    //
    // The only way we drop out here is if stillSearching is false.
    // That happens on end of data (end of the infoset string.. we're unparsing here)
    // It means we're done.
    //
    // No need to now advance the input, because we're unparsing, and we're done
    // so this input is going to be discarded since it existed only to enable
    // us to reuse the DFA for determining when to escape data while unparsing.
    val resString = fieldReg.resultString.toString

    state.dfaRegistersPool.returnToPool(fieldReg)
    state.dfaRegistersPool.finalCheck()

    (resString, shouldGenerateEscapeBlock)
  }

  /**
   * Performs escaping appropriate when escapeSchemeKind is Character for
   * unparsing.
   */
  def escapeCharacter(
    input: DataInputStream,
    field: DFAField,
    delims: Array[DFADelimiter],
    hasEscCharAsDelimiter: Boolean,
    escapeChar: Char,
    escapeEscapeChar: MaybeChar,
    state: UState
  ): (String, Boolean) = {
    Assert.invariant(delims != null)
    Assert.invariant(field != null)
    if (hasEscCharAsDelimiter)
      UnparseError(
        One(context.schemaFileLocation),
        One(state.currentLocation),
        "The dfdl:terminator and dfdl:separator may not begin with the dfdl:escapeCharacter: '%s'.",
        escapeChar
      )

    val successes: ArrayBuffer[(DFADelimiter, Registers)] = ArrayBuffer.empty
    val fieldReg: Registers = state.dfaRegistersPool.getFromPool("escapeCharacter1")

    val delimIter = new AllDelimiterIterator(ArrayBuffer(ArraySeq.unsafeWrapArray(delims): _*))

    fieldReg.reset(state, input, delimIter)

    var stillSearching: Boolean = true
    fieldReg.state = 0 // initial state is 0
    var numCharsInserted: Int = 0
    var escapeOccurred: Boolean = false
    var beforeDelimiter: DataInputStream.MarkPos = DataInputStream.MarkPos.NoMarkPos

    while (stillSearching) {

      // We want to examine each character and if it's not part of a
      // delimiter append it to the 'field' member.  If it is part of
      // a delimiter we want to perform a longest match.  We then
      // append the 'escape' character to the 'field' member followed
      // by the matched delimiter.  We then start the process again
      // starting with the character following that of the matched
      // delimiter until we reach end of data.
      //
      field.run(fieldReg)
      val dfaStatus = fieldReg.status
      beforeDelimiter = input.markPos

      fieldReg.actionNum = 0

      dfaStatus match {
        case StateKind.EndOfData => stillSearching = false
        case StateKind.Failed => stillSearching = false
        case StateKind.Paused => {
          // If we got here, that means we found a character that could be the
          // beginning of a delimiter. So we must search through the delimiters
          // and see if any match
          delimIter.reset()
          while (delimIter.hasNext()) {
            val d = delimIter.next()
            val delimReg: Registers = state.dfaRegistersPool.getFromPool("escapeCharacter2")
            delimReg.reset(state, input, delimIter)
            input.resetPos(beforeDelimiter)
            beforeDelimiter = input.markPos
            d.run(delimReg)
            val delimStatus = delimReg.status
            delimStatus match {
              case StateKind.Succeeded => {
                // found a matching delmiter that we may need to escape. It is
                // possible that there is another delimiter that is a
                // longer match or is matched earlier, so add it to a list
                // and we will determine that later.
                successes += (d -> delimReg)
              }
              case _ => {
                // this delim did not match, ignore it and discard its register
                state.dfaRegistersPool.returnToPool(delimReg)
                input.resetPos(beforeDelimiter)
              }
            }
          }

          if (successes.isEmpty) {
            // did not match any delimiters, go to the next rule in the
            // field, DFA effectively resuming the field parse. This is possible
            // if the field.run() call found a character that could
            // potentially start a delimiter, but it ended up not matching
            // any delimiters.
            fieldReg.actionNum = fieldReg.actionNum + 1
          } else {
            // matched a delimiter, need to handle escaping it
            val (matchedDelim, matchedReg) = longestMatch(successes).get
            if (
              matchedDelim.lookingFor.length() == 1 && matchedDelim.lookingFor(0) =#= escapeChar
            ) {
              if (escapeEscapeChar.isDefined)
                fieldReg.appendToField(escapeEscapeChar.get)
              else
                UnparseError(
                  One(context.schemaFileLocation),
                  One(state.currentLocation),
                  "escapeEscapeCharacter was not defined but the escapeCharacter (%s) was present in the data.",
                  escapeChar
                )
            } else { fieldReg.appendToField(escapeChar) }

            val delim = matchedReg.delimString
            delim.foreach { fieldReg.appendToField(_) }

            // position the input stream after the winning (longest)
            // delimiter
            //
            input.resetPos(beforeDelimiter)
            Assert.invariant(input.skipChars(delim.length, state))
            fieldReg.resetChars(state)
            successes.foreach { case (d, r) => state.dfaRegistersPool.returnToPool(r) }
            successes.clear()
            stillSearching = true

            escapeOccurred = true

            numCharsInserted += 1

            // resume field parse

            fieldReg.actionNum = 0
            fieldReg.state = 0

          }
        }
      }
    }
    // No need to now advance the input, because we're unparsing, and we're done
    // so this input is going to be discarded since it existed only to enable
    // us to reuse the DFA for determining when to escape data while unparsing.
    val resString = fieldReg.resultString.toString

    state.dfaRegistersPool.returnToPool(fieldReg)
    state.dfaRegistersPool.finalCheck()

    (resString, escapeOccurred)
  }

}
