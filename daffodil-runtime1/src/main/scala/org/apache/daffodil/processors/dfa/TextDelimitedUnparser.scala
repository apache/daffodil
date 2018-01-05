/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.processors.unparsers.UnparseError
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.equality._
import org.apache.daffodil.io.DataInputStream
import org.apache.daffodil.util.MaybeChar
import org.apache.daffodil.processors.AllDelimiterIterator

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
class TextDelimitedUnparser(override val context: TermRuntimeData)
  extends DelimitedUnparser {

  lazy val name: String = "TextDelimitedUnparser"
  lazy val info: String = ""

  def escape(input: DataInputStream,
    field: DFAField,
    delims: Array[DFADelimiter],
    blockEndDFA: DFADelimiter,
    escapeEscapeChar: MaybeChar,
    blockStart: String,
    blockEnd: String,
    generateEscapeBlock: Boolean, state: UState): (String, Boolean) = {

    val (resultString, shouldGenerateEscapeBlock) = escapeBlock(input, field, delims, blockEndDFA, escapeEscapeChar, state)

    val result = if (generateEscapeBlock || shouldGenerateEscapeBlock) blockStart + resultString + blockEnd else resultString
    (result, shouldGenerateEscapeBlock)
  }

  /**
   * Performs escaping for escapeSchemeKind Block for unparsing.
   */
  final def escapeBlock(input: DataInputStream,
    field: DFAField,
    delims: Array[DFADelimiter],
    blockEnd: DFADelimiter,
    escapeEscapeChar: MaybeChar,
    state: UState): (String, Boolean) = {
    Assert.invariant(delims != null)
    Assert.invariant(field != null)

    val blockEndDelimIter = new AllDelimiterIterator(ArrayBuffer(blockEnd))
    val successes: ArrayBuffer[(DFADelimiter, Registers)] = ArrayBuffer.empty

    // We need to recognize the blockEnd in addition to the other pieces of
    // text we should escape
    //
    val fieldReg: Registers = TLRegistersPool.getFromPool("escapeBlock1")

    val fieldEscapesIter = {
      val ab = ArrayBuffer((blockEnd +: delims): _*)
      new AllDelimiterIterator(ab)
    }
    val delimIter = new AllDelimiterIterator(ArrayBuffer(delims: _*))

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
          val blockEndReg: Registers = TLRegistersPool.getFromPool("escapeBlock2")
          blockEndReg.reset(state, input, blockEndDelimIter)
          blockEnd.run(blockEndReg)
          val blockEndStatus = blockEndReg.status
          blockEndStatus match {
            case StateKind.Succeeded if (!escapeEscapeChar.isDefined) => {
              // Found an escapeEnd, which requires an escapeEscapeChar, but one was not provided
              beforeDelimiter = DataInputStream.MarkPos.NoMarkPos
              UnparseError(One(context.schemaFileLocation),
                One(state.currentLocation),
                "escapeEscapeCharacter was not defined but the escapeBlockEnd (%s) was present in the data.",
                blockEnd.lookingFor)
            }
            case StateKind.Succeeded => {
              // Found an escapeEnd, that means we must insert an escapeEscapeChar

              val afterBlockEnd = input.markPos // save position immediately after the blockEnd we found.
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
                val delimReg: Registers = TLRegistersPool.getFromPool("escapeBlock3")
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
                    TLRegistersPool.returnToPool(delimReg)
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
                fieldReg.appendToField(delim) // the delim just becomes field content, because we already had an escape block start.
                successes.foreach { case (d, r) => TLRegistersPool.returnToPool(r) }
                successes.clear
                shouldGenerateEscapeBlock = true
                fieldReg.actionNum = 0
                fieldReg.state = 0
              }
              // now go around the while loop again
            } // end case StateKind.Failed for finding the block end.
          } // end blockEndStatus.status match
          TLRegistersPool.returnToPool(blockEndReg)
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

    TLRegistersPool.returnToPool(fieldReg)
    TLRegistersPool.pool.finalCheck

    (resString, shouldGenerateEscapeBlock)
  }

  /**
   * Performs escaping appropriate when escapeSchemeKind is Character for
   * unparsing.
   */
  def escapeCharacter(input: DataInputStream,
    field: DFAField,
    delims: Array[DFADelimiter],
    hasEscCharAsDelimiter: Boolean,
    escapeChar: Char,
    escapeEscapeChar: MaybeChar, state: UState): (String, Boolean) = {
    Assert.invariant(delims != null)
    Assert.invariant(field != null)

    val successes: ArrayBuffer[(DFADelimiter, Registers)] = ArrayBuffer.empty
    val fieldReg: Registers = TLRegistersPool.getFromPool("escapeCharacter1")

    val delimIter = new AllDelimiterIterator(ArrayBuffer(delims: _*))

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
            val delimReg: Registers = TLRegistersPool.getFromPool("escapeCharacter2")
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
                TLRegistersPool.returnToPool(delimReg)
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
            if (matchedDelim.lookingFor.length() == 1 && matchedDelim.lookingFor(0) =#= escapeChar) {
              if (hasEscCharAsDelimiter) { fieldReg.appendToField(escapeChar) }
              else if (escapeEscapeChar.isDefined)
                fieldReg.appendToField(escapeEscapeChar.get)
              else
                UnparseError(One(context.schemaFileLocation), One(state.currentLocation), "escapeEscapeCharacter was not defined but the escapeCharacter (%s) was present in the data.", escapeChar)
            } else { fieldReg.appendToField(escapeChar) }

            val delim = matchedReg.delimString
            delim.foreach { fieldReg.appendToField(_) }

            // position the input stream after the winning (longest)
            // delimiter
            //
            input.resetPos(beforeDelimiter)
            Assert.invariant(input.skipChars(delim.length, state))
            fieldReg.resetChars(state)
            successes.foreach { case (d, r) => TLRegistersPool.returnToPool(r) }
            successes.clear
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

    TLRegistersPool.returnToPool(fieldReg)
    TLRegistersPool.pool.finalCheck

    (resString, escapeOccurred)
  }

}
