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

package edu.illinois.ncsa.daffodil.processors.dfa

import scala.collection.mutable.ArrayBuffer
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeParserHelper
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.FieldFactoryBase
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeFactoryBase
import edu.illinois.ncsa.daffodil.processors.parsers.PaddingRuntimeMixin
import edu.illinois.ncsa.daffodil.io.DataInputStream
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.util.MaybeChar

abstract class TextDelimitedParserBase(
  override val justificationTrim: TextJustificationType.Type,
  override val parsingPadChar: MaybeChar,
  override val context: TermRuntimeData)
  extends DelimitedParser with PaddingRuntimeMixin {

  private lazy val padCharInfo = if (parsingPadChar.isDefined) parsingPadChar.toString else "NONE"
  lazy val info: String = "justification='" + justificationTrim + "', padChar='" + padCharInfo + "'"

  final def parse(input: DataInputStream, field: DFAField, delims: Array[DFADelimiter], isDelimRequired: Boolean): Maybe[ParseResult] = {
    Assert.invariant(delims != null)
    Assert.invariant(field != null)

    val successes: ArrayBuffer[(DFADelimiter, Registers)] = ArrayBuffer.empty
    val fieldReg: Registers = TLRegistersPool.getFromPool()

    fieldReg.reset(input, delims) // Initialization

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

          delims.foreach(d => { // Pick up where field left off
            input.resetPos(beforeDelimiter)
            beforeDelimiter = input.markPos
            val delimReg: Registers = TLRegistersPool.getFromPool()
            delimReg.reset(input, delims)
            d.run(delimReg)
            delimReg.status match {
              case StateKind.Succeeded => successes += (d -> delimReg)
              case _ => {
                // nothing. Just try the other delims
                TLRegistersPool.returnToPool(delimReg)
              }
            }
          })
          if (!successes.isEmpty) { stillSearching = false }
          else {
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
            input.resetPos(beforeDelimiter) // reposition input to where we were trying to find a delimiter (but did not)
            beforeDelimiter = DataInputStream.MarkPos.NoMarkPos
            fieldReg.actionNum = fieldReg.actionNum + 1 // but force it to goto next rule so it won't just retry what it just did.
            stillSearching = true
          }
        }
      }
    }
    Assert.invariant(beforeDelimiter != DataInputStream.MarkPos.NoMarkPos)
    input.resetPos(beforeDelimiter)
    val lm = longestMatch(successes)
    val result = {
      if (!lm.isDefined) {
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
          // val totalNumCharsRead = fieldReg.numCharsReadUntilDelim
          One(new ParseResult(fieldValue, Nope, ""))
        }
      } else {
        val (dfa, r) = lm.get
        // TODO: Performance - Use of a tuple here implies allocation when longestMatch is called.
        // Try to avoid an allocated structure here. Likely it can be part of the PState.
        val fieldValue: Maybe[String] = {
          val str = fieldReg.resultString.toString // TODO: Performance see above.
          val fieldNoPadding = trimByJustification(str)
          One(fieldNoPadding)
        }
        val delim: Maybe[String] = {
          One(r.delimString.toString)
        }
        val lookingFor = dfa.lookingFor
        //        val totalNumCharsRead = fieldReg.numCharsReadUntilDelim
        //        input.skipChars(totalNumCharsRead)
        One(new ParseResult(fieldValue, delim, lookingFor))
      }
    }

    successes.foreach { case (d, r) => TLRegistersPool.returnToPool(r) }
    TLRegistersPool.returnToPool(fieldReg)
    TLRegistersPool.pool.finalCheck

    result
  }

}

/**
 * Assumes that the delims DFAs were constructed with the Esc
 * and EscEsc in mind.
 */
class TextDelimitedParser(
  justArg: TextJustificationType.Type,
  padCharArg: MaybeChar,
  context: TermRuntimeData)
  extends TextDelimitedParserBase(justArg, padCharArg, context) {

  lazy val name: String = "TextDelimitedParser"

}

/**
 * Assumes that endBlock DFA was constructed with the
 * EscEsc in mind.
 */
class TextDelimitedParserWithEscapeBlock(
  justArg: TextJustificationType.Type,
  padCharArg: MaybeChar,
  context: TermRuntimeData)
  extends TextDelimitedParserBase(justArg, padCharArg, context) {

  lazy val name: String = "TextDelimitedParserWithEscapeBlock"

  val leftPadding: DFADelimiter = {
    justificationTrim match {
      case TextJustificationType.Center | TextJustificationType.Right if parsingPadChar.isDefined => CreatePaddingDFA(parsingPadChar.get)
      case _ => null
    }
  }

  val rightPadding: DFADelimiter = {
    justificationTrim match {
      case TextJustificationType.Center | TextJustificationType.Left if parsingPadChar.isDefined => CreatePaddingDFA(parsingPadChar.get)
      case _ => null
    }
  }

  protected def removeLeftPadding(input: DataInputStream, delims: Array[DFADelimiter]): Unit = {
    justificationTrim match {
      case TextJustificationType.Center | TextJustificationType.Right if parsingPadChar.isDefined => {
        val leftPaddingRegister = TLRegistersPool.getFromPool()
        leftPaddingRegister.reset(input, delims)
        leftPadding.run(leftPaddingRegister)
        TLRegistersPool.returnToPool(leftPaddingRegister)
      }
      case _ => // No left padding
    }
  }

  protected def removeRightPadding(input: DataInputStream, delims: Array[DFADelimiter]): Unit = {
    justificationTrim match {
      case TextJustificationType.Center | TextJustificationType.Left if parsingPadChar.isDefined => {
        val rightPaddingRegister = TLRegistersPool.getFromPool()
        rightPaddingRegister.reset(input, delims)
        rightPadding.run(rightPaddingRegister)
        TLRegistersPool.returnToPool(rightPaddingRegister)
      }
      case _ => // No right padding
    }
  }

  protected def parseStartBlock(input: DataInputStream, startBlock: DFADelimiter, delims: Array[DFADelimiter]): Boolean = {
    val startBlockRegister = TLRegistersPool.getFromPool()
    startBlockRegister.reset(input, delims)

    startBlock.run(startBlockRegister) // find the block start, fail otherwise
    val startStatus = startBlockRegister.status
    TLRegistersPool.returnToPool(startBlockRegister)
    startStatus match {
      case StateKind.Succeeded => true // continue
      case _ => false // Failed
    }
  }

  /**
   * Called to parse the rest of the field until we reach a block end, but
   * beyond that, after we reach a block-end out until we reach the delimiter.
   */
  protected def parseRemainder(input: DataInputStream,
    fieldEsc: DFAField,
    startBlock: DFADelimiter, endBlock: DFADelimiter,
    delims: Array[DFADelimiter], isDelimRequired: Boolean): Maybe[ParseResult] = {

    val successes: ArrayBuffer[(DFADelimiter, Registers)] = ArrayBuffer.empty

    val fieldRegister = TLRegistersPool.getFromPool()
    fieldRegister.reset(input, delims)

    var stillSearching: Boolean = true
    var foundBlockEnd: Boolean = false
    var beforeDelimiter: DataInputStream.MarkPos = DataInputStream.MarkPos.NoMarkPos
    while (stillSearching) {

      //Assert.invariant(beforeDelimiter =:= DataInputStream.MarkPos.NoMarkPos)
      fieldEsc.run(fieldRegister)
      val dfaStatus = fieldRegister.status
      beforeDelimiter = input.markPos // at this point the input is one past the end of the field.
      fieldRegister.actionNum = 0

      dfaStatus match {
        case StateKind.EndOfData => stillSearching = false
        case StateKind.Failed => stillSearching = false
        case StateKind.Paused => {
          // Pick up where field left off, we are looking for
          // the blockEnd.
          val endBlockRegister = TLRegistersPool.getFromPool()
          endBlockRegister.reset(input, delims) // copy(fieldRegister) // TODO: This should just be a reset of the registers. No need to copy.

          endBlock.run(endBlockRegister)
          val endBlockStatus = endBlockRegister.status
          TLRegistersPool.returnToPool(endBlockRegister)

          endBlockStatus match {
            case StateKind.Succeeded => {
              // Found the unescaped block end, now we need to
              // find any padding.
              this.removeRightPadding(input, delims)
              beforeDelimiter = input.markPos

              // Finally, we can look for the delimiter.
              delims.foreach(d => { // Pick up where end of block/padding left off
                val delimRegister = TLRegistersPool.getFromPool()
                input.resetPos(beforeDelimiter)
                beforeDelimiter = input.markPos
                delimRegister.reset(input, delims)

                d.run(delimRegister)
                val delimStatus = delimRegister.status
                delimStatus match {
                  case StateKind.Succeeded => successes += (d -> delimRegister)
                  case _ => {
                    // No delimiter found
                    TLRegistersPool.returnToPool(delimRegister)
                  }
                }
              })
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
              fieldRegister.resetChars

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
    //Assert.invariant(beforeDelimiter !=:= DataInputStream.MarkPos.NoMarkPos)
    input.resetPos(beforeDelimiter)
    val lm = longestMatch(successes)
    val result = {
      if (!lm.isDefined) {
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
          One(new ParseResult(fieldValue, Nope, ""))
        }
      } else {
        //
        // A delimiter was found
        //
        //Assert.invariant(lm.isDefined)
        val (dfa, r) = lm.get
        val fieldValue: Maybe[String] = {
          One(fieldRegister.resultString.toString)
        }
        val delim: Maybe[String] = {
          One(r.delimString.toString)
        }
        val lookingFor = dfa.lookingFor
        //
        // Note: we have not consumed the delimiter from
        // the input. That's an invariant. When a parser ends,
        // any delimiter is NOT consumed. It gets consumed
        // later when the Delimiter parser consumes it (normally
        // by recognizing it in the foundDelimiter 'cache' slot.
        //
        One(new ParseResult(fieldValue, delim, lookingFor))
      }
    }
    successes.foreach { case (d, r) => TLRegistersPool.returnToPool(r) }
    TLRegistersPool.returnToPool(fieldRegister)
    result
  }

  def parse(input: DataInputStream, field: DFAField, fieldEsc: DFAField,
    startBlock: DFADelimiter, endBlock: DFADelimiter,
    delims: Array[DFADelimiter], isDelimRequired: Boolean): Maybe[ParseResult] = {
    //Assert.invariant(delims != null)
    //Assert.invariant(fieldEsc != null)
    //Assert.invariant(field != null)
    //Assert.invariant(startBlock != null)
    //Assert.invariant(endBlock != null)

    removeLeftPadding(input, delims)
    val foundStartBlock = parseStartBlock(input, startBlock, delims)
    val res = if (!foundStartBlock) {
      super.parse(input, field, delims, isDelimRequired)
    } else {
      parseRemainder(input, fieldEsc, startBlock, endBlock, delims, isDelimRequired)
    }
    TLRegistersPool.pool.finalCheck

    res
  }

}

/**
 * The parser is instantiated but field setting is deferred until
 * run-time.
 */
case class TextDelimitedParserFactory(
  justArg: TextJustificationType.Type,
  parsingPadChar: MaybeChar,
  fieldFact: FieldFactoryBase,
  escapeSchemeFact: Option[EscapeSchemeFactoryBase],
  context: TermRuntimeData)
  extends Logging with Serializable {

  val preConstructedParser = {
    if (escapeSchemeFact.isDefined) {
      val scheme = escapeSchemeFact.get
      scheme.escapeKind match {
        case EscapeKind.EscapeBlock => {
          val parser =
            new TextDelimitedParserWithEscapeBlock(justArg, parsingPadChar, context)
          parser
        }
        case EscapeKind.EscapeCharacter => {
          val parser = new TextDelimitedParser(justArg, parsingPadChar, context)
          parser
        }
      }
    } else { new TextDelimitedParser(justArg, parsingPadChar, context) }
  }

  protected def constructParser(state: PState) = {
    val (delims, delimsCooked, fieldDFA, scheme) = fieldFact.getFieldDFA(state)

    (preConstructedParser, delims, delimsCooked, fieldDFA, scheme)
  }

  def getParser(state: PState): (TextDelimitedParserBase, Array[DFADelimiter], List[String], DFAField, Maybe[EscapeSchemeParserHelper]) = constructParser(state)

}
