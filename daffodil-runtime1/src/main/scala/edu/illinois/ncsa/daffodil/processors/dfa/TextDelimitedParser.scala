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

  def parse(input: DataInputStream, field: DFAField, delims: Seq[DFADelimiter], isDelimRequired: Boolean): Maybe[ParseResult] = {
    Assert.invariant(delims != null)
    Assert.invariant(field != null)

    val successes: ArrayBuffer[(DFADelimiter, Registers)] = ArrayBuffer.empty
    val fieldReg: Registers = TLRegistersPool.getFromPool()

    fieldReg.reset(input, delims) // Initialization

    var stillSearching: Boolean = true
    var stateNum: Int = 0 // initial state is 0
    var actionNum: Int = 0
    var beforeDelimiter: DataInputStream.MarkPos = DataInputStream.MarkPos.NoMarkPos
    while (stillSearching) {

      Assert.invariant(beforeDelimiter =:= DataInputStream.MarkPos.NoMarkPos)
      val dfaStatus = field.run(stateNum, fieldReg, actionNum)
      beforeDelimiter = input.markPos
      actionNum = 0
      dfaStatus.status match {
        case StateKind.EndOfData => stillSearching = false
        case StateKind.Failed => stillSearching = false
        case StateKind.Paused => {

          delims.foreach(d => { // Pick up where field left off
            input.resetPos(beforeDelimiter)
            beforeDelimiter = input.markPos
            val delimReg: Registers = TLRegistersPool.getFromPool()
            delimReg.reset(input, delims)
            val delimStatus = d.run(0, delimReg)
            delimStatus.status match {
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
            input.resetPos(beforeDelimiter) // reposition input to where we were trying to find a delimiter
            beforeDelimiter = DataInputStream.MarkPos.NoMarkPos
            actionNum = dfaStatus.actionNum + 1 // but force it to goto next rule so it won't just retry what it just did.
            stateNum = dfaStatus.currentStateNum
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
            val fieldNoPadding = trimByJustification(str)
            One(fieldNoPadding)
          }
          val totalNumCharsRead = fieldReg.numCharsReadUntilDelim
          One(new ParseResult(fieldValue, Nope, ""))
        }
      } else {
        val (dfa, r) = lm.get
        val fieldValue: Maybe[String] = {
          val str = fieldReg.resultString.toString
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

  protected def removeLeftPadding(input: DataInputStream, delims: Seq[DFADelimiter]): Unit = {
    justificationTrim match {
      case TextJustificationType.Center | TextJustificationType.Right if parsingPadChar.isDefined => {
        val leftPaddingRegister = TLRegistersPool.getFromPool()
        leftPaddingRegister.reset(input, delims)
        leftPadding.run(0, leftPaddingRegister)
        TLRegistersPool.returnToPool(leftPaddingRegister)
      }
      case _ => // No left padding
    }
  }

  protected def removeRightPadding(input: DataInputStream, delims: Seq[DFADelimiter]): Unit = {
    justificationTrim match {
      case TextJustificationType.Center | TextJustificationType.Left if parsingPadChar.isDefined => {
        val rightPaddingRegister = TLRegistersPool.getFromPool()
        rightPaddingRegister.reset(input, delims)
        rightPadding.run(0, rightPaddingRegister)
        TLRegistersPool.returnToPool(rightPaddingRegister)
      }
      case _ => // No right padding
    }
  }

  protected def parseStartBlock(input: DataInputStream, startBlock: DFADelimiter, delims: Seq[DFADelimiter]): Boolean = {
    val startBlockRegister = TLRegistersPool.getFromPool()
    startBlockRegister.reset(input, delims)

    val startStatus = startBlock.run(0, startBlockRegister) // find the block start, fail otherwise
    TLRegistersPool.returnToPool(startBlockRegister)
    startStatus.status match {
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
    delims: Seq[DFADelimiter], isDelimRequired: Boolean): Maybe[ParseResult] = {

    val successes: ArrayBuffer[(DFADelimiter, Registers)] = ArrayBuffer.empty

    val fieldRegister = TLRegistersPool.getFromPool()
    fieldRegister.reset(input, delims)

    var stillSearching: Boolean = true
    var stateNum: Int = 0 // initial state is 0
    var actionNum: Int = 0
    var foundBlockEnd: Boolean = false
    var beforeDelimiter: DataInputStream.MarkPos = DataInputStream.MarkPos.NoMarkPos
    while (stillSearching) {

      //Assert.invariant(beforeDelimiter =:= DataInputStream.MarkPos.NoMarkPos)
      val dfaStatus = fieldEsc.run(stateNum, fieldRegister, actionNum)
      beforeDelimiter = input.markPos // at this point the input is one past the end of the field. 
      actionNum = 0

      dfaStatus.status match {
        case StateKind.EndOfData => stillSearching = false
        case StateKind.Failed => stillSearching = false
        case StateKind.Paused => {
          // Pick up where field left off, we are looking for 
          // the blockEnd.
          val endBlockRegister = TLRegistersPool.getFromPool()
          endBlockRegister.reset(input, delims) // copy(fieldRegister) // TODO: This should just be a reset of the registers. No need to copy.

          val endBlockStatus = endBlock.run(0, endBlockRegister)
          TLRegistersPool.returnToPool(endBlockRegister)

          endBlockStatus.status match {
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

                val delimStatus = d.run(0, delimRegister)
                delimStatus.status match {
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
              actionNum = dfaStatus.actionNum + 1 // goto next rule
              stateNum = dfaStatus.currentStateNum
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
    delims: Seq[DFADelimiter], isDelimRequired: Boolean): Maybe[ParseResult] = {
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

  def getParser(state: PState): (TextDelimitedParserBase, Seq[DFADelimiter], List[String], DFAField, Maybe[EscapeSchemeParserHelper]) = constructParser(state)

}
