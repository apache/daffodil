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
import edu.illinois.ncsa.daffodil.processors.DFDLCharReader
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeParserHelper
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.FieldFactoryBase
import edu.illinois.ncsa.daffodil.dsom.RuntimeEncodingMixin
import edu.illinois.ncsa.daffodil.processors.EncodingInfo
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeFactoryBase

abstract class TextDelimitedParserBase(
  val justification: TextJustificationType.Type,
  val padCharOpt: Maybe[Char],
  override val context: RuntimeData,
  override val encodingInfo: EncodingInfo)
  extends DelimitedParser {

  lazy val info: String = "justification='" + justification + "', padChar='" + padCharOpt.getOrElse("NONE") + "'"

  val leftPadding: DFADelimiter = {
    justification match {
      case TextJustificationType.Center | TextJustificationType.Right if padCharOpt.isDefined => CreatePaddingDFA(padCharOpt.get)
      case _ => null
    }
  }

  val rightPadding: DFADelimiter = {
    justification match {
      case TextJustificationType.Center | TextJustificationType.Left if padCharOpt.isDefined => CreatePaddingDFA(padCharOpt.get)
      case _ => null
    }
  }

  def removeRightPadding(str: String): String = str.reverse.dropWhile(c => c == padCharOpt.get).reverse
  def removeLeftPadding(str: String): String = str.dropWhile(c => c == padCharOpt.get)
  def removePadding(str: String): String = removeLeftPadding(removeRightPadding(str))

  def parse(input: DFDLCharReader, field: DFAField, delims: Seq[DFADelimiter], isDelimRequired: Boolean): Maybe[ParseResult] = {
    Assert.invariant(delims != null)
    Assert.invariant(field != null)

    val successes: ArrayBuffer[(DFADelimiter, Registers)] = ArrayBuffer.empty
    val fieldReg: Registers = new Registers(delims)

    fieldReg.reset(input, 0)

    val initialCharPos = input.characterPos

    var stillSearching: Boolean = true
    var stateNum: Int = 0 // initial state is 0
    var actionNum: Int = 0

    var fieldResumingCharPos: Int = -1

    while (stillSearching) {
      if (fieldResumingCharPos != -1) {
        val newReader = input.atCharPos(fieldResumingCharPos)
        fieldReg.setResume(newReader)
        fieldResumingCharPos = -1
      }
      val dfaStatus = field.run(stateNum, fieldReg, actionNum)
      actionNum = 0
      fieldResumingCharPos = initialCharPos + fieldReg.numCharsReadUntilDelim + 2 // + 2 for data0, data1
      dfaStatus.status match {
        case StateKind.EndOfData => stillSearching = false
        case StateKind.Failed => stillSearching = false
        case StateKind.Paused => {
          delims.foreach(d => { // Pick up where field left off
            val delimReg: Registers = new Registers(delims)
            delimReg.copy(fieldReg)
            val delimStatus = d.run(0, delimReg)
            delimStatus.status match {
              case StateKind.Succeeded => successes += (d -> delimReg)
              case _ => {
                // resume field parse
                actionNum = dfaStatus.actionNum + 1 // goto next rule
                stateNum = dfaStatus.currentStateNum
              }
            }
          })
          if (!successes.isEmpty) { stillSearching = false }
        }
      }
    }

    val lm = longestMatch(successes)
    val result = {
      if (!lm.isDefined) {
        if (isDelimRequired) Nope
        else {
          val fieldValue: Maybe[String] = {
            val str = fieldReg.resultString.toString
            val fieldNoPadding = justification match {
              case TextJustificationType.None => str
              case TextJustificationType.Left => removeRightPadding(str)
              case TextJustificationType.Right => removeLeftPadding(str)
              case TextJustificationType.Center => removePadding(str)
            }
            One(fieldNoPadding)
          }
          val totalNumCharsRead = fieldReg.numCharsReadUntilDelim
          val numBits: Int = knownEncodingStringBitLength(fieldReg.charsReadUntilDelim.toString)
          val nextReader: DFDLCharReader = input.drop(totalNumCharsRead).asInstanceOf[DFDLCharReader]
          One(new ParseResult(fieldValue, Nope, "", totalNumCharsRead, numBits, nextReader))
        }
      } else {
        val (dfa, r) = lm.get
        val fieldValue: Maybe[String] = {
          val str = fieldReg.resultString.toString
          val fieldNoPadding = justification match {
            case TextJustificationType.None => str
            case TextJustificationType.Left => removeRightPadding(str)
            case TextJustificationType.Right => removeLeftPadding(str)
            case TextJustificationType.Center => removePadding(str)
          }
          One(fieldNoPadding)
        }
        val delim: Maybe[String] = {
          One(r.delimString.toString)
        }
        val lookingFor = dfa.lookingFor
        val totalNumCharsRead = fieldReg.numCharsReadUntilDelim
        val numBits: Int = knownEncodingStringBitLength(fieldReg.charsReadUntilDelim.toString)
        val nextReader: DFDLCharReader = input.drop(totalNumCharsRead).asInstanceOf[DFDLCharReader]

        One(new ParseResult(fieldValue, delim, lookingFor, totalNumCharsRead, numBits, nextReader))
      }
    }
    result
  }

}

/**
 * Assumes that the delims DFAs were constructed with the Esc
 * and EscEsc in mind.
 */
class TextDelimitedParser(
  justArg: TextJustificationType.Type,
  padCharArg: Maybe[Char],
  context: RuntimeData,
  encInfo: EncodingInfo)
  extends TextDelimitedParserBase(justArg, padCharArg, context, encInfo) {

  lazy val name: String = "TextDelimitedParser"

}

/**
 * Assumes that endBlock DFA was constructed with the
 * EscEsc in mind.
 */
class TextDelimitedParserWithEscapeBlock(
  justArg: TextJustificationType.Type,
  padCharArg: Maybe[Char],
  context: RuntimeData,
  encInfo: EncodingInfo)
  extends TextDelimitedParserBase(justArg, padCharArg, context, encInfo) {
  
  lazy val name: String = "TextDelimitedParserWithEscapeBlock"

  protected def removeLeftPadding(input: DFDLCharReader, delims: Seq[DFADelimiter]): Registers = {
    val leftPaddingRegister = new Registers(delims)
    justification match {
      case TextJustificationType.Center | TextJustificationType.Right if padCharOpt.isDefined => {
        leftPaddingRegister.reset(input, 0)
        leftPadding.run(0, leftPaddingRegister)
      }
      case _ => // No left padding
    }
    leftPaddingRegister
  }

  protected def removeRightPadding(input: DFDLCharReader, prevRegister: Registers, delims: Seq[DFADelimiter]): Registers = {
    val rightPaddingRegister = justification match {
      case TextJustificationType.Center | TextJustificationType.Left if padCharOpt.isDefined => {
        val rightPaddingRegister = new Registers(delims)
        rightPaddingRegister.reset(input, 0)
        rightPadding.run(0, rightPaddingRegister)
        rightPaddingRegister
      }
      case _ => prevRegister // No right padding
    }
    rightPaddingRegister
  }

  protected def parseStartBlock(readerAfterPadding: DFDLCharReader, startBlock: DFADelimiter, delims: Seq[DFADelimiter]): Maybe[Registers] = {
    val startBlockRegister = new Registers(delims)
    startBlockRegister.reset(readerAfterPadding, 0)

    val startStatus = startBlock.run(0, startBlockRegister) // find the block start, fail otherwise
    startStatus.status match {
      case StateKind.Succeeded => One(startBlockRegister) // continue
      case _ => Nope // Failed
    }
  }

  protected def parseRemainder(input: DFDLCharReader,
    fieldEsc: DFAField,
    startBlock: DFADelimiter, endBlock: DFADelimiter,
    delims: Seq[DFADelimiter], isDelimRequired: Boolean,
    startBlockRegister: Registers, leftPaddingRegister: Registers,
    initialCharPos: Int, numCharsReadAfterLeftPadding: Int): Maybe[ParseResult] = {

    val successes: ArrayBuffer[(DFADelimiter, Registers)] = ArrayBuffer.empty
    val numCharsReadAfterStartBlock = numCharsReadAfterLeftPadding + startBlockRegister.numCharsRead

    val fieldRegister = new Registers(delims)
    fieldRegister.copy(startBlockRegister)

    var endBlockRegister = new Registers(delims)
    var rightPaddingRegister = new Registers(delims)
    var stillSearching: Boolean = true
    var stateNum: Int = 0 // initial state is 0
    var actionNum: Int = 0
    var fieldResumingCharPos: Int = -1

    while (stillSearching) {
      if (fieldResumingCharPos != -1) {
        val newReader = input.atCharPos(fieldResumingCharPos)
        fieldRegister.setResume(newReader)
        fieldResumingCharPos = -1
      }
      val dfaStatus = fieldEsc.run(stateNum, fieldRegister, actionNum)
      actionNum = 0

      fieldResumingCharPos = initialCharPos + numCharsReadAfterStartBlock + fieldRegister.numCharsReadUntilDelim + 2 // + 2 for data0, data1
      dfaStatus.status match {
        case StateKind.EndOfData => stillSearching = false
        case StateKind.Failed => stillSearching = false
        case StateKind.Paused => {
          // Pick up where field left off, we are looking for 
          // the blockEnd.
          endBlockRegister = new Registers(delims)
          endBlockRegister.copy(fieldRegister)

          val endBlockStatus = endBlock.run(0, endBlockRegister)
          endBlockStatus.status match {
            case StateKind.Succeeded => {
              // Found the unescaped block end, now we need to
              // find any padding.
              val prevRegister = this.removeRightPadding(input, endBlockRegister, delims)

              // Finally, we can look for the delimiter.
              delims.foreach(d => { // Pick up where end of block/padding left off
                val delimRegister = new Registers(delims)
                delimRegister.copy(prevRegister)

                val delimStatus = d.run(0, delimRegister)
                delimStatus.status match {
                  case StateKind.Succeeded => successes += (d -> delimRegister)
                  case _ => {
                    // Not found
                  }
                }
              })
              stillSearching = false
            }
            case _ => {
              // Failed, resume parse of field
              actionNum = dfaStatus.actionNum + 1 // goto next rule
              stateNum = dfaStatus.currentStateNum
            }
          }

        }
      }
    } // End While
    val lm = longestMatch(successes)
    val result = {
      if (!lm.isDefined) {
        if (isDelimRequired) Nope
        else {
          val fieldValue: Maybe[String] = {
            One(fieldRegister.resultString.toString)
          }
          val totalCharsRead = {
            startBlockRegister.numCharsRead +
              fieldRegister.numCharsReadUntilDelim + endBlockRegister.numCharsRead +
              {
                justification match {
                  case TextJustificationType.None => 0
                  case TextJustificationType.Left => rightPaddingRegister.numCharsRead
                  case TextJustificationType.Right => leftPaddingRegister.numCharsRead
                  case TextJustificationType.Center => rightPaddingRegister.numCharsRead + leftPaddingRegister.numCharsRead
                }
              }
          }
          val totalField = {
            startBlockRegister.delimString.toString +
              fieldRegister.charsReadUntilDelim.toString + endBlockRegister.delimString.toString +
              {
                justification match {
                  case TextJustificationType.None => ""
                  case TextJustificationType.Left => rightPaddingRegister.resultString.toString
                  case TextJustificationType.Right => leftPaddingRegister.resultString.toString
                  case TextJustificationType.Center => rightPaddingRegister.resultString.toString + leftPaddingRegister.resultString.toString
                }
              }
          }
          val numBits: Int = knownEncodingStringBitLength(totalField)
          val nextReader: DFDLCharReader = input.drop(totalCharsRead).asInstanceOf[DFDLCharReader]
          One(new ParseResult(fieldValue, Nope, "", totalCharsRead, numBits, nextReader))
        }
      } else {
        val (dfa, r) = lm.get
        val fieldValue: Maybe[String] = {
          One(fieldRegister.resultString.toString)
        }
        val delim: Maybe[String] = {
          One(r.delimString.toString)
        }
        val lookingFor = dfa.lookingFor
        val totalNumCharsRead = {
          startBlockRegister.numCharsRead +
            fieldRegister.numCharsReadUntilDelim + endBlockRegister.numCharsRead +
            {
              justification match {
                case TextJustificationType.None => 0
                case TextJustificationType.Left => rightPaddingRegister.numCharsRead
                case TextJustificationType.Right => leftPaddingRegister.numCharsRead
                case TextJustificationType.Center => rightPaddingRegister.numCharsRead + leftPaddingRegister.numCharsRead
              }
            }
        }
        val totalField = {
          startBlockRegister.delimString.toString +
            fieldRegister.charsReadUntilDelim.toString + endBlockRegister.delimString +
            {
              justification match {
                case TextJustificationType.None => ""
                case TextJustificationType.Left => rightPaddingRegister.resultString.toString
                case TextJustificationType.Right => leftPaddingRegister.resultString.toString
                case TextJustificationType.Center => rightPaddingRegister.resultString.toString + leftPaddingRegister.resultString.toString
              }
            }
        }
        val numBits: Int = knownEncodingStringBitLength(totalField)
        val nextReader: DFDLCharReader = input.drop(totalNumCharsRead).asInstanceOf[DFDLCharReader]
        One(new ParseResult(fieldValue, delim, lookingFor, totalNumCharsRead, numBits, nextReader))
      }
    }
    result
  }

  def parse(input: DFDLCharReader, field: DFAField, fieldEsc: DFAField,
    startBlock: DFADelimiter, endBlock: DFADelimiter,
    delims: Seq[DFADelimiter], isDelimRequired: Boolean): Maybe[ParseResult] = {
    Assert.invariant(delims != null)
    Assert.invariant(fieldEsc != null)
    Assert.invariant(field != null)
    Assert.invariant(startBlock != null)
    Assert.invariant(endBlock != null)

    val initialCharPos = input.characterPos
    val leftPaddingRegister = removeLeftPadding(input, delims)
    val numCharsReadAfterLeftPadding = leftPaddingRegister.numCharsReadUntilDelim
    val readerAfterPadding = input.atCharPos(initialCharPos + numCharsReadAfterLeftPadding)
    val psb = parseStartBlock(readerAfterPadding, startBlock, delims)
    if (!psb.isDefined) super.parse(input, field, delims, isDelimRequired)
    else {
      val startBlockRegister = psb.get
      parseRemainder(input, fieldEsc, startBlock, endBlock, delims, isDelimRequired,
        startBlockRegister, leftPaddingRegister, initialCharPos,
        numCharsReadAfterLeftPadding)
    }
  }

}

/**
 * The parser is instantiated but field setting is deferred until
 * run-time.
 */
case class TextDelimitedParserFactory(
  justArg: TextJustificationType.Type,
  padCharArg: Maybe[Char],
  encInfo: EncodingInfo,
  fieldFact: FieldFactoryBase,
  escapeSchemeFact: Option[EscapeSchemeFactoryBase],
  context: RuntimeData)
  extends Logging with Serializable {
  
  val preConstructedParser = {
    if (escapeSchemeFact.isDefined) {
      val scheme = escapeSchemeFact.get
      scheme.escapeKind match {
        case EscapeKind.EscapeBlock => {
          val parser =
            new TextDelimitedParserWithEscapeBlock(justArg, padCharArg, context,
              encInfo)
          parser
        }
        case EscapeKind.EscapeCharacter => {
          val parser = new TextDelimitedParser(justArg, padCharArg, context,
            encInfo)
          parser
        }
      }
    } else { new TextDelimitedParser(justArg, padCharArg, context, encInfo) }
  }

  protected def constructParser(state: PState) = {
    val (postEvalState, delims, delimsCooked, fieldDFA, scheme) = fieldFact.getFieldDFA(state)

    (postEvalState, preConstructedParser, delims, delimsCooked, fieldDFA, scheme)
  }

  def getParser(state: PState): (PState, TextDelimitedParserBase, Seq[DFADelimiter], List[String], DFAField, Maybe[EscapeSchemeParserHelper]) = constructParser(state)

}
