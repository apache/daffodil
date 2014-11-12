package edu.illinois.ncsa.daffodil.processors.dfa

import scala.collection.mutable.ArrayBuffer
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.processors.DFDLCharReader
import edu.illinois.ncsa.daffodil.processors.EscapeScheme
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeBlock
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeChar
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.processors.FieldFactoryStatic
import edu.illinois.ncsa.daffodil.processors.FieldFactoryDynamic
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.FieldFactoryBase
import edu.illinois.ncsa.daffodil.dsom.RuntimeEncodingMixin
import edu.illinois.ncsa.daffodil.processors.EncodingInfo
import edu.illinois.ncsa.daffodil.processors.RuntimeData

abstract class TextDelimitedParserBase(
  val justification: TextJustificationType.Type,
  val padCharOpt: Maybe[Char],
  override val context: RuntimeData,
  override val encodingInfo: EncodingInfo,
  delims: Seq[DFADelimiter],
  field: DFAField)
  extends DelimitedParser with RuntimeEncodingMixin with Serializable {

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

  def parse(input: DFDLCharReader, isDelimRequired: Boolean): Maybe[ParseResult] = {
    Assert.invariant(delims != null)
    Assert.invariant(field != null)
    val successes: ArrayBuffer[(DFADelimiter, Registers)] = ArrayBuffer.empty
    val fieldReg: Registers = new Registers

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
            val delimReg: Registers = new Registers
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
  encInfo: EncodingInfo,
  delims: Seq[DFADelimiter],
  field: DFAField)
  extends TextDelimitedParserBase(justArg, padCharArg, context, encInfo, delims, field) {
}

/**
 * Assumes that endBlock DFA was constructed with the
 * EscEsc in mind.
 */
class TextDelimitedParserWithEscapeBlock(
  justArg: TextJustificationType.Type,
  padCharArg: Maybe[Char],
  context: RuntimeData,
  encInfo: EncodingInfo,
  delims: Seq[DFADelimiter],
  field: DFAField,
  fieldEsc: DFAField,
  startBlock: DFADelimiter,
  endBlock: DFADelimiter)
  extends TextDelimitedParserBase(justArg, padCharArg, context, encInfo, delims, field) {

  protected def removeLeftPadding(input: DFDLCharReader): Registers = {
    val leftPaddingRegister = new Registers
    justification match {
      case TextJustificationType.Center | TextJustificationType.Right if padCharOpt.isDefined => {
        leftPaddingRegister.reset(input, 0)
        leftPadding.run(0, leftPaddingRegister)
      }
      case _ => // No left padding
    }
    leftPaddingRegister
  }

  protected def removeRightPadding(input: DFDLCharReader, prevRegister: Registers): Registers = {
    val rightPaddingRegister = justification match {
      case TextJustificationType.Center | TextJustificationType.Left if padCharOpt.isDefined => {
        val rightPaddingRegister = new Registers
        rightPaddingRegister.reset(input, 0)
        rightPadding.run(0, rightPaddingRegister)
        rightPaddingRegister
      }
      case _ => prevRegister // No right padding
    }
    rightPaddingRegister
  }

  protected def parseStartBlock(readerAfterPadding: DFDLCharReader): Maybe[Registers] = {
    val startBlockRegister = new Registers
    startBlockRegister.reset(readerAfterPadding, 0)

    val startStatus = startBlock.run(0, startBlockRegister) // find the block start, fail otherwise
    startStatus.status match {
      case StateKind.Succeeded => One(startBlockRegister) // continue
      case _ => Nope // Failed
    }
  }

  protected def parseRemainder(input: DFDLCharReader, isDelimRequired: Boolean,
    startBlockRegister: Registers, leftPaddingRegister: Registers,
    initialCharPos: Int, numCharsReadAfterLeftPadding: Int): Maybe[ParseResult] = {

    val successes: ArrayBuffer[(DFADelimiter, Registers)] = ArrayBuffer.empty
    val numCharsReadAfterStartBlock = numCharsReadAfterLeftPadding + startBlockRegister.numCharsRead

    val fieldRegister = new Registers
    fieldRegister.copy(startBlockRegister)

    var endBlockRegister = new Registers
    var rightPaddingRegister = new Registers
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
          endBlockRegister = new Registers
          endBlockRegister.copy(fieldRegister)

          val endBlockStatus = endBlock.run(0, endBlockRegister)
          endBlockStatus.status match {
            case StateKind.Succeeded => {
              // Found the unescaped block end, now we need to
              // find any padding.
              val prevRegister = this.removeRightPadding(input, endBlockRegister)

              // Finally, we can look for the delimiter.
              delims.foreach(d => { // Pick up where end of block/padding left off
                val delimRegister = new Registers
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

  override def parse(input: DFDLCharReader, isDelimRequired: Boolean): Maybe[ParseResult] = {
    Assert.invariant(delims != null)
    Assert.invariant(fieldEsc != null)
    Assert.invariant(field != null)
    Assert.invariant(startBlock != null)
    Assert.invariant(endBlock != null)

    val initialCharPos = input.characterPos
    val leftPaddingRegister = removeLeftPadding(input)
    val numCharsReadAfterLeftPadding = leftPaddingRegister.numCharsReadUntilDelim
    val readerAfterPadding = input.atCharPos(initialCharPos + numCharsReadAfterLeftPadding)
    val psb = parseStartBlock(readerAfterPadding)
    if (!psb.isDefined) super.parse(input, isDelimRequired)
    else {
      val startBlockRegister = psb.get
      parseRemainder(input, isDelimRequired,
        startBlockRegister, leftPaddingRegister, initialCharPos,
        numCharsReadAfterLeftPadding)
    }
  }

}

sealed abstract class TextDelimitedParserFactoryBase(
  justArg: TextJustificationType.Type,
  padCharArg: Maybe[Char],
  val encodingInfo: EncodingInfo,
  fieldFact: FieldFactoryBase,
  context: RuntimeData) extends Logging with Serializable {

  def getParser(state: PState): (PState, TextDelimitedParserBase, List[String])

  protected def constructParser(state: PState) = {
    val (postEvalState, delims, _, delimsCooked, fieldDFA, escScheme) = fieldFact.getFieldDFA(state)
    val theParser = if (escScheme.isDefined) {
      val scheme = escScheme.get
      scheme match {
        case s: EscapeSchemeBlock => {
          val parser =
            new TextDelimitedParserWithEscapeBlock(justArg, padCharArg, context,
              encodingInfo, delims,
              fieldDFA, s.fieldEscDFA, s.blockStartDFA, s.blockEndDFA)
          parser
        }
        case s: EscapeSchemeChar => {
          val parser = new TextDelimitedParser(justArg, padCharArg, context,
            encodingInfo, delims, fieldDFA)
          parser
        }
      }
    } else {
      val parser = new TextDelimitedParser(justArg, padCharArg, context, encodingInfo, delims, fieldDFA)
      parser
    }
    (postEvalState, theParser, delimsCooked)
  }

}

/**
 * Parser fields are set at compile-time.
 */
case class TextDelimitedParserFactoryStatic(
  justArg: TextJustificationType.Type,
  padCharArg: Maybe[Char],
  encInfo: EncodingInfo,
  fieldFact: FieldFactoryStatic,
  context: RuntimeData)
  extends TextDelimitedParserFactoryBase(
    justArg,
    padCharArg,
    encInfo,
    fieldFact,
    context) {

  /**
   * Parser is instantiated and the fields are set at
   * compile-time.
   */
  protected val (_, parser, delimsCooked) = constructParser(null)

  def getParser(state: PState): (PState, TextDelimitedParserBase, List[String]) = (state, parser, delimsCooked)

}

/**
 * The parser is instantiated but field setting is deferred until
 * run-time.
 */
case class TextDelimitedParserFactoryDynamic(
  justArg: TextJustificationType.Type,
  padCharArg: Maybe[Char],
  encInfo: EncodingInfo,
  fieldFact: FieldFactoryDynamic,
  context: RuntimeData)
  extends TextDelimitedParserFactoryBase(
    justArg,
    padCharArg,
    encInfo,
    fieldFact,
    context) {

  def getParser(state: PState): (PState, TextDelimitedParserBase, List[String]) = constructParser(state)

}
