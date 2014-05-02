package edu.illinois.ncsa.daffodil.processors.dfa

import scala.util.parsing.input.Reader
import scala.collection.mutable.Queue
import edu.illinois.ncsa.daffodil.processors.DFDLCharReader
import edu.illinois.ncsa.daffodil.processors.TextJustificationType

trait HasLongestMatch {
  def longestMatch(matches: Seq[(DFADelimiter, Registers)]): Option[(DFADelimiter, Registers)] = {
    if (matches.isEmpty) return None
    val (minD, minR: Registers) = matches.minBy { case (d, r) => r.matchStartPos }

    val theFirstLongestMatch = {
      val minValue = matches.filter { case (d: DFADelimiter, r: Registers) => r.matchStartPos == minR.matchStartPos }
      minValue.maxBy { _._2.delimString.length }
    }
    Some(theFirstLongestMatch)
  }
}

abstract class TextDelimitedParserBase(val justification: TextJustificationType.Type, val padCharOpt: Option[Char], knownEncFunc: String => Int)
  extends Parser with HasLongestMatch {

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

  def delims: Seq[DFADelimiter]
  def field: DFAField

  def removeRightPadding(str: String): String = str.reverse.dropWhile(c => c == padCharOpt.get).reverse
  def removeLeftPadding(str: String): String = str.dropWhile(c => c == padCharOpt.get)
  def removePadding(str: String): String = removeLeftPadding(removeRightPadding(str))

  def parse(input: DFDLCharReader, isDelimRequired: Boolean): Option[ParseResult] = {
    val successes: Queue[(DFADelimiter, Registers)] = Queue.empty
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
      val fieldResult = field.run(stateNum, fieldReg, actionNum)
      actionNum = 0
      fieldResult match {
        case Right(num) => {
          stateNum = num
          stillSearching = false
        }
        case Left(dfaStatus) => {
          fieldResumingCharPos = initialCharPos + fieldReg.numCharsReadUntilDelim + 2 // + 2 for data0, data1
          dfaStatus.status match {
            case StateKind.EndOfData => stillSearching = false
            case StateKind.Failed => stillSearching = false
            case StateKind.Paused => {
              delims.foreach(d => { // Pick up where field left off
                val delimReg: Registers = new Registers
                delimReg.copy(fieldReg)
                d.run(0, delimReg) match {
                  case Right(num) => // Shouldn't happen?
                  case Left(delimStatus) => {
                    delimStatus.status match {
                      case StateKind.Succeeded => successes += (d -> delimReg)
                      case _ => {
                        // resume field parse
                        actionNum = dfaStatus.actionNum + 1 // goto next rule
                        stateNum = dfaStatus.currentStateNum
                      }
                    }
                  }
                }
              })
              if (!successes.isEmpty) { stillSearching = false }
            }
          }
        }
      }
    }

    val result = longestMatch(successes) match {
      case None if isDelimRequired => None
      case None => {
        val fieldValue: Option[String] = {
          val str = fieldReg.resultString.toString
          val fieldNoPadding = justification match {
            case TextJustificationType.None => str
            case TextJustificationType.Left => removeRightPadding(str)
            case TextJustificationType.Right => removeLeftPadding(str)
            case TextJustificationType.Center => removePadding(str)
          }
          Some(fieldNoPadding)
        }
        val totalNumCharsRead = fieldReg.numCharsReadUntilDelim
        val numBits: Int = knownEncFunc(fieldReg.charsReadUntilDelim.toString)
        val nextReader: DFDLCharReader = input.drop(totalNumCharsRead).asInstanceOf[DFDLCharReader]
        Some(new ParseResult(fieldValue, None, "", totalNumCharsRead, numBits, nextReader))
      }
      case Some((dfa, r)) => {
        val fieldValue: Option[String] = {
          val str = fieldReg.resultString.toString
          val fieldNoPadding = justification match {
            case TextJustificationType.None => str
            case TextJustificationType.Left => removeRightPadding(str)
            case TextJustificationType.Right => removeLeftPadding(str)
            case TextJustificationType.Center => removePadding(str)
          }
          Some(fieldNoPadding)
        }
        val delim: Option[String] = {
          Some(r.delimString.toString)
        }
        val lookingFor = dfa.lookingFor
        val totalNumCharsRead = fieldReg.numCharsReadUntilDelim
        val numBits: Int = knownEncFunc(fieldReg.charsReadUntilDelim.toString)
        val nextReader: DFDLCharReader = input.drop(totalNumCharsRead).asInstanceOf[DFDLCharReader]

        Some(new ParseResult(fieldValue, delim, lookingFor, totalNumCharsRead, numBits, nextReader))
      }
    }
    result
  }

}

/**
 * Assumes that the delims DFAs were constructed with the Esc
 * and EscEsc in mind.
 */
class TextDelimitedParser(justArg: TextJustificationType.Type, padCharArg: Option[Char],
  var delims: Seq[DFADelimiter],
  var field: DFAField, knownEncFunc: String => Int)
  extends TextDelimitedParserBase(justArg, padCharArg, knownEncFunc) {

}

/**
 * Assumes that endBlock DFA was constructed with the
 * EscEsc in mind.
 */
class TextDelimitedParserWithEscapeBlock(val justArg: TextJustificationType.Type, val padCharArg: Option[Char],
  var startBlock: DFADelimiter, var endBlock: DFADelimiter, var delims: Seq[DFADelimiter], var field: DFAField, knownEncFunc: String => Int)
  extends TextDelimitedParserBase(justArg, padCharArg, knownEncFunc) {

  override def parse(input: DFDLCharReader, isDelimRequired: Boolean): Option[ParseResult] = {
    val successes: Queue[(DFADelimiter, Registers)] = Queue.empty
    val leftPaddingRegister = new Registers

    val initialCharPos = input.characterPos

    val numCharsReadAfterLeftPadding = justification match {
      case TextJustificationType.Center | TextJustificationType.Right if padCharOpt.isDefined => {
        leftPaddingRegister.reset(input, 0)
        leftPadding.run(0, leftPaddingRegister)
        leftPaddingRegister.numCharsReadUntilDelim
      }
      case _ => 0 // No left padding
    }

    val readerAfterPadding = input.atCharPos(initialCharPos + numCharsReadAfterLeftPadding)

    val startBlockRegister = new Registers
    startBlockRegister.reset(readerAfterPadding, numCharsReadAfterLeftPadding)

    val startBlockResult = startBlock.run(0, startBlockRegister) // find the block start, fail otherwise
    startBlockResult match {
      case Right(num) => // Shouldn't happen
      case Left(startStatus) => {
        startStatus.status match {
          case StateKind.Succeeded => // continue
          case _ => return None // Failed
        }
      }
    }

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
      val fieldResult = field.run(stateNum, fieldRegister, actionNum)
      actionNum = 0
      fieldResult match {
        case Right(num) => {
          stateNum = num
          stillSearching = false
        }
        case Left(dfaStatus) => {
          fieldResumingCharPos = initialCharPos + numCharsReadAfterStartBlock + fieldRegister.numCharsReadUntilDelim + 2 // + 2 for data0, data1
          dfaStatus.status match {
            case StateKind.EndOfData => stillSearching = false
            case StateKind.Failed => stillSearching = false
            case StateKind.Paused => {
              // Pick up where field left off, we are looking for 
              // the blockEnd.
              endBlockRegister = new Registers
              endBlockRegister.copy(fieldRegister)

              endBlock.run(0, endBlockRegister) match {
                case Right(_) => // Shouldn't happen
                case Left(endBlockStatus) => {
                  endBlockStatus.status match {
                    case StateKind.Succeeded => {
                      // Found the unescaped block end, now we need to
                      // find any padding.
                      val prevRegister = justification match {
                        case TextJustificationType.Center | TextJustificationType.Left if padCharOpt.isDefined => {
                          rightPaddingRegister = new Registers
                          rightPaddingRegister.reset(input, initialCharPos + numCharsReadAfterStartBlock +
                            fieldRegister.numCharsReadUntilDelim + endBlockRegister.numCharsRead)
                          rightPadding.run(0, rightPaddingRegister)
                          rightPaddingRegister
                        }
                        case _ => endBlockRegister // No right padding
                      }

                      // Finally, we can look for the delimiter.
                      delims.foreach(d => { // Pick up where end of block/padding left off
                        val delimRegister = new Registers
                        delimRegister.copy(prevRegister)

                        d.run(0, delimRegister) match {
                          case Right(num) => // Shouldn't happen?
                          case Left(delimStatus) => {
                            delimStatus.status match {
                              case StateKind.Succeeded => successes += (d -> delimRegister)
                              case _ => {
                                // Not found
                              }
                            }
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

            }
          }
        }
      } // End Field Result
    } // End While
    val result = longestMatch(successes) match {
      case None if isDelimRequired => None
      case None => {
        val fieldValue: Option[String] = {
          Some(fieldRegister.resultString.toString)
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
        val numBits: Int = knownEncFunc(totalField)
        val nextReader: DFDLCharReader = input.drop(totalCharsRead).asInstanceOf[DFDLCharReader]
        Some(new ParseResult(fieldValue, None, "", totalCharsRead, numBits, nextReader))
      }
      case Some((dfa, r)) => {
        val fieldValue: Option[String] = {
          Some(fieldRegister.resultString.toString)
        }
        val delim: Option[String] = {
          Some(r.delimString.toString)
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
        val numBits: Int = knownEncFunc(totalField)
        val nextReader: DFDLCharReader = input.drop(totalNumCharsRead).asInstanceOf[DFDLCharReader]
        Some(new ParseResult(fieldValue, delim, lookingFor, totalNumCharsRead, numBits, nextReader))
      }
    }
    result
  }

}
