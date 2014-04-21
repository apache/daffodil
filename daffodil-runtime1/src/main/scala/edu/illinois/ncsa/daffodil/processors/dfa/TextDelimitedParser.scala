package edu.illinois.ncsa.daffodil.processors.dfa

import scala.util.parsing.input.Reader
import scala.collection.mutable.Queue
import edu.illinois.ncsa.daffodil.processors.DFDLCharReader
import edu.illinois.ncsa.daffodil.processors.TextJustificationType

abstract class TextDelimitedParserBase(val justification: TextJustificationType.Type, val padCharOpt: Option[Char], knownEncFunc: String => Int)
  extends Parser {

  val leftPadding: DFADelimiter = {
    justification match {
      case TextJustificationType.Center | TextJustificationType.Right if padCharOpt.isDefined => CreatePaddingDFA(padCharOpt.get, new Registers())
      case _ => null
    }
  }

  val rightPadding: DFADelimiter = {
    justification match {
      case TextJustificationType.Center | TextJustificationType.Left if padCharOpt.isDefined => CreatePaddingDFA(padCharOpt.get, new Registers())
      case _ => null
    }
  }

  def delims: Seq[DFADelimiter]
  def field: DFAField

  def longestMatch(matches: Seq[DFADelimiter], isDelimRequired: Boolean): Option[DFADelimiter] = {
    if (matches.isEmpty) return None

    // Do we have any matches with delimiters?
    lazy val hasDelimMatches = matches.find(d => d.register.matchStartPos != -1).isDefined

    if (isDelimRequired || hasDelimMatches) {
      val candidates = matches.filter(_.register.matchStartPos != -1)

      if (candidates.isEmpty) return None

      val min = candidates.minBy(_.register.matchStartPos)

      val theFirstLongestMatch =
        candidates.filter(_.register.matchStartPos == min.register.matchStartPos).maxBy(_.register.delimString.length)
      return Some(theFirstLongestMatch)
    }

    // Made it here, we had no delimiter matches
    // acceptable then to take the first match since it should contain
    // the entire field.
    Some(matches(0))
  }

  def removeRightPadding(str: String): String = str.reverse.dropWhile(c => c == padCharOpt.get).reverse
  def removeLeftPadding(str: String): String = str.dropWhile(c => c == padCharOpt.get)
  def removePadding(str: String): String = removeLeftPadding(removeRightPadding(str))

  def parse(input: DFDLCharReader, isDelimRequired: Boolean): Option[ParseResult] = {
    val successes: Queue[DFADelimiter] = Queue.empty

    field.reset(input)
    delims.foreach(_.reset(input))

    val initialCharPos = input.characterPos

    var stillSearching: Boolean = true
    var stateNum: Int = 0 // initial state is 0
    var actionNum: Int = 0

    var fieldResumingCharPos: Int = -1

    while (stillSearching) {
      if (fieldResumingCharPos != -1) {
        val newReader = input.atCharPos(fieldResumingCharPos)
        field.setResume(newReader)
        fieldResumingCharPos = -1
      }
      val fieldResult = field.run(stateNum, actionNum)
      actionNum = 0
      fieldResult match {
        case Right(num) => {
          stateNum = num
          stillSearching = false
        }
        case Left(dfaStatus) => {
          fieldResumingCharPos = initialCharPos + field.register.numCharsReadUntilDelim + 2 // + 2 for data0, data1
          dfaStatus.status match {
            case StateKind.EndOfData => stillSearching = false
            case StateKind.Failed => stillSearching = false
            case StateKind.Paused => {
              delims.foreach(d => { // Pick up where field left off
                d.register.copy(field.register)
                d.run(0) match {
                  case Right(num) => // Shouldn't happen?
                  case Left(delimStatus) => {
                    delimStatus.status match {
                      case StateKind.Succeeded => successes += d
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

    val result = longestMatch(successes, isDelimRequired) match {
      case None if isDelimRequired => None
      case None => {
        val fieldValue: Option[String] = {
          if (field.register.resultString.isEmpty) None
          else {
            val str = field.register.resultString.toString
            val fieldNoPadding = justification match {
              case TextJustificationType.None => str
              case TextJustificationType.Left => removeRightPadding(str)
              case TextJustificationType.Right => removeLeftPadding(str)
              case TextJustificationType.Center => removePadding(str)
            }
            Some(fieldNoPadding)
          }
        }
        val totalNumCharsRead = field.register.numCharsReadUntilDelim
        val numBits: Int = knownEncFunc(field.register.charsReadUntilDelim.toString)
        val nextReader: DFDLCharReader = input.drop(totalNumCharsRead).asInstanceOf[DFDLCharReader]
        Some(new ParseResult(fieldValue, None, "", totalNumCharsRead, numBits, nextReader))
      }
      case Some(dfa) => {
        val r = dfa.register
        val fieldValue: Option[String] = {
          if (field.register.resultString.isEmpty) None
          else {
            val str = field.register.resultString.toString
            val fieldNoPadding = justification match {
              case TextJustificationType.None => str
              case TextJustificationType.Left => removeRightPadding(str)
              case TextJustificationType.Right => removeLeftPadding(str)
              case TextJustificationType.Center => removePadding(str)
            }
            Some(fieldNoPadding)
          }
        }
        val delim: Option[String] = {
          if (r.delimString.isEmpty) None
          else Some(r.delimString.toString)
        }
        val lookingFor = dfa.lookingFor
        // TODO: Uncomment delim charsRead and knownEncFunc
        val totalNumCharsRead = field.register.numCharsReadUntilDelim //+ r.numCharsRead 
        val numBits: Int = knownEncFunc(field.register.charsReadUntilDelim.toString) //+ knownEncFunc(r.delimString.toString)
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
    val successes: Queue[DFADelimiter] = Queue.empty

    val initialCharPos = input.characterPos

    val numCharsReadAfterLeftPadding = justification match {
      case TextJustificationType.Center | TextJustificationType.Right if padCharOpt.isDefined => {
        leftPadding.reset(input)
        leftPadding.run(0)
        leftPadding.register.getReader
        leftPadding.register.numCharsReadUntilDelim
      }
      case _ => 0 // No left padding
    }

    val readerAfterPadding = input.atCharPos(initialCharPos + numCharsReadAfterLeftPadding)

    startBlock.reset(readerAfterPadding)
    endBlock.reset(readerAfterPadding)
    field.reset(readerAfterPadding)
    delims.foreach(_.reset(readerAfterPadding))

    val startBlockResult = startBlock.run(0) // find the block start, fail otherwise
    startBlockResult match {
      case Right(num) => // Shouldn't happen
      case Left(startStatus) => {
        startStatus.status match {
          case StateKind.Succeeded => // continue
          case _ => return None // Failed
        }
      }
    }
    
    val numCharsReadAfterStartBlock = numCharsReadAfterLeftPadding + startBlock.register.numCharsRead

    field.register.copy(startBlock.register)

    var stillSearching: Boolean = true
    var stateNum: Int = 0 // initial state is 0
    var actionNum: Int = 0
    var fieldResumingCharPos: Int = -1

    while (stillSearching) {
      if (fieldResumingCharPos != -1) {
        val newReader = input.atCharPos(fieldResumingCharPos)
        field.setResume(newReader)
        fieldResumingCharPos = -1
      }
      val fieldResult = field.run(stateNum, actionNum)
      actionNum = 0
      fieldResult match {
        case Right(num) => {
          stateNum = num
          stillSearching = false
        }
        case Left(dfaStatus) => {
          fieldResumingCharPos = initialCharPos + numCharsReadAfterStartBlock + field.register.numCharsReadUntilDelim + 2 // + 2 for data0, data1
          dfaStatus.status match {
            case StateKind.EndOfData => stillSearching = false
            case StateKind.Failed => stillSearching = false
            case StateKind.Paused => {
              // Pick up where field left off, we are looking for 
              // the blockEnd.
              endBlock.register.copy(field.register)
              endBlock.run(0) match {
                case Right(_) => // Shouldn't happen
                case Left(endBlockStatus) => {
                  endBlockStatus.status match {
                    case StateKind.Succeeded => {
                      // Found the unescaped block end, now we need to
                      // find any padding.
                      val prevRegister = justification match {
                        case TextJustificationType.Center | TextJustificationType.Left if padCharOpt.isDefined => {
                          rightPadding.reset(input)
                          rightPadding.run(0)
                          rightPadding.register.getReader
                          rightPadding.register
                        }
                        case _ => endBlock.register // No right padding
                      }

                      // Finally, we can look for the delimiter.
                      delims.foreach(d => { // Pick up where end of block/padding left off
                        d.register.copy(prevRegister)
                        d.run(0) match {
                          case Right(num) => // Shouldn't happen?
                          case Left(delimStatus) => {
                            delimStatus.status match {
                              case StateKind.Succeeded => successes += d
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
    val result = longestMatch(successes, isDelimRequired) match {
      case None if isDelimRequired => None
      case None => {
        val fieldValue: Option[String] = {
          if (field.register.resultString.isEmpty) None
          else Some(field.register.resultString.toString)
        }
        val totalCharsRead = {
          startBlock.register.numCharsRead +
            field.register.numCharsReadUntilDelim + endBlock.register.numCharsRead +
            {
              justification match {
                case TextJustificationType.None => 0
                case TextJustificationType.Left => rightPadding.register.numCharsRead
                case TextJustificationType.Right => leftPadding.register.numCharsRead
                case TextJustificationType.Center => rightPadding.register.numCharsRead + leftPadding.register.numCharsRead
              }
            }
        }
        val totalField = {
          startBlock.register.delimString.toString +
            field.register.charsReadUntilDelim.toString + endBlock.register.delimString.toString +
            {
              justification match {
                case TextJustificationType.None => ""
                case TextJustificationType.Left => rightPadding.register.resultString.toString
                case TextJustificationType.Right => leftPadding.register.resultString.toString
                case TextJustificationType.Center => rightPadding.register.resultString.toString + leftPadding.register.resultString.toString
              }
            }
        }
        val numBits: Int = knownEncFunc(totalField)
        val nextReader: DFDLCharReader = input.drop(totalCharsRead).asInstanceOf[DFDLCharReader]
        Some(new ParseResult(fieldValue, None, "", totalCharsRead, numBits, nextReader))
      }
      case Some(dfa) => {
        val r = dfa.register
        val fieldValue: Option[String] = {
          if (field.register.resultString.isEmpty) None
          else Some(field.register.resultString.toString)
        }
        val delim: Option[String] = {
          if (r.delimString.isEmpty) None
          else Some(r.delimString.toString)
        }
        val lookingFor = dfa.lookingFor
        // TODO: Uncomment delim charsRead and knownEncFunc
        val totalNumCharsRead = {
          startBlock.register.numCharsRead +
            field.register.numCharsReadUntilDelim + endBlock.register.numCharsRead +
            {
              justification match {
                case TextJustificationType.None => 0
                case TextJustificationType.Left => rightPadding.register.numCharsRead
                case TextJustificationType.Right => leftPadding.register.numCharsRead
                case TextJustificationType.Center => rightPadding.register.numCharsRead + leftPadding.register.numCharsRead
              }
            }
        }
        val totalField = {
          startBlock.register.delimString.toString +
            field.register.charsReadUntilDelim.toString + endBlock.register.delimString +
            {
              justification match {
                case TextJustificationType.None => ""
                case TextJustificationType.Left => rightPadding.register.resultString.toString
                case TextJustificationType.Right => leftPadding.register.resultString.toString
                case TextJustificationType.Center => rightPadding.register.resultString.toString + leftPadding.register.resultString.toString
              }
            }
        }
        val numBits: Int = knownEncFunc(totalField) //+ knownEncFunc(r.delimString.toString)
        val nextReader: DFDLCharReader = input.drop(totalNumCharsRead).asInstanceOf[DFDLCharReader]
        Some(new ParseResult(fieldValue, delim, lookingFor, totalNumCharsRead, numBits, nextReader))
      }
    }
    result
  }

}
