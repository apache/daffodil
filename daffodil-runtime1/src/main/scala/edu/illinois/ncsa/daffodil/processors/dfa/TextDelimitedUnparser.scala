package edu.illinois.ncsa.daffodil.processors.dfa

import scala.collection.mutable.ArrayBuffer
import edu.illinois.ncsa.daffodil.exceptions.Assert
import scala.util.parsing.input.CharSequenceReader
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.unparsers.UnparseError
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.io.DataInputStream

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
    delims: Seq[DFADelimiter],
    blockEndDFA: DFADelimiter,
    escapeEscapeChar: Maybe[Char],
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
  def escapeBlock(input: DataInputStream,
    field: DFAField,
    delims: Seq[DFADelimiter],
    blockEnd: DFADelimiter,
    escapeEscapeChar: Maybe[Char],
    state: UState): (String, Boolean) = {
    Assert.invariant(delims != null)
    Assert.invariant(field != null)

    val successes: ArrayBuffer[(DFADelimiter, Registers)] = ArrayBuffer.empty

    // We need to recognize the blockEnd in addition to the other pieces of
    // text we should escape
    //
    val fieldReg: Registers = new Registers(blockEnd +: delims)

    fieldReg.reset(input)

    val initialCharPos = 0

    var stillSearching: Boolean = true
    var stateNum: Int = 0 // initial state is 0
    var actionNum: Int = 0
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
      Assert.invariant(beforeDelimiter =:= DataInputStream.MarkPos.NoMarkPos)
      val dfaStatus = field.run(stateNum, fieldReg, actionNum)
      actionNum = 0
      beforeDelimiter = input.markPos

      dfaStatus.status match {
        case StateKind.EndOfData => stillSearching = false
        case StateKind.Failed => stillSearching = false
        case StateKind.Paused => {

          // We check for a blockEnd first, if it exists then we MUST
          // generate an escape block
          //
          val blockEndReg: Registers = new Registers(Seq(blockEnd))
          blockEndReg.reset(input)
          val blockEndStatus = blockEnd.run(0, blockEndReg)
          blockEndStatus.status match {
            case StateKind.Succeeded if (!escapeEscapeChar.isDefined) => {
              beforeDelimiter = DataInputStream.MarkPos.NoMarkPos
              UnparseError(One(context.schemaFileLocation),
                One(state.currentLocation),
                "escapeEscapeCharacter was not defined but the escapeBlockEnd (%s) was present in the data.",
                blockEnd.lookingFor)
            }
            case StateKind.Succeeded => {
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
              blockEnd.foreach(fieldReg.appendToField(_))
              input.resetPos(afterBlockEnd) // we want to resume scanning after the blockEnd.
              shouldGenerateEscapeBlock = true
              beforeDelimiter = DataInputStream.MarkPos.NoMarkPos
              actionNum = 0
              stateNum = 0
              // now go around the while loop again
            }
            case _ => {
              // Looking for the blockEnd failed, check for the other pieces
              // of text we should generate an escape block for
              //
              delims.foreach(d => { // Pick up where field left off
                val delimReg: Registers = new Registers(delims)
                input.resetPos(beforeDelimiter)
                beforeDelimiter = input.markPos
                delimReg.reset(input)
                val delimStatus = d.run(0, delimReg)
                delimStatus.status match {
                  case StateKind.Succeeded => {
                    successes += (d -> delimReg)
                  }
                  case _ => {
                    // resume field parse
                    //
                    // FIXME: Is this correct? There could
                    // be multiple delims being tested. This code is evaluated if ONE of them 
                    // does not parse successfully, but others might.
                    //
                    // Commenting out for now.
                    // actionNum = dfaStatus.actionNum + 1 // goto next rule
                    // stateNum = dfaStatus.currentStateNum
                  }
                }
              })
              input.resetPos(beforeDelimiter)
              beforeDelimiter = DataInputStream.MarkPos.NoMarkPos
              fieldReg.resetChars
              if (successes.isEmpty) {
                actionNum = dfaStatus.actionNum + 1 // goto next rule
                stateNum = dfaStatus.currentStateNum
              } else {
                val (matchedDelim, matchedReg) = longestMatch(successes).get
                val delim = matchedReg.delimString
                delim.foreach(fieldReg.appendToField(_)) // the delim just becomes field content, because we already had an escape block start.
                successes.clear
                shouldGenerateEscapeBlock = true
                actionNum = 0
                stateNum = 0
              }
              // now go around the while loop again
            } // end case StateKind.Failed for finding the block end.
          } // end blockEndStatus.status match
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
    (fieldReg.resultString.toString, shouldGenerateEscapeBlock)
  }

  /**
   * Performs escaping appropriate when escapeSchemeKind is Character for
   * unparsing.
   */
  def escapeCharacter(input: DataInputStream,
    field: DFAField,
    delims: Seq[DFADelimiter],
    hasEscCharAsDelimiter: Boolean,
    escapeChar: Char,
    escapeEscapeChar: Maybe[Char], state: UState): (String, Boolean) = {
    Assert.invariant(delims != null)
    Assert.invariant(field != null)

    val successes: ArrayBuffer[(DFADelimiter, Registers)] = ArrayBuffer.empty
    val fieldReg: Registers = new Registers(delims)

    fieldReg.reset(input)

    val initialCharPos = 0

    var stillSearching: Boolean = true
    var stateNum: Int = 0 // initial state is 0
    var actionNum: Int = 0
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
      val dfaStatus = field.run(stateNum, fieldReg, actionNum)
      beforeDelimiter = input.markPos

      actionNum = 0

      dfaStatus.status match {
        case StateKind.EndOfData => stillSearching = false
        case StateKind.Failed => stillSearching = false
        case StateKind.Paused => {
          delims.foreach(d => { // Pick up where field left off
            val delimReg: Registers = new Registers(delims)
            delimReg.reset(input)
            input.resetPos(beforeDelimiter)
            beforeDelimiter = input.markPos
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
          if (!successes.isEmpty) {
            val (matchedDelim, matchedReg) = longestMatch(successes).get
            if (matchedDelim.lookingFor.length() == 1 && matchedDelim.lookingFor(0) =:= escapeChar) {
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
            Assert.invariant(input.skipChars(delim.length))
            fieldReg.resetChars
            successes.clear
            stillSearching = true

            escapeOccurred = true

            numCharsInserted += 1

            // resume field parse

            actionNum = 0
            stateNum = 0

          }
        }
      }
    }
    // No need to now advance the input, because we're unparsing, and we're done
    // so this input is going to be discarded since it existed only to enable
    // us to reuse the DFA for determining when to escape data while unparsing.
    (fieldReg.resultString.toString, escapeOccurred)
  }

}