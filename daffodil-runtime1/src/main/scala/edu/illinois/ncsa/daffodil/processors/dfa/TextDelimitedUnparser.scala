package edu.illinois.ncsa.daffodil.processors.dfa

import scala.collection.mutable.ArrayBuffer
import edu.illinois.ncsa.daffodil.exceptions.Assert
import scala.util.parsing.input.CharSequenceReader
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.DFDLCharReader
import edu.illinois.ncsa.daffodil.processors.unparsers.UnparseError
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData

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

  def escape(input: DFDLCharReader,
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
  def escapeBlock(input: DFDLCharReader,
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

    fieldReg.reset(input, 0)

    val initialCharPos = 0

    var stillSearching: Boolean = true
    var stateNum: Int = 0 // initial state is 0
    var actionNum: Int = 0
    var numCharsInserted: Int = 0

    var fieldResumingCharPos: Int = -1

    var shouldGenerateEscapeBlock: Boolean = false

    while (stillSearching) {
      if (fieldResumingCharPos != -1) {
        val newReader = input.atCharPos(fieldResumingCharPos)
        fieldReg.resumeForUnparse(newReader)
        fieldResumingCharPos = -1
      }
      // We want to examine each character and if it's not part of a
      // delimiter append it to the 'field' member.  If it is part of
      // a delimiter we want to perform a longest match.  We then
      // append the 'escape' character to the 'field' member followed
      // by the matched delimiter.  We then start the process again
      // starting with the character following that of the matched 
      // delimiter until we reach end of data.
      //
      val dfaStatus = field.run(stateNum, fieldReg, actionNum)
      actionNum = 0

      dfaStatus.status match {
        case StateKind.EndOfData => stillSearching = false
        case StateKind.Failed => stillSearching = false
        case StateKind.Paused => {

          // We check for a blockEnd first, if it exists then we MUST
          // generate an escape block
          //
          val blockEndReg: Registers = new Registers(Seq(blockEnd))
          blockEndReg.copy(fieldReg)
          val blockEndStatus = blockEnd.run(0, blockEndReg)
          blockEndStatus.status match {
            case StateKind.Succeeded if (!escapeEscapeChar.isDefined) => UnparseError(One(context.schemaFileLocation),
              One(state),
              "escapeEscapeCharacter was not defined but the escapeBlockEnd (%s) was present in the data.",
              blockEnd.lookingFor)
            case StateKind.Succeeded => {
              fieldReg.appendToField(escapeEscapeChar.get)
              blockEndReg.delimString.foreach(fieldReg.appendToField(_))
              numCharsInserted += 1
              shouldGenerateEscapeBlock = true

              // resume field parse
              fieldResumingCharPos = initialCharPos + fieldReg.numCharsRead - numCharsInserted // subtract for inserted escape character

              actionNum = 0
              stateNum = 0
            }
            case _ => {
              // Looking for the blockEnd failed, check for the other pieces
              // of text we should generate an escape block for
              //
              delims.foreach(d => { // Pick up where field left off
                val delimReg: Registers = new Registers(delims)
                delimReg.copy(fieldReg)
                val delimStatus = d.run(0, delimReg)
                delimStatus.status match {
                  case StateKind.Succeeded => {
                    successes += (d -> delimReg)
                  }
                  case _ => {
                    // resume field parse
                    actionNum = dfaStatus.actionNum + 1 // goto next rule
                    stateNum = dfaStatus.currentStateNum
                  }
                }
              })
              if (!successes.isEmpty) {
                val (matchedDelim, matchedReg) = longestMatch(successes).get
                matchedReg.delimString.foreach(fieldReg.appendToField(_))
                successes.clear

                shouldGenerateEscapeBlock = true

                // resume field parse
                fieldResumingCharPos = initialCharPos + fieldReg.numCharsRead - numCharsInserted // subtract for inserted escape character

                actionNum = 0
                stateNum = 0
              }
            }
          }
        }
      }
    }

    (fieldReg.resultString.toString, shouldGenerateEscapeBlock)
  }

  /**
   * Performs escaping appropriate when escapeSchemeKind is Character for
   * unparsing.
   */
  def escapeCharacter(input: DFDLCharReader,
    field: DFAField,
    delims: Seq[DFADelimiter],
    escapeChar: Char,
    escapeEscapeChar: Maybe[Char], state: UState): (String, Boolean) = {
    Assert.invariant(delims != null)
    Assert.invariant(field != null)

    val successes: ArrayBuffer[(DFADelimiter, Registers)] = ArrayBuffer.empty
    val fieldReg: Registers = new Registers(delims)

    fieldReg.reset(input, 0)

    val initialCharPos = 0

    var stillSearching: Boolean = true
    var stateNum: Int = 0 // initial state is 0
    var actionNum: Int = 0
    var numCharsInserted: Int = 0

    var fieldResumingCharPos: Int = -1

    var escapeOccurred: Boolean = false

    while (stillSearching) {
      if (fieldResumingCharPos != -1) {
        val newReader = input.atCharPos(fieldResumingCharPos)
        fieldReg.resumeForUnparse(newReader)
        fieldResumingCharPos = -1
      }
      // We want to examine each character and if it's not part of a
      // delimiter append it to the 'field' member.  If it is part of
      // a delimiter we want to perform a longest match.  We then
      // append the 'escape' character to the 'field' member followed
      // by the matched delimiter.  We then start the process again
      // starting with the character following that of the matched 
      // delimiter until we reach end of data.
      //
      val dfaStatus = field.run(stateNum, fieldReg, actionNum)
      actionNum = 0

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
          if (!successes.isEmpty) {
            val (matchedDelim, matchedReg) = longestMatch(successes).get
            if (matchedDelim.lookingFor.length() == 1 && matchedDelim.lookingFor(0) == escapeChar) {
              if (escapeEscapeChar.isDefined)
                fieldReg.appendToField(escapeEscapeChar.get)
              else
                UnparseError(One(context.schemaFileLocation), One(state), "escapeEscapeCharacter was not defined but the escapeCharacter (%s) was present in the data.", escapeChar)
            } else { fieldReg.appendToField(escapeChar) }
            matchedReg.delimString.foreach(fieldReg.appendToField(_))
            successes.clear

            escapeOccurred = true

            numCharsInserted += 1

            // resume field parse
            fieldResumingCharPos = initialCharPos + fieldReg.numCharsRead - numCharsInserted // subtract for inserted escape character

            actionNum = 0
            stateNum = 0
          }
        }
      }
    }

    (fieldReg.resultString.toString, escapeOccurred)
  }

}