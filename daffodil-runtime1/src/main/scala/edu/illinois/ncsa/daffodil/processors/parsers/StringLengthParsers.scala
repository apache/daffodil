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

package edu.illinois.ncsa.daffodil.processors.parsers

import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.charset.MalformedInputException
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.processors.DFDLCharCounter
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.processors.TextReader
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.InfosetSimpleElement
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.util.Misc

abstract class StringLengthParser(
  override val justificationTrim: TextJustificationType.Type,
  val parsingPadChar: Maybe[Char],
  erd: ElementRuntimeData)
  extends PrimParser(erd) with TextReader with PaddingRuntimeMixin {
  override def toString = String.format("%sParser(%s)", parserName, lengthText)

  def lengthText: String
  def parserName: String

  def getLength(pstate: PState): Long
  def parseInput(start: PState, charset: Charset, nBytes: Long): Unit

  def parse(start: PState): Unit = withParseErrorThrowing(start) {

    log(LogLevel.Debug, "Parsing starting at bit position: %s", start.bitPos)

    val nBytes = getLength(start)
    log(LogLevel.Debug, "Explicit length %s", nBytes)

    if (start.bitPos % 8 != 0) {
      PE(start, "%s - not byte aligned.", parserName)
      return
    }

    try {
      parseInput(start, erd.encodingInfo.knownEncodingCharset.charset, nBytes)
      return
    } catch {
      // Malformed input exception indicates bytes/bits couldn't be decoded into 
      // characters.
      //
      // This won't actually be thrown until encodingErrorPolicy='error' is
      // implemented. That is tricky, because we must read-exact, we can't convert
      // a buffer of characters but error out on a decoding error because that
      // part of the buffer might never be consumed. It just represents perhaps 
      // decoding ahead past end of string into some binary data perhaps.
      //
      case m: MalformedInputException => {
        PE(start, "%s - MalformedInputException: \n%s", parserName, m.getMessage())
        return
      }
      //
      // Thrown if the length is explicit but there aren't enough bytes/bits to
      // meet the length.
      //
      case e: IndexOutOfBoundsException => {
        PE(start, "%s - Insufficient Bits in field: IndexOutOfBounds: \n%s", parserName, e.getMessage())
        return
      }
    }
  }
}

class StringFixedLengthInVariableWidthCharactersParser(
  numChars: Long,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  override val lengthText: String)
  extends StringLengthInCharsParser(numChars, justificationTrim: TextJustificationType.Type,
    pad: Maybe[Char],
    erd) {

  lazy val parserName = "StringFixedLengthInVariableWidthCharacters"
}

class StringVariableLengthInBytesParser(
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  override val length: CompiledExpression,
  override val lengthText: String)
  extends StringLengthInBytesParser(justificationTrim: TextJustificationType.Type,
    pad: Maybe[Char],
    erd)
  with HasVariableLength {

  lazy val parserName = "StringVariableLengthInBytes"
}

class StringVariableLengthInBytesVariableWidthCharactersParser(
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  override val length: CompiledExpression,
  override val lengthText: String)
  extends StringLengthInBytesParser(justificationTrim: TextJustificationType.Type,
    pad: Maybe[Char],
    erd)
  with HasVariableLength {

  lazy val parserName = "StringVariableLengthInBytesVariableWidthCharacters"
}

class StringVariableLengthInVariableWidthCharactersParser(
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  override val length: CompiledExpression,
  override val lengthText: String)
  extends StringLengthInBytesParser(
    justificationTrim: TextJustificationType.Type,
    pad: Maybe[Char],
    erd)
  with HasVariableLength {

  lazy val parserName = "StringVariableLengthInVariableWidthCharacters"
}

class StringFixedLengthInBytesVariableWidthCharactersParser(
  nBytes: Long,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  override val lengthText: String)
  extends StringLengthInBytesParser(
    justificationTrim: TextJustificationType.Type,
    pad: Maybe[Char],
    erd) {

  lazy val parserName = "StringFixedLengthInBytesVariableWidthCharacters"

  def getLength(pstate: PState): Long = nBytes
}

class StringFixedLengthInBytesFixedWidthCharactersParser(
  nBytes: Long,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  override val lengthText: String)
  extends StringLengthInBytesParser(
    justificationTrim: TextJustificationType.Type,
    pad: Maybe[Char],
    erd) {

  lazy val parserName = "StringFixedLengthInBytesFixedWidthCharacters"

  def getLength(pstate: PState): Long = nBytes

}

abstract class StringLengthInCharsParser(
  nChars: Long,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData)
  extends StringLengthParser(justificationTrim, pad, erd) {

  def getLength(pstate: PState): Long = nChars

  def parseInput(start: PState, charset: Charset, nChars: Long): Unit = {
    // do nothing
  }

  override def parse(start: PState): Unit = withParseErrorThrowing(start) {

    log(LogLevel.Debug, "Parsing starting at bit position: %s", start.bitPos)

    // no longer require alignment (some encodings aren't whole bytes)
    // if (start.bitPos % 8 != 0) { return PE(start, "StringFixedLengthInVariableWidthCharacters - not byte aligned.") }

    log(LogLevel.Debug, "Retrieving reader")

    val reader = getReader(erd.encodingInfo.knownEncodingCharset.charset, start.bitPos, start)

    val field = reader.getStringInChars(nChars.toInt).toString()
    val fieldLength = field.length

    if (fieldLength != nChars.toInt) {
      PE(start, "Parse failed to find exactly %s characters.", nChars)
      return
    } else {
      val parsedField = trimByJustification(field)
      val parsedBits = erd.encodingInfo.knownEncodingStringBitLength(field)
      val endBitPos = start.bitPos + parsedBits

      log(LogLevel.Debug, "Parsed: %s", field)
      log(LogLevel.Debug, "Ended at bit position: %s", endBitPos)

      val endCharPos = if (start.charPos == -1) nChars else start.charPos + nChars
      start.simpleElement.setDataValue(parsedField)

      val nextReader = reader.atBitPos(endBitPos)
      start.setPos(endBitPos, endCharPos, One(nextReader))
      return
    }
  }

}

abstract class StringLengthInBytesParser(
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData)
  extends StringLengthParser(justificationTrim, pad, erd) {

  def formatValue(value: String): String = {
    value
  }

  def parseInput(start: PState, charset: Charset, nBytes: Long): Unit = {
    val in = start.inStream
    val decoder = charset.newDecoder()

    val reader = getReader(charset, start.bitPos, start)

    // This next block of lines needs to become functionality of the
    // reader so it can be shared, and decoding is all called from one
    // place. 
    val bytes = in.getBytes(start.bitPos, nBytes.toInt)
    val cb = decoder.decode(ByteBuffer.wrap(bytes))
    val result = cb.toString
    val endBitPos = start.bitPos + erd.encodingInfo.knownEncodingStringBitLength(result)
    log(LogLevel.Debug, "Parsed: " + result)
    log(LogLevel.Debug, "Ended at bit position " + endBitPos)
    val endCharPos = start.charPos + result.length
    // 
    // Maintain our global count of number of characters.
    // TODO: get rid of global counter for a dataProcessor-saved one. 
    // 
    DFDLCharCounter.incr(result.length)

    val currentElement: InfosetSimpleElement = start.simpleElement
    val trimmedResult = trimByJustification(result)

    // Assert.invariant(currentElement.getName != "_document_")
    // Note: this side effect is backtracked, because at points of uncertainty, pre-copies of a node are made
    // and when backtracking occurs they are used to replace the nodes modified by sub-parsers.
    currentElement.setDataValue(formatValue(trimmedResult))
    // 
    // if the number of bytes was a multiple of the codepointWidth then 
    // we will have parsed all the bytes, so the endBitPos and endCharPos 
    // are synchronized still. 
    // 

    // TODO: Shouldn't the 8 * nBytes really be codepointWidth * nBytes?
    if ((endBitPos - start.bitPos) == (8 * nBytes)) {
      start.setPos(endBitPos, endCharPos, One(reader))
    } else {
      Assert.invariant((endBitPos - start.bitPos) < (8 * nBytes))
      start.setPos(endBitPos, -1, Nope)
      // -1 means a subsequent primitive will have to construct
      // a new reader at said bitPosition              
    }
  }
}

