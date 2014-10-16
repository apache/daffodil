package edu.illinois.ncsa.daffodil.processors.parsers

import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.charset.MalformedInputException
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.dsom.SchemaComponent
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
import edu.illinois.ncsa.daffodil.dsom.RuntimeEncodingMixin
import edu.illinois.ncsa.daffodil.processors.EncodingInfo

abstract class StringLengthParser(
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  override val encodingInfo: EncodingInfo)
  extends PrimParser(erd) with TextReader with RuntimeEncodingMixin {
  override def toString = String.format("%sParser(%s)", parserName, lengthText)

  def lengthText: String
  def parserName: String

  def getLength(pstate: PState): (Long, PState)
  def parseInput(start: PState, charset: Charset, nBytes: Long): PState

  def removeRightPadding(str: String): String = str.reverse.dropWhile(c => c == pad.get).reverse
  def removeLeftPadding(str: String): String = str.dropWhile(c => c == pad.get)
  def removePadding(str: String): String = removeRightPadding(removeLeftPadding(str))

  def trimByJustification(str: String): String = {
    val result = justificationTrim match {
      case TextJustificationType.None => str
      case TextJustificationType.Right => removeLeftPadding(str)
      case TextJustificationType.Left => removeRightPadding(str)
      case TextJustificationType.Center => removePadding(str)
    }
    result
  }

  def parse(pstate: PState): PState = withParseErrorThrowing(pstate) {

    log(LogLevel.Debug, "Parsing starting at bit position: %s", pstate.bitPos)

    val (nBytes, start) = getLength(pstate)
    log(LogLevel.Debug, "Explicit length %s", nBytes)

    if (start.bitPos % 8 != 0) { return PE(start, "%s - not byte aligned.", parserName) }

    try {
      val postState = parseInput(start, dcharset.charset, nBytes)
      return postState
    } catch {
      case m: MalformedInputException => { return PE(start, "%s - MalformedInputException: \n%s", parserName, m.getMessage()) }
      case e: IndexOutOfBoundsException => {
        return PE(start, "%s - Insufficient Bits in field: IndexOutOfBounds: \n%s", parserName, e.getMessage())
      }
      case u: UnsuppressableException => throw u
      //      case e: Exception => {
      //        val foo = e
      //        return PE(start, "%s - Exception:\n%s", parserName, e.getMessage())
      //        }
    }
    pstate
  }
}

class StringFixedLengthInVariableWidthCharactersParser(
  numChars: Long,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  encInfo: EncodingInfo,
  override val lengthText: String)
  extends StringLengthInCharsParser(numChars, justificationTrim: TextJustificationType.Type,
    pad: Maybe[Char],
    erd,
    encInfo) {

  lazy val parserName = "StringFixedLengthInVariableWidthCharacters"
}

class StringVariableLengthInBytesParser(
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  encInfo: EncodingInfo,
  override val length: CompiledExpression,
  override val lengthText: String)
  extends StringLengthInBytesParser(justificationTrim: TextJustificationType.Type,
    pad: Maybe[Char],
    erd,
    encInfo)
  with HasVariableLength {

  lazy val parserName = "StringVariableLengthInBytes"
}

class StringVariableLengthInBytesVariableWidthCharactersParser(
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  encInfo: EncodingInfo,
  override val length: CompiledExpression,
  override val lengthText: String)
  extends StringLengthInBytesParser(justificationTrim: TextJustificationType.Type,
    pad: Maybe[Char],
    erd,
    encInfo)
  with HasVariableLength {

  lazy val parserName = "StringVariableLengthInBytesVariableWidthCharacters"
}

class StringVariableLengthInVariableWidthCharactersParser(
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  encInfo: EncodingInfo,
  override val length: CompiledExpression,
  override val lengthText: String)
  extends StringLengthInBytesParser(
    justificationTrim: TextJustificationType.Type,
    pad: Maybe[Char],
    erd,
    encInfo)
  with HasVariableLength {

  lazy val parserName = "StringVariableLengthInVariableWidthCharacters"
}

class StringFixedLengthInBytesVariableWidthCharactersParser(
  nBytes: Long,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  encInfo: EncodingInfo,
  override val lengthText: String)
  extends StringLengthInBytesParser(
    justificationTrim: TextJustificationType.Type,
    pad: Maybe[Char],
    erd,
    encInfo) {

  lazy val parserName = "StringFixedLengthInBytesVariableWidthCharacters"

  def getLength(pstate: PState): (Long, PState) = {
    (nBytes, pstate)
  }
}

class StringFixedLengthInBytesFixedWidthCharactersParser(
  nBytes: Long,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  encInfo: EncodingInfo,
  override val lengthText: String)
  extends StringLengthInBytesParser(
    justificationTrim: TextJustificationType.Type,
    pad: Maybe[Char],
    erd,
    encInfo) {

  lazy val parserName = "StringFixedLengthInBytesFixedWidthCharacters"

  def getLength(pstate: PState): (Long, PState) = {
    (nBytes, pstate)
  }
}

abstract class StringLengthInCharsParser(
  nChars: Long,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  encInfo: EncodingInfo)
  extends StringLengthParser(justificationTrim, pad, erd, encInfo) {

  def getLength(pstate: PState): (Long, PState) = {
    (nChars, pstate)
  }

  def parseInput(start: PState, charset: Charset, nChars: Long): PState = start

  override def parse(start: PState): PState = withParseErrorThrowing(start) {

    log(LogLevel.Debug, "Parsing starting at bit position: %s", start.bitPos)

    // no longer require alignment (some encodings aren't whole bytes)
    // if (start.bitPos % 8 != 0) { return PE(start, "StringFixedLengthInVariableWidthCharacters - not byte aligned.") }

    log(LogLevel.Debug, "Retrieving reader")

    val reader = getReader(dcharset.charset, start.bitPos, start)

    val field = reader.getStringInChars(nChars.toInt).toString()
    val fieldLength = field.length

    if (fieldLength != nChars.toInt) {
      return PE(start, "Parse failed to find exactly %s characters.", nChars)
    } else {
      val parsedField = trimByJustification(field)
      val parsedBits = knownEncodingStringBitLength(field)
      val endBitPos = start.bitPos + parsedBits

      log(LogLevel.Debug, "Parsed: %s", field)
      log(LogLevel.Debug, "Ended at bit position: %s", endBitPos)

      val endCharPos = if (start.charPos == -1) nChars else start.charPos + nChars
      start.simpleElement.setDataValue(parsedField)

      val nextReader = reader.atBitPos(endBitPos)
      val postState = start.withPos(endBitPos, endCharPos, One(nextReader))
      return postState
    }
  }

}

abstract class StringLengthInBytesParser(
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  erd: ElementRuntimeData,
  encInfo: EncodingInfo)
  extends StringLengthParser(justificationTrim, pad, erd, encInfo) {
  def formatValue(value: String): String = {
    value
  }

  def parseInput(start: PState, charset: Charset, nBytes: Long): PState = {
    val in = start.inStream
    val decoder = charset.newDecoder()

    val reader = getReader(charset, start.bitPos, start)

    // This next block of lines needs to become functionality of the
    // reader so it can be shared, and decoding is all called from one
    // place. 
    val bytes = in.getBytes(start.bitPos, nBytes.toInt)
    val cb = decoder.decode(ByteBuffer.wrap(bytes))
    val result = cb.toString
    val endBitPos = start.bitPos + knownEncodingStringBitLength(result)
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
    val postState = {
      // TODO: Shouldn't the 8 * nBytes really be codepointWidth * nBytes?
      if ((endBitPos - start.bitPos) == (8 * nBytes)) {
        start.withPos(endBitPos, endCharPos, One(reader))
      } else {
        Assert.invariant((endBitPos - start.bitPos) < (8 * nBytes))
        start.withPos(endBitPos, -1, Nope)
        // -1 means a subsequent primitive will have to construct
        // a new reader at said bitPosition              
      }
    }

    return postState
  }
}
