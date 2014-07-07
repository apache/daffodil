package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.dsom.ListOfStringValueAsLiteral
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.grammar.Gram
import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.processors.DFDLDelimParser
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.dsom.R
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.processors.TextReader
import edu.illinois.ncsa.daffodil.processors.DelimParseFailure
import edu.illinois.ncsa.daffodil.processors.DelimParseSuccess

class LiteralNilPatternParser(
  val padChar: String,
  val justificationTrim: TextJustificationType.Type,
  d: ThreadLocal[DFDLDelimParser],
  gram: Gram,
  e: ElementBase)
  extends LiteralNilParserBase(d: ThreadLocal[DFDLDelimParser], gram, e)
  with HasPadding
  with TextReader {
  val pattern = e.lengthPattern

  def parse(start: PState): PState = {
    // withLoggingLevel(LogLevel.Info) 
    {

      val postEvalState = start //start.withVariables(vars)

      log(LogLevel.Debug, "%s - Looking for: %s Count: %s", eName, nilValuesCooked, nilValuesCooked.length)

      val bytePos = (postEvalState.bitPos >> 3).toInt
      log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, postEvalState.bitPos)
      log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

      if (postEvalState.bitPos % 8 != 0) { return PE(start, "LiteralNilPattern - not byte aligned.") }

      log(LogLevel.Debug, "Retrieving reader state.")
      val reader = getReader(charset, start.bitPos, start)

      val result = d.get.parseInputPatterned(pattern, reader, start)

      result match {
        case _: DelimParseFailure =>
          return PE(postEvalState, "%s - %s - Parse failed.", this.toString(), eName)
        case s: DelimParseSuccess => {
          // We have a field, is it empty?
          val field = trimByJustification(s.field)
          val isFieldEmpty = field.length() == 0

          if (isFieldEmpty && isEmptyAllowed) {
            // Valid!
            start.parentElement.makeNil()
            return postEvalState // Empty, no need to advance
          } else if (isFieldEmpty && !isEmptyAllowed) {
            // Fail!
            return PE(postEvalState, "%s - Empty field found but not allowed!", eName)
          } else if (d.get.isFieldDfdlLiteral(field, nilValuesCooked.toSet)) {
            // Contains a nilValue, Success!
            start.parentElement.makeNil()

            val numBits = s.numBits //e.knownEncodingStringBitLength(result.field)

            val endCharPos =
              if (postEvalState.charPos == -1) s.field.length
              else postEvalState.charPos + s.field.length
            val endBitPos = numBits + start.bitPos

            log(LogLevel.Debug, "%s - Found %s", eName, s.field)
            log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPos >> 3))
            log(LogLevel.Debug, "%s - Ended at bit position ", eName, endBitPos)

            return postEvalState.withPos(endBitPos, endCharPos, One(s.next)) // Need to advance past found nilValue
          } else {
            // Fail!
            return PE(postEvalState, "%s - Does not contain a nil literal!", eName)
          }
        }
      }
    }
  }
}

class LiteralNilExplicitParser(
  val padChar: String,
  val justificationTrim: TextJustificationType.Type,
  nUnits: Long,
  d: ThreadLocal[DFDLDelimParser],
  gram: Gram,
  e: ElementBase)
  extends LiteralNilParserBase(d: ThreadLocal[DFDLDelimParser], gram, e)
  with HasPadding
  with TextReader {
  val pattern = e.lengthPattern

  def parse(start: PState): PState = {
    // withLoggingLevel(LogLevel.Info) 
    {

      val postEvalState = start //start.withVariables(vars)

      log(LogLevel.Debug, "%s - Looking for: %s Count: %s", eName, nilValuesCooked, nilValuesCooked.length)

      val bytePos = (postEvalState.bitPos >> 3).toInt
      log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, postEvalState.bitPos)
      log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

      if (postEvalState.bitPos % 8 != 0) { return PE(start, "LiteralNilPattern - not byte aligned.") }

      log(LogLevel.Debug, "Retrieving reader state.")
      val reader = getReader(charset, start.bitPos, start)

      //        val byteReader = in.byteReader.atPos(bytePos)
      //        val reader = byteReader.charReader(decoder.charset().name())

      val result = d.get.parseInputPatterned(pattern, reader, start)

      result match {
        case _: DelimParseFailure =>
          return PE(postEvalState, "%s - %s - Parse failed.", this.toString(), eName)
        case s: DelimParseSuccess => {
          // We have a field, is it empty?
          val field = trimByJustification(s.field)
          val isFieldEmpty = field.length() == 0

          if (isFieldEmpty && isEmptyAllowed) {
            // Valid!
            start.parentElement.makeNil()
            return postEvalState // Empty, no need to advance
          } else if (isFieldEmpty && !isEmptyAllowed) {
            // Fail!
            return PE(postEvalState, "%s - Empty field found but not allowed!", eName)
          } else if (d.get.isFieldDfdlLiteral(field, nilValuesCooked.toSet)) {
            // Contains a nilValue, Success!
            start.parentElement.makeNil()

            val numBits = s.numBits //e.knownEncodingStringBitLength(result.field)
            //val endCharPos = start.charPos + result.field.length()
            val endCharPos =
              if (postEvalState.charPos == -1) s.field.length
              else postEvalState.charPos + s.field.length
            val endBitPos = numBits + start.bitPos

            log(LogLevel.Debug, "%s - Found %s", eName, s.field)
            log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPos >> 3))
            log(LogLevel.Debug, "%s - Ended at bit position ", eName, endBitPos)

            //return postEvalState.withPos(endBitPos, endCharPos) // Need to advance past found nilValue
            return postEvalState.withPos(endBitPos, endCharPos, One(s.next)) // Need to advance past found nilValue
          } else {
            // Fail!
            return PE(postEvalState, "%s - Does not contain a nil literal!", eName)
          }
        }
      }
    }
  }
}

class LiteralNilExplicitLengthInCharsParser(
  val padChar: String,
  val justificationTrim: TextJustificationType.Type,
  d: ThreadLocal[DFDLDelimParser],
  gram: Gram,
  e: ElementBase)
  extends LiteralNilParserBase(d: ThreadLocal[DFDLDelimParser], gram, e)
  with TextReader
  with HasPadding {
  override def toBriefXML(depthLimit: Int = -1): String = {
    "<" + name + " nilValue='" + e.nilValue + "'/>"
  }

  val expr = e.length
  val exprText = expr.prettyExpr

  def parse(start: PState): PState = {
    // withLoggingLevel(LogLevel.Info) 
    {

      //val postEvalState = start //start.withVariables(vars)

      val R(nCharsAsAny, newVMap) = expr.evaluate(start.parentElement, start.variableMap, start)
      val nChars = nCharsAsAny.asInstanceOf[String] //nBytesAsAny.asInstanceOf[Long]
      val postEvalState = start.withVariables(newVMap)
      log(LogLevel.Debug, "Explicit length %s", nChars)

      val pattern = "(?s)^.{%s}".format(nChars)

      log(LogLevel.Debug, "%s - Looking for: %s Count: %s", eName, nilValuesCooked, nilValuesCooked.length)

      val bytePos = (postEvalState.bitPos >> 3).toInt
      log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, postEvalState.bitPos)
      log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

      // Don't check this here. This can vary by encoding.
      //if (postEvalState.bitPos % 8 != 0) { return PE(start, "LiteralNilPattern - not byte aligned.") }

      log(LogLevel.Debug, "Retrieving reader state.")
      val reader = getReader(charset, start.bitPos, start)

      if (nChars == 0 && isEmptyAllowed) {
        log(LogLevel.Debug, "%s - explicit length of 0 and %ES; found as nilValue.", eName)
        postEvalState.parentElement.makeNil()
        return postEvalState // Empty, no need to advance
      }

      val result = d.get.parseInputPatterned(pattern, reader, postEvalState)

      result match {
        case _: DelimParseFailure =>
          return PE(postEvalState, "%s - %s - Parse failed.", this.toString(), eName)
        case s: DelimParseSuccess => {
          // We have a field, is it empty?
          val field = trimByJustification(s.field)
          val isFieldEmpty = field.length() == 0

          if (isFieldEmpty && isEmptyAllowed) {
            // Valid!
            start.parentElement.makeNil()
            return postEvalState // Empty, no need to advance
          } else if (isFieldEmpty && !isEmptyAllowed) {
            // Fail!
            return PE(postEvalState, "%s - Empty field found but not allowed!", eName)
          } else if (d.get.isFieldDfdlLiteral(field, nilValuesCooked.toSet)) {
            // Contains a nilValue, Success!
            start.parentElement.makeNil()

            val numBits = s.numBits //e.knownEncodingStringBitLength(result.field)
            val endCharPos =
              if (postEvalState.charPos == -1) s.field.length
              else postEvalState.charPos + s.field.length
            val endBitPos = numBits + start.bitPos

            log(LogLevel.Debug, "%s - Found %s", eName, s.field)
            log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPos >> 3))
            log(LogLevel.Debug, "%s - Ended at bit position ", eName, endBitPos)

            return postEvalState.withPos(endBitPos, endCharPos, One(s.next)) // Need to advance past found nilValue
          } else {
            // Fail!
            return PE(postEvalState, "%s - Does not contain a nil literal!", eName)
          }
        }
      }
    }
  }
}

class LiteralNilKnownLengthInBytesParser(
  val padChar: String,
  val justificationTrim: TextJustificationType.Type,
  lengthInBytes: Long,
  d: ThreadLocal[DFDLDelimParser],
  gram: Gram,
  e: ElementBase)
  extends LiteralNilInBytesParserBase(d: ThreadLocal[DFDLDelimParser], gram, e) {
  final def computeLength(start: PState) = {
    (lengthInBytes, start.variableMap)
  }
}

class LiteralNilExplicitLengthInBytesParser(
  val padChar: String,
  val justificationTrim: TextJustificationType.Type,
  d: ThreadLocal[DFDLDelimParser],
  gram: Gram,
  e: ElementBase)
  extends LiteralNilInBytesParserBase(d: ThreadLocal[DFDLDelimParser], gram, e) {
  val expr = e.length
  val exprText = expr.prettyExpr

  final def computeLength(start: PState) = {
    val R(nBytesAsAny, newVMap) = expr.evaluate(start.parentElement, start.variableMap, start)
    val nBytes = nBytesAsAny.toString().toLong //nBytesAsAny.asInstanceOf[Long]
    (nBytes, newVMap)
  }
}

abstract class LiteralNilParserBase(d: ThreadLocal[DFDLDelimParser], gram: Gram, e: ElementBase)
  extends PrimParser(gram, e) {
  val name = gram.prettyName

  override def toBriefXML(depthLimit: Int = -1): String = {
    "<" + name + " nilValue='" + e.nilValue + "'/>"
  }

  val charset = e.knownEncodingCharset
  val isEmptyAllowed = e.nilValue.contains("%ES;")
  val eName = e.toString()
  val nilValuesCooked = new ListOfStringValueAsLiteral(e.nilValue, e).cooked
  val charsetName = charset.name()
}

abstract class LiteralNilInBytesParserBase(d: ThreadLocal[DFDLDelimParser], gram: Gram, e: ElementBase)
  extends LiteralNilParserBase(d, gram, e)
  with HasPadding {

  protected def computeLength(start: PState): (Long, VariableMap)

  def parse(start: PState): PState = {
    //      withLoggingLevel(LogLevel.Debug) 
    {

      // TODO: What if someone passes in nBytes = 0 for Explicit length, is this legal?

      val (nBytes: Long, newVMap: VariableMap) = computeLength(start)
      val postEvalState = start.withVariables(newVMap)
      log(LogLevel.Debug, "Explicit length %s", nBytes)

      //val postEvalState = start //start.withVariables(vars)

      log(LogLevel.Debug, "%s - Looking for: %s Count: %s", eName, nilValuesCooked, nilValuesCooked.length)
      val in = postEvalState.inStream

      val bytePos = (postEvalState.bitPos >> 3).toInt
      log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, postEvalState.bitPos)
      log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

      // some encodings aren't whole bytes
      // if (postEvalState.bitPos % 8 != 0) { return PE(postEvalState, "LiteralNilPattern - not byte aligned.") }

      val decoder = charset.newDecoder()

      try {
        val reader = in.getCharReader(charset, postEvalState.bitPos)
        val bytes = in.getBytes(postEvalState.bitPos, nBytes.toInt)
        val cb = decoder.decode(ByteBuffer.wrap(bytes))
        val result = cb.toString
        val trimmedResult = trimByJustification(result)
        val endBitPos = postEvalState.bitPos + (nBytes.toInt * 8)
        val endCharPos = if (postEvalState.charPos == -1) result.length() else postEvalState.charPos + result.length()

        // We have a field, is it empty?
        val isFieldEmpty = trimmedResult.length == 0 //result.length() == 0

        if (isFieldEmpty && isEmptyAllowed) {
          // Valid!
          postEvalState.parentElement.makeNil()
          return postEvalState // Empty, no need to advance
        } else if (isFieldEmpty && !isEmptyAllowed) {
          // Fail!
          return PE(postEvalState, "%s - Empty field found but not allowed!", eName)
        } else if (d.get.isFieldDfdlLiteral(trimmedResult, nilValuesCooked.toSet)) {
          // Contains a nilValue, Success!
          postEvalState.parentElement.makeNil()

          log(LogLevel.Debug, "%s - Found %s", eName, trimmedResult)
          log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPos >> 3))
          log(LogLevel.Debug, "%s - Ended at bit position ", eName, endBitPos)

          return postEvalState.withPos(endBitPos, endCharPos, One(reader)) // Need to advance past found nilValue
        } else {
          // Fail!
          return PE(postEvalState, "%s - Does not contain a nil literal!", eName)
        }
      } catch {
        case e: IndexOutOfBoundsException => {
          // In this case, we failed to get the bytes
          if (isEmptyAllowed) {
            // Valid!
            postEvalState.parentElement.makeNil()
            return postEvalState // Empty, no need to advance
          } else {
            return PE(postEvalState, "%s - Insufficient Bytes in field; required %s", name, nBytes)
          }
        }
        case u: UnsuppressableException => throw u
        case e: Exception => { return PE(postEvalState, "%s - Exception: \n%s", name, e.getMessage()) }
      }
    }
  }

}