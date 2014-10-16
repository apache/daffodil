package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.TextReader
import edu.illinois.ncsa.daffodil.processors.PrimParser
import java.nio.charset.Charset
import edu.illinois.ncsa.daffodil.processors.DFDLDelimParser
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.processors.DelimParseFailure
import edu.illinois.ncsa.daffodil.processors.DelimParseSuccess
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.Padded
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.processors.EncodingInfo

class StringPatternMatchedParser(pattern: String,
  erd: ElementRuntimeData,
  override val encodingInfo: EncodingInfo,
  override val justificationTrim: TextJustificationType.Type,
  override val padChar: String)
  extends PrimParser(erd) with TextReader with HasPadding {

  // The pattern will always be defined

  lazy val dp = new ThreadLocal[DFDLDelimParser] {
    override def initialValue() = {
      new DFDLDelimParser(erd, encodingInfo)
    }
  }

  // TODO: Add parameter for changing CharBuffer size

  val eName = erd.name

  def parse(start: PState): PState = withParseErrorThrowing(start) {

    log(LogLevel.Debug, "StringPatternMatched - %s - Parsing pattern at byte position: %s", eName, (start.bitPos >> 3))
    log(LogLevel.Debug, "StringPatternMatched - %s - Parsing pattern at bit position: %s", eName, start.bitPos)

    // some encodings aren't whole bytes.
    // if (start.bitPos % 8 != 0) { return PE(start, "StringPatternMatched - not byte aligned.") }

    val bytePos = (start.bitPos >> 3).toInt

    log(LogLevel.Debug, "Retrieving reader")

    val reader = getReader(dcharset.charset, start.bitPos, start)

    val result = dp.get.parseInputPatterned(pattern, reader, start)

    val postState = result match {
      case _: DelimParseFailure => {
        // A no match means zero length.  
        // Because we check for Nil first, this is valid and allowed.
        // Since it's zero length, the start state is the end state. 
        start.simpleElement.setDataValue("") // empty string is the value.
        start
      }
      case s: DelimParseSuccess => {
        val endBitPos = start.bitPos + s.numBits
        log(LogLevel.Debug, "StringPatternMatched - Parsed: %s", s.field)
        log(LogLevel.Debug, "StringPatternMatched - Ended at bit position %s", endBitPos)

        val endCharPos = if (start.charPos == -1) s.field.length() else start.charPos + s.field.length()
        val field = trimByJustification(s.field)
        start.simpleElement.setDataValue(field)
        start.withPos(endBitPos, endCharPos, One(s.next))
      }

    }
    postState
  }
}