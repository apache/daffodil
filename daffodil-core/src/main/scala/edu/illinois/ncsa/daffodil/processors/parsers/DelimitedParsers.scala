package edu.illinois.ncsa.daffodil.processors.parsers

import java.nio.charset.Charset
import java.nio.charset.MalformedInputException
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.dsom.ListOfStringValueAsLiteral
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.processors.DFDLCharReader
import edu.illinois.ncsa.daffodil.processors.DFDLDelimParser
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.EscapeScheme
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeBlock
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeChar
import edu.illinois.ncsa.daffodil.processors.FieldFactoryBase
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.ParseError
import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.processors.TextReader
import edu.illinois.ncsa.daffodil.processors.dfa
import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter
import edu.illinois.ncsa.daffodil.processors.dfa.DFAField
import edu.illinois.ncsa.daffodil.processors.dfa.DelimsMatcher
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParser
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParserWithEscapeBlock
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.util.Maybe.toMaybe
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParserFactoryBase
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.dsom.RuntimeEncodingMixin

class StringDelimitedParser(
  erd: ElementRuntimeData,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  ff: FieldFactoryBase,
  pf: TextDelimitedParserFactoryBase,
  isDelimRequired: Boolean,
  allTerminatingMarkup: List[(CompiledExpression, String, String)],
  dcharset: DFDLCharset,
  val knownEncodingIsFixedWidth: Boolean,
  val knownEncodingWidthInBits: Int,
  val knownEncodingName: String)
  extends PrimParser(erd)
  with TextReader
  with RuntimeEncodingMixin {

  lazy val delimListForPrint = allTerminatingMarkup.map { case (delimValue, _, _) => delimValue.prettyExpr }
  override def toBriefXML(depthLimit: Int = -1): String = {
    "<" + erd.prettyName + " " + delimListForPrint + "/>"
  }
  override def toString = erd.prettyName + "(" + delimListForPrint + ")"

  val dp = new DFDLDelimParser(knownEncodingIsFixedWidth, knownEncodingWidthInBits, knownEncodingName)

  def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): PState = {
    val res = {
      if (!parseResult.isDefined) this.PE(state, "%s - %s - Parse failed.", this.toString(), erd.prettyName)
      else {
        val result = parseResult.get
        val field = result.field.getOrElse("")
        val numBits = result.numBits
        val endCharPos = if (state.charPos == -1) result.numCharsRead else state.charPos + result.numCharsRead
        val endBitPos = state.bitPos + numBits
        state.simpleElement.setDataValue(field)
        val stateWithPos = state.withPos(endBitPos, endCharPos, One(result.next))
        if (result.matchedDelimiterValue.isDefined) stateWithPos.mpstate.withDelimitedText(result.matchedDelimiterValue.get, result.originalDelimiterRep)
        return stateWithPos
      }
    }
    res
  }

  def parse(start: PState): PState = withParseErrorThrowing(start) {

    // TODO: DFDL-451 - Has been put on the backburner until we can figure out the appropriate behavior
    //
    //      gram.checkDelimiterDistinctness(esObj.escapeSchemeKind, optPadChar, finalOptEscChar,
    //        finalOptEscEscChar, optEscBlkStart, optEscBlkEnd, delimsCooked, postEscapeSchemeEvalState)

    val (postEvalState, textParser, delimsCooked) = pf.getParser(start)

    val bytePos = (postEvalState.bitPos >> 3).toInt

    val reader = getReader(dcharset.charset, postEvalState.bitPos, postEvalState)
    val hasDelim = delimsCooked.length > 0

    start.mpstate.clearDelimitedText

    val result = try {
      textParser.parse(reader, isDelimRequired)
    } catch {
      case mie: MalformedInputException =>
        throw new ParseError(One(erd), Some(postEvalState), "Malformed input, length: %s", mie.getInputLength())
    }
    processResult(result, postEvalState)
  }
}

class LiteralNilDelimitedEndOfDataParser(
  erd: ElementRuntimeData,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  ff: FieldFactoryBase,
  pf: TextDelimitedParserFactoryBase,
  allTerminatingMarkup: List[(CompiledExpression, String, String)],
  nilValues: Seq[String],
  dcharset: DFDLCharset,
  knownEncodingIsFixedWidth: Boolean,
  knownEncodingWidthInBits: Int,
  knownEncodingName: String)
  extends StringDelimitedParser(erd, justificationTrim, pad, ff, pf, false, allTerminatingMarkup, dcharset, knownEncodingIsFixedWidth, knownEncodingWidthInBits, knownEncodingName) {

  val isEmptyAllowed = nilValues.contains("%ES;") // TODO: move outside parser

  val isDelimRequired: Boolean = false

  override def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): PState = {
    val res = {
      if (!parseResult.isDefined) this.PE(state, "%s - %s - Parse failed.", this.toString(), erd.prettyName)
      else {
        val result = parseResult.get
        // We have a field, is it empty?
        val field = result.field.getOrElse("")
        val isFieldEmpty = field.length() == 0 // Note: field has been stripped of padChars

        if (isFieldEmpty && !isEmptyAllowed) {
          return this.PE(state, "%s - %s - Parse failed.", this.toString(), erd.prettyName)
        } else if ((isFieldEmpty && isEmptyAllowed) || // Empty, but must advance past padChars if there were any. 
          dp.isFieldDfdlLiteral(field, nilValues.toSet)) { // Not empty, but matches.
          // Contains a nilValue, Success!
          state.thisElement.setNilled()

          val numBits = result.numBits
          val endCharPos = if (state.charPos == -1) result.numCharsRead else state.charPos + result.numCharsRead
          val endBitPos = numBits + state.bitPos

          val stateWithPos = state.withPos(endBitPos, endCharPos, One(result.next))
          if (result.matchedDelimiterValue.isDefined) stateWithPos.mpstate.withDelimitedText(result.matchedDelimiterValue.get, result.originalDelimiterRep)
          return stateWithPos
        } else {
          // Fail!
          return this.PE(state, "%s - Does not contain a nil literal!", erd.prettyName)
        }
      }
    }
    res
  }
}

class HexBinaryDelimitedParser(
  erd: ElementRuntimeData,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  ff: FieldFactoryBase,
  pf: TextDelimitedParserFactoryBase,
  isDelimRequired: Boolean,
  allTerminatingMarkup: List[(CompiledExpression, String, String)],
  knownEncodingIsFixedWidth: Boolean,
  knownEncodingWidthInBits: Int,
  knownEncodingName: String)
  extends StringDelimitedParser(erd, justificationTrim, pad, ff, pf, isDelimRequired, allTerminatingMarkup, new DFDLCharset("ISO-8859-1"), knownEncodingIsFixedWidth, knownEncodingWidthInBits, knownEncodingName) {

  // override val charset: Charset = Charset.forName("ISO-8859-1")

  override def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): PState = {
    val res = {
      if (!parseResult.isDefined) this.PE(state, "%s - %s - Parse failed.", this.toString(), erd.prettyName)
      else {
        val result = parseResult.get
        val field = result.field.getOrElse("")
        val numBits = knownEncodingStringBitLength(field)
        val endCharPos = if (state.charPos == -1) result.numCharsRead else state.charPos + result.numCharsRead
        val endBitPos = state.bitPos + numBits
        val hexStr = field.map(c => c.toByte.formatted("%02X")).mkString
        state.simpleElement.setDataValue(hexStr)
        val stateWithPos = state.withPos(endBitPos, endCharPos, One(result.next))
        if (result.matchedDelimiterValue.isDefined) stateWithPos.mpstate.withDelimitedText(result.matchedDelimiterValue.get, result.originalDelimiterRep)
        return stateWithPos
      }
    }
    res
  }
}
