package edu.illinois.ncsa.daffodil.processors.parsers

import java.nio.charset.Charset
import java.nio.charset.MalformedInputException

import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.dsom.ListOfStringValueAsLiteral
import edu.illinois.ncsa.daffodil.dsom.SchemaComponent
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.processors.DFDLCharReader
import edu.illinois.ncsa.daffodil.processors.DFDLDelimParser
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

class StringDelimitedParser(
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  ff: FieldFactoryBase,
  isDelimRequired: Boolean,
  gram: Gram,
  contextArg: SchemaComponent)
  extends PrimParser(gram: Gram, contextArg: SchemaComponent)
  with TextReader {

  val e = contextArg.asInstanceOf[ElementBase]
  val eName = e.toString()
  val charset = e.knownEncodingCharset
  val tm = e.allTerminatingMarkup
  val cname = toString
  val dp = new DFDLDelimParser(e.knownEncodingStringBitLengthFunction)

  override def toBriefXML(depthLimit: Int = -1): String = {
    "<" + gram.name + " " + delimListForPrint + "/>"
  }

  lazy val delimListForPrint = tm.map { case (delimValue, _, _) => delimValue.prettyExpr }
  //    override def toString = cname + "(" + tm.map { _.prettyExpr } + ")"
  override def toString = cname + "(" + delimListForPrint + ")"

  def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): PState = {
    val res = {
      if (!parseResult.isDefined) this.PE(state, "%s - %s - Parse failed.", this.toString(), eName)
      else {
        val result = parseResult.get
        val field = result.field.getOrElse("")
        val numBits = result.numBits
        log(LogLevel.Debug, "%s - Parsed: %s Parsed Bytes: %s (bits %s)", eName, field, numBits / 8, numBits)
        val endCharPos = if (state.charPos == -1) result.numCharsRead else state.charPos + result.numCharsRead
        val endBitPos = state.bitPos + numBits
        val currentElement = state.parentElement
        currentElement.setDataValue(field)
        val stateWithPos = state.withPos(endBitPos, endCharPos, One(result.next))
        if (result.matchedDelimiterValue.isDefined) stateWithPos.mpstate.withDelimitedText(result.matchedDelimiterValue.get, result.originalDelimiterRep)
        return stateWithPos
      }
    }
    res
  }

  def parseMethod(reader: DFDLCharReader, delims: Seq[DFADelimiter], isDelimRequired: Boolean,
    escScheme: Maybe[EscapeScheme], delimsMatcher: DelimsMatcher, fieldDFA: DFAField): Maybe[dfa.ParseResult] = {

    val result: Maybe[dfa.ParseResult] = {
      if (escScheme.isDefined) {
        val scheme = escScheme.get
        scheme match {
          case s: EscapeSchemeBlock => {
            val parser = new TextDelimitedParserWithEscapeBlock(justificationTrim, pad,
              s.blockStartDFA, s.blockEndDFA, delims, s.fieldEscDFA, fieldDFA, e.knownEncodingStringBitLengthFunction)
            parser.parse(reader, isDelimRequired)
          }
          case s: EscapeSchemeChar => {
            val parser = new TextDelimitedParser(justificationTrim, pad, delims, fieldDFA, e.knownEncodingStringBitLengthFunction)
            parser.parse(reader, isDelimRequired)
          }
        }
      } else {
        val parser = new TextDelimitedParser(justificationTrim, pad, delims, fieldDFA, e.knownEncodingStringBitLengthFunction)
        parser.parse(reader, isDelimRequired)
      }
    }
    result
  }

  def parse(start: PState): PState = withParseErrorThrowing(start) {

    // TODO: DFDL-451 - Has been put on the backburner until we can figure out the appropriate behavior
    //
    //      gram.checkDelimiterDistinctness(esObj.escapeSchemeKind, optPadChar, finalOptEscChar,
    //        finalOptEscEscChar, optEscBlkStart, optEscBlkEnd, delimsCooked, postEscapeSchemeEvalState)

    val (postEvalState, delims, delimsMatcher, delimsCooked, fieldDFA, scheme) = ff.getFieldDFA(start)

    log(LogLevel.Debug, "%s - Looking for: %s Count: %s", eName, delimsCooked, delimsCooked.length)

    val bytePos = (postEvalState.bitPos >> 3).toInt
    log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, postEvalState.bitPos)
    log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

    val reader = getReader(charset, postEvalState.bitPos, postEvalState)
    val hasDelim = delimsCooked.length > 0

    start.mpstate.clearDelimitedText

    val result = try {
      parseMethod(reader, delims, isDelimRequired, scheme, delimsMatcher, fieldDFA)
    } catch {
      case mie: MalformedInputException =>
        throw new ParseError(e, Some(postEvalState), "Malformed input, length: %s", mie.getInputLength())
    }
    processResult(result, postEvalState)
  }
}

class LiteralNilDelimitedEndOfDataParser(justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  ff: FieldFactoryBase,
  gram: Gram,
  contextArg: SchemaComponent) extends StringDelimitedParser(justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  ff: FieldFactoryBase,
  false,
  gram: Gram,
  contextArg: SchemaComponent) {
  val nilValuesCooked = new ListOfStringValueAsLiteral(e.nilValue, e).cooked
  val isEmptyAllowed = e.nilValue.contains("%ES;") // TODO: move outside parser

  val isDelimRequired: Boolean = false

  override def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): PState = {
    val res = {
      if (!parseResult.isDefined) this.PE(state, "%s - %s - Parse failed.", this.toString(), eName)
      else {
        val result = parseResult.get
        // We have a field, is it empty?
        val field = result.field.getOrElse("")
        val isFieldEmpty = field.length() == 0 // Note: field has been stripped of padChars

        if (isFieldEmpty && !isEmptyAllowed) {
          return this.PE(state, "%s - %s - Parse failed.", this.toString(), eName)
        } else if ((isFieldEmpty && isEmptyAllowed) || // Empty, but must advance past padChars if there were any. 
          dp.isFieldDfdlLiteral(field, nilValuesCooked.toSet)) { // Not empty, but matches.
          // Contains a nilValue, Success!
          state.parentElement.makeNil()

          val numBits = result.numBits
          val endCharPos = if (state.charPos == -1) result.numCharsRead else state.charPos + result.numCharsRead
          val endBitPos = numBits + state.bitPos

          log(LogLevel.Debug, "%s - Found %s", eName, result.field)
          log(LogLevel.Debug, "%s - Ended at byte position %s", eName, (endBitPos >> 3))
          log(LogLevel.Debug, "%s - Ended at bit position ", eName, endBitPos)

          val stateWithPos = state.withPos(endBitPos, endCharPos, One(result.next))
          if (result.matchedDelimiterValue.isDefined) stateWithPos.mpstate.withDelimitedText(result.matchedDelimiterValue.get, result.originalDelimiterRep)
          return stateWithPos
        } else {
          // Fail!
          return this.PE(state, "%s - Does not contain a nil literal!", eName)
        }
      }
    }
    res
  }
}

class HexBinaryDelimitedParser(justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  ff: FieldFactoryBase,
  isDelimRequired: Boolean,
  gram: Gram,
  contextArg: SchemaComponent) extends StringDelimitedParser(justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  ff: FieldFactoryBase,
  isDelimRequired: Boolean,
  gram: Gram,
  contextArg: SchemaComponent) {
  override val charset: Charset = Charset.forName("ISO-8859-1")

  override def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): PState = {
    val res = {
      if (!parseResult.isDefined) this.PE(state, "%s - %s - Parse failed.", this.toString(), eName)
      else {
        val result = parseResult.get
        val field = result.field.getOrElse("")
        val numBits = e.knownEncodingStringBitLengthFunction(field)
        log(LogLevel.Debug, "%s - Parsed: %s Parsed Bytes: %s (bits %s)", eName, field, numBits / 8, numBits)
        val endCharPos = if (state.charPos == -1) result.numCharsRead else state.charPos + result.numCharsRead
        val endBitPos = state.bitPos + numBits
        val currentElement = state.parentElement
        val hexStr = field.map(c => c.toByte.formatted("%02X")).mkString
        currentElement.setDataValue(hexStr)
        val stateWithPos = state.withPos(endBitPos, endCharPos, One(result.next))
        if (result.matchedDelimiterValue.isDefined) stateWithPos.mpstate.withDelimitedText(result.matchedDelimiterValue.get, result.originalDelimiterRep)
        return stateWithPos
      }
    }
    res
  }
}

class HexBinaryDelimitedEndOfDataParser(
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  ff: FieldFactoryBase,
  gram: Gram,
  contextArg: SchemaComponent)
  extends HexBinaryDelimitedParser(
    justificationTrim: TextJustificationType.Type,
    pad: Maybe[Char],
    ff: FieldFactoryBase,
    false,
    gram: Gram,
    contextArg: SchemaComponent)

