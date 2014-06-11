package edu.illinois.ncsa.daffodil.processors

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.charset.MalformedInputException
import scala.Array.canBuildFrom
import scala.util.parsing.input.Reader
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }
import edu.illinois.ncsa.daffodil.processors.dfa.DFA
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParser
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParserWithEscapeBlock
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.processors.dfa.CreateFieldDFA
import edu.illinois.ncsa.daffodil.processors.dfa.CreateDelimiterMatcher
import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter
import edu.illinois.ncsa.daffodil.processors.dfa.CreateFieldDFA
import edu.illinois.ncsa.daffodil.processors.dfa.CreateFieldDFA
import edu.illinois.ncsa.daffodil.processors.dfa.CreateFieldDFA
import edu.illinois.ncsa.daffodil.processors.dfa.Registers
import edu.illinois.ncsa.daffodil.processors.dfa.CreateDelimiterDFA
import edu.illinois.ncsa.daffodil.processors.dfa.TextPaddingParser
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthKind
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind._
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.processors.dfa.DelimsMatcher
import edu.illinois.ncsa.daffodil.processors.dfa.DFAField

abstract class StringLength(e: ElementBase)
  extends DelimParserBase(e, true)
  with TextReader
  with Padded
  with WithParseErrorThrowing {

  val charset = e.knownEncodingCharset
  val stringLengthInBitsFnc = e.knownEncodingStringBitLengthFunction
  val codepointWidth = e.knownEncodingWidthInBits

  def lengthText: String
  def parserName: String

  def getLength(pstate: PState): (Long, PState)
  def parseInput(start: PState, charset: Charset, nBytes: Long): PState

  def parser: DaffodilParser = new PrimParser(this, e) {
    override def toString = String.format("%sParser(%s)", parserName, lengthText)

    def parse(pstate: PState): PState = withParseErrorThrowing(pstate) {

      log(LogLevel.Debug, "Parsing starting at bit position: %s", pstate.bitPos)

      val (nBytes, start) = getLength(pstate)
      log(LogLevel.Debug, "Explicit length %s", nBytes)

      if (start.bitPos % 8 != 0) { return PE(start, "%s - not byte aligned.", parserName) }

      try {
        val postState = parseInput(start, charset, nBytes)
        return postState
      } catch {
        case m: MalformedInputException => { return PE(start, "%s - MalformedInputException: \n%s", parserName, m.getMessage()) }
        case e: IndexOutOfBoundsException => {
          return PE(start, "%s - Insufficient Bits in field: IndexOutOfBounds: \n%s", parserName, e.getMessage())
        }
        case u: UnsuppressableException => throw u
        case e: Exception => { return PE(start, "%s - Exception: \n%s", parserName, e.getStackTraceString) }
      }
      pstate
    }
  }

  def unparser: Unparser = new Unparser(e) {
    override def toString = String.format("%sUnparser(%s)", parserName, lengthText)
    //    val encoder = e.knownEncodingEncoder

    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

trait VariableLength { self: Terminal =>
  // Length is an expression
  private lazy val eb = self.context.asInstanceOf[ElementBase]
  val expr = eb.length
  val exprText = expr.prettyExpr

  def getLength(pstate: PState): (Long, PState) = {
    val R(lengthAsAny, newVMap) = expr.evaluate(pstate.parentElement, pstate.variableMap, pstate)
    val length = lengthAsAny.asInstanceOf[Long]
    val start = pstate.withVariables(newVMap)
    (length, start)
  }
}

trait FixedLength { self: Terminal =>
  // Length is a constant
  private lazy val eb = self.context.asInstanceOf[ElementBase]
  //Assert.invariant(eb.knownEncodingWidthInBits != -1)
  //  def getLength(pstate: PState): (Long, PState) = {
  //    (eb.fixedLength, pstate)
  //  }
}

abstract class StringLengthInChars(e: ElementBase, nChars: Long)
  extends StringLength(e)
  with WithParseErrorThrowing {

  def getLength(pstate: PState): (Long, PState) = {
    (nChars, pstate)
  }

  def parseInput(start: PState, charset: Charset, nChars: Long): PState = start

  override def parser: DaffodilParser = new PrimParser(this, e) {
    String.format("%sParser(%s)", parserName, lengthText)

    def parse(start: PState): PState = withParseErrorThrowing(start) {

      log(LogLevel.Debug, "Parsing starting at bit position: %s", start.bitPos)

      // no longer require alignment (some encodings aren't whole bytes)
      // if (start.bitPos % 8 != 0) { return PE(start, "StringFixedLengthInVariableWidthCharacters - not byte aligned.") }

      log(LogLevel.Debug, "Retrieving reader")

      val reader = getReader(charset, start.bitPos, start)

      val field = reader.getStringInChars(nChars.toInt).toString()
      val fieldLength = field.length

      if (fieldLength != nChars.toInt) {
        return PE(start, "Parse failed to find exactly %s characters.", nChars)
      } else {
        val parsedField = trimByJustification(field)
        val parsedBits = e.knownEncodingStringBitLengthFunction(field)
        val endBitPos = start.bitPos + parsedBits

        log(LogLevel.Debug, "Parsed: %s", field)
        log(LogLevel.Debug, "Ended at bit position: %s", endBitPos)

        val endCharPos = if (start.charPos == -1) nChars else start.charPos + nChars
        val currentElement = start.parentElement
        currentElement.setDataValue(parsedField)

        val nextReader = reader.atBitPos(endBitPos)
        val postState = start.withPos(endBitPos, endCharPos, One(nextReader))
        return postState
      }
    }
  }

}

abstract class StringLengthInBytes(e: ElementBase)
  extends StringLength(e) {

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
    val endBitPos = start.bitPos + stringLengthInBitsFnc(result)
    log(LogLevel.Debug, "Parsed: " + result)
    log(LogLevel.Debug, "Ended at bit position " + endBitPos)
    val endCharPos = start.charPos + result.length
    // 
    // Maintain our global count of number of characters.
    // TODO: get rid of global counter for a dataProcessor-saved one. 
    // 
    DFDLCharCounter.incr(result.length)

    val currentElement = start.parentElement
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

abstract class HexBinaryLengthInBytes(e: ElementBase)
  extends StringLengthInBytes(e) {

  override val charset: Charset = Charset.forName("ISO-8859-1")
  override val stringLengthInBitsFnc = {
    def stringBitLength(str: String) = {
      // variable width encoding, so we have to convert each character 
      // We assume here that it will be a multiple of bytes
      // that is, that variable-width encodings are all some number
      // of bytes.
      str.getBytes(charset).length * 8
    }
    stringBitLength _
  }
  override def formatValue(value: String) = {
    val hexStr = value.map(c => c.toByte.formatted("%02X")).mkString
    hexStr
  }
}

case class HexBinaryFixedLengthInBytes(e: ElementBase, nBytes: Long)
  extends HexBinaryLengthInBytes(e) with FixedLength {

  lazy val parserName = "HexBinaryFixedLengthInBytes"
  lazy val lengthText = e.length.constantAsString

  def getLength(pstate: PState): (Long, PState) = {
    (nBytes, pstate)
  }
}

case class HexBinaryFixedLengthInBits(e: ElementBase, nBits: Long)
  extends HexBinaryLengthInBytes(e) with FixedLength {

  lazy val parserName = "HexBinaryFixedLengthInBits"
  lazy val lengthText = e.length.constantAsString

  def getLength(pstate: PState): (Long, PState) = {
    val nBytes = scala.math.ceil(nBits / 8).toLong
    (nBytes, pstate)
  }
}

case class HexBinaryVariableLengthInBytes(e: ElementBase)
  extends HexBinaryLengthInBytes(e) with VariableLength {

  lazy val parserName = "HexBinaryVariableLengthInBytes"
  lazy val lengthText = exprText
}

case class StringFixedLengthInBytesFixedWidthCharacters(e: ElementBase, nBytes: Long)
  extends StringLengthInBytes(e)
  with FixedLength {

  lazy val parserName = "StringFixedLengthInBytesFixedWidthCharacters"
  lazy val lengthText = e.length.constantAsString

  def getLength(pstate: PState): (Long, PState) = {
    (nBytes, pstate)
  }
  // val maxBytes = DaffodilTunableParameters.maxFieldContentLengthInBytes
  //  var cbuf: CharBuffer = CharBuffer.allocate(0) // TODO: Performance: get a char buffer from a pool.
  //  var cbufSize = 0
}

case class StringFixedLengthInBytesVariableWidthCharacters(e: ElementBase, nBytes: Long)
  extends StringLengthInBytes(e)
  with FixedLength {

  lazy val parserName = "StringFixedLengthInBytesVariableWidthCharacters"
  lazy val lengthText = nBytes.toString()

  def getLength(pstate: PState): (Long, PState) = {
    (nBytes, pstate)
  }

  // val maxBytes = DaffodilTunableParameters.maxFieldContentLengthInBytes
  //  var cbuf: CharBuffer = CharBuffer.allocate(0) // TODO: Performance: get a char buffer from a pool.
  //  var cbufSize = 0
}

case class StringFixedLengthInVariableWidthCharacters(e: ElementBase, numChars: Long)
  extends StringLengthInChars(e, numChars)
  with FixedLength {

  lazy val parserName = "StringFixedLengthInVariableWidthCharacters"
  lazy val lengthText = e.length.constantAsString

  // val maxBytes = DaffodilTunableParameters.maxFieldContentLengthInBytes
  //  var cbuf: CharBuffer = CharBuffer.allocate(0) // TODO: Performance: get a char buffer from a pool.
  //  var cbufSize = 0
}

case class StringVariableLengthInBytes(e: ElementBase)
  //extends Terminal(e, true)
  extends StringLengthInBytes(e)
  with VariableLength {

  lazy val parserName = "StringVariableLengthInBytes"
  lazy val lengthText = exprText

  // val maxBytes = DaffodilTunableParameters.maxFieldContentLengthInBytes
  //  var cbuf: CharBuffer = CharBuffer.allocate(0) // TODO: Performance: get a char buffer from a pool.
  //  var cbufSize = 0
}

case class StringVariableLengthInBytesVariableWidthCharacters(e: ElementBase)
  //extends Terminal(e, true)
  extends StringLengthInBytes(e)
  with VariableLength {

  lazy val parserName = "StringVariableLengthInBytesVariableWidthCharacters"
  lazy val lengthText = exprText

  // val maxBytes = DaffodilTunableParameters.maxFieldContentLengthInBytes
  //  var cbuf: CharBuffer = CharBuffer.allocate(0) // TODO: Performance: get a char buffer from a pool.
  //  var cbufSize = 0
}

case class StringVariableLengthInVariableWidthCharacters(e: ElementBase)
  extends StringLengthInBytes(e)
  with VariableLength {

  lazy val parserName = "StringVariableLengthInVariableWidthCharacters"
  lazy val lengthText = e.length.constantAsString

  // val maxBytes = DaffodilTunableParameters.maxFieldContentLengthInBytes
  //  var cbuf: CharBuffer = CharBuffer.allocate(0) // TODO: Performance: get a char buffer from a pool.
  //  var cbufSize = 0
}

case class StringPatternMatched(e: ElementBase)
  extends Terminal(e, true)
  with WithParseErrorThrowing with TextReader with Padded {

  val charset = e.knownEncodingCharset
  val pattern = e.lengthPattern

  def parser: DaffodilParser = new PrimParser(this, e) {
    override def toString = "StringPatternMatched"

    // The pattern will always be defined

    lazy val dp = new ThreadLocal[DFDLDelimParser] {
      override def initialValue() = {
        new DFDLDelimParser(e.knownEncodingStringBitLengthFunction)
      }
    }

    // TODO: Add parameter for changing CharBuffer size

    val eName = e.toString()

    def parse(start: PState): PState = withParseErrorThrowing(start) {
      // withLoggingLevel(LogLevel.Info) 
      {

        log(LogLevel.Debug, "StringPatternMatched - %s - Parsing pattern at byte position: %s", eName, (start.bitPos >> 3))
        log(LogLevel.Debug, "StringPatternMatched - %s - Parsing pattern at bit position: %s", eName, start.bitPos)

        // some encodings aren't whole bytes.
        // if (start.bitPos % 8 != 0) { return PE(start, "StringPatternMatched - not byte aligned.") }

        val bytePos = (start.bitPos >> 3).toInt

        log(LogLevel.Debug, "Retrieving reader")

        val reader = getReader(charset, start.bitPos, start)

        val result = dp.get.parseInputPatterned(pattern, reader, start)

        val postState = result match {
          case _: DelimParseFailure => {
            // A no match means zero length.  
            // Because we check for Nil first, this is valid and allowed.
            // Since it's zero length, the start state is the end state. 
            val currentElement = start.parentElement
            currentElement.setDataValue("") // empty string is the value.
            start
          }
          case s: DelimParseSuccess => {
            val endBitPos = start.bitPos + s.numBits
            log(LogLevel.Debug, "StringPatternMatched - Parsed: %s", s.field)
            log(LogLevel.Debug, "StringPatternMatched - Ended at bit position %s", endBitPos)

            val endCharPos = if (start.charPos == -1) s.field.length() else start.charPos + s.field.length()
            val currentElement = start.parentElement
            val field = trimByJustification(s.field)
            currentElement.setDataValue(field)
            start.withPos(endBitPos, endCharPos, One(s.next))
          }

        }
        postState
      }
    }
  }

  def unparser: Unparser = new Unparser(e) {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

class Delimiters(delimDFAs: DFADelimiter, delims: List[String])

/**
 * Here allTerminatingMarkup is a List[(CompiledExpression, ElementName, ElementPath)]
 */
sealed abstract class DelimiterFactoryBase(allTerminatingMarkup: List[(CompiledExpression, String, String)],
  context: ThrowsSDE, elemBase: ElementBase) extends Logging {

  // These static delims are used whether we're static or dynamic
  // because even a dynamic can have some static from enclosing scopes.
  val staticDelimsRaw = allTerminatingMarkup.filter {
    case (delimValue, _, _) => delimValue.isConstant
  }.map {
    case (delimValue, _, _) => delimValue.constantAsString
  }

  val dynamicDelimsRaw = allTerminatingMarkup.filter { case (delimValue, _, _) => !delimValue.isConstant }

  val staticDelimsCooked = staticDelimsRaw.map(raw => { new ListOfStringValueAsLiteral(raw.toString, elemBase).cooked }).flatten
  val staticDelimsDFAs = { CreateDelimiterDFA(staticDelimsCooked) }

  /**
   * Called at compile time in static case, at runtime for dynamic case.
   */
  def errorIfDelimsHaveWSPStar(delims: List[String], ctxt: ThrowsSDE): Unit = {
    if (delims.filter(x => x == "%WSP*;").length > 0) {
      // We cannot detect this error until expressions have been evaluated!
      log(LogLevel.Debug, "%s - Failed due to WSP* detected as a delimiter for lengthKind=delimited.", elemBase.toString())
      ctxt.schemaDefinitionError("WSP* cannot be used as a delimiter when lengthKind=delimited.")
    }
  }

  protected def constEval(knownValue: String) = {
    val l = new StringValueAsLiteral(knownValue, context)
    val constValue = l.cooked
    constValue
  }

  def getDelims(pstate: PState, elemBase: ElementBase): (Seq[DFADelimiter], DelimsMatcher, List[String], Option[VariableMap])

}

case class DelimiterFactoryStatic(allTerminatingMarkup: List[(CompiledExpression, String, String)],
  context: ThrowsSDE,
  elemBase: ElementBase)
  extends DelimiterFactoryBase(allTerminatingMarkup, context, elemBase) {

  errorIfDelimsHaveWSPStar(staticDelimsCooked, elemBase)

  val delimsMatcher = CreateDelimiterMatcher(staticDelimsDFAs)

  def getDelims(pstate: PState, elemBase: ElementBase): (Seq[DFADelimiter], DelimsMatcher, List[String], Option[VariableMap]) = {
    (staticDelimsDFAs, delimsMatcher, staticDelimsCooked, Some(pstate.variableMap))
  }
}

case class DelimiterFactoryDynamic(allTerminatingMarkup: List[(CompiledExpression, String, String)],
  context: ThrowsSDE,
  elemBase: ElementBase)
  extends DelimiterFactoryBase(allTerminatingMarkup, context, elemBase)
  with Dynamic {

  val tmCE = allTerminatingMarkup.map { case (delimValue, _, _) => delimValue }
  val dynamicDelimsCached: List[CachedDynamic[String]] = cacheConstantExpression(tmCE) {
    (a: Any) => constEval(a.asInstanceOf[String])
  }

  def getDelimValue(knownValue: String, state: PState): String = {
    val l = new StringValueAsLiteral(knownValue, state)
    l.cooked
  }

  def getDelims(pstate: PState, elemBase: ElementBase): (Seq[DFADelimiter], DelimsMatcher, List[String], Option[VariableMap]) = {

    // Evaluate dynamic delimiters if they exist
    val (afterDelimEval, dynamicDelimsCooked) = evalWithConversion(pstate, dynamicDelimsCached) {
      (s: PState, c: Any) =>
        {
          getDelimValue(c.asInstanceOf[String], s)
        }
    }
    val vars = afterDelimEval.variableMap

    // Combine dynamic and with static delims if they exist
    val delimsCooked = dynamicDelimsCooked.union(staticDelimsCooked)

    errorIfDelimsHaveWSPStar(delimsCooked, pstate)

    val dynamicDelimsDFAs = CreateDelimiterDFA(dynamicDelimsCooked)
    val delimDFAs = dynamicDelimsDFAs.union(staticDelimsDFAs)
    val delimsMatcher = CreateDelimiterMatcher(delimDFAs)

    (delimDFAs, delimsMatcher, delimsCooked, Some(vars))
  }

}

trait HasEscapeScheme { self: StringDelimited =>

  var escEscChar: Maybe[Char] = Nope
  var escChar: Maybe[Char] = Nope
  var blockStart: Option[DFADelimiter] = None
  var blockEnd: Option[DFADelimiter] = None

  protected def constEval(knownValue: Option[String]) = {
    val optConstValue = knownValue match {
      case None => None
      case Some(constValue) => {
        val l = new SingleCharacterLiteralES(constValue, context)
        val result = l.cooked
        One(result)
      }
    }
    optConstValue
  }

  protected def evalAsConstant(knownValue: Maybe[CompiledExpression]) = {
    if (!knownValue.isDefined) Nope
    else if (knownValue.get.isConstant) {
      val constValue = knownValue.get.constantAsString
      val l = new SingleCharacterLiteralES(constValue, context)
      val result = l.cooked
      One(result)
    } else Nope
  }

  protected def runtimeEvalEsc(optEsc: Maybe[CompiledExpression], state: PState): (Maybe[String], PState) = {
    val (finalOptEsc, afterEscEval) =
      if (!optEsc.isDefined) (Nope, state)
      else {
        val R(res, newVMap) = optEsc.get.evaluate(state.parentElement, state.variableMap, state)
        val l = new SingleCharacterLiteralES(res.toString, context)
        val resultEsc = l.cooked
        val newState = state.withVariables(newVMap)
        (One(resultEsc), newState)
      }
    (finalOptEsc, afterEscEval)
  }

}

sealed abstract class EscapeScheme
case class EscapeSchemeChar(private val escChar: Maybe[String], private val escEscChar: Maybe[String])
  extends EscapeScheme {
  val ec: Maybe[Char] = if (escChar.isDefined) One(escChar.get.charAt(0)) else Nope
  val eec: Maybe[Char] = if (escEscChar.isDefined) One(escEscChar.get.charAt(0)) else Nope
}
case class EscapeSchemeBlock(private val escEscChar: Maybe[String],
  blockStart: String,
  blockEnd: String)
  extends EscapeScheme {
  // Should note there that fieldDFA (not here) is dependent on
  // the whether or not the delimiters are constant or not.
  // As a result, it cannot be generated here.

  val eec: Maybe[Char] = if (escEscChar.isDefined) One(escEscChar.get.charAt(0)) else Nope
  val blockStartDFA: DFADelimiter = CreateDelimiterDFA(blockStart)
  val blockEndDFA: DFADelimiter = CreateDelimiterDFA(blockEnd)
  val blockEndMatcher = CreateDelimiterMatcher(Seq(blockEndDFA))
  val fieldEscDFA = CreateFieldDFA(blockEndMatcher, eec)

}

/**
 * What's going on here is that we have three factory classes:
 *
 * 1. EscapeScheme
 * 2. Delimiters
 * 3. Field
 *
 * Field depends on Delimiters and EscapeScheme. As such it needs to handle
 * dynamic/static cases. Delimiters can be static or dynamic and so need to
 * be generated prior to Field. EscapeScheme can also be static or
 * dynamic and so needs to also be generated prior to Field.
 *
 * We use information regarding whether or not we have Static vs. Dynamic
 * Delimiters/EscapeScheme to determine if we can statically generate the
 * Field. So there is some cascading going on here as a result of these
 * dependencies.
 */
sealed abstract class FieldFactoryBase(ef: Option[EscapeSchemeFactoryBase],
  df: DelimiterFactoryBase,
  context: ThrowsSDE,
  elemBase: ElementBase) {

  def getFieldDFA(state: PState): (PState, Seq[DFADelimiter], DelimsMatcher, List[String], DFAField, Option[EscapeScheme])
}
case class FieldFactoryStatic(ef: Option[EscapeSchemeFactoryStatic],
  df: DelimiterFactoryStatic,
  context: ThrowsSDE,
  elemBase: ElementBase)
  extends FieldFactoryBase(ef, df, context, elemBase) {

  val fieldDFA = {
    val dfa =
      if (ef.isDefined) {
        val scheme = ef.get.theScheme
        val res = scheme match {
          case s: EscapeSchemeBlock => CreateFieldDFA(df.delimsMatcher)
          case s: EscapeSchemeChar => CreateFieldDFA(df.delimsMatcher, s.ec, s.eec)
        }
        res
      } else {
        CreateFieldDFA(df.delimsMatcher)
      }
    dfa
  }

  def getFieldDFA(state: PState): (PState, Seq[DFADelimiter], DelimsMatcher, List[String], DFAField, Option[EscapeScheme]) = {
    val theScheme = if (ef.isDefined) {
      Some(ef.get.theScheme)
    } else None
    (state, df.staticDelimsDFAs, df.delimsMatcher, df.staticDelimsCooked, fieldDFA, theScheme)
  }
}
case class FieldFactoryDynamic(ef: Option[EscapeSchemeFactoryBase],
  df: DelimiterFactoryBase,
  context: ThrowsSDE,
  elemBase: ElementBase)
  extends FieldFactoryBase(ef, df, context, elemBase) {

  def getFieldDFA(start: PState): (PState, Seq[DFADelimiter], DelimsMatcher, List[String], DFAField, Option[EscapeScheme]) = {
    val (postEscapeSchemeEvalState, scheme) = {
      if (ef.isDefined) {
        val factory = ef.get
        val (state, escScheme) = factory.getEscapeScheme(start)
        (state, Some(escScheme))
      } else (start, None)
    }

    val (delims, delimsMatcher, delimsCooked, vars) = df.getDelims(postEscapeSchemeEvalState, elemBase)

    // We must feed variable context out of one evaluation and into the next.
    // So that the resulting variable map has the updated status of all evaluated variables.
    val postEvalState = vars match {
      case Some(v) => postEscapeSchemeEvalState.withVariables(v)
      case None => postEscapeSchemeEvalState
    }

    val fieldDFA =
      if (scheme.isDefined) {
        val theScheme = scheme.get
        val res = theScheme match {
          case s: EscapeSchemeBlock => CreateFieldDFA(delimsMatcher)
          case s: EscapeSchemeChar => CreateFieldDFA(delimsMatcher, s.ec, s.eec)
        }
        res
      } else {
        CreateFieldDFA(delimsMatcher)
      }
    (postEvalState, delims, delimsMatcher, delimsCooked, fieldDFA, scheme)
  }
}

abstract class EscapeSchemeFactoryBase(scheme: DFDLEscapeScheme,
  context: ThrowsSDE) {
  protected def constEval(knownValue: Option[String]) = {
    val optConstValue = knownValue match {
      case None => None
      case Some(constValue) => {
        val l = new SingleCharacterLiteralES(constValue, context)
        val result = l.cooked
        Some(result)
      }
    }
    optConstValue
  }

  protected def evalAsConstant(knownValue: Option[CompiledExpression]) = {
    knownValue match {
      case None => None
      case Some(ce) if ce.isConstant => {
        val constValue = ce.constantAsString
        val l = new SingleCharacterLiteralES(constValue, context)
        val result = l.cooked
        Some(result)
      }
      case Some(_) => None
    }
  }

  protected def constEval(knownValue: String, context: ThrowsSDE) = {
    val l = new SingleCharacterLiteralES(knownValue, context)
    val result = l.cooked
    result
  }

  protected def getOptEscChar = {
    scheme.escapeKind match {
      case EscapeKind.EscapeBlock => None
      case EscapeKind.EscapeCharacter => {
        if (!scheme.optionEscapeCharacter.isDefined) {
          context.SDE("escapeCharacter cannot be the empty string when EscapeSchemeKind is Character.")
        }
        scheme.optionEscapeCharacter
      }
    }
  }

  protected def getOptEscEscChar = {
    scheme.optionEscapeEscapeCharacter
  }

  protected def getEscValue(escChar: String, context: ThrowsSDE): String = {
    val l = new SingleCharacterLiteralES(escChar, context).cooked
    l
  }

  protected def getBlockStart: String = {
    if (scheme.escapeKind == EscapeKind.EscapeCharacter) return ""
    if (scheme.escapeBlockStart == "") { context.SDE("escapeBlockStart cannot be the empty string when EscapeSchemeKind is Block.") }

    val bs = new StringValueAsLiteral(scheme.escapeBlockStart, context).cooked
    bs
  }

  protected def getBlockEnd: String = {
    if (scheme.escapeKind == EscapeKind.EscapeCharacter) return ""
    if (scheme.escapeBlockEnd == "") { context.SDE("escapeBlockEnd cannot be the empty string when EscapeSchemeKind is Block.") }

    val be = new StringValueAsLiteral(scheme.escapeBlockEnd, context).cooked
    be
  }

  def getEscapeScheme(state: PState): (PState, EscapeScheme)

}
class EscapeSchemeFactoryStatic(scheme: DFDLEscapeScheme,
  context: ThrowsSDE)
  extends EscapeSchemeFactoryBase(scheme, context) {

  val escChar = evalAsConstant(getOptEscChar)
  val escEscChar = evalAsConstant(getOptEscEscChar)
  val blockStart = getBlockStart
  val blockEnd = getBlockEnd

  def generateEscapeScheme: EscapeScheme = {
    val result = scheme.escapeKind match {
      case EscapeKind.EscapeBlock => new EscapeSchemeBlock(escEscChar, blockStart, blockEnd)
      case EscapeKind.EscapeCharacter => new EscapeSchemeChar(escChar, escEscChar)
    }
    result
  }

  val theScheme = generateEscapeScheme

  def getEscapeScheme(state: PState) = {
    (state, theScheme)
  }
}

class EscapeSchemeFactoryDynamic(scheme: DFDLEscapeScheme,
  context: ThrowsSDE)
  extends EscapeSchemeFactoryBase(scheme, context) with Dynamic {

  val escapeCharacterCached: Maybe[CachedDynamic[String]] = {
    scheme.escapeKind match {
      case EscapeKind.EscapeBlock => // do nothing
      case EscapeKind.EscapeCharacter => {
        if (!scheme.optionEscapeCharacter.isDefined) {
          context.SDE("escapeCharacter cannot be the empty string when EscapeSchemeKind is Character.")
        }
      }
    }
    val ec = scheme.optionEscapeCharacter match {
      case None => Nope
      case Some(c) => One(c)
    }
    cacheConstantExpression(ec) {
      (a: Any) => constEval(a.asInstanceOf[String], context)
    }
  }

  val escapeEscapeCharacterCached: Maybe[CachedDynamic[String]] = cacheConstantExpression(scheme.optionEscapeEscapeCharacter) {
    (a: Any) => constEval(a.asInstanceOf[String], context)
  }

  val blockStart = getBlockStart
  val blockEnd = getBlockEnd

  def getEscapeScheme(state: PState) = {
    val (finalState, theScheme) = scheme.escapeKind match {
      case EscapeKind.EscapeCharacter => {
        val (afterEscCharEval, finalOptEscChar) = evalWithConversion(state, escapeCharacterCached) {
          (s: PState, c: Any) =>
            {
              getEscValue(c.asInstanceOf[String], s)
            }
        }
        val (afterEscEscCharEval, finalOptEscEscChar) = evalWithConversion(afterEscCharEval, escapeEscapeCharacterCached) {
          (s: PState, c: Any) =>
            {
              getEscValue(c.asInstanceOf[String], s)
            }
        }
        (afterEscEscCharEval, new EscapeSchemeChar(finalOptEscChar, finalOptEscEscChar))
      }
      case EscapeKind.EscapeBlock => {
        val (afterEscEscCharEval, finalOptEscEscChar) = evalWithConversion(state, escapeEscapeCharacterCached) {
          (s: PState, c: Any) =>
            {
              getEscValue(c.asInstanceOf[String], s)
            }
        }
        (afterEscEscCharEval, new EscapeSchemeBlock(finalOptEscEscChar, blockStart, blockEnd))
      }
    }
    (finalState, theScheme)
  }
}

abstract class StringDelimited(e: ElementBase)
  extends DelimParserBase(e, true)
  with TextReader
  with Padded
  with HasEscapeScheme
  with WithParseErrorThrowing {

  // TODO: DFDL-451 - Has been placed on the backburner until we can figure out the appropriate behavior
  //
  //  requiredEvaluations(gram.checkDelimiterDistinctness(esObj.escapeSchemeKind, optPadChar, optEscChar,
  //    optEscEscChar, optEscBlkStart, optEscBlkEnd, staticDelimsCooked, elemBase))

  def isDelimRequired: Boolean

  val es = e.optionEscapeScheme

  val tm = e.allTerminatingMarkup
  val cname = toString

  val eName = e.toString()
  val charset = e.knownEncodingCharset
  val elemBase = e

  val hasDynamicDelims: Boolean =
    e.allTerminatingMarkup.exists { case (delimValue, _, _) => !delimValue.isConstant }

  val escf = escSchemeFactory
  val df = delimiterFactory
  val ff = fieldFactory

  val pad: Maybe[Char] = if (padChar.isEmpty()) Nope else One(padChar.charAt(0))

  val leftPaddingOpt: Option[TextPaddingParser] = {
    if (!pad.isDefined) None
    else Some(new TextPaddingParser(pad.get, e.knownEncodingStringBitLengthFunction))
  }

  def escSchemeFactory: Option[EscapeSchemeFactoryBase] = {
    if (es.isDefined) {
      val scheme = es.get
      val isConstant = scheme.escapeKind match {
        case EscapeKind.EscapeBlock => {
          (scheme.optionEscapeEscapeCharacter.isEmpty ||
            scheme.optionEscapeEscapeCharacter.get.isConstant)
        }
        case EscapeKind.EscapeCharacter => {
          (scheme.optionEscapeCharacter.isEmpty ||
            scheme.optionEscapeCharacter.get.isConstant) &&
            (scheme.optionEscapeEscapeCharacter.isEmpty ||
              scheme.optionEscapeEscapeCharacter.get.isConstant)
        }
      }
      val theScheme =
        if (isConstant) new EscapeSchemeFactoryStatic(scheme, context)
        else new EscapeSchemeFactoryDynamic(scheme, context)
      Some(theScheme)
    } else None
  }

  def delimiterFactory: DelimiterFactoryBase = {
    val factory =
      if (hasDynamicDelims) {
        new DelimiterFactoryDynamic(e.allTerminatingMarkup, context, elemBase)
      } else {
        new DelimiterFactoryStatic(e.allTerminatingMarkup, context, elemBase)
      }
    factory
  }

  def fieldFactory: FieldFactoryBase = {
    val hasDynamicEscapeScheme =
      if (escf.isDefined) escf.get.isInstanceOf[EscapeSchemeFactoryDynamic]
      else false

    val fieldDFAFact =
      if (!hasDynamicDelims && !hasDynamicEscapeScheme)
        new FieldFactoryStatic(escf.asInstanceOf[Option[EscapeSchemeFactoryStatic]], df.asInstanceOf[DelimiterFactoryStatic], context, elemBase)
      else new FieldFactoryDynamic(escf, df, context, elemBase)
    fieldDFAFact
  }

  def parseMethod(reader: DFDLCharReader, delims: Seq[DFADelimiter], isDelimRequired: Boolean,
    escScheme: Maybe[EscapeScheme], delimsMatcher: DelimsMatcher, fieldDFA: DFAField): Maybe[dfa.ParseResult] = {

    val result: Maybe[dfa.ParseResult] = {
      if (escScheme.isDefined) {
        val scheme = escScheme.get
        scheme match {
          case s: EscapeSchemeBlock => {
            val parser = new TextDelimitedParserWithEscapeBlock(justificationTrim, pad,
              s.blockStartDFA, s.blockEndDFA, delims, s.fieldEscDFA, fieldDFA, elemBase.knownEncodingStringBitLengthFunction)
            parser.parse(reader, isDelimRequired)
          }
          case s: EscapeSchemeChar => {
            val parser = new TextDelimitedParser(justificationTrim, pad, delims, fieldDFA, elemBase.knownEncodingStringBitLengthFunction)
            parser.parse(reader, isDelimRequired)
          }
        }
      } else {
        val parser = new TextDelimitedParser(justificationTrim, pad, delims, fieldDFA, elemBase.knownEncodingStringBitLengthFunction)
        parser.parse(reader, isDelimRequired)
      }
    }
    result
  }

  /**
   * Called at compile time in static case, at runtime for dynamic case.
   */
  def errorIfDelimsHaveWSPStar(delims: List[String]): Unit = {
    if (delims.filter(x => x == "%WSP*;").length > 0) {
      // We cannot detect this error until expressions have been evaluated!
      log(LogLevel.Debug, "%s - Failed due to WSP* detected as a delimiter for lengthKind=delimited.", eName)
      elemBase.schemaDefinitionError("WSP* cannot be used as a delimiter when lengthKind=delimited.")
    }
  }

  def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): PState = {
    val res = {
      if (!parseResult.isDefined) parser.PE(state, "%s - %s - Parse failed.", this.toString(), eName)
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

  val gram = this

  def parser: DaffodilParser = new PrimParser(this, e) {

    override def toBriefXML(depthLimit: Int = -1): String = {
      "<" + gram.name + " " + delimListForPrint + "/>"
    }

    lazy val delimListForPrint = tm.map { case (delimValue, _, _) => delimValue.prettyExpr }
    //    override def toString = cname + "(" + tm.map { _.prettyExpr } + ")"
    override def toString = cname + "(" + delimListForPrint + ")"

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

  def unparser: Unparser = new Unparser(e) {
    //    override def toString = cname + "(" + tm.map { _.prettyExpr } + ")"
    override def toString = cname + "(" + tm.map { case (delimValue, _, _) => delimValue.prettyExpr } + ")"

    def unparse(start: UState): UState =
      // withLoggingLevel(LogLevel.Info) 
      {
        val data = start.currentElement.getText

        val encoder = charset.newEncoder()
        start.outStream.setEncoder(encoder)
        start.outStream.fillCharBuffer(data)
        log(LogLevel.Debug, "Unparsed: " + start.outStream.getData)
        start
      }
  }
}

case class StringDelimitedEndOfData(e: ElementBase)
  extends StringDelimited(e) {
  val isDelimRequired: Boolean = false
}

abstract class HexBinaryDelimited(e: ElementBase) extends StringDelimited(e) {
  override val charset: Charset = Charset.forName("ISO-8859-1")

  override def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): PState = {
    val res = {
      if (!parseResult.isDefined) parser.PE(state, "%s - %s - Parse failed.", this.toString(), eName)
      else {
        val result = parseResult.get
        val field = result.field.getOrElse("")
        val numBits = elemBase.knownEncodingStringBitLengthFunction(field)
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

case class HexBinaryDelimitedEndOfData(e: ElementBase)
  extends HexBinaryDelimited(e) {
  val isDelimRequired: Boolean = false
}

case class LiteralNilDelimitedEndOfData(eb: ElementBase)
  extends StringDelimited(eb) {
  val nilValuesCooked = new ListOfStringValueAsLiteral(eb.nilValue, eb).cooked
  val isEmptyAllowed = eb.nilValue.contains("%ES;") // TODO: move outside parser

  val isDelimRequired: Boolean = false

  override def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): PState = {
    val res = {
      if (!parseResult.isDefined) parser.PE(state, "%s - %s - Parse failed.", this.toString(), eName)
      else {
        val result = parseResult.get
        // We have a field, is it empty?
        val field = result.field.getOrElse("")
        val isFieldEmpty = field.length() == 0 // Note: field has been stripped of padChars

        if (isFieldEmpty && !isEmptyAllowed) {
          return parser.PE(state, "%s - %s - Parse failed.", this.toString(), eName)
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
          return parser.PE(state, "%s - Does not contain a nil literal!", eName)
        }
      }
    }
    res
  }

}

case class PrefixLength(e: ElementBase) extends Primitive(e, e.lengthKind == LengthKind.Prefixed)

class OptionalInfixSep(term: Term, sep: => Gram, guard: Boolean = true) extends Terminal(term, guard) {

  val sepParser = sep.parser

  def parser: DaffodilParser = new PrimParser(this, term) {

    override def toString = "<OptionalInfixSep>" + sepParser.toString() + "</OptionalInfixSep>"

    def parse(start: PState): PState = {
      if (start.mpstate.arrayPos > 1) sepParser.parse1(start, term)
      else if (start.mpstate.groupPos > 1) sepParser.parse1(start, term)
      else start
    }
  }

  def unparser = DummyUnparser(term)
}

