/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors

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
import edu.illinois.ncsa.daffodil.processors.unparsers.{ Unparser => DaffodilUnparser }
import edu.illinois.ncsa.daffodil.processors.dfa.DFA
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParser
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParserWithEscapeBlock
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.processors.dfa.CreateFieldDFA
import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter
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
import edu.illinois.ncsa.daffodil.processors.dfa.DFAField
import edu.illinois.ncsa.daffodil.processors.parsers.StringVariableLengthInBytesParser
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.processors.parsers.HexBinaryVariableLengthInBytesParser
import edu.illinois.ncsa.daffodil.processors.parsers.StringFixedLengthInVariableWidthCharactersParser
import edu.illinois.ncsa.daffodil.processors.parsers.StringFixedLengthInBytesFixedWidthCharactersParser
import edu.illinois.ncsa.daffodil.processors.parsers.StringFixedLengthInBytesVariableWidthCharactersParser
import edu.illinois.ncsa.daffodil.processors.parsers.LiteralNilDelimitedEndOfDataParser
import edu.illinois.ncsa.daffodil.processors.parsers.StringVariableLengthInVariableWidthCharactersParser
import edu.illinois.ncsa.daffodil.processors.parsers.StringVariableLengthInBytesVariableWidthCharactersParser
import edu.illinois.ncsa.daffodil.dsom.DFDLEscapeScheme
import edu.illinois.ncsa.daffodil.processors.parsers.StringDelimitedParser
import edu.illinois.ncsa.daffodil.processors.parsers.HexBinaryFixedLengthInBytesParser
import edu.illinois.ncsa.daffodil.processors.parsers.HexBinaryDelimitedParser
import edu.illinois.ncsa.daffodil.processors.parsers.HexBinaryFixedLengthInBitsParser
import edu.illinois.ncsa.daffodil.dsom.Term
import edu.illinois.ncsa.daffodil.dsom.DFDLEscapeScheme
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParserFactory
import edu.illinois.ncsa.daffodil.processors.parsers.HasPadding
import edu.illinois.ncsa.daffodil.processors.parsers.StringPatternMatchedParser
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.processors.parsers.OptionalInfixSepParser
import edu.illinois.ncsa.daffodil.processors.unparsers.StringFixedLengthInBytesFixedWidthCharactersUnparser
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParserFactory
import edu.illinois.ncsa.daffodil.processors.unparsers.LiteralNilDelimitedEndOfDataUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.StringDelimitedUnparser

abstract class StringLength(e: ElementBase)
  extends DelimParserBase(e, true)
  with Padded {

  def lengthText: String
  def parserName: String

  val padCharChar = optPadChar.map(s => s.charAt(0))

}

abstract class HexBinaryLengthInBytes(e: ElementBase)
  extends StringLength(e) {

}

case class HexBinaryFixedLengthInBytes(e: ElementBase, nBytes: Long)
  extends HexBinaryLengthInBytes(e) {

  lazy val parserName = "HexBinaryFixedLengthInBytes"
  lazy val lengthText = nBytes.toString

  def getLength(pstate: PState): (Long, PState) = {
    (nBytes, pstate)
  }

  def parser: DaffodilParser = new HexBinaryFixedLengthInBytesParser(nBytes, justificationTrim,
    padCharChar,
    e.elementRuntimeData,
    lengthText)
}

case class HexBinaryFixedLengthInBits(e: ElementBase, nBits: Long)
  extends HexBinaryLengthInBytes(e) {

  lazy val parserName = "HexBinaryFixedLengthInBits"
  lazy val lengthText = nBits.toString

  def getLength(pstate: PState): (Long, PState) = {
    val nBytes = scala.math.ceil(nBits / 8).toLong
    (nBytes, pstate)
  }

  def parser: DaffodilParser = new HexBinaryFixedLengthInBitsParser(nBits, justificationTrim,
    padCharChar,
    e.elementRuntimeData,
    lengthText)
}

case class HexBinaryVariableLengthInBytes(e: ElementBase)
  extends HexBinaryLengthInBytes(e) {

  lazy val parserName = "HexBinaryVariableLengthInBytes"
  lazy val lengthText = e.length.prettyExpr

  def parser: DaffodilParser = new HexBinaryVariableLengthInBytesParser(justificationTrim,
    padCharChar,
    e.elementRuntimeData,
    e.knownEncodingWidthInBits,
    e.length,
    lengthText)
}

case class StringFixedLengthInBytesFixedWidthCharacters(e: ElementBase, nBytes: Long)
  extends StringLength(e) {

  lazy val parserName = "StringFixedLengthInBytesFixedWidthCharacters"
  lazy val lengthText = nBytes.toString

  def getLength(pstate: PState): (Long, PState) = {
    (nBytes, pstate)
  }

  override lazy val parser: DaffodilParser = new StringFixedLengthInBytesFixedWidthCharactersParser(
    nBytes,
    justificationTrim,
    padCharChar,
    e.elementRuntimeData,
    e.encodingInfo,
    lengthText)

  override lazy val unparser: DaffodilUnparser = new StringFixedLengthInBytesFixedWidthCharactersUnparser(
    nBytes,
    justificationTrim,
    padCharChar,
    e.elementRuntimeData,
    e.encodingInfo,
    lengthText)
}

case class StringFixedLengthInBytesVariableWidthCharacters(e: ElementBase, nBytes: Long)
  extends StringLength(e) {

  lazy val parserName = "StringFixedLengthInBytesVariableWidthCharacters"
  lazy val lengthText = nBytes.toString

  def getLength(pstate: PState): (Long, PState) = {
    (nBytes, pstate)
  }

  override lazy val parser: DaffodilParser = new StringFixedLengthInBytesVariableWidthCharactersParser(
    nBytes,
    justificationTrim,
    padCharChar,
    e.elementRuntimeData,
    e.encodingInfo,
    lengthText)
}

case class StringFixedLengthInVariableWidthCharacters(e: ElementBase, numChars: Long)
  extends StringLength(e) {

  lazy val parserName = "StringFixedLengthInVariableWidthCharacters"
  lazy val lengthText = numChars.toString

  override lazy val parser: DaffodilParser = new StringFixedLengthInVariableWidthCharactersParser(
    numChars,
    justificationTrim,
    padCharChar,
    e.elementRuntimeData,
    e.encodingInfo,
    lengthText)
}

case class StringVariableLengthInBytes(e: ElementBase)
  extends StringLength(e) {

  lazy val parserName = "StringVariableLengthInBytes"
  lazy val lengthText = e.length.prettyExpr

  override lazy val parser: DaffodilParser = new StringVariableLengthInBytesParser(
    justificationTrim,
    padCharChar,
    e.elementRuntimeData,
    e.encodingInfo,
    e.length,
    lengthText)
}

case class StringVariableLengthInBytesVariableWidthCharacters(e: ElementBase)
  extends StringLength(e) {

  lazy val parserName = "StringVariableLengthInBytesVariableWidthCharacters"
  lazy val lengthText = e.length.prettyExpr

  override lazy val parser: DaffodilParser = new StringVariableLengthInBytesVariableWidthCharactersParser(
    justificationTrim,
    padCharChar,
    e.elementRuntimeData,
    e.encodingInfo,
    e.length,
    lengthText)
}

case class StringVariableLengthInVariableWidthCharacters(e: ElementBase)
  extends StringLength(e) {

  lazy val parserName = "StringVariableLengthInVariableWidthCharacters"
  lazy val lengthText = e.length.prettyExpr

  override lazy val parser: DaffodilParser = new StringVariableLengthInVariableWidthCharactersParser(
    justificationTrim,
    padCharChar,
    e.elementRuntimeData,
    e.encodingInfo,
    e.length,
    lengthText)
}

case class StringPatternMatched(e: ElementBase)
  extends Terminal(e, true)
  with Padded {

  val pattern = e.lengthPattern

  def parser: DaffodilParser = {
    PatternChecker.checkPattern(pattern, e)
    new StringPatternMatchedParser(pattern, e.elementRuntimeData,
      e.encodingInfo, justificationTrim, padChar)
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
        val (res, newVMap) = optEsc.get.evaluate(state)
        val l = new SingleCharacterLiteralES(res.toString, context)
        val resultEsc = l.cooked
        val newState = state.withVariables(newVMap)
        (One(resultEsc), newState)
      }
    (finalOptEsc, afterEscEval)
  }

}

abstract class StringDelimited(e: ElementBase)
  extends DelimParserBase(e, true)
  with Padded
  with HasEscapeScheme {

  // TODO: DFDL-451 - Has been placed on the backburner until we can figure out the appropriate behavior
  //
  //  requiredEvaluations(gram.checkDelimiterDistinctness(esObj.escapeSchemeKind, optPadChar, optEscChar,
  //    optEscEscChar, optEscBlkStart, optEscBlkEnd, staticDelimsCooked, elemBase))

  def isDelimRequired: Boolean

  lazy val es = e.optionEscapeScheme

  lazy val tm = e.allTerminatingMarkup
  lazy val cname = toString

  lazy val eName = e.toString()

  lazy val hasDynamicDelims: Boolean =
    e.allTerminatingMarkup.exists { case (delimValue, _, _) => !delimValue.isConstant }

  lazy val pad: Maybe[Char] = if (padChar.isEmpty()) Nope else One(padChar.charAt(0))

  lazy val leftPaddingOpt: Option[TextPaddingParser] = {
    if (!pad.isDefined) None
    else Some(new TextPaddingParser(pad.get, e.runtimeData, e.encodingInfo))
  }

  lazy val isEscapeSchemeConstant = {
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
      isConstant
    } else false
  }

  lazy val escapeSchemeFactory = createEscSchemeFactory
  lazy val fieldFactory = createFieldFactory
  lazy val parserFactory = createParserFactory

  def createEscSchemeFactory: Option[EscapeSchemeFactoryBase] = {
    if (es.isDefined) {
      val scheme = es.get

      val theScheme = {
        if (isEscapeSchemeConstant) EscapeSchemeFactoryStatic(scheme.escapeScheme, context.runtimeData)
        else EscapeSchemeFactoryDynamic(scheme.escapeScheme, context.runtimeData)
      }

      Some(theScheme)
    } else None
  }

  def createFieldFactory: FieldFactoryBase = {
    val fieldDFAFact = {
      if (escapeSchemeFactory.isDefined) {
        if (isEscapeSchemeConstant) FieldFactoryStatic(escapeSchemeFactory, context.runtimeData)
        else FieldFactoryDynamic(escapeSchemeFactory, context.runtimeData)
      } else { FieldFactoryStatic(escapeSchemeFactory, context.runtimeData) }
    }

    fieldDFAFact
  }

  def createParserFactory: TextDelimitedParserFactory = {
    val theParserFact = TextDelimitedParserFactory(
      justificationTrim, pad, e.encodingInfo, fieldFactory, escapeSchemeFactory, context.runtimeData)
    theParserFact
  }

  /**
   * Called at compile time in static case, at runtime for dynamic case.
   */
  def errorIfDelimsHaveWSPStar(delims: List[String]): Unit = {
    if (delims.filter(x => x == "%WSP*;").length > 0) {
      // We cannot detect this error until expressions have been evaluated!
      log(LogLevel.Debug, "%s - Failed due to WSP* detected as a delimiter for lengthKind=delimited.", eName)
      context.schemaDefinitionError("WSP* cannot be used as a delimiter when lengthKind=delimited.")
    }
  }

  override def parser: DaffodilParser = new StringDelimitedParser(
    e.elementRuntimeData,
    justificationTrim,
    pad,
    fieldFactory,
    parserFactory,
    isDelimRequired,
    e.encodingInfo)

  //FIXME: must do escaping in case that the value contains a delimiter.
  override def unparser: DaffodilUnparser = new StringDelimitedUnparser(e.elementRuntimeData, e.encodingInfo)
}

case class StringDelimitedEndOfData(e: ElementBase)
  extends StringDelimited(e) {
  val isDelimRequired: Boolean = false
}

abstract class HexBinaryDelimited(e: ElementBase)
  extends StringDelimited(e) {

  override lazy val parser: DaffodilParser = new HexBinaryDelimitedParser(
    e.elementRuntimeData,
    justificationTrim,
    pad,
    fieldFactory,
    parserFactory,
    isDelimRequired,
    e.encodingInfo)

}

case class HexBinaryDelimitedEndOfData(e: ElementBase)
  extends HexBinaryDelimited(e) {
  val isDelimRequired: Boolean = false
}

case class LiteralNilDelimitedEndOfData(eb: ElementBase)
  extends StringDelimited(eb) {
  lazy val nilValuesCooked = new ListOfStringValueAsLiteral(eb.nilValue, eb).cooked
  lazy val nilValueString = nilValuesCooked(0).replace("%ES;", "")
  lazy val isEmptyAllowed = eb.nilValue.contains("%ES;") // TODO: move outside parser

  lazy val isDelimRequired: Boolean = false

  override lazy val parser: DaffodilParser =
    new LiteralNilDelimitedEndOfDataParser(
      eb.elementRuntimeData,
      justificationTrim,
      pad,
      fieldFactory,
      parserFactory,
      nilValuesCooked,
      eb.encodingInfo)

  override lazy val unparser: DaffodilUnparser =
    new LiteralNilDelimitedEndOfDataUnparser(eb.elementRuntimeData, nilValueString, eb.encodingInfo)

}

case class PrefixLength(e: ElementBase) extends Primitive(e, e.lengthKind == LengthKind.Prefixed)

class OptionalInfixSep(term: Term, sep: => Gram, guard: Boolean = true) extends Terminal(term, guard) {

  def parser: DaffodilParser = new OptionalInfixSepParser(term.runtimeData, sep.parser)
}

