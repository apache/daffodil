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
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParserFactoryStatic
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParserFactoryDynamic
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParserFactoryBase
import edu.illinois.ncsa.daffodil.processors.parsers.HasPadding
import edu.illinois.ncsa.daffodil.processors.parsers.StringPatternMatchedParser
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.processors.parsers.OptionalInfixSepParser

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

  override def parser: DaffodilParser = new StringFixedLengthInBytesFixedWidthCharactersParser(
    nBytes,
    justificationTrim,
    padCharChar,
    e.elementRuntimeData,
    e.knownEncodingCharset,
    e.knownEncodingIsFixedWidth,
    e.knownEncodingWidthInBits,
    e.knownEncodingName,
    lengthText)
}

case class StringFixedLengthInBytesVariableWidthCharacters(e: ElementBase, nBytes: Long)
  extends StringLength(e) {

  lazy val parserName = "StringFixedLengthInBytesVariableWidthCharacters"
  lazy val lengthText = nBytes.toString

  def getLength(pstate: PState): (Long, PState) = {
    (nBytes, pstate)
  }

  override def parser: DaffodilParser = new StringFixedLengthInBytesVariableWidthCharactersParser(
    nBytes,
    justificationTrim,
    padCharChar,
    e.elementRuntimeData,
    e.knownEncodingCharset,
    e.knownEncodingIsFixedWidth,
    e.knownEncodingWidthInBits,
    e.knownEncodingName,
    lengthText)
}

case class StringFixedLengthInVariableWidthCharacters(e: ElementBase, numChars: Long)
  extends StringLength(e) {

  lazy val parserName = "StringFixedLengthInVariableWidthCharacters"
  lazy val lengthText = numChars.toString

  override def parser: DaffodilParser = new StringFixedLengthInVariableWidthCharactersParser(
    numChars,
    justificationTrim,
    padCharChar,
    e.elementRuntimeData,
    e.knownEncodingCharset,
    e.knownEncodingIsFixedWidth,
    e.knownEncodingWidthInBits,
    e.knownEncodingName,
    lengthText)
}

case class StringVariableLengthInBytes(e: ElementBase)
  extends StringLength(e) {

  lazy val parserName = "StringVariableLengthInBytes"
  lazy val lengthText = e.length.prettyExpr

  override def parser: DaffodilParser = new StringVariableLengthInBytesParser(
    justificationTrim,
    padCharChar,
    e.elementRuntimeData,
    e.knownEncodingCharset,
    e.knownEncodingIsFixedWidth,
    e.knownEncodingWidthInBits,
    e.knownEncodingName,
    e.length,
    lengthText)
}

case class StringVariableLengthInBytesVariableWidthCharacters(e: ElementBase)
  extends StringLength(e) {

  lazy val parserName = "StringVariableLengthInBytesVariableWidthCharacters"
  lazy val lengthText = e.length.prettyExpr

  override def parser: DaffodilParser = new StringVariableLengthInBytesVariableWidthCharactersParser(
    justificationTrim,
    padCharChar,
    e.elementRuntimeData,
    e.knownEncodingCharset,
    e.knownEncodingIsFixedWidth,
    e.knownEncodingWidthInBits,
    e.knownEncodingName,
    e.length,
    lengthText)
}

case class StringVariableLengthInVariableWidthCharacters(e: ElementBase)
  extends StringLength(e) {

  lazy val parserName = "StringVariableLengthInVariableWidthCharacters"
  lazy val lengthText = e.length.prettyExpr

  override def parser: DaffodilParser = new StringVariableLengthInVariableWidthCharactersParser(
    justificationTrim,
    padCharChar,
    e.elementRuntimeData,
    e.knownEncodingCharset,
    e.knownEncodingIsFixedWidth,
    e.knownEncodingWidthInBits,
    e.knownEncodingName,
    e.length,
    lengthText)
}

case class StringPatternMatched(e: ElementBase)
  extends Terminal(e, true)
  with Padded {

  val charset = e.knownEncodingCharset
  val pattern = e.lengthPattern

  def parser: DaffodilParser = new StringPatternMatchedParser(charset, pattern, e.elementRuntimeData,
    e.knownEncodingIsFixedWidth, e.knownEncodingWidthInBits, e.knownEncodingName, justificationTrim, padChar)

}

class Delimiters(delimDFAs: DFADelimiter, delims: List[String])

/**
 * Here allTerminatingMarkup is a List[(CompiledExpression, ElementName, ElementPath)]
 */
sealed abstract class DelimiterFactoryBase(allTerminatingMarkup: List[(CompiledExpression, String, String)],
  context: ThrowsSDE) extends Logging with Serializable {

  // These static delims are used whether we're static or dynamic
  // because even a dynamic can have some static from enclosing scopes.
  val staticDelimsRaw = allTerminatingMarkup.filter {
    case (delimValue, _, _) => delimValue.isConstant
  }.map {
    case (delimValue, _, _) => delimValue.constantAsString
  }

  val dynamicDelimsRaw = allTerminatingMarkup.filter { case (delimValue, _, _) => !delimValue.isConstant }

  val staticDelimsCooked = staticDelimsRaw.map(raw => { new ListOfStringValueAsLiteral(raw.toString, context).cooked }).flatten
  val staticDelimsDFAs = { CreateDelimiterDFA(staticDelimsCooked) }

  /**
   * Called at compile time in static case, at runtime for dynamic case.
   */
  def errorIfDelimsHaveWSPStar(delims: List[String], ctxt: ThrowsSDE): Unit = {
    if (delims.filter(x => x == "%WSP*;").length > 0) {
      // We cannot detect this error until expressions have been evaluated!
      log(LogLevel.Debug, "%s - Failed due to WSP* detected as a delimiter for lengthKind=delimited.", context.toString())
      ctxt.schemaDefinitionError("WSP* cannot be used as a delimiter when lengthKind=delimited.")
    }
  }

  protected def constEval(knownValue: String) = {
    val l = new StringValueAsLiteral(knownValue, context)
    val constValue = l.cooked
    constValue
  }

  def getDelims(pstate: PState): (Seq[DFADelimiter], DelimsMatcher, List[String], Option[VariableMap])

}

case class DelimiterFactoryStatic(allTerminatingMarkup: List[(CompiledExpression, String, String)],
  context: ThrowsSDE)
  extends DelimiterFactoryBase(allTerminatingMarkup, context) {

  errorIfDelimsHaveWSPStar(staticDelimsCooked, context)

  val delimsMatcher = CreateDelimiterMatcher(staticDelimsDFAs)

  def getDelims(pstate: PState): (Seq[DFADelimiter], DelimsMatcher, List[String], Option[VariableMap]) = {
    (staticDelimsDFAs, delimsMatcher, staticDelimsCooked, Some(pstate.variableMap))
  }
}

case class DelimiterFactoryDynamic(allTerminatingMarkup: List[(CompiledExpression, String, String)],
  context: ThrowsSDE)
  extends DelimiterFactoryBase(allTerminatingMarkup, context)
  with Dynamic {

  val tmCE = allTerminatingMarkup.map { case (delimValue, _, _) => delimValue }
  val dynamicDelimsCached: List[CachedDynamic[String]] = cacheConstantExpression(tmCE) {
    (a: Any) => constEval(a.asInstanceOf[String])
  }

  def getDelimValue(knownValue: String, state: PState): String = {
    val l = new StringValueAsLiteral(knownValue, state)
    l.cooked
  }

  def getDelims(pstate: PState): (Seq[DFADelimiter], DelimsMatcher, List[String], Option[VariableMap]) = {

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
        val (res, newVMap) = optEsc.get.evaluate(state)
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
  context: ThrowsSDE) extends Serializable {

  def getFieldDFA(state: PState): (PState, Seq[DFADelimiter], DelimsMatcher, List[String], DFAField, Option[EscapeScheme])
}
case class FieldFactoryStatic(ef: Option[EscapeSchemeFactoryStatic],
  df: DelimiterFactoryStatic,
  context: ThrowsSDE)
  extends FieldFactoryBase(ef, df, context) {

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
  context: ThrowsSDE)
  extends FieldFactoryBase(ef, df, context) {

  def getFieldDFA(start: PState): (PState, Seq[DFADelimiter], DelimsMatcher, List[String], DFAField, Option[EscapeScheme]) = {
    val (postEscapeSchemeEvalState, scheme) = {
      if (ef.isDefined) {
        val factory = ef.get
        val (state, escScheme) = factory.getEscapeScheme(start)
        (state, Some(escScheme))
      } else (start, None)
    }

    val (delims, delimsMatcher, delimsCooked, vars) = df.getDelims(postEscapeSchemeEvalState)

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

abstract class EscapeSchemeFactoryBase(
    escapeSchemeObject: EscapeSchemeObject,
    context: ThrowsSDE)
    extends Serializable {
  
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
    escapeSchemeObject.escapeKind match {
      case EscapeKind.EscapeBlock => None
      case EscapeKind.EscapeCharacter => {
        if (!escapeSchemeObject.optionEscapeCharacter.isDefined) {
          context.SDE("escapeCharacter cannot be the empty string when EscapeSchemeKind is Character.")
        }
        escapeSchemeObject.optionEscapeCharacter
      }
    }
  }

  protected def getEscValue(escChar: String, context: ThrowsSDE): String = {
    val l = new SingleCharacterLiteralES(escChar, context).cooked
    l
  }

  protected def getBlockStart: String = {
    if (escapeSchemeObject.escapeKind == EscapeKind.EscapeCharacter) Assert.usageError("getBlockStart called when escapeKind = character")
    if (escapeSchemeObject.optionEscapeBlockStart.get == "") { context.SDE("escapeBlockStart cannot be the empty string when EscapeSchemeKind is Block.") }

    val bs = new StringValueAsLiteral(escapeSchemeObject.optionEscapeBlockStart.get, context).cooked
    bs
  }

  protected def getBlockEnd: String = {
    if (escapeSchemeObject.escapeKind == EscapeKind.EscapeCharacter) Assert.usageError("getBlockStart called when escapeKind = character")
    if (escapeSchemeObject.optionEscapeBlockEnd.get == "") { context.SDE("escapeBlockEnd cannot be the empty string when EscapeSchemeKind is Block.") }

    val be = new StringValueAsLiteral(escapeSchemeObject.optionEscapeBlockEnd.get, context).cooked
    be
  }

  def getEscapeScheme(state: PState): (PState, EscapeScheme)

}
class EscapeSchemeFactoryStatic(
    escapeSchemeObject: EscapeSchemeObject,
    context: ThrowsSDE)
    extends EscapeSchemeFactoryBase(escapeSchemeObject, context) {

  val escChar = evalAsConstant(getOptEscChar)
  val escEscChar = evalAsConstant(escapeSchemeObject.optionEscapeEscapeCharacter)

  def generateEscapeScheme: EscapeScheme = {
    val result = escapeSchemeObject.escapeKind match {
      case EscapeKind.EscapeBlock => new EscapeSchemeBlock(escEscChar, getBlockStart, getBlockEnd)
      case EscapeKind.EscapeCharacter => new EscapeSchemeChar(escChar, escEscChar)
    }
    result
  }

  val theScheme = generateEscapeScheme

  def getEscapeScheme(state: PState) = {
    (state, theScheme)
  }
}

class EscapeSchemeFactoryDynamic(
    escapeSchemeObject: EscapeSchemeObject,
    context: ThrowsSDE)
    extends EscapeSchemeFactoryBase(escapeSchemeObject, context) with Dynamic {

  val escapeCharacterCached: Maybe[CachedDynamic[String]] = {
    escapeSchemeObject.escapeKind match {
      case EscapeKind.EscapeBlock => // do nothing
      case EscapeKind.EscapeCharacter => {
        if (!escapeSchemeObject.optionEscapeCharacter.isDefined) {
          context.SDE("escapeCharacter cannot be the empty string when EscapeSchemeKind is Character.")
        }
      }
    }
    val ec = escapeSchemeObject.optionEscapeCharacter match {
      case None => Nope
      case Some(c) => One(c)
    }
    cacheConstantExpression(ec) {
      (a: Any) => constEval(a.asInstanceOf[String], context)
    }
  }

  val escapeEscapeCharacterCached: Maybe[CachedDynamic[String]] = cacheConstantExpression(escapeSchemeObject.optionEscapeEscapeCharacter) {
    (a: Any) => constEval(a.asInstanceOf[String], context)
  }

  def getEscapeScheme(state: PState) = {
    val (finalState, theScheme) = escapeSchemeObject.escapeKind match {
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
        (afterEscEscCharEval, new EscapeSchemeBlock(finalOptEscEscChar, getBlockStart, getBlockEnd))
      }
    }
    (finalState, theScheme)
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
  lazy val charset = e.knownEncodingCharset

  lazy val hasDynamicDelims: Boolean =
    e.allTerminatingMarkup.exists { case (delimValue, _, _) => !delimValue.isConstant }

  lazy val pad: Maybe[Char] = if (padChar.isEmpty()) Nope else One(padChar.charAt(0))

  lazy val leftPaddingOpt: Option[TextPaddingParser] = {
    if (!pad.isDefined) None
    else Some(new TextPaddingParser(pad.get, e.knownEncodingIsFixedWidth, e.knownEncodingWidthInBits, e.knownEncodingName))
  }

  lazy val escapeSchemeFactory = createEscSchemeFactory
  lazy val delimiterFactory = createDelimiterFactory
  lazy val fieldFactory = createFieldFactory
  lazy val parserFactory = createParserFactory

  def createEscSchemeFactory: Option[EscapeSchemeFactoryBase] = {
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
      
      val theScheme = {
        if (isConstant) new EscapeSchemeFactoryStatic(scheme.escapeScheme, context.runtimeData)
        else new EscapeSchemeFactoryDynamic(scheme.escapeScheme, context.runtimeData)
      }
      
      Some(theScheme)
    } else None
  }

  def createDelimiterFactory: DelimiterFactoryBase = {
    val factory =
      if (hasDynamicDelims) {
        new DelimiterFactoryDynamic(e.allTerminatingMarkup, context.runtimeData)
      } else {
        new DelimiterFactoryStatic(e.allTerminatingMarkup, context.runtimeData)
      }
    factory
  }

  def createFieldFactory: FieldFactoryBase = {
    val hasDynamicEscapeScheme =
      escapeSchemeFactory.getOrElse(false).isInstanceOf[EscapeSchemeFactoryDynamic]

    val fieldDFAFact =
      if (!hasDynamicDelims && !hasDynamicEscapeScheme)
        new FieldFactoryStatic(escapeSchemeFactory.asInstanceOf[Option[EscapeSchemeFactoryStatic]],
          delimiterFactory.asInstanceOf[DelimiterFactoryStatic], context.runtimeData)
      else new FieldFactoryDynamic(escapeSchemeFactory, delimiterFactory, context.runtimeData)
    fieldDFAFact
  }

  def createParserFactory: TextDelimitedParserFactoryBase = {
    val theParserFact = fieldFactory match {
      case ffs: FieldFactoryStatic => {
        val pStatic = new TextDelimitedParserFactoryStatic(
          justificationTrim, pad, e.knownEncodingIsFixedWidth, e.knownEncodingWidthInBits, e.knownEncodingName, ffs, context.runtimeData)
        pStatic
      }
      case ffd: FieldFactoryDynamic => {
        val pDynamic = new TextDelimitedParserFactoryDynamic(
          justificationTrim, pad, e.knownEncodingIsFixedWidth, e.knownEncodingWidthInBits, e.knownEncodingName, ffd, context.runtimeData)
        pDynamic
      }
    }
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

  lazy val gram = this

  def parser: DaffodilParser = new StringDelimitedParser(
    e.elementRuntimeData,
    justificationTrim,
    pad,
    fieldFactory,
    parserFactory,
    isDelimRequired,
    e.allTerminatingMarkup,
    e.knownEncodingCharset,
    e.knownEncodingIsFixedWidth,
    e.knownEncodingWidthInBits,
    e.knownEncodingName)

  //def unparser: Unparser = new DummyUnparser(e)
}

case class StringDelimitedEndOfData(e: ElementBase)
  extends StringDelimited(e) {
  val isDelimRequired: Boolean = false
}

abstract class HexBinaryDelimited(e: ElementBase)
  extends StringDelimited(e) {
  override lazy val charset = new DFDLCharset("ISO-8859-1")

  override def parser: DaffodilParser = new HexBinaryDelimitedParser(
    e.elementRuntimeData,
    justificationTrim,
    pad,
    fieldFactory,
    parserFactory,
    isDelimRequired,
    e.allTerminatingMarkup,
    e.knownEncodingIsFixedWidth,
    e.knownEncodingWidthInBits,
    e.knownEncodingName)

}

case class HexBinaryDelimitedEndOfData(e: ElementBase)
  extends HexBinaryDelimited(e) {
  val isDelimRequired: Boolean = false
}

case class LiteralNilDelimitedEndOfData(eb: ElementBase)
  extends StringDelimited(eb) {
  lazy val nilValuesCooked = new ListOfStringValueAsLiteral(eb.nilValue, eb).cooked
  lazy val isEmptyAllowed = eb.nilValue.contains("%ES;") // TODO: move outside parser

  lazy val isDelimRequired: Boolean = false

  override def parser: DaffodilParser =
    new LiteralNilDelimitedEndOfDataParser(
      eb.elementRuntimeData,
      justificationTrim,
      pad,
      fieldFactory,
      parserFactory,
      eb.allTerminatingMarkup,
      nilValuesCooked,
      eb.knownEncodingCharset,
      eb.knownEncodingIsFixedWidth,
      eb.knownEncodingWidthInBits,
      eb.knownEncodingName)

}

case class PrefixLength(e: ElementBase) extends Primitive(e, e.lengthKind == LengthKind.Prefixed)

class OptionalInfixSep(term: Term, sep: => Gram, guard: Boolean = true) extends Terminal(term, guard) {

  def parser: DaffodilParser = new OptionalInfixSepParser(term.runtimeData, sep.parser)
}

