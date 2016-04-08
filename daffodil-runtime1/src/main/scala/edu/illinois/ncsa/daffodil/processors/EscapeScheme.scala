package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.processors.dfa.CreateFieldDFA
import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter
import edu.illinois.ncsa.daffodil.processors.dfa.CreateDelimiterDFA
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.GenerateEscape
import edu.illinois.ncsa.daffodil.util.MaybeChar

sealed abstract class EscapeSchemeParserHelper
case class EscapeSchemeCharParserHelper(val ec: Char, val eec: MaybeChar)
  extends EscapeSchemeParserHelper {

  override def toString() = "<EscapeSchemeChar escapeChar='" + ec +
    "' escapeEscapeChar='" + (if (eec.isDefined) eec.get.toString else "") + "'/>"
}
case class EscapeSchemeBlockParserHelper(val eec: MaybeChar,
  blockStart: String,
  blockEnd: String)
  extends EscapeSchemeParserHelper {
  // Should note there that fieldDFA (not here) is dependent on
  // the whether or not the delimiters are constant or not.
  // As a result, it cannot be generated here.

  val blockStartDFA: DFADelimiter = CreateDelimiterDFA(blockStart)
  val blockEndDFA: DFADelimiter = CreateDelimiterDFA(blockEnd)
  val fieldEscDFA = CreateFieldDFA(blockEndDFA, eec)

  override def toString() = "<EscapeSchemeBlock escapeEscapeChar='" + (if (eec.isDefined) eec.get.toString else "") +
    "' blockStart='" + blockStart + "' blockEnd='" + blockEnd + "'/>"
}

sealed abstract class EscapeSchemeUnparserHelper {
  def lookingFor: Array[DFADelimiter]
}
case class EscapeSchemeCharUnparserHelper(val ec: Char, val eec: MaybeChar, extraEscChar: Seq[Char])
  extends EscapeSchemeUnparserHelper {

  // We need to look for the escapeCharacter and the extraEscapedCharacters
  //
  val escCharDFA: DFADelimiter = CreateDelimiterDFA(ec.toString)
  val escEscCharDFA: Maybe[DFADelimiter] = if (eec.isDefined) One(CreateDelimiterDFA(eec.toString)) else Nope
  val extraEscCharsDFAs: Array[DFADelimiter] = CreateDelimiterDFA(extraEscChar.map(_.toString))

  override val lookingFor = {
    val res: Array[DFADelimiter] =
      if (escEscCharDFA.isDefined) escCharDFA +: escCharDFA +: escEscCharDFA.get +: escEscCharDFA.get +: extraEscCharsDFAs
      else escCharDFA +: escCharDFA +: extraEscCharsDFAs
    res
  }

  override def toString() = "<EscapeSchemeChar escapeChar='" + ec +
    "' escapeEscapeChar='" + (if (eec.isDefined) eec.get.toString else "") + "' extraEscapedChars='" + extraEscChar.mkString(" ") + "'/>"
}
case class EscapeSchemeBlockUnparserHelper(val eec: MaybeChar,
  blockStart: String,
  blockEnd: String,
  private val extraEscChar: Seq[Char],
  generateEscapeBlock: GenerateEscape)
  extends EscapeSchemeUnparserHelper {

  // We need to look for the blockEnd
  //
  val blockEndDFA: DFADelimiter = CreateDelimiterDFA(blockEnd)
  val blockStartDFA: DFADelimiter = CreateDelimiterDFA(blockStart)
  val fieldEscDFA = CreateFieldDFA(blockEndDFA, eec)
  val extraEscCharsDFAs: Array[DFADelimiter] = CreateDelimiterDFA(extraEscChar.map(_.toString))

  override val lookingFor = blockStartDFA +: extraEscCharsDFAs

  override def toString() = "<EscapeSchemeBlock escapeEscapeChar='" + (if (eec.isDefined) eec.get.toString else "") +
    "' blockStart='" + blockStart + "' blockEnd='" + blockEnd + "' generateEscapeBlock='" + generateEscapeBlock + "'/>"
}
