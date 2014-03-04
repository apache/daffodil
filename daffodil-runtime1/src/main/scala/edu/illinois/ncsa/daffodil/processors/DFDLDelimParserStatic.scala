package edu.illinois.ncsa.daffodil.processors

import scala.util.parsing.input.Reader
import edu.illinois.ncsa.daffodil.exceptions.Assert

class DFDLDelimParserStatic(stringBitLengthFunction: String => Int)
  extends DFDLDelimParserCommon(stringBitLengthFunction) {

  def parseInput(field: Parser[(Vector[String], String)], seps: Parser[String], terms: Parser[String],
    input: Reader[Char], justification: TextJustificationType.Type): DelimParseResult = {
    val result = this.synchronized { this.parseInputDefaultContent(field, seps, terms, input, justification) }
    result
  }

  def parseInputDelimiter(inputDelimiterParser: Parser[String], isLocalDelimParser: Parser[String],
    input: Reader[Char]): DelimParseResult = {
    val res = this.synchronized { this.parse(this.log(inputDelimiterParser)("DelimParser.parseInputDelimiter.allDelims"), input) }

    // TODO: This seems pretty inefficient. We're redoing a match in order to know 
    // whether it was local or remote?? 
    val result = res match {
      case s @ Success(delimiterResult, next) => {
        // We have a result but was it a remote or local match?

        val subResult = this.synchronized { this.parseAll(this.log(isLocalDelimParser)("DelimParser.parseInputDelimiter.isLocal"), delimiterResult) }
        val delimiterLocation = if (subResult.isEmpty) DelimiterLocation.Remote else DelimiterLocation.Local
        //
        // TODO: ?? Is None the right thing to pass here?? If we pass none, then it is 
        // going to determine the length based on the delimiterResult. Does that include
        // everything it needs to include?
        //
        DelimParseSuccessFactory(s, delimiterResult, DelimiterType.NotDelimited, None, // Is None right?
          delimiterLocation)
      }
      case NoSuccess(msg, next) => DelimParseFailure(msg, next)
    }
    result
  }

  def parseInputEscapeBlock(escapeBlockParser: Parser[(Vector[String], String)], seps: Parser[String],
    terms: Parser[String], input: Reader[Char], justification: TextJustificationType.Type,
    removeEscapeBlocksRegex: String, parseInputParser: Parser[(Vector[String], String)]): DelimParseResult = {
    val result1 = this.parseInputEscapeBlockContent(escapeBlockParser, seps, terms, input, justification)
    val result2 = result1 match {
      case s: DelimParseSuccess => {
        val field = s.get
        val newField = removeEscapeBlocks(field, removeEscapeBlocksRegex)
        s.copy(fieldArg = newField, nextArg = s.next)
      }
      case f: DelimParseFailure => this.parseInputDefaultContent(parseInputParser, seps, terms,
        input, justification)
    }
    result2
  }

  private def removeEscapeBlocks(input: String, removeEscapeBlockRegex: String): String = {
    input.replaceAll(removeEscapeBlockRegex, "")
  }

  def parseInputEscapeCharacter(escapeCharacterParser: Parser[(Vector[String], String)],
    seps: Parser[String], terms: Parser[String], input: Reader[Char],
    justification: TextJustificationType.Type,
    removeEscapeCharacterRegex: scala.util.matching.Regex,
    removeUnescapedEscapesRegex: String,
    removeEscapeEscapesThatEscapeRegex: String,
    removeEscapeRegex: String, es: String, eses: String): DelimParseResult = {
    val result1 = this.parseInputEscapeCharContent(escapeCharacterParser, seps, terms, input, justification)
    val result2 = result1 match {
      case s: DelimParseSuccess => {
        val field = s.get
        val newField = removeEscapeCharacters(field, eses, es, removeEscapeCharacterRegex, removeUnescapedEscapesRegex,
          removeEscapeEscapesThatEscapeRegex, removeEscapeRegex)
        s.copy(fieldArg = newField, nextArg = s.next)
      }
      case f: DelimParseFailure => f
    }
    result2
  }

  /**
   * Assumes 'input' has had its delimiter picked off end already if it existed.
   */
  private def removeEscapeCharacters(input: String, eses: String, es: String,
    removeEscCharsSameRegex: scala.util.matching.Regex,
    removeUnescapedEscapesRegex: String,
    removeEscapeEscapesThatEscapeRegex: String,
    removeEscapeRegex: String): String = {
    if (eses.equals(es)) {
      return removeEscapeCharactersSame(input, removeEscCharsSameRegex)
    } else {
      return removeEscapeCharactersDiff(input, eses, removeUnescapedEscapesRegex,
        removeEscapeEscapesThatEscapeRegex, removeEscapeRegex)
    }
  }

  private def removeEscapeCharactersSame(input: String,
    removeEscCharsSameRegex: scala.util.matching.Regex): String = {
    // used to cleanup escape characters 
    val ERSplit = removeEscCharsSameRegex
    def removeActiveEscapes(str: String): String = {
      val res = str match {
        case ERSplit(before, theEsc, delim, after) => {
          val rest = removeActiveEscapes(after)
          before + delim + rest
        }
        case ERSplit(before, delim, after) => {
          val rest = removeActiveEscapes(after)
          before + delim + rest
        }
        case _ => str
      }
      res
    }
    removeActiveEscapes(input)
  }

  private def removeEscapeCharactersDiff(input: String, eses: String,
    removeUnescapedEscapesRegex: String, removeEscapeEscapesThatEscapeRegex: String,
    removeEscapeRegex: String): String = {
    // TODO: Move regular expressions out into central class
    if (eses.length() > 0) {
      val removeUnescapedEscapes = removeUnescapedEscapesRegex //rRemoveUnescapedEscapes.format(eses, es)
      val removeEscapeEscapesThatEscape = removeEscapeEscapesThatEscapeRegex //rRemoveEscapeEscapesThatEscape.format(eses, es)
      val r1 = input.replaceAll(removeUnescapedEscapes, "")
      val r2 = r1.replaceAll(removeEscapeEscapesThatEscape, "")
      return r2
    }
    val rRemoveEscape = removeEscapeRegex
    val r1 = input.replaceAll(rRemoveEscape, "")
    r1
  }

  def parseInputNCharacters(inputNCharsParser: Parser[String], input: Reader[Char],
    removePaddingParser: Option[Parser[String]], justification: TextJustificationType.Type): DelimParseResult = {
    // For debug can use this logging parser instead.
    val res = this.synchronized { this.parse(this.log(inputNCharsParser)("DelimParser.parseInputNCharacters"), input) }

    res match {
      case s @ Success(field, next) => {
        val fieldNoPadding = removePadding(removePaddingParser, justification, field)
        DelimParseSuccessFactory(Success(fieldNoPadding, next), "", DelimiterType.NotDelimited, Some(field), DelimiterLocation.Local)
      }
      case f: NoSuccess => DelimParseFailure(f.msg, f.next)
    }
  }

  def removePadding(removePaddingParser: Option[Parser[String]], justification: TextJustificationType.Type,
    input: String): String = {
    val result = removePaddingParser match {
      case Some(p) => {
        val res = this.synchronized { this.parse(this.log(p)({
          justification match {
            case TextJustificationType.Left => "DelimParser.removePadding.leftJustified"
            case TextJustificationType.Right => "DelimParser.removePadding.rightJustified"
            case TextJustificationType.Center => "DelimParser.removePadding.centerJustified"
            case TextJustificationType.None => Assert.invariantFailed("should not be none if we're trimming.")
          }
        }), input) }
        res.getOrElse(input)
      }
      case None => input
    }
    result
  }

  def parseInputPatterned(patternParser: Parser[String], input: Reader[Char]): DelimParseResult = {

    // FOR DEBUGGING might want this logging version
    val res = this.synchronized { this.parse(this.log(patternParser)("DelimParser.parseInputPatterned"), input) }

    res match {
      case s @ Success(_, _) => DelimParseSuccessFactory(s, "", DelimiterType.NotDelimited, None, DelimiterLocation.Local)
      case f: NoSuccess => DelimParseFailure(f.msg, f.next)
    }
  }

}
