package edu.illinois.ncsa.daffodil.processors
import scala.util.parsing.input.Reader
import edu.illinois.ncsa.daffodil.processors.DelimiterType._
import edu.illinois.ncsa.daffodil.processors.DelimiterLocation._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.exceptions.Assert

class DFDLDelimParser(stringBitLengthFunction: String => Int) extends DFDLDelimParserCommon(stringBitLengthFunction) {

  def parseInputPatterned(pattern: String, input: Reader[Char]): DelimParseResult = {
    val entry = this.generateInputPatternedParser(pattern)

    // FOR DEBUGGING might want this logging version
    val res = this.synchronized { this.parse(this.log(entry)("DelimParser.parseInputPatterned"), input) }
    //val res = this.parse(entry, input)

    res match {
      case s @ Success(_, _) => DelimParseSuccessFactory(s, "", DelimiterType.NotDelimited, None, DelimiterLocation.Local)
      case f: NoSuccess => DelimParseFailure(f.msg, f.next)
    }
  }
  
  def parseInputPatterned(pattern: Parser[String], input: Reader[Char]): DelimParseResult = {
 
    // FOR DEBUGGING might want this logging version
    val res = this.synchronized { this.parse(this.log(pattern)("DelimParser.parseInputPatterned"), input) }
    //val res = this.parse(entry, input)

    res match {
      case s @ Success(_, _) => DelimParseSuccessFactory(s, "", DelimiterType.NotDelimited, None, DelimiterLocation.Local)
      case f: NoSuccess => DelimParseFailure(f.msg, f.next)
    }
  }

  def parseInputNCharacters(nChars: Long, input: Reader[Char]): DelimParseResult = {

    val entry = this.generateInputNCharactersParser(nChars)

    // For debug can use this logging parser instead.
    val res = this.synchronized { this.parse(this.log(entry)("DelimParser.parseInputNCharacters"), input) }

    res match {
      case s @ Success(_, _) => DelimParseSuccessFactory(s, "", DelimiterType.NotDelimited, None, DelimiterLocation.Local)
      case f: NoSuccess => DelimParseFailure(f.msg, f.next)
    }
  }
  
  def parseInputNCharacters(nChars: Parser[String], input: Reader[Char]): DelimParseResult = {

    // For debug can use this logging parser instead.
    val res = this.synchronized { this.parse(this.log(nChars)("DelimParser.parseInputNCharacters"), input) }

    res match {
      case s @ Success(_, _) => DelimParseSuccessFactory(s, "", DelimiterType.NotDelimited, None, DelimiterLocation.Local)
      case f: NoSuccess => DelimParseFailure(f.msg, f.next)
    }
  }

}
