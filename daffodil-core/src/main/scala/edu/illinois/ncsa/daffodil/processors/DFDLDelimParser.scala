package edu.illinois.ncsa.daffodil.processors
import scala.util.parsing.input.Reader
import edu.illinois.ncsa.daffodil.processors.DelimiterType._
import edu.illinois.ncsa.daffodil.processors.DelimiterLocation._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.AnnotatedSchemaComponent
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE

class DFDLDelimParser(stringBitLengthFunction: String => Int) extends DFDLDelimParserCommon(stringBitLengthFunction) {

  def parseInputPatterned(pattern: String, input: Reader[Char], sc: ThrowsSDE): DelimParseResult = {
    val entry = this.generateInputPatternedParser(pattern, sc)

    // FOR DEBUGGING might want this logging version
    val res = this.synchronized { this.parse(this.log(entry)("DelimParser.parseInputPatterned"), input) }
    //val res = this.parse(entry, input)

    res match {
      case s @ Success(_, _) => DelimParseSuccessFactory(s, "", DelimiterType.NotDelimited, None, DelimiterLocation.Local)
      case f: NoSuccess => DelimParseFailure(f.msg, f.next)
    }
  }

  def parseInputPatterned(pattern: Parser[String], input: Reader[Char]): DelimParseResult = {
    val entry = pattern

    // FOR DEBUGGING might want this logging version
    val res = this.synchronized { this.parse(this.log(entry)("DelimParser.parseInputPatterned"), input) }
    //val res = this.parse(entry, input)

    res match {
      case s @ Success(_, _) => DelimParseSuccessFactory(s, "", DelimiterType.NotDelimited, None, DelimiterLocation.Local)
      case f: NoSuccess => DelimParseFailure(f.msg, f.next)
    }
  }

}
