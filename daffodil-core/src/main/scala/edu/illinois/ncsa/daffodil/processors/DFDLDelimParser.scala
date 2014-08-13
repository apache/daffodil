package edu.illinois.ncsa.daffodil.processors
import scala.util.parsing.input.Reader
import edu.illinois.ncsa.daffodil.processors.DelimiterType._
import edu.illinois.ncsa.daffodil.processors.DelimiterLocation._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.AnnotatedSchemaComponent
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._

class DFDLDelimParser(knownEncodingIsFixedWidth: Boolean,
  knownEncodingWidthInBits: Int,
  knownEncodingName: String) extends DFDLDelimParserCommon(knownEncodingIsFixedWidth, knownEncodingWidthInBits, knownEncodingName) {

  def parseInputPatterned(pattern: String, input: Reader[Char], sc: ThrowsSDE): DelimParseResult = {
    val entry = this.generateInputPatternedParser(pattern, sc)
    // FIXME: Performance - hoist out... this compiles regex here at match time.

    // FOR DEBUGGING might want this logging version
    val res = this.parse(this.log(entry)("DelimParser.parseInputPatterned"), input)
    //val res = this.parse(entry, input)

    res match {
      case s @ Success(_, _) => DelimParseSuccessFactory(s, "", DelimiterType.NotDelimited, Nope, DelimiterLocation.Local)
      case f: NoSuccess => DelimParseFailure(f.msg, f.next)
    }
  }

  def parseInputPatterned(pattern: Parser[String], input: Reader[Char]): DelimParseResult = {
    val entry = pattern

    // FOR DEBUGGING might want this logging version
    val res = this.parse(this.log(entry)("DelimParser.parseInputPatterned"), input)
    //val res = this.parse(entry, input)

    res match {
      case s @ Success(_, _) => DelimParseSuccessFactory(s, "", DelimiterType.NotDelimited, Nope, DelimiterLocation.Local)
      case f: NoSuccess => DelimParseFailure(f.msg, f.next)
    }
  }

}
