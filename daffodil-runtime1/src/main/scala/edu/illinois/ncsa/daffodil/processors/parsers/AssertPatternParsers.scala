package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.processors.charset._
import java.nio.charset.Charset
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.Debug
import edu.illinois.ncsa.daffodil.util.PreSerialization

abstract class AssertPatternParserBase(
  eName: String,
  kindString: String,
  rd: TermRuntimeData,
  testPattern: String,
  message: String)
  extends PrimParser(rd)
  with TextReader {

  override def toBriefXML(depthLimit: Int = -1) = {
    "<" + kindString + ">" + testPattern + "</" + kindString + ">"
  }

  private lazy val compiledPattern = ScalaPatternParser.compilePattern(testPattern, rd)

  final def parse(start: PState): Unit = {
    withParseErrorThrowing(start) {
      val bytePos = (start.bitPos >> 3).toInt
      log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, start.bitPos)
      log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

      log(LogLevel.Debug, "%s - Looking for testPattern = %s", eName, testPattern)

      if (start.bitPos % 8 != 0) {
        PE(start, "%s - not byte aligned.", eName)
        return
      }

      log(LogLevel.Debug, "Retrieving reader")

      val reader = getReader(rd.encodingInfo.knownEncodingCharset.charset, start.bitPos, start)

      val result = ScalaPatternParser.parseInputPatterned(compiledPattern, reader)

      afterParse(start, result)
    }
  }

  protected def afterParse(start: PState, result: ScalaPatternParser.PatternParseResult): Unit
}

class AssertPatternParser(
  eName: String,
  kindString: String,
  rd: TermRuntimeData,
  testPattern: String,
  message: String)
  extends AssertPatternParserBase(eName, kindString, rd, testPattern, message) {

  import ScalaPatternParser._

  def afterParse(start: PState, result: PatternParseResult) {
    result match {
      case s if result.isSuccess => {
        val endBitPos = start.bitPos + s.numBits(rd)
        log(LogLevel.Debug, "Assert Pattern success for testPattern %s", testPattern)
      }
      case f => {
        log(LogLevel.Debug, "Assert Pattern fail for testPattern %s\nDetails: %s", testPattern, f.msg)
        val diag = new AssertionFailed(rd.schemaFileLocation, start, message, One(f.msg))
        start.setFailed(diag)
      }
    }
  }
}

class DiscriminatorPatternParser(
  testPattern: String,
  eName: String,
  kindString: String,
  rd: TermRuntimeData,
  message: String)
  extends AssertPatternParserBase(eName, kindString, rd, testPattern, message) {

  import ScalaPatternParser._

  def afterParse(start: PState, result: PatternParseResult) {
    // Only want to set the discriminator if it is true
    // we do not want to modify it unless it's true
    result match {
      case s if s.isSuccess => start.setDiscriminator(true)
      case f => {
        val diag = new AssertionFailed(rd.schemaFileLocation, start, message, One(f.msg))
        start.setFailed(diag)
      }
    }
  }
}