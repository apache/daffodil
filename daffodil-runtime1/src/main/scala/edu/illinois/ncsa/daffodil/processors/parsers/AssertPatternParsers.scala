package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.util.LogLevel
import java.util.regex.Matcher
import edu.illinois.ncsa.daffodil.util.OnStack

abstract class AssertPatternParserBase(
  eName: String,
  kindString: String,
  rd: TermRuntimeData,
  testPattern: String,
  message: String)
  extends PrimParserObject(rd) {

  override def toBriefXML(depthLimit: Int = -1) = {
    "<" + kindString + ">" + testPattern + "</" + kindString + ">"
  }

  // private lazy val compiledPattern = ScalaPatternParser.compilePattern(testPattern, rd)

  lazy val pattern = ("(?s)" + testPattern).r.pattern // imagine a really big expensive pattern to compile.
  object withMatcher extends OnStack[Matcher](pattern.matcher(""))

  final def parse(start: PState): Unit = {
    withParseErrorThrowing(start) {
      val bytePos = (start.bitPos >> 3).toInt
      log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, start.bitPos)
      log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

      log(LogLevel.Debug, "%s - Looking for testPattern = %s", eName, testPattern)

      val dis = start.dataInputStream
      val mark = dis.markPos
      withMatcher { m =>
        val isMatch = dis.lookingAt(m)
        afterParse(start, isMatch, m)
      }
      dis.resetPos(mark)
    }
  }

  protected def afterParse(start: PState, isMatch: Boolean, matcher: Matcher): Unit
}

class AssertPatternParser(
  eName: String,
  kindString: String,
  rd: TermRuntimeData,
  testPattern: String,
  message: String)
  extends AssertPatternParserBase(eName, kindString, rd, testPattern, message) {

  def afterParse(start: PState, isMatch: Boolean, matcher: Matcher) {
    if (isMatch) {
      log(LogLevel.Debug, "Assert Pattern success for testPattern %s", testPattern)
    } else {
      log(LogLevel.Debug, "Assert Pattern fail for testPattern %s", testPattern)
      val diag = new AssertionFailed(rd.schemaFileLocation, start, message)
      start.setFailed(diag)
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

  def afterParse(start: PState, isMatch: Boolean, matcher: Matcher) {
    if (isMatch) {
      // Only want to set the discriminator if it is true
      // we do not want to modify it unless it's true
      start.setDiscriminator(true)
    } else {
      val diag = new AssertionFailed(rd.schemaFileLocation, start, message)
      start.setFailed(diag)
    }
  }
}
