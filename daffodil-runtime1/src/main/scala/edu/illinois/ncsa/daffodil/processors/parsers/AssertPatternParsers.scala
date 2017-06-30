/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors._
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
