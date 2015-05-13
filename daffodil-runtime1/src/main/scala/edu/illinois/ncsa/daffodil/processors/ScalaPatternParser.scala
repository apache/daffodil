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

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Reader
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE

/**
 * Too many things named Parser.
 *
 * A ScalaPatternParser is an encapsulation of Scala's built in Combinator parsers
 * from scala.util.parsing.combinator.Parsers.
 *
 * The only reason why we need these combinator parsers at all, in the
 * runtime, is that they allow regex matching against an unbounded source
 * of characters.
 *
 * Java's regular Pattern and Matcher operate on finite CharSequence objects.
 * We need a way to match against a stream of characters. Scala's combinator
 * parsers accept a Reader[Char] and consume characters one by one from it
 * without bound.
 *
 * This is much more convenient than dealing with finite CharSequence objects,
 * resizing them if not big enough, dealing with all the situations like
 * having hit the end without resolving match or not, etc.
 */
object ScalaPatternParser extends RegexParsers {

  override val skipWhitespace = false

  /**
   * We encapsulate a ParseResult in this additional wrapper because it avoids
   * the problem of things like strings being auto-coerced into scala's combinator parsers.
   *
   * This is a value class, so requires (in theory, if the scala compiler is doing its job)
   * no additional object allocation beyond what the underlying ParseResult object requires.
   */
  final class PatternParseResult(val v: ParseResult[String]) extends AnyVal {
    private def success = v.asInstanceOf[Success]
    private def failure = v.asInstanceOf[Failure]
    def isSuccess: Boolean = v.isInstanceOf[Success]
    def isFailure = !isSuccess
    def next = v.next.asInstanceOf[DFDLCharReader]
    def nextReader = v.next
    def numBits(termRuntimeData: TermRuntimeData): Int = termRuntimeData.encodingInfo.knownEncodingStringBitLength(field)
    def numCharsRead: Int = field.length
    def field = success.result
    def get = v.get
    def msg = failure.msg
  }
  private type Success = super.Success[String]
  private type Failure = super.NoSuccess

  @inline def apply(v: ScalaPatternParser.Parser[String]) = new ScalaPatternParser(v)

  /**
   * The only operation on ScalaPatternParsers is to parse them against a reader.
   */
  final def parseInputPatterned(scalaParser: ScalaPatternParser, input: Reader[Char]): PatternParseResult = {
    scalaParser.parse(input)
  }

  /**
   * The only way to create a ScalaPatternParser is by compiling a regex string
   */
  final def compilePattern(pattern: String, sc: ThrowsSDE): ScalaPatternParser = {
    try {
      // ?s is a flag that means that the . (dot) character matches newlines as well as within-a-line spaces
      val thePattern = ("(?s)" + pattern).r
      val entry = ScalaPatternParser(thePattern)
      entry
    } catch {
      case ex: java.util.regex.PatternSyntaxException => sc.SDE("%s", ex.getMessage)
    }
  }

}

final class ScalaPatternParser private (val v: ScalaPatternParser.Parser[String]) {
  def parse(input: Reader[Char]): ScalaPatternParser.PatternParseResult = {
    val res = ScalaPatternParser.parse(v, input)
    new ScalaPatternParser.PatternParseResult(res)
  }
}

