/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors.dfa

import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.Reader
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.RuntimeData

/**
 * Parent class of all DFA text parsers.
 */
abstract class Parser extends Serializable {
  def name: String
  def info: String

  def context: RuntimeData

  override def toString(): String = name + "(context='" + context + "', " + info + ")"
}

/**
 * Parent class for 'delimited' DFA text parsers.  Needs 'longest match'
 * functionality.
 */
abstract class DelimitedParser extends Parser {
  /**
   * This function takes in a list of matches (in an ArrayBuffer for constant
   * append and random access) and returns the match that starts earliest in
   * the data. If multiple matches start at the same point in the data, the
   * match with the longer delimiter length is used as a tie breaker
   */
  protected def longestMatch(matches: ArrayBuffer[(DFADelimiter, Registers)]): Maybe[(DFADelimiter, Registers)] = {
    val len = matches.length
    if (len == 0) return Nope
    if (len == 1) return Some(matches(0))

    // these variables hold the Match/Registers of the match that starts earliest in
    // the data, using longest delimiter length as a tie breaker. Assume the
    // first match is the earliest longest to begin.
    var firstLongestMatchSoFar = matches(0)
    var (_, firstLongestRegSoFar) = firstLongestMatchSoFar

    var currIndex = 1 // skip the zeroth match since we assumed it was the first longest
    while (currIndex < len) {
      val currMatch = matches(currIndex)
      val (_, currReg) = currMatch
      if (currReg.matchStartPos < firstLongestRegSoFar.matchStartPos ||
        (currReg.matchStartPos == firstLongestRegSoFar.matchStartPos && currReg.delimString.length > firstLongestRegSoFar.delimString.length)) {
        firstLongestRegSoFar = currReg
        firstLongestMatchSoFar = currMatch
      }

      currIndex += 1
    }

    One(firstLongestMatchSoFar)
  }
}

class ParseResult(val field: Maybe[String],
  val matchedDelimiterValue: Maybe[String],
  val originalDelimiterRep: String) {

  private val format = "<DFAParseResult field='%s' foundDelimiter='%s' searchedFor='%s'/>"

  private lazy val fieldStr = field.getOrElse("NOT-FOUND")
  private lazy val matchedDelimStr = matchedDelimiterValue.getOrElse("NOT-FOUND")
  private lazy val originalDelimiterStr = originalDelimiterRep

  private lazy val formattedString = format.format(fieldStr, matchedDelimStr, originalDelimiterStr)

  override def toString(): String = formattedString
}
