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

package org.apache.daffodil.processors.dfa

import scala.collection.mutable.ArrayBuffer
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.processors.RuntimeData

/**
 * Parent class of all DFA text parsers.
 */
abstract class Parser extends Serializable {
  def name: String
  def info: String

  def context: RuntimeData

  override def toString(): String = name + "(context='" + context + "', " + info + ")"
}

class LongestMatchTracker {
  val longestMatches: ArrayBuffer[DFADelimiter] = ArrayBuffer.empty
  var longestMatchedStartPos: Int = Int.MaxValue
  var longestMatchedString: String = null

  def successfulMatch(matchedStartPos: Int, matchedString: StringBuilder, dfa: DFADelimiter, dfaIndex: Int) {
    if (longestMatches.isEmpty) {
      // first match, make it the longest
      longestMatchedStartPos = matchedStartPos
      longestMatchedString = matchedString.toString
      longestMatches.append(dfa)
    } else if (matchedStartPos < longestMatchedStartPos) {
      // match starts earlier than previous matches, make it the longest
      longestMatchedStartPos = matchedStartPos
      longestMatchedString = matchedString.toString
      longestMatches.reduceToSize(0)
      longestMatches.append(dfa)
    } else if (matchedStartPos == longestMatchedStartPos) {
      if (matchedString.length > longestMatchedString.length) {
        // match starts at the same point as previous matches, but
        // is longer. make it the only match
        longestMatchedString = matchedString.toString
        longestMatches.reduceToSize(0)
        longestMatches.append(dfa)
      } else if (matchedString.length == longestMatchedString.length) {
        // match starts at the same point as previous matches,
        // and is the same length. add it to matches
        longestMatches.append(dfa)
      }
    }
  }
}

class ParseResult(val field: Maybe[String],
  val matchedDelimiterValue: Maybe[String],
  val matchedDFAs: ArrayBuffer[DFADelimiter]) {

  override def toString(): String = {
  
    val fieldStr = if (field.isDefined) field.get else "NOT-FOUND"
    val matchedDelimStr = if (matchedDelimiterValue.isDefined) matchedDelimiterValue.get else "NOT-FOUND"
    "<DFAParseResult field='%s' foundDelimiter='%s' />".format(fieldStr, matchedDelimStr)
  }
}
