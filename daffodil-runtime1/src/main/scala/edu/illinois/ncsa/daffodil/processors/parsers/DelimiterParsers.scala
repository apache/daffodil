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

package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.dfa.TextParser
import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter
import edu.illinois.ncsa.daffodil.util.Enum
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData
import edu.illinois.ncsa.daffodil.processors.TextParserRuntimeMixin
import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.processors.DelimiterIterator
import edu.illinois.ncsa.daffodil.processors.LocalTypedDelimiterIterator
import edu.illinois.ncsa.daffodil.processors.RemoteTypedDelimiterIterator
import edu.illinois.ncsa.daffodil.processors.RemoteTerminatingMarkupAndLocalTypedDelimiterIterator
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.util.Misc
import scala.collection.mutable.ArrayBuffer

object DelimiterTextType extends Enum {
  abstract sealed trait Type extends EnumValueType
  case object Initiator extends Type
  case object Separator extends Type
  case object Terminator extends Type
  case object Other extends Type /* for DelimiterDFAs that are things like escapes that we have other ways of tracking what they are */
}

class DelimiterTextParser(
  rd: TermRuntimeData,
  textParser: TextParser,
  positionalInfo: String,
  delimiterType: DelimiterTextType.Type)
  extends PrimParser 
  with TextParserRuntimeMixin {

  override lazy val runtimeDependencies = rd.encodingInfo.runtimeDependencies
  override def context = rd

  override val nom = delimiterType.toString

  private def containsLocalMatch(delimiters: ArrayBuffer[DFADelimiter], state: PState): Boolean = {
    var i = delimiters.length - 1
    // scan matches in reverse. local matches are going to be at the end
    while (i >= 0) {
      val dfa = delimiters(i)
      if (dfa.indexInDelimiterStack < state.mpstate.delimitersLocalIndexStack.top) {
        // we found a remote delim. since all remote delimiters are first, and
        // we are traversing this in reverse, we know all remaining delims are
        // remotes, so we can return early
        return false
      }
      // note, it is possible for dfaIndex to be greater than the length of the
      // found delimiters. This just means a found delimiter went out of scope
      // (and thus, not a local delimiter)
      if (dfa.delimType == delimiterType && dfa.indexInDelimiterStack < state.mpstate.delimiters.length) {
        // found a local delim
        return true
      }
      i -= 1
    }
    return false
  }

  private def findES(delimIter: DelimiterIterator): Maybe[DFADelimiter] = {
    delimIter.reset()
    while (delimIter.hasNext()) {
      val d = delimIter.next()
      if (d.lookingFor == "%ES;") {
        return One(d)
      }
    }
    Nope
  }

  private def findLocalES(state: PState): Maybe[DFADelimiter] = {
    val delimIter = new LocalTypedDelimiterIterator(delimiterType, state.mpstate.delimiters, state.mpstate.delimitersLocalIndexStack)
    findES(delimIter)
  }

  private def findRemoteES(state: PState): Maybe[DFADelimiter] = {
    val delimIter = new RemoteTypedDelimiterIterator(delimiterType, state.mpstate.delimiters, state.mpstate.delimitersLocalIndexStack)
    findES(delimIter)
  }

  override def parse(start: PState): Unit = {

    setupEncoding(start, rd)

    val foundDelimiter =
      if (delimiterType == DelimiterTextType.Initiator || !start.delimitedParseResult.isDefined) {
        //
        // We are going to scan for delimiter text.
        //
        // FIXME: This is incorrect. It grabs all local delimiters even if we're last in a group so the separator
        // cannot be relevant, or if we're in the middle of a group with required elements following so
        // the terminator cannot be relevant.
        //
        // Fixing this is going to require the compiler to pre-compute the relevant delimiters for every
        // Term. (relevant delimiters meaning the specific compiled expressions that are relevant.)
        // PERFORMANCE: this should also help performance by eliminating the construction of lists/sets of
        // these things at run time.
        //
        // FIXME: This is incorrect. It is going to get too many delimiters. See above.
        // Nowever, code below does assume that we always find a match, even to incorrect markup, so fixing this is
        // more than just getting the right set of remote delims here.
        //
        // Note: It isn't even as simple as precalculating all the delimiters
        // (which isn't easy to start with). For example, sometimes we do not
        // know if we're at the last in the group until runtime due to an
        // expression that determines occursCount. So this functions needs to
        // make a runtime calculation to determine if it is the last in a
        // group, which might be difficult to do.
        val delimIter = new RemoteTerminatingMarkupAndLocalTypedDelimiterIterator(delimiterType, start.mpstate.delimiters, start.mpstate.delimitersLocalIndexStack)

        val result = textParser.parse(start.dataInputStream, delimIter, true)
        result
      } else {
        start.delimitedParseResult
      }

    if (foundDelimiter.isDefined) {
      if (!containsLocalMatch(foundDelimiter.get.matchedDFAs, start)) {
        // It was a remote delimiter but we should have found a local one.
        PE(start, "Found out of scope delimiter: %s", Misc.remapStringToVisibleGlyphs(foundDelimiter.get.matchedDelimiterValue.get))
        return
      }

      // Consume the found local delimiter
      val nChars = foundDelimiter.get.matchedDelimiterValue.get.length
      val wasDelimiterTextSkipped = start.dataInputStream.skipChars(nChars)
      Assert.invariant(wasDelimiterTextSkipped)
      start.clearDelimitedParseResult()
    } else {
      // Did not find delimiter.
      //
      // Still can be ok if ES is a delimiter. That's allowed when we're not lengthKind='delimited'
      // That is, it is allowed if the delimiter is just part of the data syntax, but is not being
      // used to determine length.
      if (findLocalES(start).isDefined) {
        // found local ES, nothing to consume
        return
      } else {
        val rES = findRemoteES(start)
        if (rES.isDefined) {
          // has remote but not local (PE)
          PE(start, "Found out of scope delimiter: %ES;")
          return
        } else {
          // no match and no ES in delims
          PE(start, "Delimiter not found.")
          return
        }
      }
    }
  }
}
