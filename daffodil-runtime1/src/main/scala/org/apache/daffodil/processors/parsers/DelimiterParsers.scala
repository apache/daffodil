/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.processors.parsers

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.dfa.TextParser
import org.apache.daffodil.processors.dfa.DFADelimiter
import org.apache.daffodil.util.Enum
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.processors.DelimiterIterator
import org.apache.daffodil.processors.LocalTypedDelimiterIterator
import org.apache.daffodil.processors.RemoteTypedDelimiterIterator
import org.apache.daffodil.processors.RemoteTerminatingMarkupAndLocalTypedDelimiterIterator
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.util.Misc
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
  extends TextPrimParser {

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
        //
        // TODO: PERFORMANCE: this should also help performance by eliminating the construction of lists/sets of
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

        val result = textParser.parse(start, start.dataInputStream, delimIter, true)
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
      val wasDelimiterTextSkipped = start.dataInputStream.skipChars(nChars, start)
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
