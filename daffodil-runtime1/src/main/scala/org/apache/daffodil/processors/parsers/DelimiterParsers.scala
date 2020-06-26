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
import org.apache.daffodil.processors.LocalTypedDelimiterIterator
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
  delimiterType: DelimiterTextType.Type,
  isDelimited: Boolean,
  mustMatchNonZeroData: Boolean)
  extends TextPrimParser {

  override lazy val runtimeDependencies = rd.encodingInfo.runtimeDependencies
  override def context = rd

  override val nom = delimiterType.toString

  private def containsLocalMatch(matchedDelimiters: ArrayBuffer[DFADelimiter], state: PState): Boolean = {
    val localIndexStart = state.mpstate.delimitersLocalIndexStack.top
    val inScopeDelimiters = state.mpstate.delimiters

    var foundLocalDFAIndex = -1

    // scan matches in reverse. local matches are going to be at the end
    var i = matchedDelimiters.length - 1
    while (foundLocalDFAIndex < 0 && i >= 0) {
      val dfa = matchedDelimiters(i)
      if (dfa.delimType == delimiterType) {
        foundLocalDFAIndex = inScopeDelimiters.indexWhere(dfa eq _, localIndexStart)
      }
      i -= 1
    }
    foundLocalDFAIndex >= 0
  }

  override def parse(start: PState): Unit = {

    val maybeDelimIter =
      if (delimiterType == DelimiterTextType.Terminator && !isDelimited) {
        Maybe(new LocalTypedDelimiterIterator(delimiterType, start.mpstate.delimiters, start.mpstate.delimitersLocalIndexStack.top))
      } else if (delimiterType == DelimiterTextType.Initiator || !start.delimitedParseResult.isDefined) {
        Maybe(new RemoteTerminatingMarkupAndLocalTypedDelimiterIterator(delimiterType, start.mpstate.delimiters, start.mpstate.delimitersLocalIndexStack.top))
      } else {
        Nope
      }

    val foundDelimiter =
      if (maybeDelimIter.isDefined) {
        textParser.parse(start, start.dataInputStream, maybeDelimIter.get, true)
      } else {
        start.delimitedParseResult
      }

    if (foundDelimiter.isDefined) {
      if (!containsLocalMatch(foundDelimiter.get.matchedDFAs, start)) {
        // It was a remote delimiter but we should have found a local one.
        PE(start, "Found out of scope delimiter: '%s' '%s'",
          foundDelimiter.get.matchedDFAs(0).lookingFor,
          Misc.remapStringToVisibleGlyphs(foundDelimiter.get.matchedDelimiterValue.get))
        return
      }

      // Consume the found local delimiter but also check if it was supposed to match
      // a non-zero number of bits and throw a runtime SDE if necessary
      val nChars = foundDelimiter.get.matchedDelimiterValue.get.length
      if (mustMatchNonZeroData && nChars == 0) {
        start.SDE("The initiator must match non-zero length data when dfdl:initiatedContent is 'yes'.")
      }
      val wasDelimiterTextSkipped = start.dataInputStream.skipChars(nChars, start)
      Assert.invariant(wasDelimiterTextSkipped)
      start.clearDelimitedParseResult()
    } else {
      // no match found, gather up the local typed delims for an error message

      val scannedDelims = maybeDelimIter.get
      scannedDelims.reset()

      // skip remote delims
      while (scannedDelims.hasNext() && scannedDelims.isRemote) {
        // do nothing, hasNext will increment the iterator
      }

      // gather local typed delims
      var localTypedDelims = scannedDelims.next().lookingFor
      while (scannedDelims.hasNext()) {
        localTypedDelims = localTypedDelims + " " + scannedDelims.next().lookingFor
      }

      PE(start, "%s '%s' not found", delimiterType.toString, localTypedDelims)
      return
    }
  }
}
