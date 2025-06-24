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

package org.apache.daffodil.runtime1.processors.dfa

import org.apache.daffodil.io.DataInputStream
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.runtime1.processors.DelimiterIterator
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.parsers.PState

class TextParser(override val context: TermRuntimeData) extends DFAParser {

  override lazy val name: String = "TextParser"
  override lazy val info: String = "" // Nothing additional to add here

  def parse(
    state: PState,
    input: DataInputStream,
    delimIter: DelimiterIterator
  ): Maybe[ParseResult] = {

    val lmt = new LongestMatchTracker()

    var m = input.markPos
    delimIter.reset()
    while (delimIter.hasNext()) {
      val d = delimIter.next()
      val reg = state.dfaRegistersPool.getFromPool("TextParser1")
      reg.reset(state, input, delimIter, m)
      m = input.markPos
      d.run(reg)
      if (reg.status == StateKind.Succeeded) {
        lmt.successfulMatch(reg.matchStartPos, reg.delimString, d, delimIter.currentIndex)
      }
      state.dfaRegistersPool.returnToPool(reg)
    }
    input.resetPos(m)

    val result = {
      if (lmt.longestMatches.isEmpty) {
        Nope
      } else {
        val delim: Maybe[String] = {
          One(lmt.longestMatchedString.toString)
        }
        One(new ParseResult(Nope, delim, lmt.longestMatches))
      }
    }

    state.dfaRegistersPool.finalCheck()

    result
  }
}
