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
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.runtime1.processors.DelimiterIterator
import org.apache.daffodil.runtime1.processors.parsers.PState

/**
 * Checks whether one of the in-scope delimiters (separator or
 * terminator) matches right at the current position, with no field content
 * scanning, no escape scheme handling, and no data consumption: the input
 * position is always restored to where it started, whether or not a
 * delimiter was found.
 *
 * Used to determine whether a representation at this position is
 * genuinely zero-length, independent of any field-content DFA.
 */
object ZeroLengthDelimiterScanner {
  def scan(
    state: PState,
    input: DataInputStream,
    delimIter: DelimiterIterator
  ): Maybe[ParseResult] = {
    val lmt = new LongestMatchTracker()
    val startPos = input.markPos
    delimIter.reset()
    while (delimIter.hasNext()) {
      val d = delimIter.next()
      input.resetPos(startPos)
      val delimReg: Registers = state.dfaRegistersPool.getFromPool("ZeroLengthDelimiterScanner")
      delimReg.reset(state, input, delimIter)
      d.run(delimReg)
      if (delimReg.status == StateKind.Succeeded) {
        lmt.successfulMatch(
          delimReg.matchStartPos,
          delimReg.delimString,
          d,
          delimIter.currentIndex
        )
      }
      state.dfaRegistersPool.returnToPool(delimReg)
    }
    // zero-length by construction: never consume, regardless of outcome
    input.resetPos(startPos)
    if (lmt.longestMatches.isEmpty) Nope
    else One(new ParseResult(One(""), One(lmt.longestMatchedString), lmt.longestMatches))
  }
}
