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

import scala.collection.mutable.ArrayBuffer

import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.runtime1.processors.RuntimeData

/**
 * Parent class of all DFA text parsers.
 */
abstract class Unparser extends Serializable {
  def name: String
  def info: String

  def context: RuntimeData

  override def toString(): String = name + "(context='" + context + "', " + info + ")"
}

abstract class DelimitedUnparser extends Unparser {

  /**
   * This function takes in a list of matches (in an ArrayBuffer for constant
   * append and random access) and returns the match that starts earliest in
   * the data. If multiple matches start at the same point in the data, the
   * match with the longer delimiter length is used as a tie breaker
   */
  protected def longestMatch(
    matches: ArrayBuffer[(DFADelimiter, Registers)]
  ): Maybe[(DFADelimiter, Registers)] = {
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
      if (
        currReg.matchStartPos < firstLongestRegSoFar.matchStartPos ||
        (currReg.matchStartPos == firstLongestRegSoFar.matchStartPos && currReg.delimString.length > firstLongestRegSoFar.delimString.length)
      ) {
        firstLongestRegSoFar = currReg
        firstLongestMatchSoFar = currMatch
      }

      currIndex += 1
    }

    One(firstLongestMatchSoFar)
  }
}
