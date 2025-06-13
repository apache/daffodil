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
import org.apache.daffodil.runtime1.processors.RuntimeData

/**
 * Parent class of all DFA text parsers.
 */
abstract class DFAParser extends Serializable {
  def name: String
  def info: String

  def context: RuntimeData

  override def toString(): String = name + "(context='" + context + "', " + info + ")"
}

class LongestMatchTracker {
  val longestMatches: ArrayBuffer[DFADelimiter] = new ArrayBuffer[DFADelimiter]
  var longestMatchedStartPos: Int = Int.MaxValue
  var longestMatchedString: String = null

  def successfulMatch(
    matchedStartPos: Int,
    matchedString: StringBuilder,
    dfa: DFADelimiter,
    dfaIndex: Int
  ): Unit = {
    if (longestMatches.isEmpty) {
      // first match, make it the longest
      longestMatchedStartPos = matchedStartPos
      longestMatchedString = matchedString.toString
      longestMatches.append(dfa)
    } else if (matchedStartPos < longestMatchedStartPos) {
      // match starts earlier than previous matches, make it the longest
      longestMatchedStartPos = matchedStartPos
      longestMatchedString = matchedString.toString
      longestMatches.dropRightInPlace(longestMatches.length)
      longestMatches.append(dfa)
    } else if (matchedStartPos == longestMatchedStartPos) {
      if (matchedString.length > longestMatchedString.length) {
        // match starts at the same point as previous matches, but
        // is longer. make it the only match
        longestMatchedString = matchedString.toString
        longestMatches.dropRightInPlace(longestMatches.length)
        longestMatches.append(dfa)
      } else if (matchedString.length == longestMatchedString.length) {
        // match starts at the same point as previous matches,
        // and is the same length. add it to matches
        longestMatches.append(dfa)
      }
    }
  }
}

class ParseResult(
  val field: Maybe[String],
  val matchedDelimiterValue: Maybe[String],
  val matchedDFAs: ArrayBuffer[DFADelimiter]
) {

  override def toString(): String = {

    val status =
      if (matchedDelimiterValue.isDefined) "Matched"
      else "NoMatch"
    val fieldStr =
      if (field.isDefined) " field='%s'".format(field.get)
      else ""
    val matchedDelimStr =
      if (matchedDelimiterValue.isDefined)
        " foundDelimiter='%s'".format(matchedDelimiterValue.get)
      else ""
    "<DFAParseResult status='%s'%s%s/>".format(status, fieldStr, matchedDelimStr)
  }
}
