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

package org.apache.daffodil.runtime1.processors.parsers

import java.util.regex.Matcher
import java.util.regex.Pattern
import scala.collection.mutable.Queue

import org.apache.daffodil.lib.util.OnStack
import org.apache.daffodil.runtime1.processors.Delimiter

trait NilMatcherMixin {

  protected def cookedNilValuesForParse: List[String]

  protected def ignoreCase: Boolean

  final lazy val isEmptyAllowed: Boolean =
    cookedNilValuesForParse.contains("%ES;")

  /**
   * Constructs an Array of Parser[String] which holds the Parser representations
   * of the delimList.
   *
   * Constructs an Array of String which holds the Regex representations of the
   * delimList.
   */
  private def buildDelims(delimList: Set[String], ignoreCase: Boolean): Array[String] = {
    val delimsRegex: Queue[String] = Queue.empty

    // We probably always want delims ordered:
    // Multi-char delims containing WSP+/*, WSP+, WSP*, multi-char delims, WSP, single-char delims

    sortDelims(delimList).toList.foreach(str => {
      val d = new Delimiter()
      d.compileDelimiter(str, ignoreCase)
      delimsRegex.enqueue(d.delimRegExParseDelim) // The regex representing the actual delimiter
    })
    delimsRegex.toArray
  }

  private def sortDelims(delimList: Set[String]): Seq[String] = {
    val wspStarByItself = delimList.filter(s => s == "%WSP*;")
    val wspPlusByItself = delimList.filter(s => s == "%WSP+;")

    val filteredDelimList = (delimList -- (wspStarByItself.union(wspPlusByItself)))

    val multiCharUnboundedLength =
      filteredDelimList.filter(s => (s.contains("%WSP*;") || s.contains("%WSP+;")))
    val multiChar = (filteredDelimList -- multiCharUnboundedLength).filter(s => s.length() > 1)
    val singleChar = filteredDelimList -- (multiChar.union(multiCharUnboundedLength))

    val sortedUnbounded = multiCharUnboundedLength.toArray[String]
    val sortedMultiChar = multiChar.toArray[String]

    scala.util.Sorting.quickSort(sortedUnbounded)
    scala.util.Sorting.quickSort(sortedMultiChar)

    val orderedResultSeq: Seq[String] =
      sortedUnbounded.reverse.toSeq ++ wspPlusByItself ++ wspStarByItself ++ sortedMultiChar.reverse.toSeq ++ singleChar
    orderedResultSeq
  }

  /**
   * Combines the delimiters into a single alternation
   */
  private def combineDelimitersRegex(
    sepsRegex: Array[String],
    termsRegex: Array[String]
  ): String = {
    val sb = new StringBuilder()
    sepsRegex.foreach(x => {
      sb.append(x)
      sb.append("|")
    })
    termsRegex.foreach(x => {
      sb.append(x)
      sb.append("|")
    })
    val delimRegex = sb.toString().replaceFirst("[\\|]$", "")

    delimRegex
  }

  private def getDfdlLiteralRegex(dfdlLiteralList: Set[String]): String = {
    val regex = this.buildDelims(dfdlLiteralList, ignoreCase)
    combineDelimitersRegex(regex, Array.empty[String])
  }

  private lazy val patternFlags: Int = {
    if (ignoreCase) Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE else 0
  }

  private lazy val patternLiteralValue: Pattern = {
    val dfdlLiteralRegex = getDfdlLiteralRegex(cookedNilValuesForParse.toSet)
    val p = Pattern.compile(dfdlLiteralRegex, patternFlags)
    p
  }

  private lazy val patternLiteralCharacter: Pattern = {
    // LiteralCharacter is legal for only fixed length elements.
    // It's a single character or byte that, when repeated to
    // length of the element, is the nil representation.
    //
    val dfdlLiteralRegex = getDfdlLiteralRegex(cookedNilValuesForParse.toSet)
    val sb = new StringBuilder(dfdlLiteralRegex)
    sb.append("+")
    val p = Pattern.compile(sb.toString(), patternFlags)
    p
  }

  private def newMatcher(): Matcher = {
    patternLiteralValue.matcher("")
  }

  private def newLiteralCharacterMatcher(): Matcher = {
    patternLiteralCharacter.matcher("")
  }

  object withFieldNilMatcher extends OnStack[Matcher](newMatcher(), (m: Matcher) => m.reset(""))
  object withFieldNilLiteralCharacterMatcher
    extends OnStack[Matcher](newLiteralCharacterMatcher(), (m: Matcher) => m.reset(""))

  // TODO: does this handle %ES; or do we have to have outside separate checks for that?
  // There is a separate check right now in LiteralNilDelimitedOrEndOfData.
  final def isFieldNilLiteralValue(field: String): Boolean = {
    withFieldNilMatcher { matcher =>
      matcher.reset(field)
      matcher.find()
      matcher.matches()
    }
  }

  final def isFieldNilLiteralCharacter(field: String): Boolean = {
    withFieldNilLiteralCharacterMatcher { matcher =>
      matcher.reset(field)
      matcher.find()
      matcher.matches()
    }
  }
}
