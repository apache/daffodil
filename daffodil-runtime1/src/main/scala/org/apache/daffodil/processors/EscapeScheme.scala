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

package org.apache.daffodil.processors

import org.apache.daffodil.processors.dfa.CreateFieldDFA
import org.apache.daffodil.processors.dfa.DFADelimiter
import org.apache.daffodil.processors.dfa.CreateDelimiterDFA
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.schema.annotation.props.gen.GenerateEscape
import org.apache.daffodil.util.MaybeChar
import org.apache.daffodil.processors.parsers.DelimiterTextType
import org.apache.daffodil.dsom.DPathCompileInfo

sealed abstract class EscapeSchemeParserHelper
case class EscapeSchemeCharParserHelper(val ec: Char, val eec: MaybeChar)
  extends EscapeSchemeParserHelper {

  override def toString() = "<EscapeSchemeChar escapeChar='" + ec +
    "' escapeEscapeChar='" + (if (eec.isDefined) eec.get.toString else "") + "'/>"
}
case class EscapeSchemeBlockParserHelper(
  val eec: MaybeChar,
  blockStart: String,
  blockEnd: String,
  rd: DPathCompileInfo)
  extends EscapeSchemeParserHelper {
  // Should note there that fieldDFA (not here) is dependent on
  // the whether or not the delimiters are constant or not.
  // As a result, it cannot be generated here.

  val blockStartDFA: DFADelimiter = CreateDelimiterDFA(DelimiterTextType.Other, rd, blockStart, false)
  val blockEndDFA: DFADelimiter = CreateDelimiterDFA(DelimiterTextType.Other, rd, blockEnd, false)
  val fieldEscDFA = CreateFieldDFA(blockEndDFA, eec)

  override def toString() = "<EscapeSchemeBlock escapeEscapeChar='" + (if (eec.isDefined) eec.get.toString else "") +
    "' blockStart='" + blockStart + "' blockEnd='" + blockEnd + "'/>"
}

sealed abstract class EscapeSchemeUnparserHelper {
  def lookingFor: Array[DFADelimiter]
}
case class EscapeSchemeCharUnparserHelper(val ec: Char, val eec: MaybeChar, extraEscChar: Seq[Char], rd: DPathCompileInfo)
  extends EscapeSchemeUnparserHelper {

  // We need to look for the escapeCharacter and the extraEscapedCharacters
  //
  val escCharDFA: DFADelimiter = CreateDelimiterDFA(DelimiterTextType.Other, rd, ec.toString, false)
  val escEscCharDFA: Maybe[DFADelimiter] = if (eec.isDefined) One(CreateDelimiterDFA(DelimiterTextType.Other, rd, eec.toString, false)) else Nope
  val extraEscCharsDFAs: Array[DFADelimiter] = CreateDelimiterDFA(DelimiterTextType.Other, rd, extraEscChar.map(_.toString), false)

  override val lookingFor = {
    val res: Array[DFADelimiter] =
      if (escEscCharDFA.isDefined) escCharDFA +: escCharDFA +: escEscCharDFA.get +: escEscCharDFA.get +: extraEscCharsDFAs
      else escCharDFA +: escCharDFA +: extraEscCharsDFAs
    res
  }

  override def toString() = "<EscapeSchemeChar escapeChar='" + ec +
    "' escapeEscapeChar='" + (if (eec.isDefined) eec.get.toString else "") + "' extraEscapedChars='" + extraEscChar.mkString(" ") + "'/>"
}
case class EscapeSchemeBlockUnparserHelper(
  val eec: MaybeChar,
  blockStart: String,
  blockEnd: String,
  private val extraEscChar: Seq[Char],
  generateEscapeBlock: GenerateEscape,
  rd: DPathCompileInfo)
  extends EscapeSchemeUnparserHelper {

  // We need to look for the blockEnd
  //
  val blockEndDFA: DFADelimiter = CreateDelimiterDFA(DelimiterTextType.Other, rd, blockEnd, false)
  val blockStartDFA: DFADelimiter = CreateDelimiterDFA(DelimiterTextType.Other, rd, blockStart, false)
  val fieldEscDFA = CreateFieldDFA(blockEndDFA, eec)
  val extraEscCharsDFAs: Array[DFADelimiter] = CreateDelimiterDFA(DelimiterTextType.Other, rd, extraEscChar.map(_.toString), false)

  override val lookingFor = blockStartDFA +: extraEscCharsDFAs

  override def toString() = "<EscapeSchemeBlock escapeEscapeChar='" + (if (eec.isDefined) eec.get.toString else "") +
    "' blockStart='" + blockStart + "' blockEnd='" + blockEnd + "' generateEscapeBlock='" + generateEscapeBlock + "'/>"
}
