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

package org.apache.daffodil.runtime1.processors

import org.apache.daffodil.lib.cookers.EscapeBlockEndCooker
import org.apache.daffodil.lib.cookers.EscapeBlockStartCooker
import org.apache.daffodil.lib.cookers.EscapeCharacterCooker
import org.apache.daffodil.lib.cookers.EscapeEscapeCharacterCooker
import org.apache.daffodil.lib.cookers.ExtraEscapedCharactersCooker
import org.apache.daffodil.lib.schema.annotation.props.gen.GenerateEscape
import org.apache.daffodil.lib.util.MStackOfMaybe
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.MaybeChar
import org.apache.daffodil.runtime1.dsom._
import org.apache.daffodil.runtime1.processors.parsers.PState
import org.apache.daffodil.runtime1.processors.unparsers.UState

class EscapeCharEv(expr: CompiledExpression[String], ci: DPathCompileInfo)
  extends EvaluatableConvertedExpression[String, String](expr, EscapeCharacterCooker, ci)
  with InfosetCachedEvaluatable[String] {
  override def runtimeDependencies = Vector()
}

class EscapeEscapeCharEv(expr: CompiledExpression[String], ci: DPathCompileInfo)
  extends EvaluatableConvertedExpression[String, String](expr, EscapeEscapeCharacterCooker, ci)
  with InfosetCachedEvaluatable[String] {
  override def runtimeDependencies = Vector()
}
class ExtraEscapedCharsEv(expr: CompiledExpression[String], ci: DPathCompileInfo)
  extends EvaluatableConvertedExpression[String, Seq[String]](
    expr,
    ExtraEscapedCharactersCooker,
    ci
  )
  with InfosetCachedEvaluatable[Seq[String]] {
  override def runtimeDependencies = Vector()
}

trait EscapeSchemeCommonEv {
  def optEscapeEscapeChar: Maybe[EscapeEscapeCharEv]

  def evalAndConvertEEC(state: ParseOrUnparseState): MaybeChar = {
    if (optEscapeEscapeChar.isDefined) {
      val escEscChar = optEscapeEscapeChar.get.evaluate(state)
      MaybeChar(escEscChar.charAt(0))
    } else {
      MaybeChar.Nope
    }
  }
}

abstract class EscapeSchemeParseEv(ci: DPathCompileInfo)
  extends Evaluatable[EscapeSchemeParserHelper](ci)
  with ManuallyCachedEvaluatable[EscapeSchemeParserHelper]
  with EscapeSchemeCommonEv {

  protected def getCacheStack(state: State): MStackOfMaybe[EscapeSchemeParserHelper] = {
    state.asInstanceOf[PState].mpstate.escapeSchemeEVCache
  }
}

abstract class EscapeSchemeUnparseEv(ci: DPathCompileInfo)
  extends Evaluatable[EscapeSchemeUnparserHelper](ci)
  with ManuallyCachedEvaluatable[EscapeSchemeUnparserHelper]
  with EscapeSchemeCommonEv {

  protected def getCacheStack(state: State): MStackOfMaybe[EscapeSchemeUnparserHelper] = {
    state.asInstanceOf[UState].escapeSchemeEVCache
  }

  def extraEscapedChars: Maybe[ExtraEscapedCharsEv]

  def evalAndConvertExtraEscapedCharacters(state: ParseOrUnparseState): Seq[Char] = {
    if (extraEscapedChars.isDefined) {
      val extEscChar = extraEscapedChars.get
        .evaluate(state)
        .map(_.charAt(0))
      extEscChar
    } else {
      Seq()
    }
  }
}

class EscapeSchemeCharParseEv(
  escapeChar: EscapeCharEv,
  override val optEscapeEscapeChar: Maybe[EscapeEscapeCharEv],
  ci: DPathCompileInfo
) extends EscapeSchemeParseEv(ci) {

  override def runtimeDependencies = Vector(escapeChar) ++ optEscapeEscapeChar.toList

  def compute(state: ParseOrUnparseState) = {
    val escChar = escapeChar.evaluate(state).charAt(0)
    val optEscEscChar = evalAndConvertEEC(state)
    new EscapeSchemeCharParserHelper(escChar, optEscEscChar)
  }
}

class EscapeSchemeCharUnparseEv(
  escapeChar: EscapeCharEv,
  override val optEscapeEscapeChar: Maybe[EscapeEscapeCharEv],
  override val extraEscapedChars: Maybe[ExtraEscapedCharsEv],
  ci: DPathCompileInfo
) extends EscapeSchemeUnparseEv(ci) {

  override def runtimeDependencies =
    Vector(escapeChar) ++ optEscapeEscapeChar.toList ++ extraEscapedChars.toList

  def compute(state: ParseOrUnparseState) = {
    val escChar = escapeChar.evaluate(state).charAt(0)
    val optEscEscChar = evalAndConvertEEC(state)
    val extEscChars = evalAndConvertExtraEscapedCharacters(state)
    new EscapeSchemeCharUnparserHelper(
      escChar,
      optEscEscChar,
      extEscChars,
      ci
    )
  }
}

class EscapeSchemeBlockParseEv(
  blockStart: String,
  blockEnd: String,
  override val optEscapeEscapeChar: Maybe[EscapeEscapeCharEv],
  ci: DPathCompileInfo
) extends EscapeSchemeParseEv(ci) {

  override def runtimeDependencies = optEscapeEscapeChar.toList

  val bs = EscapeBlockStartCooker.convertConstant(blockStart, ci, forUnparse = false)
  val be = EscapeBlockEndCooker.convertConstant(blockEnd, ci, forUnparse = false)

  def compute(state: ParseOrUnparseState) = {
    val optEscEscChar = evalAndConvertEEC(state)
    new EscapeSchemeBlockParserHelper(optEscEscChar, bs, be, ci)
  }
}

class EscapeSchemeBlockUnparseEv(
  blockStart: String,
  blockEnd: String,
  override val optEscapeEscapeChar: Maybe[EscapeEscapeCharEv],
  override val extraEscapedChars: Maybe[ExtraEscapedCharsEv],
  generateEscapeBlock: GenerateEscape,
  ci: DPathCompileInfo
) extends EscapeSchemeUnparseEv(ci) {

  override def runtimeDependencies = optEscapeEscapeChar.toList ++ extraEscapedChars.toList

  val bs = EscapeBlockStartCooker.convertConstant(blockStart, ci, forUnparse = true)
  val be = EscapeBlockEndCooker.convertConstant(blockEnd, ci, forUnparse = true)

  def compute(state: ParseOrUnparseState) = {
    val optEscEscChar = evalAndConvertEEC(state)
    val extEscChars = evalAndConvertExtraEscapedCharacters(state)
    new EscapeSchemeBlockUnparserHelper(
      optEscEscChar,
      bs,
      be,
      extEscChars,
      generateEscapeBlock,
      ci
    )
  }
}
