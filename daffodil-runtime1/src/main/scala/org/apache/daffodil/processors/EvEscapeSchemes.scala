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

import org.apache.daffodil.dsom._
import org.apache.daffodil.util.MaybeChar
import org.apache.daffodil.schema.annotation.props.gen.GenerateEscape
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.util.MStackOfMaybe
import org.apache.daffodil.cookers.EscapeBlockEndCooker
import org.apache.daffodil.cookers.EscapeBlockStartCooker
import org.apache.daffodil.cookers.ExtraEscapedCharactersCooker
import org.apache.daffodil.cookers.EscapeEscapeCharacterCooker
import org.apache.daffodil.cookers.EscapeCharacterCooker
import org.apache.daffodil.processors.parsers.PState

class EscapeCharEv(expr: CompiledExpression[String], rd: DPathCompileInfo)
  extends EvaluatableConvertedExpression[String, String](
    expr,
    EscapeCharacterCooker,
    rd)
  with InfosetCachedEvaluatable[String] {
  override lazy val runtimeDependencies = Vector()
}

class EscapeEscapeCharEv(expr: CompiledExpression[String], rd: DPathCompileInfo)
  extends EvaluatableConvertedExpression[String, String](
    expr,
    EscapeEscapeCharacterCooker,
    rd)
  with InfosetCachedEvaluatable[String] {
  override lazy val runtimeDependencies = Vector()
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

abstract class EscapeSchemeParseEv(rd: DPathCompileInfo)
  extends Evaluatable[EscapeSchemeParserHelper](rd)
  with ManuallyCachedEvaluatable[EscapeSchemeParserHelper]
  with EscapeSchemeCommonEv {

  protected def getCacheStack(state: State): MStackOfMaybe[EscapeSchemeParserHelper] = {
    state.asInstanceOf[PState].mpstate.escapeSchemeEVCache
  }
}

abstract class EscapeSchemeUnparseEv(rd: DPathCompileInfo)
  extends Evaluatable[EscapeSchemeUnparserHelper](rd)
  with ManuallyCachedEvaluatable[EscapeSchemeUnparserHelper]
  with EscapeSchemeCommonEv {

  protected def getCacheStack(state: State): MStackOfMaybe[EscapeSchemeUnparserHelper] = {
    state.asInstanceOf[UState].escapeSchemeEVCache
  }

  def extraEscapedChars: Maybe[String]

  val extraEscapedCharsCooked = {
    if (extraEscapedChars.isDefined) {
      ExtraEscapedCharactersCooker.convertConstant(extraEscapedChars.get, rd, forUnparse = true).map { _.charAt(0) }
    } else {
      Seq()
    }
  }
}

class EscapeSchemeCharParseEv(
  escapeChar: EscapeCharEv,
  override val optEscapeEscapeChar: Maybe[EscapeEscapeCharEv],
  rd: DPathCompileInfo)
  extends EscapeSchemeParseEv(rd) {

  override val runtimeDependencies = Vector(escapeChar) ++ optEscapeEscapeChar.toList

  def compute(state: ParseOrUnparseState) = {
    val escChar = escapeChar.evaluate(state).charAt(0)
    val optEscEscChar = evalAndConvertEEC(state)
    new EscapeSchemeCharParserHelper(escChar, optEscEscChar)
  }
}

class EscapeSchemeCharUnparseEv(
  escapeChar: EscapeCharEv,
  override val optEscapeEscapeChar: Maybe[EscapeEscapeCharEv],
  override val extraEscapedChars: Maybe[String],
  rd: DPathCompileInfo)
  extends EscapeSchemeUnparseEv(rd) {

  override val runtimeDependencies = Vector(escapeChar) ++ optEscapeEscapeChar.toList

  def compute(state: ParseOrUnparseState) = {
    val escChar = escapeChar.evaluate(state).charAt(0)
    val optEscEscChar = evalAndConvertEEC(state)
    new EscapeSchemeCharUnparserHelper(escChar, optEscEscChar, extraEscapedCharsCooked, rd)
  }
}

class EscapeSchemeBlockParseEv(
  blockStart: String,
  blockEnd: String,
  override val optEscapeEscapeChar: Maybe[EscapeEscapeCharEv],
  rd: DPathCompileInfo)
  extends EscapeSchemeParseEv(rd) {

  override val runtimeDependencies = optEscapeEscapeChar.toList

  val bs = EscapeBlockStartCooker.convertConstant(blockStart, rd, forUnparse = false)
  val be = EscapeBlockEndCooker.convertConstant(blockEnd, rd, forUnparse = false)

  def compute(state: ParseOrUnparseState) = {
    val optEscEscChar = evalAndConvertEEC(state)
    new EscapeSchemeBlockParserHelper(optEscEscChar, bs, be, rd)
  }
}

class EscapeSchemeBlockUnparseEv(
  blockStart: String,
  blockEnd: String,
  override val optEscapeEscapeChar: Maybe[EscapeEscapeCharEv],
  override val extraEscapedChars: Maybe[String],
  generateEscapeBlock: GenerateEscape,
  rd: DPathCompileInfo)
  extends EscapeSchemeUnparseEv(rd) {

  override val runtimeDependencies = optEscapeEscapeChar.toList

  val bs = EscapeBlockStartCooker.convertConstant(blockStart, rd, forUnparse = true)
  val be = EscapeBlockEndCooker.convertConstant(blockEnd, rd, forUnparse = true)

  def compute(state: ParseOrUnparseState) = {
    val optEscEscChar = evalAndConvertEEC(state)
    new EscapeSchemeBlockUnparserHelper(optEscEscChar, bs, be, extraEscapedCharsCooked, generateEscapeBlock, rd)
  }
}
