/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.util.MaybeChar
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.GenerateEscape
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.util.MStackOfMaybe
import edu.illinois.ncsa.daffodil.cookers.EscapeBlockEndCooker
import edu.illinois.ncsa.daffodil.cookers.EscapeBlockStartCooker
import edu.illinois.ncsa.daffodil.cookers.ExtraEscapedCharactersCooker
import edu.illinois.ncsa.daffodil.cookers.EscapeEscapeCharacterCooker
import edu.illinois.ncsa.daffodil.cookers.EscapeCharacterCooker
import edu.illinois.ncsa.daffodil.processors.parsers.PState

class EscapeCharEv(expr: CompiledExpression[String], rd: RuntimeData)
  extends EvaluatableConvertedExpression[String, String](
    expr,
    EscapeCharacterCooker,
    rd)
  with InfosetCachedEvaluatable[String] {
  override lazy val runtimeDependencies = Nil
}

class EscapeEscapeCharEv(expr: CompiledExpression[String], rd: RuntimeData)
  extends EvaluatableConvertedExpression[String, String](
    expr,
    EscapeEscapeCharacterCooker,
    rd)
  with InfosetCachedEvaluatable[String] {
  override lazy val runtimeDependencies = Nil
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

abstract class EscapeSchemeParseEv(rd: RuntimeData)
  extends Evaluatable[EscapeSchemeParserHelper](rd)
  with ManuallyCachedEvaluatable[EscapeSchemeParserHelper]
  with EscapeSchemeCommonEv {

  protected def getCacheStack(state: State): MStackOfMaybe[EscapeSchemeParserHelper] = {
    state.asInstanceOf[PState].mpstate.escapeSchemeEVCache
  }
}

abstract class EscapeSchemeUnparseEv(rd: RuntimeData)
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

class EscapeSchemeCharParseEv(escapeChar: EscapeCharEv,
  override val optEscapeEscapeChar: Maybe[EscapeEscapeCharEv],
  rd: RuntimeData)
  extends EscapeSchemeParseEv(rd) {

  override val runtimeDependencies = List(escapeChar) ++ optEscapeEscapeChar.toList

  def compute(state: ParseOrUnparseState) = {
    val escChar = escapeChar.evaluate(state).charAt(0)
    val optEscEscChar = evalAndConvertEEC(state)
    new EscapeSchemeCharParserHelper(escChar, optEscEscChar)
  }
}

class EscapeSchemeCharUnparseEv(escapeChar: EscapeCharEv,
  override val optEscapeEscapeChar: Maybe[EscapeEscapeCharEv],
  override val extraEscapedChars: Maybe[String],
  rd: RuntimeData)
  extends EscapeSchemeUnparseEv(rd) {

  override val runtimeDependencies = List(escapeChar) ++ optEscapeEscapeChar.toList

  def compute(state: ParseOrUnparseState) = {
    val escChar = escapeChar.evaluate(state).charAt(0)
    val optEscEscChar = evalAndConvertEEC(state)
    new EscapeSchemeCharUnparserHelper(escChar, optEscEscChar, extraEscapedCharsCooked, rd)
  }
}

class EscapeSchemeBlockParseEv(blockStart: String,
  blockEnd: String,
  override val optEscapeEscapeChar: Maybe[EscapeEscapeCharEv],
  rd: RuntimeData)
  extends EscapeSchemeParseEv(rd) {

  override val runtimeDependencies = optEscapeEscapeChar.toList

  val bs = EscapeBlockStartCooker.convertConstant(blockStart, rd, forUnparse = false)
  val be = EscapeBlockEndCooker.convertConstant(blockEnd, rd, forUnparse = false)

  def compute(state: ParseOrUnparseState) = {
    val optEscEscChar = evalAndConvertEEC(state)
    new EscapeSchemeBlockParserHelper(optEscEscChar, bs, be, rd)
  }
}

class EscapeSchemeBlockUnparseEv(blockStart: String,
  blockEnd: String,
  override val optEscapeEscapeChar: Maybe[EscapeEscapeCharEv],
  override val extraEscapedChars: Maybe[String],
  generateEscapeBlock: GenerateEscape,
  rd: RuntimeData)
  extends EscapeSchemeUnparseEv(rd) {

  override val runtimeDependencies = optEscapeEscapeChar.toList

  val bs = EscapeBlockStartCooker.convertConstant(blockStart, rd, forUnparse = true)
  val be = EscapeBlockEndCooker.convertConstant(blockEnd, rd, forUnparse = true)

  def compute(state: ParseOrUnparseState) = {
    val optEscEscChar = evalAndConvertEEC(state)
    new EscapeSchemeBlockUnparserHelper(optEscEscChar, bs, be, extraEscapedCharsCooked, generateEscapeBlock, rd)
  }
}
