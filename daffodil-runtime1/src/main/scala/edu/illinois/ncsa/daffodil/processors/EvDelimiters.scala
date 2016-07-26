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
import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter
import edu.illinois.ncsa.daffodil.processors.dfa.CreateDelimiterDFA
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.processors.parsers.DelimiterTextType

trait DelimiterEvMixin[+T <: AnyRef]
    extends ExprEvalMixin[String] { self: Evaluatable[T] =>

  def expr: CompiledExpression[String]
  def converter: Converter[String, List[String]]
  def trd: TermRuntimeData

  override final def toBriefXML(depth: Int = -1) = if (this.isConstant) this.constValue.toString else expr.toBriefXML(depth)

  protected def evalAndConvert(state: ParseOrUnparseState): List[String] = {
    val expressionResult = eval(expr, state)

    val converterResult = state match {
      case cs: CompileState => converter.convertConstant(expressionResult, trd, false)
      case _ => converter.convertRuntime(expressionResult, trd, false)
    }
    converterResult
  }
}

abstract class DelimiterParseEv(delimType: DelimiterTextType.Type, override val expr: CompiledExpression[String], override val trd: TermRuntimeData)
    extends Evaluatable[Array[DFADelimiter]](trd)
    with InfosetCachedEvaluatable[Array[DFADelimiter]]
    with DelimiterEvMixin[Array[DFADelimiter]] {

  override lazy val runtimeDependencies = Nil

  def isKnownNonEmpty = !isConstant || optConstant.get.length > 0

  override protected def compute(state: ParseOrUnparseState): Array[DFADelimiter] = {
    if (state.isInstanceOf[UState]) {
      Assert.invariantFailed("State was UState in Parser Evaluatable")
    }

    val converterResult = evalAndConvert(state)
    if (converterResult.length == 1 && converterResult(0) == "") {
      Array()
    } else {
      CreateDelimiterDFA(delimType, trd, converterResult)
    }
  }
}

abstract class DelimiterUnparseEv(delimType: DelimiterTextType.Type, override val expr: CompiledExpression[String], outputNewLine: OutputNewLineEv, override val trd: TermRuntimeData)
    extends Evaluatable[Option[DFADelimiter]](trd)
    with InfosetCachedEvaluatable[Option[DFADelimiter]]
    with DelimiterEvMixin[Option[DFADelimiter]] {

  override lazy val runtimeDependencies = Seq(outputNewLine)

  override protected def compute(state: ParseOrUnparseState): Option[DFADelimiter] = {
    if (state.isInstanceOf[PState]) {
      Assert.invariantFailed("State was PState in Unparser Evaluatable")
    }

    val converterResult = evalAndConvert(state)
    if (converterResult.length == 1 && converterResult(0) == "") {
      None
    } else {
      val onl = outputNewLine.evaluate(state)
      Some(CreateDelimiterDFA(delimType, trd, converterResult(0), onl))
    }
  }
}

class InitiatorParseEv(expr: CompiledExpression[String], trd: TermRuntimeData)
    extends DelimiterParseEv(DelimiterTextType.Initiator, expr, trd) {

  override val converter = InitiatorCooker
}

class InitiatorUnparseEv(expr: CompiledExpression[String], outputNewLine: OutputNewLineEv, trd: TermRuntimeData)
    extends DelimiterUnparseEv(DelimiterTextType.Initiator, expr, outputNewLine, trd) {

  override val converter = InitiatorCooker
}

class TerminatorParseEv(expr: CompiledExpression[String], isLengthKindDelimited: Boolean, trd: TermRuntimeData)
    extends DelimiterParseEv(DelimiterTextType.Terminator, expr, trd) {

  override val converter = if (isLengthKindDelimited) TerminatorCookerNoES else TerminatorCooker
}

class TerminatorUnparseEv(expr: CompiledExpression[String], isLengthKindDelimited: Boolean, outputNewLine: OutputNewLineEv, trd: TermRuntimeData)
    extends DelimiterUnparseEv(DelimiterTextType.Terminator, expr, outputNewLine, trd) {

  override val converter = if (isLengthKindDelimited) TerminatorCookerNoES else TerminatorCooker
}

class SeparatorParseEv(expr: CompiledExpression[String], trd: TermRuntimeData)
    extends DelimiterParseEv(DelimiterTextType.Separator, expr, trd) {

  override val converter = SeparatorCooker
}

class SeparatorUnparseEv(expr: CompiledExpression[String], outputNewLine: OutputNewLineEv, trd: TermRuntimeData)
    extends DelimiterUnparseEv(DelimiterTextType.Separator, expr, outputNewLine, trd) {

  override val converter = SeparatorCooker
}
