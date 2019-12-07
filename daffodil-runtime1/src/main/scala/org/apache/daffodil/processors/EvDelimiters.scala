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
import org.apache.daffodil.processors.dfa.DFADelimiter
import org.apache.daffodil.processors.dfa.CreateDelimiterDFA
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.processors.parsers.DelimiterTextType
import org.apache.daffodil.cookers.TerminatorCookerNoES
import org.apache.daffodil.cookers.SeparatorCooker
import org.apache.daffodil.cookers.InitiatorCooker
import org.apache.daffodil.cookers.TerminatorCooker
import org.apache.daffodil.cookers.Converter
import org.apache.daffodil.processors.parsers.PState

trait DelimiterEvMixin[+T <: AnyRef]
  extends ExprEvalMixin[String] { self: Evaluatable[T] =>

  final def isKnownNonEmpty = expr.isKnownNonEmpty

  def expr: CompiledExpression[String]
  def converter: Converter[String, List[String]]

  override final def toBriefXML(depth: Int = -1) = if (this.isConstant) this.constValue.toString else expr.toBriefXML(depth)

  protected def evalAndConvert(state: ParseOrUnparseState): List[String] = {
    val expressionResult = eval(expr, state)

    val converterResult = state match {
      case cs: CompileState => converter.convertConstant(expressionResult, ci, false)
      case _ => converter.convertRuntime(expressionResult, ci, false)
    }
    converterResult
  }
}

abstract class DelimiterParseEv(
  delimType: DelimiterTextType.Type,
  override val expr: CompiledExpression[String],
  ignoreCase: Boolean,
  override val ci: DPathCompileInfo)
  extends Evaluatable[Array[DFADelimiter]](ci)
  with InfosetCachedEvaluatable[Array[DFADelimiter]]
  with DelimiterEvMixin[Array[DFADelimiter]] {

  override lazy val runtimeDependencies = Vector()

  override protected def compute(state: ParseOrUnparseState): Array[DFADelimiter] = {
    if (state.isInstanceOf[UState]) {
      Assert.invariantFailed("State was UState in Parser Evaluatable")
    }

    val converterResult = evalAndConvert(state)
    if (converterResult.length == 1 && converterResult(0) == "") {
      Array()
    } else {
      CreateDelimiterDFA(delimType, ci, converterResult, ignoreCase)
    }
  }
}

abstract class DelimiterUnparseEv(
  delimType: DelimiterTextType.Type,
  override val expr: CompiledExpression[String],
  outputNewLine: OutputNewLineEv,
  override val ci: DPathCompileInfo)
  extends Evaluatable[Array[DFADelimiter]](ci)
  with InfosetCachedEvaluatable[Array[DFADelimiter]]
  with DelimiterEvMixin[Array[DFADelimiter]] {

  override lazy val runtimeDependencies = Seq(outputNewLine)

  override protected def compute(state: ParseOrUnparseState): Array[DFADelimiter] = {
    if (state.isInstanceOf[PState]) {
      Assert.invariantFailed("State was PState in Unparser Evaluatable")
    }

    val converterResult = evalAndConvert(state)
    if (converterResult.length == 1 && converterResult(0) == "") {
      Array()
    } else {
      val onl = outputNewLine.evaluate(state)
      CreateDelimiterDFA(delimType, ci, converterResult, onl)
    }
  }
}

class InitiatorParseEv(expr: CompiledExpression[String], ignoreCase: Boolean, trd: DPathCompileInfo)
  extends DelimiterParseEv(DelimiterTextType.Initiator, expr, ignoreCase, trd) {

  override val converter = InitiatorCooker
}

class InitiatorUnparseEv(expr: CompiledExpression[String], outputNewLine: OutputNewLineEv, trd: DPathCompileInfo)
  extends DelimiterUnparseEv(DelimiterTextType.Initiator, expr, outputNewLine, trd) {

  override val converter = InitiatorCooker
}

class TerminatorParseEv(expr: CompiledExpression[String], isLengthKindDelimited: Boolean, ignoreCase: Boolean, trd: DPathCompileInfo)
  extends DelimiterParseEv(DelimiterTextType.Terminator, expr, ignoreCase, trd) {

  override val converter = if (isLengthKindDelimited) TerminatorCookerNoES else TerminatorCooker
}

class TerminatorUnparseEv(expr: CompiledExpression[String], isLengthKindDelimited: Boolean, outputNewLine: OutputNewLineEv, trd: DPathCompileInfo)
  extends DelimiterUnparseEv(DelimiterTextType.Terminator, expr, outputNewLine, trd) {

  override val converter = if (isLengthKindDelimited) TerminatorCookerNoES else TerminatorCooker
}

class SeparatorParseEv(expr: CompiledExpression[String], ignoreCase: Boolean, trd: DPathCompileInfo)
  extends DelimiterParseEv(DelimiterTextType.Separator, expr, ignoreCase, trd) {

  override val converter = SeparatorCooker
}

class SeparatorUnparseEv(expr: CompiledExpression[String], outputNewLine: OutputNewLineEv, trd: DPathCompileInfo)
  extends DelimiterUnparseEv(DelimiterTextType.Separator, expr, outputNewLine, trd) {

  override val converter = SeparatorCooker
}
