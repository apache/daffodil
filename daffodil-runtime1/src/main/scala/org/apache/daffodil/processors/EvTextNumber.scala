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
import org.apache.daffodil.cookers.TextStandardGroupingSeparatorCooker
import org.apache.daffodil.cookers.TextBooleanFalseRepCooker
import org.apache.daffodil.cookers.TextStandardExponentRepCooker
import org.apache.daffodil.cookers.TextBooleanTrueRepCooker
import org.apache.daffodil.cookers.TextStandardDecimalSeparatorCooker

class TextStandardDecimalSeparatorEv(expr: CompiledExpression[String], trd: DPathCompileInfo)
  extends EvaluatableConvertedExpression[String, List[String]](
    expr,
    TextStandardDecimalSeparatorCooker,
    trd)
  with InfosetCachedEvaluatable[List[String]] {
  override lazy val runtimeDependencies = Vector()
}

class TextStandardGroupingSeparatorEv(expr: CompiledExpression[String], trd: DPathCompileInfo)
  extends EvaluatableConvertedExpression[String, String](
    expr,
    TextStandardGroupingSeparatorCooker,
    trd)
  with InfosetCachedEvaluatable[String] {
  override lazy val runtimeDependencies = Vector()
}

class TextStandardExponentRepEv(expr: CompiledExpression[String], trd: DPathCompileInfo)
  extends EvaluatableConvertedExpression[String, String](
    expr,
    TextStandardExponentRepCooker,
    trd)
  with InfosetCachedEvaluatable[String] {
  override lazy val runtimeDependencies = Vector()
}

class TextBooleanTrueRepEv(exprT: CompiledExpression[String], falseRepEv: TextBooleanFalseRepEv, mustBeSameLength: Boolean, trd: DPathCompileInfo)
  extends EvaluatableConvertedExpression[String, List[String]](
    exprT,
    TextBooleanTrueRepCooker,
    trd)
  with InfosetCachedEvaluatable[List[String]] {
  override lazy val runtimeDependencies = Vector()

  override final protected def compute(state: ParseOrUnparseState): List[String] = {
    if (mustBeSameLength) {

      //All values of textBooleanTrueRep and textBooleanFalseRep must be equal in length
      val textBooleanTrueReps: List[String] = super.compute(state)
      val textBooleanFalseReps: List[String] = falseRepEv.evaluate(state)

      val trueLength = textBooleanTrueReps(0).length
      val falseLength = textBooleanFalseReps(0).length
      if (trueLength != falseLength ||
        textBooleanTrueReps.exists(x => x.length != trueLength) ||
        textBooleanFalseReps.exists(x => x.length != falseLength)) {
        trd.schemaDefinitionError("If dfdl:lengthKind is 'explicit' or 'implicit' and either dfdl:textPadKind or dfdl:textTrimKind  is 'none' then both dfdl:textBooleanTrueRep and dfdl:textBooleanFalseRep must have the same length.")
      }
      textBooleanTrueReps
    } else {
      super.compute(state)
    }
  }
}

class TextBooleanFalseRepEv(expr: CompiledExpression[String], trd: DPathCompileInfo)
  extends EvaluatableConvertedExpression[String, List[String]](
    expr,
    TextBooleanFalseRepCooker,
    trd)
  with InfosetCachedEvaluatable[List[String]] {
  override lazy val runtimeDependencies = Vector()
}
