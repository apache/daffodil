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

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.FailureType
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.runtime1.dpath.ParserDiscriminatorNonBlocking
import org.apache.daffodil.runtime1.dpath.ParserNonBlocking
import org.apache.daffodil.runtime1.dsom.CompiledExpression
import org.apache.daffodil.runtime1.infoset.DataValue
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Failure
import org.apache.daffodil.runtime1.processors.RuntimeData
import org.apache.daffodil.runtime1.processors.Success
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.VariableRuntimeData

/**
 * Common parser base class for any parser that evaluates an expression.
 */
abstract class ExpressionEvaluationParser(
  expr: CompiledExpression[AnyRef],
  contextParam: RuntimeData
) extends PrimParserNoData {

  override val context: RuntimeData = contextParam

  override def runtimeDependencies = Vector()

  override def childProcessors = Vector()

  /**
   * Modifies the PState
   */
  protected def eval(start: PState): DataValuePrimitive = {
    val res = expr.evaluate(start)
    DataValue.unsafeFromAnyRef(res)
  }
}

class IVCParser(expr: CompiledExpression[AnyRef], e: ElementRuntimeData)
  extends ExpressionEvaluationParser(expr, e) {

  Assert.invariant(e.isSimpleType)

  def parse(start: PState): Unit = {
    Logger.log.debug(s"This is ${toString}")
    val currentElement = start.simpleElement
    val res = eval(start)
    currentElement.setDataValue(res)
    if (start.processorStatus ne Success) return
  }
}

final class SetVariableParser(
  expr: CompiledExpression[AnyRef],
  decl: VariableRuntimeData,
  trd: TermRuntimeData
) extends ExpressionEvaluationParser(expr, decl) {

  override val context = trd

  def parse(start: PState): Unit = {
    Logger.log.debug(s"This is ${toString}") // important. Don't toString unless we have to log.
    val res = eval(start)
    if (start.processorStatus.isInstanceOf[Failure]) return
    start.setVariable(decl, res, decl)
  }
}

final class NewVariableInstanceStartParser(vrd: VariableRuntimeData, trd: TermRuntimeData)
  extends PrimParser {

  override def context = trd

  override def runtimeDependencies = Vector()

  def parse(start: PState): Unit = {
    val nvi = start.newVariableInstance(vrd)

    if (vrd.maybeDefaultValueExpr.isDefined) {
      val dve = vrd.maybeDefaultValueExpr.get
      val res = DataValue.unsafeFromAnyRef(dve.evaluate(start))
      nvi.setDefaultValue(res)
    } else if (nvi.firstInstanceInitialValue.isDefined) {
      // The NVI will inherit the default value of the original variable instance
      // This will also inherit any externally provided bindings.
      nvi.setDefaultValue(nvi.firstInstanceInitialValue)
    }
  }
}

final class NewVariableInstanceEndParser(vrd: VariableRuntimeData, trd: TermRuntimeData)
  extends PrimParser {

  override def context = trd
  override def runtimeDependencies = Vector()

  def parse(start: PState) = {
    start.removeVariableInstance(vrd)
  }
}

final class AssertExpressionEvaluationParser(
  override val messageExpr: CompiledExpression[String],
  override val discrim: Boolean, // are we a discriminator or not.
  decl: RuntimeData,
  expr: CompiledExpression[AnyRef],
  override val failureType: FailureType
) extends ExpressionEvaluationParser(expr, decl)
  with AssertParserMixin {

  def parse(start: PState): Unit = {
    Logger.log.debug(s"This is ${toString}")
    //
    // This now informs us of the success/failure of the expression
    // evaluation via side-effect on the start state passed here.
    //
    val res =
      try {
        if (discrim) {
          start.dState.setMode(ParserDiscriminatorNonBlocking)
        }
        eval(start)
      } finally {
        start.dState.setMode(ParserNonBlocking)
      }
    //
    // a PE during evaluation of an assertion is a PE
    //
    // Removed this assert check because eval now side-effects start to
    // contain the result status.
    // Assert.invariant(!start.processorStatus.isInstanceOf[Failure])
    //
    // Assert.invariant(res != null)
    if (start.processorStatus ne Success) return

    val testResult = res.getBoolean
    handleAssertionResult(testResult, start, context)
  }
}
