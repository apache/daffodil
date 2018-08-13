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

package org.apache.daffodil.processors.parsers

import org.apache.daffodil.dpath.ParserDiscriminatorNonBlocking
import org.apache.daffodil.dpath.ParserNonBlocking
import org.apache.daffodil.dsom.CompiledExpression
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.infoset.InfosetSimpleElement
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.Failure
import org.apache.daffodil.processors.RuntimeData
import org.apache.daffodil.processors.Success
import org.apache.daffodil.processors.VariableRuntimeData
import org.apache.daffodil.util.LogLevel

// import java.lang.{ Boolean => JBoolean }

/**
 * Common parser base class for any parser that evaluates an expression.
 */
abstract class ExpressionEvaluationParser(
  expr: CompiledExpression[AnyRef],
  override val context: RuntimeData)
  extends PrimParserNoData {

  override lazy val runtimeDependencies = Vector()

  override lazy val childProcessors = Vector()

  /**
   * Modifies the PState
   */
  protected def eval(start: PState): AnyRef = {
    expr.evaluate(start)
  }
}

class IVCParser(expr: CompiledExpression[AnyRef], e: ElementRuntimeData)
  extends ExpressionEvaluationParser(expr, e) {
  Assert.invariant(e.isSimpleType)

  def parse(start: PState): Unit =
    // withLoggingLevel(LogLevel.Info)
    {
      log(LogLevel.Debug, "This is %s", toString)
      val currentElement: InfosetSimpleElement = start.simpleElement
      val res = eval(start)
      currentElement.setDataValue(res)
      if (start.processorStatus ne Success) return
    }
}

class SetVariableParser(expr: CompiledExpression[AnyRef], decl: VariableRuntimeData)
  extends ExpressionEvaluationParser(expr, decl) {

  def parse(start: PState): Unit = {
    log(LogLevel.Debug, "This is %s", toString) // important. Don't toString unless we have to log.
    val res = eval(start)
    res match {
      case ps: PState => return ;
      case _ => /*fall through*/ }
    if (start.processorStatus.isInstanceOf[Failure]) return
    // val vmap = start.variableMap
    start.setVariable(decl, res, decl, start)
  }
}

class NewVariableInstanceStartParser(
  override val context: RuntimeData)
  extends PrimParser {
  context.notYetImplemented("newVariableInstance")
  override lazy val runtimeDependencies = Vector()

  def parse(pstate: PState) = {
    context.notYetImplemented("newVariableInstance")
  }
}

class NewVariableInstanceEndParser(
  override val context: RuntimeData)
  extends PrimParser {
  context.notYetImplemented("newVariableInstance")
  override lazy val runtimeDependencies = Vector()

  def parse(pstate: PState) = {
    context.notYetImplemented("newVariableInstance")
  }
}

class AssertExpressionEvaluationParser(
  msg: String,
  discrim: Boolean, // are we a discriminator or not.
  decl: RuntimeData,
  expr: CompiledExpression[AnyRef])
  extends ExpressionEvaluationParser(expr, decl) {

  def parse(start: PState): Unit =
    // withLoggingLevel(LogLevel.Info)
    {
      log(LogLevel.Debug, "This is %s", toString)
      //
      // This now informs us of the success/failure of the expression
      // evaluation via side-effect on the start state passed here.
      //
      val res =
        try {
          if (discrim)
            start.dState.setMode(ParserDiscriminatorNonBlocking)
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

      val testResult = res.asInstanceOf[Boolean]
      if (testResult) {
        start.setDiscriminator(discrim)
      } else {
        // The assertion failed. Prepare a failure message etc. in case backtracking ultimately fails from here.
        val diag = new AssertionFailed(decl.schemaFileLocation, start, msg)
        start.setFailed(diag)
      }
    }
}
