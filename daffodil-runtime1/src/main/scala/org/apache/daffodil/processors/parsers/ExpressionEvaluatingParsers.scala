/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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
  rd: RuntimeData)
  extends ParserObject(rd) {

  override lazy val childProcessors = Nil

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
  decl: RuntimeData)
  extends PrimParserObject(decl) {
  decl.notYetImplemented("newVariableInstance")
  def parse(pstate: PState) = {
    decl.notYetImplemented("newVariableInstance")
  }
}

class NewVariableInstanceEndParser(
  decl: RuntimeData)
  extends PrimParserObject(decl) {
  decl.notYetImplemented("newVariableInstance")
  def parse(pstate: PState) = {
    decl.notYetImplemented("newVariableInstance")
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
