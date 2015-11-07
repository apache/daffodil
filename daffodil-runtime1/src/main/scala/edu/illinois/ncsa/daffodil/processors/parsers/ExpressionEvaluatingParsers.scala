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

package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression

/**
 * Common parser base class for any parser that evaluates an expression.
 */
abstract class ExpressionEvaluationParser(
  expr: CompiledExpression,
  rd: RuntimeData)
  extends Parser(rd) with WithParseErrorThrowing {

  override lazy val childProcessors = Nil

  /**
   * Returns a pair. Modifies the PState
   */
  protected def eval(start: PState): Any = {
    expr.evaluate(start)
  }
}

class IVCParser(expr: CompiledExpression, e: ElementRuntimeData)
  extends ExpressionEvaluationParser(expr, e) {
  Assert.invariant(e.isSimpleType)

  def parse(start: PState): Unit =
    // withLoggingLevel(LogLevel.Info)
    {
      withParseErrorThrowing(start) {
        log(LogLevel.Debug, "This is %s", toString)
        val currentElement: InfosetSimpleElement = start.simpleElement
        val res = eval(start)
        if (start.status ne Success) return
        currentElement.setDataValue(res)
      }
    }
}

class SetVariableParser(expr: CompiledExpression, decl: VariableRuntimeData)
  extends ExpressionEvaluationParser(expr, decl) {

  def parse(start: PState): Unit = {
    withParseErrorThrowing(start) {
      log(LogLevel.Debug, "This is %s", toString) // important. Don't toString unless we have to log.
      val res = eval(start)
      res match {
        case ps: PState => return ;
        case _ => /*fall through*/ }
      if (start.status.isInstanceOf[Failure]) return
      // val vmap = start.variableMap
      start.setVariable(decl.globalQName, res, decl, start)
    }
  }
}

class NewVariableInstanceStartParser(
  decl: RuntimeData)
  extends PrimParser(decl) {
  decl.notYetImplemented("newVariableInstance")
  def parse(pstate: PState) = {
    decl.notYetImplemented("newVariableInstance")
  }
}

class NewVariableInstanceEndParser(
  decl: RuntimeData)
  extends PrimParser(decl) {
  decl.notYetImplemented("newVariableInstance")
  def parse(pstate: PState) = {
    decl.notYetImplemented("newVariableInstance")
  }
}

class AssertExpressionEvaluationParser(
  msg: String,
  discrim: Boolean, // are we a discriminator or not.
  decl: RuntimeData,
  expr: CompiledExpression)
  extends ExpressionEvaluationParser(expr, decl) {

  def parse(start: PState): Unit =
    // withLoggingLevel(LogLevel.Info)
    {
      withParseErrorThrowing(start) {
        log(LogLevel.Debug, "This is %s", toString)
        //
        // This now informs us of the success/failure of the expression
        // evaluation via side-effect on the start state passed here.
        //
        val res = eval(start)
        //
        // a PE during evaluation of an assertion is a PE
        //
        // Removed this assert check because eval now side-effects start to
        // contain the result status.
        // Assert.invariant(!start.status.isInstanceOf[Failure])
        //
        // Assert.invariant(res != null)
        if (start.status ne Success) return

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
}
