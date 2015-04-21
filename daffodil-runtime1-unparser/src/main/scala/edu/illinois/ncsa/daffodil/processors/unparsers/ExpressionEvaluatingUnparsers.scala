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

package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.processors.charset._
import java.nio.charset.Charset
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.Debug
import edu.illinois.ncsa.daffodil.util.PreSerialization

/**
 * Common parser base class for any parser that evaluates an expression.
 */

abstract class ExpressionEvaluationUnparser(expr: CompiledExpression, rd: RuntimeData)
  extends Unparser(rd) with WithParseErrorThrowing {

  override lazy val childProcessors = Nil

  /**
   * Modifies the UState (variable-read transitions)
   * and returns the value of the expression.
   */
  protected def eval(start: UState): Any = {
    expr.evaluate(start)
  }
}

class SetVariableUnparser(expr: CompiledExpression, decl: VariableRuntimeData)
  extends ExpressionEvaluationUnparser(expr, decl) {

  def unparse(start: UState): Unit = {
    log(Debug("This is %s", toString)) // important. Don't toString unless we have to log. 

    val someValue = eval(start)

    if (start.status.isInstanceOf[Failure])
      UnparseError(One(decl.schemaFileLocation), One(start), "%s - Evaluation failed for %s.", nom, expr)

    val newVMap = start.variableMap.setVariable(decl.globalQName, someValue, decl)
    start.withVariables(newVMap)

    if (start.status.isInstanceOf[Failure])
      UnparseError(One(decl.schemaFileLocation), One(start), "%s - SetVariable failed for %s.", nom, expr)
  }

}

class NewVariableInstanceStartUnparser(
  decl: RuntimeData)
  extends Unparser(decl) {
  override lazy val childProcessors = Nil

  decl.notYetImplemented("newVariableInstance")
  def unparse(ustate: UState) = {
    decl.notYetImplemented("newVariableInstance")
  }
}

class NewVariableInstanceEndUnparser(
  decl: RuntimeData)
  extends Unparser(decl) {
  override lazy val childProcessors = Nil

  decl.notYetImplemented("newVariableInstance")
  def unparse(ustate: UState) = {
    decl.notYetImplemented("newVariableInstance")
  }
}

