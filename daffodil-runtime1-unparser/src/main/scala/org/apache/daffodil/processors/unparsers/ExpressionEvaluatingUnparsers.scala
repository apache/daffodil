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

package org.apache.daffodil.processors.unparsers

import org.apache.daffodil.dsom.CompiledExpression
import org.apache.daffodil.processors.NonTermRuntimeData
import org.apache.daffodil.processors.VariableRuntimeData
import org.apache.daffodil.dpath.SuspendableExpression
import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.processors.RuntimeData

final class SetVariableSuspendableExpression(
  override val expr: CompiledExpression[AnyRef],
  override val rd: VariableRuntimeData,
  referencingContext: NonTermRuntimeData)
  extends SuspendableExpression {

  override protected def processExpressionResult(ustate: UState, v: AnyRef) {
    val newVMap =
      ustate.variableMap.setVariable(rd, v, referencingContext, ustate)

    ustate.setVariables(newVMap)
  }

  override protected def maybeKnownLengthInBits(ustate: UState) = MaybeULong(0)
}

/**
 * Used when unparsing to evaluate dfdl:setVariable statements.
 *
 * TODO: Possible bug. This will allow expressions to forward reference, even
 * when the variables are being referenced from expressions that are NOT
 * allowed to forward reference - e.g., property value expressions such
 * as delimiters and byte order.
 *
 * This forward suspension is only supposed to be allowed for dfdl:outputValueCalc.
 */
final class SetVariableUnparser(
  val expr: CompiledExpression[AnyRef],
  override val context: VariableRuntimeData,
  referencingContext: NonTermRuntimeData)
  extends PrimUnparserNoData {

  override lazy val runtimeDependencies = Nil

  override lazy val childProcessors = Nil

  def suspendableExpression =
    new SetVariableSuspendableExpression(
      expr, context, referencingContext)

  override def unparse(state: UState): Unit = {
    suspendableExpression.run(state)
  }

}

// When implemented this almost certainly wants to be a combinator
// Not two separate unparsers.
class NewVariableInstanceStartUnparser(override val context: RuntimeData)
  extends PrimUnparserNoData {

  override lazy val runtimeDependencies = Nil

  override lazy val childProcessors = Nil

  context.notYetImplemented("newVariableInstance")

  override def unparse(ustate: UState) = {
    context.notYetImplemented("newVariableInstance")
  }
}

class NewVariableInstanceEndUnparser(override val context: RuntimeData)
  extends PrimUnparserNoData {

  override lazy val runtimeDependencies = Nil

  override lazy val childProcessors = Nil

  context.notYetImplemented("newVariableInstance")

  override def unparse(ustate: UState) = {
    context.notYetImplemented("newVariableInstance")
  }
}
