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
import org.apache.daffodil.infoset.DataValue
import org.apache.daffodil.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.infoset.DataValue.DataValuePrimitiveNullable
import org.apache.daffodil.infoset.InfosetSimpleElement
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.processors.Failure
import org.apache.daffodil.processors.RuntimeData
import org.apache.daffodil.processors.Success
import org.apache.daffodil.processors.TypeCalculator
import org.apache.daffodil.processors.VariableRuntimeData
import org.apache.daffodil.schema.annotation.props.gen.FailureType
import org.apache.daffodil.util.Logger

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
    val currentElement: InfosetSimpleElement = start.simpleElement
    val res = eval(start)
    currentElement.setDataValue(res)
    if (start.processorStatus ne Success) return
  }
}

/*
 * Run a parser for an element that does not occur in the infoset
 * Prior to running, a temporary element (of the type expected by the parser) will be created in the infoset,
 * After running, the infoset will be reverted to its original state, but any other side effect of parsing will remain
 *
 * Additionally, the dataValue of the element the parser parsed will be returned
 */
trait WithDetachedParser {
  def runDetachedParser(pstate: PState, detachedParser: Parser, erd: ElementRuntimeData): DataValuePrimitiveNullable = {
    /*
     * The parse1 being called here is that of ElementCombinator1, which expects to begin and end in the parent
     * of whatever element it is parsing. parse1 will create the new element and append it to the end of the
     * children list of the parent.
     *
     * The parse() call we are in currently is in the middle of the above process already.
     * To use the detachedParser, we need to unwind then rewind the work that ElementCombinator1 has already done
     *  (in addition to reverting the infoset changes that repTypeParser made). the general flow is:
     *
     *  1) priorElement = pstate.infoset
     *  2) pstate.infoset = pstate.infoset.parent
     *  3) pstate.infoset.mark
     *  4) distachedParser.parse1
     *  5) pstate.infoset.restore
     *  6) pstate.infoset = priorElement
     *
     *  Note that we are only restoring the infoset. Any other side effects the repTypeParser has on pstate
     *  (such as advancing the bit posistion) will remain.
     *
     *  If repTypeParser has an error (either thrown or status), we percolate it up to our caller.
     */

    val priorElement = pstate.infoset
    pstate.setParent(pstate.infoset.diParent)

    // This isn't actually a point of uncertainty, we just use the logic to
    // allow resetting the infoset after we create the detached parser
    val ans = pstate.withPointOfUncertainty("WithDetachedParser", erd) { pou =>
    
      detachedParser.parse1(pstate)

      val res: DataValuePrimitiveNullable = pstate.processorStatus match {
        case Success => pstate.infoset.children.last.asSimple.dataValue
        case _       => DataValue.NoValue
      }

      // Restore the infoset. withPointOfUncertainty will discard the pou when
      // this block ends, thus keeping the rest of the modified state
      pou.restoreInfoset(pstate)

      res
    }

    pstate.setParent(priorElement)

    ans
  }
}

class TypeValueCalcParser(typeCalculator: TypeCalculator, repTypeParser: Parser, e: ElementRuntimeData, repTypeRuntimeData: ElementRuntimeData)
  extends CombinatorParser(e)
  with WithDetachedParser {
  override lazy val childProcessors = Vector(repTypeParser)
  override lazy val runtimeDependencies: Vector[Evaluatable[AnyRef]] = Vector()
  
  override def parse(pstate: PState): Unit = {
    val repValue: DataValuePrimitiveNullable = runDetachedParser(pstate, repTypeParser, repTypeRuntimeData)
    val repValueType = repTypeRuntimeData.optPrimType.get
    pstate.dataProc.get.ssrd
    if (pstate.processorStatus == Success) {
      Assert.invariant(repValue.isDefined)
      val logicalValue: DataValuePrimitiveNullable = typeCalculator.inputTypeCalcParse(pstate, context, repValue.getNonNullable, repValueType)
      if (pstate.processorStatus == Success) {
        Assert.invariant(logicalValue.isDefined)
        pstate.simpleElement.setDataValue(logicalValue)
      }
    }
  }

}

class SetVariableParser(expr: CompiledExpression[AnyRef], decl: VariableRuntimeData)
  extends ExpressionEvaluationParser(expr, decl) {

  def parse(start: PState): Unit = {
    Logger.log.debug(s"This is ${toString}") // important. Don't toString unless we have to log.
    val res = eval(start)
    if (start.processorStatus.isInstanceOf[Failure]) return
    start.setVariable(decl, res, decl)
  }
}

class NewVariableInstanceStartParser(override val context: VariableRuntimeData)
  extends PrimParser {
  override lazy val runtimeDependencies = Vector()

  def parse(start: PState): Unit = {
    val nvi = start.newVariableInstance(context)

    if (context.maybeDefaultValueExpr.isDefined) {
      val dve = context.maybeDefaultValueExpr.get
      val res = DataValue.unsafeFromAnyRef(dve.evaluate(start))
      nvi.setDefaultValue(res)
    } else if (nvi.firstInstanceInitialValue.isDefined){
      // The NVI will inherit the default value of the original variable instance
      // This will also inherit any externally provided bindings.
      nvi.setDefaultValue(nvi.firstInstanceInitialValue)
    }
  }
}

class NewVariableInstanceEndParser(override val context: VariableRuntimeData)
  extends PrimParser {
  override lazy val runtimeDependencies = Vector()

  def parse(start: PState) = {
    start.removeVariableInstance(context)
  }
}

class AssertExpressionEvaluationParser(
  override val messageExpr: CompiledExpression[String],
  override val discrim: Boolean, // are we a discriminator or not.
  decl: RuntimeData,
  expr: CompiledExpression[AnyRef],
  failureType: FailureType)
  extends ExpressionEvaluationParser(expr, decl)
  with AssertMessageEvaluationMixin {

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
    if (testResult) {
      if (discrim) start.resolvePointOfUncertainty()
    } else {
      val message = getAssertFailureMessage(start)
      if (failureType == FailureType.ProcessingError) {
        val diag = new AssertionFailed(decl.schemaFileLocation, start, message)
        start.setFailed(diag)
      } else
        start.SDW("Assertion " + message)
    }
  }
}
