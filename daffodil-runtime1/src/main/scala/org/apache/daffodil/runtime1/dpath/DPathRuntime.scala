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

package org.apache.daffodil.runtime1.dpath

import java.lang.{ Number => JNumber }
import scala.collection.compat.immutable.ArraySeq
import scala.xml.NodeSeq.seqToNodeSeq

import org.apache.daffodil.lib.api.DaffodilTunables
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.SchemaFileLocation
import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.runtime1.dsom.DPathCompileInfo
import org.apache.daffodil.runtime1.dsom.SchemaDefinitionDiagnosticBase
import org.apache.daffodil.runtime1.dsom.SchemaDefinitionError
import org.apache.daffodil.runtime1.infoset.DINode
import org.apache.daffodil.runtime1.infoset.DataValue
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitiveNullable
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueString
import org.apache.daffodil.runtime1.infoset.InfosetException
import org.apache.daffodil.runtime1.processors.CompileState
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.ProcessingError
import org.apache.daffodil.runtime1.processors.VariableException
import org.apache.daffodil.runtime1.processors.VariableHasNoValue
import org.apache.daffodil.runtime1.processors.VariableRuntimeData

class CompiledDPath(val ops: RecipeOp*) extends Serializable {

  def this(ops: List[RecipeOp]) = this(ArraySeq.unsafeWrapArray(ops.toArray): _*)

  override def toString =
    toXML.toString

  def toXML = <CompiledDPath>{ops.map { _.toXML }}</CompiledDPath>

  /**
   * For parsing or for backward-referencing expressions when unparsing.
   */
  def runExpression(state: ParseOrUnparseState, dstate: DState): Unit = {
    dstate.opIndex = 0

    // This if statement is necessary as defineVariable statements can contain
    // expressions that are evaluated prior to an infoset being generated
    if (state.currentNode.isDefined) {
      dstate.setCurrentNode(state.thisElement.asInstanceOf[DINode])
      dstate.setContextNode(state.thisElement.asInstanceOf[DINode]) // used for diagnostics
    }

    dstate.setOccursIndex(state.occursPos)
    dstate.setErrorOrWarn(state)
    dstate.resetValue()
    dstate.isCompile = state match {
      case cs: CompileState => true
      case _ => false
    }
    run(dstate)
  }

  /**
   * Used at compilation time to evaluate expressions to determine
   * if they are constant valued.
   *
   * TODO: constant folding really should operate on sub-expressions of expressions
   * so that part of an expression can be constant, not necessarily the whole thing.
   */
  def runExpressionForConstant(
    sfl: SchemaFileLocation,
    compileInfo: DPathCompileInfo,
    tunable: DaffodilTunables
  ): DataValuePrimitiveNullable = {

    //
    // we use a special dummy dstate here that errors out via throw
    // if the evaluation tries to get a processor state or node.
    //
    val dstate = new DStateForConstantFolding(compileInfo, tunable)
    val isConstant: Boolean =
      try {
        run(dstate)
        // it ran, so must have produced a constant value
        val v = dstate.currentValue
        Assert.invariant(v.isDefined)
        // the only way dstate can have a value is if setCurrentValue was called
        // so this is redundant. Remove?
        // dstate.setCurrentValue(v) // side effect nulls out the current node
        true
      } catch {
        //
        // We use InfosetException to indicate that the DState was manipulated
        // in a way that is not consistent with a constant expression. Such as trying to do
        // anything with the infoset other than saving and restoring current position in the infoset.
        // Ditto trying to read a variable.
        case _: InfosetException | _: VariableException | _: java.lang.IllegalStateException =>
          false // useful place for breakpoint
        // if the expression is all literals, but illegal such as xs:int("foobar") then
        // all the pieces are constant, but evaluating will throw NumberFormatException
        // or dfdl:length='{ 5 / 0 }' - contrived yes, but in larger expressions misakes like this
        // are typically typographical errors so it is good to pick them up here.
        case e: java.lang.ArithmeticException =>
          throw new SchemaDefinitionError(Some(sfl), None, e.getMessage())
        case e: java.lang.NumberFormatException =>
          throw new SchemaDefinitionError(Some(sfl), None, e.getMessage())
        case e: InvalidPrimitiveDataException =>
          throw new SchemaDefinitionError(Some(sfl), None, e.getMessage())
        case e: java.lang.IndexOutOfBoundsException => false
        case e: java.lang.IllegalArgumentException => false
        case e: FNErrorException => false
        case e: SchemaDefinitionDiagnosticBase =>
          throw new SchemaDefinitionError(Some(sfl), None, e.getMessage())
        case e: ProcessingError =>
          throw new SchemaDefinitionError(Some(sfl), None, e.getMessage())
      }
    val res =
      if (isConstant) dstate.currentValue else DataValue.NoValue
    res
  }

  def run(dstate: DState): Unit = {
    // We are running a subexpression. The currentNode may have been changed by
    // previous expressions, so we need to reset the currentNode back to the
    // original contextNode. It is up to the caller of this subexpression to
    // save and reset the currentNode if they need it. Note that contextNode
    // might not be defined when compiling for constants, but at runtime
    // contextNode should always be defined.
    if (dstate.contextNode.isDefined) dstate.setCurrentNode(dstate.contextNode.get)

    dstate.opIndex = 0
    var i = 0
    while (i < ops.length) {
      val op = ops(i)
      op.run(dstate)
      i += 1
    }
  }
}

abstract class RecipeOp extends Serializable {

  def run(dstate: DState): Unit

  protected def subRecipes: Seq[CompiledDPath] = Nil

  protected def toXML(s: String): scala.xml.Node = toXMLVarargs(new scala.xml.Text(s))

  protected def toXMLVarargs(children: scala.xml.Node*): scala.xml.Node = toXML(children.toSeq)

  protected def toXML(children: scala.xml.NodeSeq): scala.xml.Node = {
    val name = Misc.getNameFromClass(this)
    scala.xml.Elem(
      null,
      name,
      scala.xml.Null,
      scala.xml.TopScope,
      children.isEmpty,
      children: _*
    )
  }

  /**
   * default behavior is inherited and it displays a RecipeOp assuming there are no children
   * to display. Override and make it call one of the above toXML methods if
   * there are children to display.
   */
  def toXML: scala.xml.Node = toXML(scala.xml.NodeSeq.Empty)

}

abstract class RecipeOpWithSubRecipes(recipes: List[CompiledDPath]) extends RecipeOp {

  override def subRecipes: List[CompiledDPath] = recipes

  def this(recipes: CompiledDPath*) = this(recipes.toList)

}

case class VRef(vrd: VariableRuntimeData, context: ThrowsSDE) extends RecipeOp {

  override def run(dstate: DState): Unit = {
    if (dstate.parseOrUnparseState.isEmpty)
      throw new VariableHasNoValue(vrd.globalQName, vrd)
    val value = dstate.parseOrUnparseState.get.getVariable(vrd, context)
    dstate.setCurrentValue(value)
  }

  override def toXML = toXML("$" + vrd.globalQName.toPrettyString)

}

case class Literal(v: DataValuePrimitive) extends RecipeOp {
  override def run(dstate: DState): Unit = {
    dstate.setCurrentValue(v)
  }
  override def toXML = toXML(v.toString)

}

case class IF(
  predRecipe: CompiledDPath,
  thenPartRecipe: CompiledDPath,
  elsePartRecipe: CompiledDPath
) extends RecipeOpWithSubRecipes(predRecipe, thenPartRecipe, elsePartRecipe) {

  override def run(dstate: DState): Unit = {
    val savedNode = dstate.currentNode
    predRecipe.run(dstate)
    val predValue = dstate.currentValue.getBoolean
    dstate.setCurrentNode(savedNode)
    if (predValue) {
      thenPartRecipe.run(dstate)
    } else {
      elsePartRecipe.run(dstate)
    }
    // should have a value now. IF-Then-Else is always
    // evaluated for a value.
    Assert.invariant(dstate.currentValue.isDefined)
  }

  override def toXML =
    <if>
      <pred>{predRecipe.toXML}</pred>
      <then>{thenPartRecipe.toXML}</then>
      <else>{elsePartRecipe.toXML}</else>
    </if>
}

trait BinaryOpMixin { self: RecipeOp =>
  def op: String
  def left: CompiledDPath
  def right: CompiledDPath
  override def subRecipes: Seq[CompiledDPath] = Seq(left, right)

  override def toXML: scala.xml.Node =
    toXMLVarargs(new scala.xml.Text(op), left.toXML, right.toXML)
}

case class CompareOperator(cop: CompareOpBase, left: CompiledDPath, right: CompiledDPath)
  extends RecipeOp
  with BinaryOpMixin {

  override def op = Misc.getNameFromClass(cop)

  override def run(dstate: DState): Unit = {
    val savedNode = dstate.currentNode
    left.run(dstate)
    val leftValue = dstate.currentValue.getNonNullable
    dstate.setCurrentNode(savedNode)
    right.run(dstate)
    val rightValue = dstate.currentValue.getNonNullable
    val result = cop.operate(leftValue, rightValue)
    dstate.setCurrentValue(result)
  }
}

case class NumericOperator(nop: NumericOp, left: CompiledDPath, right: CompiledDPath)
  extends RecipeOp
  with BinaryOpMixin {

  override def op = Misc.getNameFromClass(nop)

  override def run(dstate: DState): Unit = {
    val savedNode = dstate.currentNode
    left.run(dstate)
    val leftValue = dstate.currentValue.getNumber
    dstate.setCurrentNode(savedNode)
    right.run(dstate)
    val rightValue = dstate.currentValue.getNumber
    val result = nop.operate(leftValue, rightValue)
    dstate.setCurrentValue(result)
  }
}

trait NumericOp {

  /**
   * It is such a pain that there is no scala.math.Number base class above
   * all the numeric types.
   */
  def operate(v1: JNumber, v2: JNumber): JNumber
}

abstract class Converter extends RecipeOp {

  def typeNames = {
    // This is a total hack. Grab the type names of this converter
    // by spliting the class name at the "To" in the middle.
    val names = Misc.getNameFromClass(this).split("To").toList
    Assert.invariant(names.length == 2)
    val List(fromTypeName, toTypeName) = names
    (fromTypeName, toTypeName)
  }

  override def run(dstate: DState): Unit = {
    val arg = dstate.currentValue.getNonNullable
    val res =
      try {
        computeValue(arg, dstate)
      } catch {
        case e: InvalidPrimitiveDataException => {
          val (fromTypeName, toTypeName) = typeNames
          val msg =
            if (e.getMessage() != null && e.getMessage() != "") e.getMessage()
            else "No other details are available."
          val err = new NumberFormatException(
            "Cannot convert '%s' from %s type to %s (%s).".format(
              arg.getAnyRef.toString,
              fromTypeName,
              toTypeName,
              msg
            )
          )
          throw err
        }
      }
    dstate.setCurrentValue(res)
  }

  def computeValue(str: DataValuePrimitive, dstate: DState): DataValuePrimitive
}

trait ToString extends Converter {
  override def computeValue(a: DataValuePrimitive, dstate: DState): DataValueString =
    a.getAnyRef.toString
}

/**
 * In some cases, expressions that are never used lack context and so cannot be compiled.
 * An example of this is when a schema has an unused global complex type or group
 * definition with an expression and compileAllTopLevel is enabled. Compiling such
 * expressions requires knowledge about where the complex type or group is used. But if
 * they are never used then we don't have that context and can't correctly compile the
 * expression.
 *
 * Fortunately, because the global complex types/group is never used it it safe to ignore
 * the expression. Unfortunately, parts of Daffodil code still require a valid
 * CompiledExpression. In these cases, we can use this RuntimeAbort recipe as the recipe
 * of the the CompiledExpression. This recipe throws an IllegalStateException during
 * constant folding so that the expression is treated as non-constant and makes the rest
 * of Daffodil happy, but throws an invariant if it is ever evaluated at runtime.
 */
case class RuntimeAbortOp(expr: String) extends RecipeOp {
  override def run(dstate: DState): Unit = {
    dstate match {
      case _: DStateForConstantFolding => throw new java.lang.IllegalStateException
      case _ =>
        Assert.invariantFailed(
          s"Expression should not have been evaluated during runtime: $expr"
        )
    }
  }
}
