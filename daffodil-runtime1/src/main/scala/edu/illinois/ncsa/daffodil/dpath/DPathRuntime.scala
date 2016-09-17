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

package edu.illinois.ncsa.daffodil.dpath

import scala.xml.NodeSeq.seqToNodeSeq
import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionDiagnosticBase
import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionError
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.processors.DINode
import edu.illinois.ncsa.daffodil.processors.InfosetException
import edu.illinois.ncsa.daffodil.processors.VariableException
import edu.illinois.ncsa.daffodil.processors.ProcessingError
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.processors.ParseOrUnparseState
import edu.illinois.ncsa.daffodil.processors.VariableRuntimeData
import java.lang.{ Number => JNumber }
import java.math.{ BigInteger => JBigInt, BigDecimal => JBigDecimal }
import edu.illinois.ncsa.daffodil.processors.CompileState

class CompiledDPath(val ops: RecipeOp*) extends Serializable {

  def this(ops: List[RecipeOp]) = this(ops.toArray: _*)

  override def toString =
    toXML.toString

  def toXML = <CompiledDPath>{ ops.map { _.toXML } }</CompiledDPath>

  /**
   * For parsing or for backward-referencing expressions when unparsing.
   */
  def runExpression(state: ParseOrUnparseState, dstate: DState) {
    dstate.opIndex = 0
    dstate.setCurrentNode(state.thisElement.asInstanceOf[DINode])
    dstate.setVMap(state.variableMap)
    dstate.setContextNode(state.thisElement.asInstanceOf[DINode]) // used for diagnostics
    dstate.setArrayPos(state.arrayPos)
    dstate.setErrorOrWarn(state)
    dstate.resetValue
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
  def runExpressionForConstant(sfl: SchemaFileLocation): Option[AnyRef] = {

    //
    // we use a special dummy dstate here that errors out via throw
    // if the evaluation tries to get a processor state or node.
    //
    val dstate = new DStateForConstantFolding
    val isConstant: Boolean =
      try {
        run(dstate)
        // it ran, so must have produced a constant value
        val v = dstate.currentValue
        Assert.invariant(v != null)
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
        case e: java.lang.NumberFormatException => throw new SchemaDefinitionError(Some(sfl), None, e.getMessage)
        case e: java.lang.IndexOutOfBoundsException => false
        case e: java.lang.IllegalArgumentException => false
        case e: SchemaDefinitionDiagnosticBase => throw new SchemaDefinitionError(Some(sfl), None, e.getMessage)
        case e: ProcessingError => throw new SchemaDefinitionError(Some(sfl), None, e.getMessage)
      }
    val res =
      if (isConstant) Some(dstate.currentValue) else None
    res
  }

  def run(dstate: DState) {
    dstate.opIndex = 0
    var i = 0
    while (i < ops.length) {
      val op = ops(i)
      op.run(dstate)
      i += 1
    }
  }
}

abstract class RecipeOp
  extends Serializable {

  def run(dstate: DState): Unit

  protected def subRecipes: Seq[CompiledDPath] = Nil

  protected def toXML(s: String): scala.xml.Node = toXML(new scala.xml.Text(s))

  protected def toXML(children: scala.xml.Node*): scala.xml.Node = toXML(children.toSeq)

  protected def toXML(children: scala.xml.NodeSeq): scala.xml.Node = {
    val name = Misc.getNameFromClass(this)
    scala.xml.Elem(null, name, scala.xml.Null, scala.xml.TopScope, children.isEmpty, children: _*)
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

case class VRef(vrd: VariableRuntimeData, context: ThrowsSDE)
  extends RecipeOp {

  override def run(dstate: DState) {
    Assert.invariant(dstate.vmap != null)
    val (res, newVMap) = dstate.vmap.readVariable(vrd, context)
    dstate.setVMap(newVMap)
    dstate.setCurrentValue(res)
  }

  override def toXML = toXML("$" + vrd.globalQName.toPrettyString)

}

case class Literal(v: Any) extends RecipeOp {
  override def run(dstate: DState) {
    dstate.setCurrentValue(v)
  }
  override def toXML = toXML(v.toString)

}

case class IF(predRecipe: CompiledDPath, thenPartRecipe: CompiledDPath, elsePartRecipe: CompiledDPath)
  extends RecipeOpWithSubRecipes(predRecipe, thenPartRecipe, elsePartRecipe) {

  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    predRecipe.run(dstate)
    val predValue = dstate.currentValue.asInstanceOf[Boolean]
    dstate.setCurrentNode(savedNode)
    if (predValue) {
      thenPartRecipe.run(dstate)
    } else {
      elsePartRecipe.run(dstate)
    }
    // should have a value now. IF-Then-Else is always
    // evaluated for a value.
    Assert.invariant(dstate.currentValue != null)
  }

  override def toXML =
    <if>
      <pred>{ predRecipe.toXML }</pred>
      <then>{ thenPartRecipe.toXML }</then>
      <else>{ elsePartRecipe.toXML }</else>
    </if>
}

trait BinaryOpMixin { self: RecipeOp =>
  def op: String
  def left: CompiledDPath
  def right: CompiledDPath
  override def subRecipes: Seq[CompiledDPath] = Seq(left, right)

  override def toXML: scala.xml.Node = toXML(new scala.xml.Text(op), left.toXML, right.toXML)
}

case class CompareOperator(cop: CompareOpBase, left: CompiledDPath, right: CompiledDPath)
  extends RecipeOp with BinaryOpMixin {

  override def op = Misc.getNameFromClass(cop)

  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    left.run(dstate)
    val leftValue = dstate.currentValue
    dstate.setCurrentNode(savedNode)
    right.run(dstate)
    val rightValue = dstate.currentValue
    val result = cop.operate(leftValue, rightValue)
    dstate.setCurrentValue(result)
  }
}

case class NumericOperator(nop: NumericOp, left: CompiledDPath, right: CompiledDPath)
  extends RecipeOp with BinaryOpMixin {

  override def op = Misc.getNameFromClass(nop)

  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    left.run(dstate)
    val leftValue = dstate.currentValue.asInstanceOf[JNumber]
    dstate.setCurrentNode(savedNode)
    right.run(dstate)
    val rightValue = dstate.currentValue.asInstanceOf[JNumber]
    val result = nop.operate(leftValue, rightValue)
    dstate.setCurrentValue(result)
  }
}

trait NumericOp {
  import scala.language.implicitConversions

  implicit def BigDecimalToJBigDecimal(bd: BigDecimal): JBigDecimal = bd.bigDecimal
  implicit def BigIntToJBigInt(bi: BigInt): JBigInt = bi.bigInteger

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

  override def run(dstate: DState) {
    val arg = dstate.currentValue
    val res =
      try {
        computeValue(arg, dstate)
      } catch {
        case e: NumberFormatException => {
          val (fromTypeName, toTypeName) = typeNames
          val msg =
            if (e.getMessage() != null && e.getMessage() != "") e.getMessage()
            else "No other details are available."
          val err = new NumberFormatException("Cannot convert '%s' from %s type to %s (%s).".format(arg.toString, fromTypeName, toTypeName, msg))
          throw err
        }
      }
    dstate.setCurrentValue(res)
  }

  def computeValue(str: AnyRef, dstate: DState): AnyRef
}

trait ToString extends Converter {
  override def computeValue(a: AnyRef, dstate: DState): AnyRef = a.toString
}

case object StringToNonEmptyString extends RecipeOp {
  override def run(dstate: DState) {
    val current = dstate.currentValue.asInstanceOf[String]
    if (current.length == 0)
      throw new IllegalArgumentException("String value may not be empty.")
  }
}
