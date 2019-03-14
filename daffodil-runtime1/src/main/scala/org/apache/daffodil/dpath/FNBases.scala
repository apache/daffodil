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

package org.apache.daffodil.dpath

import scala.collection.mutable.ListBuffer
import scala.xml.NodeSeq.seqToNodeSeq

import org.apache.daffodil.exceptions.Assert
import java.lang.{ Integer => JInt, Long => JLong, Double => JDouble, Boolean => JBoolean }
import java.math.{ BigInteger => JBigInt, BigDecimal => JBigDecimal }

trait CompareOpBase {
  def operate(v1: AnyRef, v2: AnyRef): AnyRef
}

trait NumberCompareOp extends CompareOpBase {
  /**
   * It is such a pain that there is no scala.math.Number base class above
   * all the numeric types.
   */
  def operate(v1: AnyRef, v2: AnyRef): JBoolean
}

trait StringCompareOp extends CompareOpBase {
  /**
   * According to Scala spec the compare method
   * returns x where:
   * x < 0 when v1 < v2
   * x == 0 when v1 == v2
   * x > 0 when v1 > v2
   *
   * This mimics the fn:compare method closely.
   */
  def compare(v1: AnyRef, v2: AnyRef): Int = v1.asInstanceOf[String].compare(v2.asInstanceOf[String])
}

abstract class CompareOp
  extends RecipeOp with BinaryOpMixin {

  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    left.run(dstate)
    val leftValue = dstate.currentValue
    // Now reset back to the original node to evaluate the right
    dstate.setCurrentNode(savedNode)
    right.run(dstate)
    val rightValue = dstate.currentValue
    val result = compare(op, leftValue, rightValue)
    dstate.setCurrentValue(result)
  }

  def compare(op: String, v1: AnyRef, v2: AnyRef): JBoolean
}

case class BooleanOp(op: String, left: CompiledDPath, right: CompiledDPath)
  extends RecipeOp with BinaryOpMixin {
  override def run(dstate: DState) {
    val savedNode = dstate.currentNode
    left.run(dstate)
    val leftValue = dstate.currentValue.asInstanceOf[Boolean] // convertToBoolean(dstate.currentValue, dstate.pstate)

    val result =
      if ((op == "and" && leftValue == false) ||
        (op == "or" && leftValue == true)) {
        leftValue
      } else {
        dstate.setCurrentNode(savedNode)
        right.run(dstate)
        val rightValue = dstate.currentValue.asInstanceOf[Boolean] // convertToBoolean(dstate.currentValue, dstate.pstate)
        rightValue
      }

    dstate.setCurrentValue(result)
  }
}
case class NegateOp(recipe: CompiledDPath) extends RecipeOpWithSubRecipes(recipe) {
  override def run(dstate: DState) {
    recipe.run(dstate)
    val value = dstate.currentValue match {
      case i: JInt => i * -1
      case l: JLong => l * (-1L)
      case d: JDouble => d * -1.0
      case bi: JBigInt => bi.negate()
      case bd: JBigDecimal => bd.negate()
      case bi: BigInt => bi.underlying().negate()
      case bd: BigDecimal => bd.underlying().negate()
      case _ => Assert.invariantFailed("not a number: " + dstate.currentValue.toString)
    }
    dstate.setCurrentValue(value)
  }

  override def toXML: scala.xml.Node = <Negate>{ recipe.toXML }</Negate>
}

abstract class FNOneArg(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends RecipeOpWithSubRecipes(recipe) {
  override def run(dstate: DState) {
    recipe.run(dstate)
    val arg = dstate.currentValue
    dstate.setCurrentValue(computeValue(arg, dstate))
  }

  override def toXML = toXML(recipe.toXML)

  def computeValue(str: AnyRef, dstate: DState): AnyRef
}

abstract class FNTwoArgs(recipes: List[CompiledDPath])
  extends RecipeOpWithSubRecipes(recipes) {
  override def run(dstate: DState) {

    val recipe1 = recipes(0)
    val recipe2 = recipes(1)

    val savedNode = dstate.currentNode
    dstate.resetValue()
    recipe1.run(dstate)
    val arg1 = dstate.currentValue

    dstate.setCurrentNode(savedNode)
    recipe2.run(dstate)
    val arg2 = dstate.currentValue

    val res = computeValue(arg1, arg2, dstate)
    
    dstate.setCurrentValue(res)
  }

  def computeValue(arg1: AnyRef, arg2: AnyRef, dstate: DState): AnyRef

  override def toXML = toXML(recipes.map { _.toXML })
}

abstract class FNTwoArgsNodeAndValue(recipes: List[CompiledDPath])
  extends RecipeOpWithSubRecipes(recipes) {
  override def run(dstate: DState) {

    val recipe1 = recipes(0)
    val recipe2 = recipes(1)

    val savedNode = dstate.currentNode
    dstate.resetValue()
    recipe1.run(dstate)
    val arg1 = dstate.currentNode

    dstate.setCurrentNode(savedNode)
    recipe2.run(dstate)
    val arg2 = dstate.currentValue

    dstate.setCurrentValue(computeValue(arg1, arg2, dstate))
  }

  def computeValue(arg1: AnyRef, arg2: AnyRef, dstate: DState): AnyRef

  override def toXML = toXML(recipes.map { _.toXML })
}

abstract class FNThreeArgs(recipes: List[CompiledDPath]) extends RecipeOpWithSubRecipes(recipes) {
  override def run(dstate: DState) {

    val recipe1 = recipes(0)
    val recipe2 = recipes(1)
    val recipe3 = recipes(2)

    val savedNode = dstate.currentNode
    recipe1.run(dstate)
    val arg1 = dstate.currentValue

    dstate.setCurrentNode(savedNode)
    recipe2.run(dstate)
    val arg2 = dstate.currentValue

    dstate.setCurrentNode(savedNode)
    recipe3.run(dstate)
    val arg3 = dstate.currentValue
    dstate.setCurrentValue(computeValue(arg1, arg2, arg3, dstate))
  }

  def computeValue(arg1: AnyRef, arg2: AnyRef, arg3: AnyRef, dstate: DState): AnyRef

  override def toXML = toXML(recipes.map { _.toXML })
}

abstract class FNArgsList(recipes: List[CompiledDPath]) extends RecipeOpWithSubRecipes(recipes) {
  override def run(dstate: DState) {

    val savedNode = dstate.currentNode

    // FIXME: rewrite to use an OnStack ListBuffer, and
    // a while loop with index vs. the foreach.
    val args: List[Any] = {
      val list = new ListBuffer[Any]

      recipes.foreach { recipe =>
        recipe.run(dstate)
        list += dstate.currentValue
        dstate.setCurrentNode(savedNode)
      }
      list.toList
    }

    dstate.setCurrentValue(computeValue(args, dstate))
  }

  def computeValue(args: List[Any], dstate: DState): AnyRef
}
