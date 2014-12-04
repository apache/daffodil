package edu.illinois.ncsa.daffodil.dpath

import scala.collection.mutable.ListBuffer
import scala.math.BigDecimal.int2bigDecimal
import scala.math.BigInt.int2bigInt
import scala.xml.NodeSeq.seqToNodeSeq

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Misc

trait CompareOpBase {
  def operate(v1: Any, v2: Any): Any
}

trait NumberCompareOp extends CompareOpBase {
  /**
   * It is such a pain that there is no scala.math.Number base class above
   * all the numeric types.
   */
  def operate(v1: Any, v2: Any): Boolean
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
  def compare(v1: Any, v2: Any): Int = v1.asInstanceOf[String].compare(v2.asInstanceOf[String])
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

  def compare(op: String, v1: Any, v2: Any): Boolean
}

case class EqualityCompareOp(op: String, left: CompiledDPath, right: CompiledDPath)
  extends CompareOp {

  def compare(op: String, v1: Any, v2: Any): Boolean = {
    (op, v1, v2) match {
      case ("eq", a, b) => a == b
      case ("ne", a, b) => a != b
      case _ => Assert.notYetImplemented("operator " + op +
        " on types " + Misc.getNameFromClass(v1) + ", " +
        Misc.getNameFromClass(v2))
    }
  }
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
    val savedNode = dstate.currentNode
    recipe.run(dstate)
    val value = dstate.currentValue match {
      case i: Int => i * -1
      case l: Long => l * (-1L)
      case d: Double => d * 1.0
      case bi: BigInt => bi * -1
      case bd: BigDecimal => bd * -1
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

  def computeValue(str: Any, dstate: DState): Any
}

abstract class FNTwoArgs(recipes: List[CompiledDPath])
  extends RecipeOpWithSubRecipes(recipes) {
  override def run(dstate: DState) {

    val List(recipe1, recipe2) = recipes

    val savedNode = dstate.currentNode
    dstate.resetValue()
    recipe1.run(dstate)
    val arg1 = dstate.currentValue

    dstate.setCurrentNode(savedNode)
    recipe2.run(dstate)
    val arg2 = dstate.currentValue

    dstate.setCurrentValue(computeValue(arg1, arg2, dstate))
  }

  def computeValue(arg1: Any, arg2: Any, dstate: DState): Any

  override def toXML = toXML(recipes.map { _.toXML })
}

abstract class FNThreeArgs(recipes: List[CompiledDPath]) extends RecipeOpWithSubRecipes(recipes) {
  override def run(dstate: DState) {

    val List(recipe1, recipe2, recipe3) = recipes

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

  def computeValue(arg1: Any, arg2: Any, arg3: Any, dstate: DState): Any

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

  def computeValue(args: List[Any], dstate: DState): Any
}
