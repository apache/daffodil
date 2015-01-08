package edu.illinois.ncsa.daffodil.dpath

import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml._

import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionDiagnosticBase
import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionError
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.processors.DINode
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.ProcessingError
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.xml.RefQName

class CompiledDPath(val ops: RecipeOp*) extends Serializable {

  def this(ops: List[RecipeOp]) = this(ops.toArray: _*)

  override def toString = toXML.toString

  def toXML = <CompiledDPath>{ ops.map { _.toXML } }</CompiledDPath>

  def runExpression(pstate: PState) {
    val dstate = pstate.dstate
    dstate.setCurrentNode(pstate.infoset.asInstanceOf[DINode])
    dstate.setVMap(pstate.variableMap)
    dstate.setPState(pstate)
    dstate.resetValue
    run(dstate)
  }

  /**
   * Used at compilation time to evaluate expressions to determine
   * if they are constant valued.
   *
   * TODO: constnat folding really should operate on sub-expressions of expressions
   * so that part of an expression can be constant, not necessarily the whole thing.
   */
  def runExpressionForConstant(sfl: SchemaFileLocation): Option[Any] = {

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
        // We use IllegalStateException to indicate that the DState was manipulated 
        // in a way that is not consistent with a constant expression. Such as trying to do 
        // anything with the infoset other than saving and restoring current position in the infoset. 
        case e: java.lang.IllegalStateException =>
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
    var i = 0
    // Assert.invariant(ops.length > 0)
    while (i < ops.length) {
      val op = ops(i)
      op.run(dstate)
      i = i + 1
    }
  }
}

abstract class RecipeOp
  extends Serializable {
  import AsIntConverters._

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

case class VRef(qn: RefQName, context: ThrowsSDE)
  extends RecipeOp {

  val expName = qn.toExpandedName

  override def run(dstate: DState) {
    Assert.invariant(dstate.vmap != null)
    val (res, newVMap) = dstate.vmap.readVariable(expName, context)
    dstate.setVMap(newVMap)
    dstate.setCurrentValue(res)
  }

  override def toXML = toXML("$" + qn.toPrettyString)

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
    val leftValue = dstate.currentValue
    dstate.setCurrentNode(savedNode)
    right.run(dstate)
    val rightValue = dstate.currentValue
    val result = nop.operate(leftValue, rightValue)
    dstate.setCurrentValue(result)
  }
}

trait NumericOp {
  /**
   * It is such a pain that there is no scala.math.Number base class above
   * all the numeric types.
   */
  def operate(v1: Any, v2: Any): Any
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

  def computeValue(str: Any, dstate: DState): Any
}

trait ToString extends Converter {
  override def computeValue(a: Any, dstate: DState) = a.toString
}

case object StringToNonEmptyString extends RecipeOp {
  override def run(dstate: DState) {
    val current = dstate.currentValue.asInstanceOf[String]
    if (current.length == 0)
      throw new IllegalArgumentException("String value may not be empty.")
  }
}

