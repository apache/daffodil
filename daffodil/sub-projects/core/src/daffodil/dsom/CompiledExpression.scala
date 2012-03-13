package daffodil.dsom

import daffodil.exceptions.Assert

/**
 * For the DFDL path/expression language, this
 * type checks the expression (SDE if not properly typed)
 * and provides the opportunity to compile it for efficient evaluation. 
 * 
 * The schemaNode is the schema component 
 * where the path is being evaluated which due to scoping, may not 
 * be the same one where it is defined. It is the combination of a 
 * property valued expression with a schema node that defines 
 * an evaluation of an expression. 
 * 
 * Note that an expression could be constant in some contexts, not others.
 * E.g., if a DFDL schema defines a format where the delimiters are in a header record,
 * then those are constant once you are parsing the body records.
 * 
 * TODO: provide enough scope information for this to optimize.
 */
abstract class CompiledExpression {
  /**
   * used to determine whether we need a runtime evaluation or
   * we can just use a constant value. 
   * 
   * Important because while many DFDL properties can have expressions
   * as their values, much of the time people will not take advantage 
   * of this generality.
   */
  def isConstant : Boolean
  
  /**
   * tells us if the property is non-empty. This is true if it is a constant non-empty expression
   * (that is, is not ""), but it is also true if it is evaluated as a runtime expression that it is
   * not allowed to return "".
   * 
   * Issue: are there properties which are string-valued, and where "" can in fact be returned at run time?
   * Assumed no. This was clarified in an errata to the DFDL spec.
   */
  def isKnownNonEmpty : Boolean

  /**
   * used to obtain a constant value. 
   * 
   * isConstantValue must be true or this will throw.
   */
  def constant : Any
  
  /**
   * evaluation - the runtime
   */
  def evaluate(rootedAt : org.jdom.Element) : Any
}

case class ConstantProperty[T](value: T) extends CompiledExpression {
    def isConstant = true
    def isKnownNonEmpty = value != ""
    def constant: T = value
    def evaluate(pre: org.jdom.Element) = constant
  }

class ExpressionCompiler(edecl : SchemaComponent) {
  /**
   * just checks for starting unescaped { 
   */
  val startsWithExpressionSyntax = """\{[^\{].*"""
 
  def compile[T](convertTo : Symbol, expr : String) : CompiledExpression = {
    val converted : T = convertTo match {
      case 'Long => expr.toLong.asInstanceOf[T]
      case 'String => expr.asInstanceOf[T]
      case _ => Assert.usageError("Runtime properties can only be Long or String")
    }
    Assert.subset(!expr.matches(startsWithExpressionSyntax), "Use of expressions to compute properties is not supported.")
    ConstantProperty[T](converted)
  }
}