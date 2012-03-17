package daffodil.dsom

import daffodil.exceptions._
import daffodil.processors.xpath._
import javax.xml.xpath._
import daffodil.processors.VariableMap

/**
 * For the DFDL path/expression language, this provides the place to
 * type check the expression (SDE if not properly typed)
 * and provides the opportunity to compile it for efficient evaluation. 
 * 
 * The schemaNode is the schema component 
 * where the path is being evaluated which due to scoping, may not 
 * be the same one where it is defined. It is the combination of a 
 * property valued expression with a schema node that defines 
 * an evaluation of an expression. 
 * 
 * TODO: Consider - that an expression could be constant in some contexts, not others.
 * E.g., if a DFDL schema defines a format where the delimiters are in a header record,
 * then those are constant once you are parsing the body records. This does imply 
 * keeping around the xpath compiler at runtime, which may not be desirable from a 
 * code size perspective. Whether it's worth it to compile or not is also a question 
 * of how often each xpath will be repeated.
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
  def evaluate(rootedAt : org.jdom.Element, variables : VariableMap) : Any


}

object CompiledExpressionUtil {
  def converter[T](convertTo: Symbol, expr: Any) = {
    val str: String = expr match {
      case n: org.jdom.Element => n.getText()
      case s: String => s
    }
    convertTo match {
      case 'Long => str.toLong.asInstanceOf[T]
      case 'String => str
      case 'Element => expr.asInstanceOf[org.jdom.Element]
      case _ => Assert.usageError("Runtime properties can only be Long, String, or Element")
    }
  }
    
}

case class ConstantProperty[T](value: T) extends CompiledExpression {
    def isConstant = true
    def isKnownNonEmpty = value != ""
    def constant: T = value
    def evaluate(pre: org.jdom.Element, variables : VariableMap) = constant
  }

case class ExpressionProperty[T](convertTo : Symbol, 
    xpathText : String, 
    xpathExprFactory: VariableMap => XPathExpression) extends CompiledExpression {
    def isConstant = false
    def isKnownNonEmpty = true // expressions are not allowed to return empty string
    def constant: T = Assert.usageError("Boolean isConstant is false. Cannot request a constant value.")
 
    def evaluate(pre: org.jdom.Element, variables : VariableMap) = {
      val xpathRes = XPathUtil.evalExpression(xpathText, xpathExprFactory, variables, pre)
      val converted = xpathRes match {
        case StringResult(s) => CompiledExpressionUtil.converter(convertTo, s)
        case NodeResult(n) => {
          CompiledExpressionUtil.converter(convertTo, n)
        }
      }
      converted
    }
  }


class ExpressionCompiler(edecl : AnnotatedMixin) {
  /**
   * The only way I know to check if the compiled expression was just a constant
   * is to evaluate it in an environment where if it touches anything (variables, jdom tree, etc.)
   * it will throw. No throw means a value came back and it must be a constant.
   */
  def constantValue(xpathExprFactory: VariableMap => XPathExpression): Option[String] = {
    val dummyRoot = new org.jdom.Element("dummy", "dummy")
    val dummyDoc = new org.jdom.Document(dummyRoot)
    val dummyVars = new VariableMap
    val result =
      try {
        val res = XPathUtil.evalExpression("", xpathExprFactory, dummyVars, dummyRoot) 
        res match {
          case StringResult(s) => Some(s)
          case NodeResult(s) => Assert.invariantFailed("Can't evaluate to a node when testing for isConstant")
        }
      }
      catch {
        case e: XPathEvaluationException => None
        case e: Exception => None // dangerous
      }
    result
  }

  def compile[T](convertTo: Symbol, expr: String): CompiledExpression = {
    if (!XPathUtil.isExpression(expr)) {
      // not an expression. For some properties like delimiters, you can use a literal string 
      // whitespace separated list of literal strings, or an expression in { .... }
      new ConstantProperty(expr)
    } else {

      val xpath = XPathUtil.getExpression(expr)
      val compiledXPath = XPathUtil.compileExpression(xpath, edecl.namespaces)
      val cv = constantValue(compiledXPath)
      val compiledExpression = cv match {
        case Some(s) => {
          convertTo match {
            case 'String => new ConstantProperty(s.asInstanceOf[String])
            case 'Long => new ConstantProperty(s.asInstanceOf[String].toLong)
            case 'Element => new ConstantProperty(s.asInstanceOf[org.jdom.Element])
          }
        }
        case None => new ExpressionProperty(convertTo, expr, compiledXPath)
      }
      compiledExpression
    }
  }
}