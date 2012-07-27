package daffodil.dsom

import daffodil.exceptions._
import daffodil.processors.xpath._
import javax.xml.xpath._
import daffodil.processors.VariableMap
import org.jdom.Element
import daffodil.processors.xpath.XPathUtil.CompiledExpressionFactory
import daffodil.util.Logging
import daffodil.util.Debug
import daffodil.util.LogLevel
import daffodil.xml.XMLUtils
import daffodil.processors.EmptyVariableMap

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
abstract class CompiledExpression(val prettyExpr : String) {
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
  def constantAsString = constant.toString
  def constantAsLong = constantAsString.toLong
  
  /**
   * evaluation - the runtime
   */
  def evaluate(rootedAt : org.jdom.Parent, variables : VariableMap) : Any


}

object CompiledExpressionUtil {
  
  def converter[T](convertTo: Symbol, expr: Any) = {
    val str: String = expr match {
      case n: org.jdom.Element => n.getText()
      case s: String => s
    }
    val res = convertTo match {
      case 'Long => str.toLong.asInstanceOf[T]
      case 'Double => str.toDouble.asInstanceOf[T]
      case 'String => str
      case 'Element => expr.asInstanceOf[org.jdom.Element]
      case _ => Assert.usageError("Runtime properties can only be Long, String, or Element")
    }
    res.asInstanceOf[T]
  }
    
}

case class ConstantProperty[T](value: T) extends CompiledExpression(value.toString) {
    def isConstant = true
    def isKnownNonEmpty = value != ""
    def constant: T = value
    def evaluate(pre: org.jdom.Parent, variables : VariableMap) = constant
  }

case class ExpressionProperty[T](convertTo : Symbol, 
    xpathText : String, 
    xpathExprFactory: CompiledExpressionFactory) extends CompiledExpression(xpathText) {
    def isConstant = false
    def isKnownNonEmpty = true // expressions are not allowed to return empty string
    def constant: T = Assert.usageError("Boolean isConstant is false. Cannot request a constant value.")
 
    def evaluate(pre: org.jdom.Parent, variables : VariableMap): T = {
      val xpathRes = XPathUtil.evalExpression(xpathText, xpathExprFactory, variables, pre)
      val converted : T = xpathRes match {
        case StringResult(s) => {
          val cs = CompiledExpressionUtil.converter[T](convertTo, s)
          cs
        }
        case NodeResult(n) => {
          CompiledExpressionUtil.converter(convertTo, n)
        }
      }
      converted
    }
  }


class ExpressionCompiler(edecl : SchemaComponent) extends Logging {
    
  def expandedName(qname : String) = {
    val (uri, localTypeName) = XMLUtils.QName(edecl.xml, qname, edecl.schemaDocument)
    val expName = XMLUtils.expandedQName(uri, localTypeName)
    expName
  }
  
  def convertTypeString(expandedTypeName : String) = {
    Assert.usage(expandedTypeName != null)
    expandedTypeName match {
      // TODO: make this insensitive to the prefix used (sometimes xs, sometimes xsd, sometimes whatever)
      case XMLUtils.XSD_STRING => 'String
      case XMLUtils.XSD_LONG => 'Long
      case XMLUtils.XSD_DOUBLE => 'Double
      case XMLUtils.XSD_INT => 'Long
      case _ => Assert.notYetImplemented()
    }
  }
  
  /**
   * The only way I know to check if the compiled expression was just a constant
   * is to evaluate it in an environment where if it touches anything (variables, jdom tree, etc.)
   * it will throw. No throw means a value came back and it must be a constant.
   */
  def constantValue(xpathExprFactory: CompiledExpressionFactory): Option[String] = 
    withLoggingLevel(LogLevel.Debug){
    val dummyVars = EmptyVariableMap
    val result =
      try {
        val res = XPathUtil.evalExpression(
            xpathExprFactory.expression + " (to see if constant)", 
            xpathExprFactory, 
            dummyVars, 
            null) // context node is not needed to see if an expression is a constant. 
        res match {
          case StringResult(s) => {
            log(Debug("%s is constant", xpathExprFactory.expression))
            Some(s)
          }
          case NodeResult(s) => Assert.invariantFailed("Can't evaluate to a node when testing for isConstant")
        }
      }
      catch {
        case e: XPathExpressionException => {
           log(Debug("%s is NOT constant (due to %s)", xpathExprFactory.expression, e.toString))
           None
        }
        case e: Exception => {
          Assert.invariantFailed("Didn't get an XPathExpressionException. Got: " + e)
        }
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
      val compiledXPath = XPathUtil.compileExpression(xpath, edecl.namespaces, Some(edecl))
      val cv = constantValue(compiledXPath)
      val compiledExpression = cv match {
        case Some(s) => {
          convertTo match {
            case 'String => new ConstantProperty(s.asInstanceOf[String])
            case 'Long => new ConstantProperty(s.asInstanceOf[String].toLong)
            case 'Element => new ConstantProperty(s.asInstanceOf[org.jdom.Element])
            case 'Double => new ConstantProperty(s.asInstanceOf[String].toDouble)
          }
        }
        case None => new ExpressionProperty(convertTo, expr, compiledXPath)
      }
      compiledExpression
    }
  }
}