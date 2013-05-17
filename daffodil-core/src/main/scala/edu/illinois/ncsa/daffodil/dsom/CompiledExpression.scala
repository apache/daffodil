package edu.illinois.ncsa.daffodil.dsom

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.processors.xpath._
import javax.xml.xpath._
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.processors.xpath.XPathUtil.CompiledExpressionFactory
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.processors.EmptyVariableMap
import edu.illinois.ncsa.daffodil.processors.WithParseErrorThrowing
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.InfosetElement
import scala.xml.Node

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
abstract class CompiledExpression(val prettyExpr: String) {
  /**
   * used to determine whether we need a runtime evaluation or
   * we can just use a constant value.
   *
   * Important because while many DFDL properties can have expressions
   * as their values, much of the time people will not take advantage
   * of this generality.
   */
  def isConstant: Boolean

  /**
   * tells us if the property is non-empty. This is true if it is a constant non-empty expression
   * (that is, is not ""), but it is also true if it is evaluated as a runtime expression that it is
   * not allowed to return "".
   *
   * Issue: are there properties which are string-valued, and where "" can in fact be returned at run time?
   * Assumed no. This was clarified in an errata to the DFDL spec.
   */
  def isKnownNonEmpty: Boolean

  /**
   * used to obtain a constant value.
   *
   * isConstantValue must be true or this will throw.
   */
  def constant: Any
  def constantAsString = constant.toString
  def constantAsLong = constantAsString.toLong

  /**
   * evaluation - the runtime
   *
   * Note that since we can reference variables, and those might never have been read,
   * the act of evaluating them changes the variableMap state potentially. So an
   * updated variableMap must be returned as well.
   */

  def evaluate(rootedAt: InfosetElement, variables: VariableMap, pstate: PState): R

}

case class R(res: Any, vmap: VariableMap)

case class ConstantExpression[T](value: T) extends CompiledExpression(value.toString) {
  def isConstant = true
  def isKnownNonEmpty = value != ""
  def constant: T = value
  def evaluate(pre: InfosetElement, variables: VariableMap, ignored: PState): R = R(constant, variables)
}

case class RuntimeExpression[T <: AnyRef](convertTo: ConvertToType.Type,
  xpathText: String,
  xpathExprFactory: CompiledExpressionFactory,
  scArg: SchemaComponent)
  extends CompiledExpression(xpathText)
  with WithParseErrorThrowing
  with TypeConversions {

  lazy val context = scArg
  def isConstant = false
  def isKnownNonEmpty = true // expressions are not allowed to return empty string
  def constant: T = Assert.usageError("Boolean isConstant is false. Cannot request a constant value.")

    def toXPathType(convertTo: ConvertToType.Type) =
      convertTo match {
        case ConvertToType.Long => XPathConstants.NUMBER
        case ConvertToType.Double => XPathConstants.NUMBER
        case ConvertToType.Int | ConvertToType.UInt | ConvertToType.Integer |
          ConvertToType.UInteger | ConvertToType.Short | ConvertToType.UShort => XPathConstants.NUMBER
        case ConvertToType.Decimal | ConvertToType.Byte | ConvertToType.UByte | ConvertToType.ULong => XPathConstants.NUMBER
        case ConvertToType.String => XPathConstants.STRING
        case ConvertToType.Element => XPathConstants.NODE
        case ConvertToType.Boolean => XPathConstants.BOOLEAN
        case _ => Assert.invariantFailed("convertTo not valid value: " + convertTo)
      }

  def evaluate(pre: InfosetElement, variables: VariableMap, pstate: PState): R = {
    val xpathResultType = toXPathType(convertTo)
    val xpathRes = try {
      DFDLFunctions.currentPState = Some(pstate)
      variables.currentPState = Some(pstate)
      pre.evalExpression(xpathText, xpathExprFactory, variables, xpathResultType)
    } catch {
      case e: XPathException => {
        // runtime processing error in expression evaluation
        val ex = if (e.getMessage() == null) e.getCause() else e
        PE("Expression evaluation failed. Details: %s", ex)
      }
    } finally {
      DFDLFunctions.currentPState = None // put it back off
      variables.currentPState = None
    }
    val newVariableMap = xpathExprFactory.getVariables() // after evaluation, variables might have updated states.
    val converted: T = xpathRes match {
      case NotANumberResult(v) => {
        PE("Expression %s evaluated to something that is not a number (NaN): %s.", xpathText, v)
      }
      case NumberResult(n) => {
        val convertedResult = try {
          convertTo match {
            case ConvertToType.Long => convertToLong(n.toString, pstate).asInstanceOf[T]
            case ConvertToType.Double => convertToDouble(n.toString, pstate).asInstanceOf[T]
            case ConvertToType.Int => convertToInt(n.toString, pstate).asInstanceOf[T]
            case ConvertToType.Byte => convertToByte(n.toString, pstate).asInstanceOf[T]
            case ConvertToType.UByte => convertToUByte(n.toString, pstate).asInstanceOf[T]
            case ConvertToType.Short => convertToShort(n.toString, pstate).asInstanceOf[T]
            case ConvertToType.UShort => convertToUShort(n.toString, pstate).asInstanceOf[T]
            case ConvertToType.UInt => convertToUInt(n.toString, pstate).asInstanceOf[T]
            case ConvertToType.Boolean => convertToBoolean(n.toString, pstate).asInstanceOf[T]
            case ConvertToType.ULong => convertToULong(n.toString, pstate).asInstanceOf[T]
            case ConvertToType.Integer => convertToInteger(n.toString, pstate).asInstanceOf[T]
            case ConvertToType.Decimal => convertToDecimal(n.toString, pstate).asInstanceOf[T]
            case ConvertToType.UInteger => convertToNonNegativeInteger(n.toString, pstate).asInstanceOf[T]
            case _ => n.asInstanceOf[T]
          }
        } catch {
          // Note that the converToXXX functions all SDE themselves on conversion errors.
          // Here we just want to catch the exceptions that are the result of asInstanceOf[T]
          // or other unforeseen exceptions.
          case u: UnsuppressableException => throw u
          case cex: ClassCastException => pstate.SDE("Cannot convert %s to %s. Error %s", n, convertTo, cex)
        }
        convertedResult
      }
      case StringResult(s) => {
        Assert.invariant(convertTo == ConvertToType.String)
        s.asInstanceOf[T]
      }
      case BooleanResult(v) => {
        Assert.invariant(convertTo == ConvertToType.Boolean)
        v.asInstanceOf[T]
      }
      case NodeResult(n) => {
        n.asInstanceOf[T]
      }
    }
    R(converted, newVariableMap)
  }
}

object ConvertToType extends Enum {
  sealed abstract trait Type extends EnumValueType
  case object String extends Type
  case object Byte extends Type
  case object Short extends Type
  case object Int extends Type
  case object Long extends Type
  case object UByte extends Type
  case object UShort extends Type
  case object UInt extends Type
  case object ULong extends Type
  case object Double extends Type
  case object Integer extends Type
  case object UInteger extends Type
  case object Decimal extends Type
  case object Boolean extends Type
  case object Element extends Type
}

class ExpressionCompiler(edecl: SchemaComponent) extends Logging with TypeConversions {

  def expandedName(qname: String) = {
    val (uri, localTypeName) = XMLUtils.QName(edecl.xml, qname, edecl.schemaDocument)
    val expName = XMLUtils.expandedQName(uri, localTypeName)
    expName
  }

  // TODO FIXME - XPath 1.0 vs 2.0 return types. This is not valid even for XPath 1.0
  def convertTypeString(expandedTypeName: String) = {
    Assert.usage(expandedTypeName != null)
    expandedTypeName match {
      case XMLUtils.XSD_STRING => ConvertToType.String
      case XMLUtils.XSD_BYTE => ConvertToType.Byte
      case XMLUtils.XSD_SHORT => ConvertToType.Short
      case XMLUtils.XSD_INT => ConvertToType.Int
      case XMLUtils.XSD_LONG => ConvertToType.Long
      case XMLUtils.XSD_UNSIGNED_BYTE => ConvertToType.UByte
      case XMLUtils.XSD_UNSIGNED_SHORT => ConvertToType.UShort
      case XMLUtils.XSD_UNSIGNED_INT => ConvertToType.UInt
      case XMLUtils.XSD_DOUBLE => ConvertToType.Double
      case XMLUtils.XSD_FLOAT => ConvertToType.Double
      case XMLUtils.XSD_BOOLEAN => ConvertToType.Boolean
      case XMLUtils.XSD_HEX_BINARY => ConvertToType.String
      case XMLUtils.XSD_DATE => ConvertToType.String
      case XMLUtils.XSD_DATE_TIME => ConvertToType.String
      case XMLUtils.XSD_TIME => ConvertToType.String
      case XMLUtils.XSD_NON_NEGATIVE_INTEGER => ConvertToType.UInteger
      case XMLUtils.XSD_INTEGER => ConvertToType.Integer
      case XMLUtils.XSD_UNSIGNED_LONG => ConvertToType.ULong
      case XMLUtils.XSD_DECIMAL => ConvertToType.Decimal
    }
  }

  /**
   * The only way I know to check if the compiled expression was just a constant
   * is to evaluate it in an environment where if it touches anything (variables, jdom tree, etc.)
   * it will throw. No throw means a value came back and it must be a constant.
   */
  def constantValue(xpathExprFactory: CompiledExpressionFactory): Option[Any] =
    // withLoggingLevel(LogLevel.Info) 
    {
      val dummyVars = EmptyVariableMap
      val result =
        try {
          DFDLFunctions.currentPState = None // no state if we're trying for a constant value.
          val res = XPathUtil.evalExpression(
            xpathExprFactory.expression + " (to see if constant)",
            xpathExprFactory,
            dummyVars,
            null, // context node is not needed to see if an expression is a constant.
            XPathConstants.STRING) // <-- This looks like it always evaluates to StringResult
          res match {
            case StringResult(s) => {
              log(LogLevel.Debug, "%s is constant", xpathExprFactory.expression)
              Some(s)
            }
            case BooleanResult(s) => {
              log(LogLevel.Debug, "%s is constant", xpathExprFactory.expression)
              Some(s)
            }
            case _ => Assert.invariantFailed("Can't evaluate to " + res + " when testing for isConstant")
          }
        } catch {
          case u: UnsuppressableException => throw u
          case e: XPathExpressionException => {
            log(LogLevel.Debug, "%s is NOT constant (due to %s)", xpathExprFactory.expression, e.toString)
            None
          }
          case e: SchemaDefinitionError => {
            // TODO differentiate between the xpath being syntax-invalid (hence, an SDE, not a constant/runtime distinction
            // and other SDEs like variable not defined, which just indicates (for here), that the expression 
            // is non-constant.
            log(LogLevel.Debug, "%s is NOT constant (due to %s)", xpathExprFactory.expression, e.toString)
            None
          }
          //          case e : Exception => {
          //            Assert.invariantFailed("Didn't get an XPathExpressionException. Got: " + e)
          //          }
        } finally {
          DFDLFunctions.currentPState = None
        }
      result
    }

  def compileTimeConvertToLong(s: Any) = convertToLong(s, edecl)
  def compileTimeConvertToDouble(s: Any) = convertToDouble(s, edecl)
  def compileTimeConvertToBoolean(s: Any) = convertToBoolean(s, edecl)
  def compileTimeConvertToByte(s: Any) = convertToByte(s, edecl)
  def compileTimeConvertToShort(s: Any) = convertToShort(s, edecl)
  def compileTimeConvertToInt(s: Any) = convertToInt(s, edecl)
  def compileTimeConvertToUByte(s: Any) = convertToUByte(s, edecl)
  def compileTimeConvertToUShort(s: Any) = convertToUShort(s, edecl)
  def compileTimeConvertToUInt(s: Any) = convertToUInt(s, edecl)
  def compileTimeConvertToULong(s: Any) = convertToULong(s, edecl)
  def compileTimeConvertToInteger(s: Any) = convertToInteger(s, edecl)
  def compileTimeConvertToNonNegativeInteger(s: Any) = convertToNonNegativeInteger(s, edecl)
  def compileTimeConvertToDecimal(s: Any) = convertToDecimal(s, edecl)

  def compile[T](convertTo: ConvertToType.Type, property: Found): CompiledExpression = {

    val expr: String = property.value
    val xmlForNamespaceResolution = property.location.xml
    if (!XPathUtil.isExpression(expr)) {
      // not an expression. For some properties like delimiters, you can use a literal string 
      // whitespace separated list of literal strings, or an expression in { .... }
      new ConstantExpression(expr)
    } else {

      // This is important. The namespace bindings we use must be
      // those from the object where the property carrying the expression 
      // was written, not those of the edecl object where the property 
      // value is being used/compiled. JIRA DFDL-407
      val exprNSBindings = XMLUtils.namespaceBindings(xmlForNamespaceResolution.scope)
      val xpath = XPathUtil.getExpression(expr)
      val compiledXPath =
        try {
          //
          // We also want SDEs from expression compilation issued with the 
          // schema component where the property was found as the file/line information.
          // (So the user can go there and see the expression.)
          //
          // This can happen. The length and occursCount properties CAN be scoped,
          // or placed on simpleType definitions so their expressions are shared, and
          // are not on the same lexical object that has that length or occurrances.
          // 
          val scWherePropertyWasLocated = property.location.asInstanceOf[SchemaComponent]
          XPathUtil.compileExpression(xpath, exprNSBindings, scWherePropertyWasLocated)
        } catch {
          case e: XPathExpressionException => {
            val exc = e // debugger never seems to show the case variable itself.
            val realExc = if (e.getCause() != null) e.getCause() else exc
            // Assert.invariant(realExc != null) // it's always an encapsulation of an underlying error.
            edecl.SDE("XPath Compilation Error: %s", realExc)
          }
        }
      val cv = constantValue(compiledXPath)
      val compiledExpression = cv match {
        case Some(s) => {
          convertTo match {
            case ConvertToType.String => new ConstantExpression(s)
            // Evaluating to an Element when we're a constant makes no sense.
            // case 'Element => new ConstantExpression(s.asInstanceOf[org.jdom.Element])
            case ConvertToType.Element => Assert.usageError("Evaluating to an Element when we're a constant makes no sense.")
            case ConvertToType.Byte => new ConstantExpression(compileTimeConvertToByte(s))
            case ConvertToType.UByte => new ConstantExpression(compileTimeConvertToUByte(s))
            case ConvertToType.Short => new ConstantExpression(compileTimeConvertToShort(s))
            case ConvertToType.UShort => new ConstantExpression(compileTimeConvertToUShort(s))
            case ConvertToType.Int => new ConstantExpression(compileTimeConvertToInt(s))
            case ConvertToType.UInt => new ConstantExpression(compileTimeConvertToUInt(s))
            case ConvertToType.Long => new ConstantExpression(compileTimeConvertToLong(s))
            case ConvertToType.ULong => new ConstantExpression(compileTimeConvertToULong(s))
            case ConvertToType.Double => new ConstantExpression(compileTimeConvertToDouble(s))
            case ConvertToType.Integer => new ConstantExpression(compileTimeConvertToInteger(s))
            case ConvertToType.UInteger => new ConstantExpression(compileTimeConvertToNonNegativeInteger(s))
            case ConvertToType.Decimal => new ConstantExpression(compileTimeConvertToDecimal(s))
            case ConvertToType.Boolean => new ConstantExpression(compileTimeConvertToBoolean(s))
          }
        }
        case None => new RuntimeExpression(convertTo, expr, compiledXPath, edecl)
      }
      compiledExpression
    }
  }
}
