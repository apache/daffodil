package daffodil.processors.xpath

/**
 * Copyright (c) 2010 NCSA.  All rights reserved.
 * Developed by: NCSA Cyberenvironments and Technologies
 *               University of Illinois at Urbana-Champaign
 *               http://cet.ncsa.uiuc.edu/
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal with the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the names of NCSA, University of Illinois, nor the names of its
 *     contributors may be used to endorse or promote products derived from this
 *     Software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * WITH THE SOFTWARE.
 *
 */

/*
 * Created By: Alejandro Rodriguez < alejandr @ ncsa . uiuc . edu >
 * Date: 2010
 */

import javax.xml.xpath._
import javax.xml.xpath.XPathConstants._
import javax.xml.namespace.QName
import org.jdom.Element
import org.jdom.Text
import org.jdom.Parent
import net.sf.saxon.om.NamespaceConstant
import net.sf.saxon.jdom.NodeWrapper
import net.sf.saxon.jdom.DocumentWrapper
import net.sf.saxon.Configuration
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.xpath.XPathEvaluator
import daffodil.exceptions._
import daffodil.processors.VariableMap
import daffodil.xml.XMLUtils
import daffodil.util.Logging
import daffodil.util.Debug
import daffodil.util.LogLevel
import scala.xml.NamespaceBinding
import javax.xml.XMLConstants
import daffodil.dsom.SchemaComponent
import java.util.HashMap
import daffodil.processors.PState
import scala.util.parsing.combinator.RegexParsers
import daffodil.dsom.LocalElementDecl
import daffodil.dsom.GlobalElementDecl
import daffodil.dsom.ElementBase
import daffodil.dsom.EntityReplacer

abstract class DFDLFunction(val name: String, val arity: Int) extends XPathFunction {
  val qName = new QName(XMLUtils.DFDL_NAMESPACE, name)
  val ID = (qName, arity)

  DFDLFunctions.functionList +:= this

  def getContext(pstate: PState): ElementBase = {
    // Assumes that a JDOM element was already created
    val currentElement = pstate.parentElement
    val res = currentElement.schemaComponent(pstate)
    res
  }

  // TODO: Do we want to use a List here? Direct access takes time to traverse.
  def evaluate(args: java.util.List[_]): Object = {
    if (args.size() < arity)
      throw new XPathExpressionException("Wrong number of arguments to " + name + ". Should be " + arity + ". Args were: " + args)
    val state = DFDLFunctions.currentPState
    val res = state match {
      // None can happen when we're testing if something is a constant.
      case None => throw new XPathExpressionException("State not bound for use by DFDL expression functions.")
      case Some(pstate) => {
        evaluate1(args, pstate)
      }
    }
    res
  }

  protected def evaluate1(args: java.util.List[_], pstate: PState): Object
}

object DFDLFunctions {
  var functionList: List[DFDLFunction] = Nil

  /**
   * This var must be bound to the current state when an expression is evaluated so that
   * the DFDL functions which access the state can work.
   */
  var currentPState: Option[PState] = None
}

/**
 * DEPRECATED
 */
object DFDLPositionFunction extends DFDLFunction("position", 0) {
  def evaluate1(hasNoArgs: java.util.List[_], pstate: PState): Object = {
    val res: java.lang.Long = pstate.arrayPos
    res
  }
}

/**
 * The new name for the dfdl:position function is dfdl:occursIndex.
 */
object DFDLOccursIndexFunction extends DFDLFunction("occursIndex", 0) {
  def evaluate1(hasNoArgs: java.util.List[_], pstate: PState): Object = {
    val res: java.lang.Long = pstate.arrayPos
    res
  }
}

/**
 * This function converts a string that contains DFDL entites.
 */
object DFDLStringFunction extends DFDLFunction("string", 1) {
  def evaluate1(args: java.util.List[_], pstate: PState): Object = {
    val xmlString = args.get(0) match {
      case s: String => s
      case other => {
        // argument wasn't a string
        // 
        // TODO: anyway to inform XPath compiler that this function needs a 
        // string as its argument type? Otherwise we have to do this runtime type check.
        // 
        getContext(pstate).schemaDefinitionError("Type error. Argument was not a string: %s", other)
      }
    }
    val dfdlString = EntityReplacer.replaceAll(xmlString)
    val remappedString = XMLUtils.remapXMLIllegalCharactersToPUA(dfdlString)
    remappedString
  }
}

object DFDLCheckConstraintsFunction extends DFDLFunction("checkConstraints", 1) {
  import daffodil.dsom.FacetTypes._
  import util.control.Breaks._

  def evaluate1(args: java.util.List[_], pstate: PState): Object = {
    // Assumes that a JDOM element was already created
    val expr = args.get(0)
    val currentElement = pstate.parentElement
    val e = getContext(pstate)
    // We have an ElementBase, retrieve the constraints
    val patterns = e.patternValues
    val data = currentElement.dataValue
    if (!currentElement.isNil && patterns.size > 0) {
      val check = checkPatterns(data, patterns)
      if (!check) {
        return java.lang.Boolean.FALSE
      }
    }
    // Note: dont check occurs counts // if(!checkMinMaxOccurs(e, pstate.arrayPos)) { return java.lang.Boolean.FALSE }
    java.lang.Boolean.TRUE
  }

  def checkPatterns(data: String, patterns: Seq[ElemFacetsR]): Boolean = {
    var isSuccess: Boolean = false

    breakable {
      for (elem <- patterns) {
        // each pattern with an elem is OR'd
        // each pattern between elem's is AND'd

        // The way we have structured things we expect each simpleType
        // to have only one (pattern, List(regex)) where the List
        // represents all of the patterns that exist locally on this simpleType
        val elemPatternList = elem(0)._2
        breakable {
          for (pattern <- elemPatternList) {
            if (data.matches(pattern.toString())) {
              isSuccess = true
              break
            } else { isSuccess = false }
          }
        }
        if (!isSuccess) { break }
      }
    }
    return isSuccess
  }

  def checkMinMaxOccurs(element: ElementBase, position: Long): Boolean = {
    // We only want to fail here if the element is in an array AND
    // the position isn't within the confines of min/max occurs
    //
    if (element.isInstanceOf[LocalElementDecl]) {
      val led = element.asInstanceOf[LocalElementDecl]
      if (!led.isScalar) { return checkOccurrance(led.minOccurs, led.maxOccurs, position) }
    } else if (element.isInstanceOf[GlobalElementDecl]) {
      val ged = element.asInstanceOf[GlobalElementDecl]
      ged.elementRef match {
        case Some(ref) => {
          if (!ref.isScalar) { return checkOccurrance(ref.minOccurs, ref.maxOccurs, position) }
        }
        case None =>
      }
    }
    return true
  }

  def checkOccurrance(minOccurs: Int, maxOccurs: Int, position: Long): Boolean = {
    //System.err.println("checkOccurrance(%s, %s, %s)".format(minOccurs, maxOccurs, position))
    // A maxOccurs of -1 signifies unbounded
    if ( // position > minOccurs && // DON"T CHECK MIN OCCURS. 
    // That can't work. If minOccurs is 5 the first element at position 1 will fail this check.
    ((position <= maxOccurs) || (maxOccurs == -1))) { return true }
    return false
  }
}

/**
 * XPath function library for non-built-in functions
 */
object Functions {
  def get(pair: (QName, Int)) = funcs.get(pair)

  val funcList = List(
    DFDLPositionFunction,
    DFDLOccursIndexFunction,
    DFDLStringFunction,
    DFDLCheckConstraintsFunction)
  val funcs = funcList.map { f => ((f.ID, f)) }.toMap

}

/**
 * Utility object for evaluating XPath expressions
 */
object XPathUtil extends Logging {

  System.setProperty("javax.xml.xpath.XPathFactory:" + NamespaceConstant.OBJECT_MODEL_JDOM, "net.sf.saxon.xpath.XPathFactoryImpl")
  private val xpathFactory = XPathFactory.newInstance(NamespaceConstant.OBJECT_MODEL_JDOM)

  /**
   * Compile an xpath. It insures functions called actually exist etc.
   * Should help with performance also.
   *
   * Returns a VariableMap=>XPathExpression, that is,
   * a CompiledExpressionFactory
   */
  def compileExpression(dfdlExpressionRaw: String,
                        namespaces: Seq[org.jdom.Namespace],
                        context: SchemaComponent) =
    // withLoggingLevel(LogLevel.Info) 
    {
      log(Debug("Compiling expression"))
      val dfdlExpression = dfdlExpressionRaw.trim
      Assert.usage(dfdlExpression != "")
      // strip leading and trailing {...} if they are there.
      val expression = if (isExpression(dfdlExpression)) getExpression(dfdlExpression) else dfdlExpression

      // Hack around bug in Saxon JAXP support by casting to Saxon-specific class.
      // -JWC, 27Jul2012.
      val xpath = xpathFactory.newXPath().asInstanceOf[XPathEvaluator]
      var variables: VariableMap = new VariableMap() // Closed over. This is modified to supply different variables
      log(Debug("Namespaces: %s", namespaces))

      val nsContext = new javax.xml.namespace.NamespaceContext {

        val pairs = namespaces.map { ns => (ns.getPrefix, ns.getURI) }
        val ht = pairs.toMap

        def getNamespaceURI(prefix: String) = {
          if (prefix == null)
            throw new IllegalArgumentException("The prefix cannot be null.");
          ht.get(prefix).getOrElse(null)
        }
        def getPrefixes(uri: String) = Assert.invariantFailed("supposed to be unused.")
        def getPrefix(uri: String): String = Assert.invariantFailed("supposed to be unused.")
        //      {
        //        getPrefixList(uri).head
        //      }
        //      private def getPrefixList(uri : String) : Seq[String] = {
        //        val submap = ht.filter{ case (pre, ns) => uri == ns}
        //        val prefixes = submap.map{ case (pre, ns) => pre }
        //        prefixes.toSeq
        //      }
      }

      xpath setNamespaceContext (nsContext)

      // Backed out until we figure out why this breaks 9 tests in TestCompiledExpression
      // -MikeB
      //
      // Finish the hack by setting the default element namespace (Saxon's API) 
      // to the default namespace returned by the NamespaceContext (JAXP API).
      // -JWC, 27Jul2012.
      //xpath.getStaticContext().setDefaultElementNamespace(nsContext.getNamespaceURI(XMLConstants.DEFAULT_NS_PREFIX))

      xpath.setXPathVariableResolver(
        new XPathVariableResolver() {
          def resolveVariable(qName: QName): Object = {
            val varName = XMLUtils.expandedQName(qName)
            val (res, newVMap) = variables.readVariable(varName, context)
            variables = newVMap
            res
          }
        })

      xpath.setXPathFunctionResolver(
        new XPathFunctionResolver() {
          def resolveFunction(functionName: QName, arity: Int): XPathFunction = {
            val maybeF = Functions.get((functionName, arity))
            maybeF match {
              case None => throw new XPathExpressionException("no such function: " + functionName + " with arity " + arity)
              case Some(f) =>
                f
            }
          }
        })

      val xpathExpr = try {
        xpath.compile(expression)
      } catch {
        case e: XPathExpressionException => {
          val exc = e // debugger never seems to show the case variable itself.
          val realExc = e.getCause()
          // Assert.invariant(realExc != null) // it's always an encapsulation of an underlying error.

          // compilation threw an error. That's a compilation time error.
          // we just rethrow here. This is here to be a good place for a breakpoint/debug feature.
          log(Debug("compilation error in xpath expression. error %s, expression %s", exc, expression))
          throw exc
        }
      }

      // We need to supply the variables late
      val withoutVariables = new CompiledExpressionFactory(expression) {
        def getXPathExpr(runtimeVars: VariableMap) = {
          variables = runtimeVars
          xpathExpr
        }
        // we need to get the variables back at the end of exprsesion evaluation.
        def getVariables() = variables
      }

      withoutVariables // return this factory
    }

  abstract class CompiledExpressionFactory(val expression: String) {
    def getXPathExpr(runtimeVars: VariableMap): XPathExpression
    def getVariables(): VariableMap
  }

  /**
   * For unit testing only.
   * Evaluates an XPath 2 expression in one shot, from string to value.
   *
   * @param a valid XPath expression - the expression to evaluate (no surrounding braces)
   * @param variables - the variables in scope
   * @param contextNode - the context node for this expression
   * @param namespaces  - the namespaces in scope
   */
  private[xpath] def evalExpressionFromString(expression: String, variables: VariableMap,
                                              contextNode: Parent, namespaces: Seq[org.jdom.Namespace], targetType: QName = NODE): XPathResult = {

    val compiledExprExceptVariables = compileExpression(expression, namespaces, null) // null as schema component
    val res = evalExpression(expression, compiledExprExceptVariables, variables, contextNode, targetType)
    res
  }

  /**
   * Evaluates an XPath expression that has been compiled.
   *
   * Issues: expressions like { 3 } are not nodes. Asking for a node throws.
   * Asking for a string works, or returns "" on illegal things.
   * If you ask for a number, it will convert to a number or return a NaN on any failure.
   */
  def evalExpression(
    expressionForErrorMsg: String,
    compiledExprFactory: CompiledExpressionFactory,
    variables: VariableMap,
    contextNode: Parent,
    targetType: QName): XPathResult = {
    // withLoggingLevel(LogLevel.Info) 
    {
      val ce = compiledExprFactory.getXPathExpr(variables)
      log(Debug("Evaluating %s in context %s to get a %s", expressionForErrorMsg, contextNode, targetType)) // Careful. contextNode could be null.
      val o = ce.evaluate(contextNode, targetType)
      log(Debug("Evaluated to: %s", o))
      val res = (o, targetType) match {
        case (x: Element, NODE) => new NodeResult(x)
        case (x: Element, STRING) => new StringResult(x.getContent(0).toString())
        case (x: Element, NUMBER) => new NumberResult(x.getContent(0).toString().toDouble)
        case (x: Text, STRING) => new StringResult(x.getValue())
        case (x: Text, NUMBER) => new NumberResult(x.getValue().toDouble)
        case (x: java.lang.Double, NUMBER) if (x.isNaN && contextNode != null) => {
          // We got a NaN. If the path actually exists, then the result is a NaN
          // If the path doesn't exist, then we want to fail.
          val existingNode = ce.evaluate(contextNode, NODE)
          if (existingNode != null) new NumberResult(x.doubleValue)
          else throw new XPathExpressionException("no node for path " + expressionForErrorMsg)
        }
        case (x: java.lang.Double, NUMBER) => new NumberResult(x.doubleValue)
        case ("", STRING) if (contextNode != null) => {
          // We got empty string. If the path actually exists, then the result is empty string.
          // If the path doesn't exist, then we want to fail.
          val existingNode = ce.evaluate(contextNode, NODE)
          if (existingNode != null) new StringResult("")
          else throw new XPathExpressionException("no node for path " + expressionForErrorMsg)
        }
        case (x: String, STRING) => new StringResult(x)
        case (x: java.lang.Boolean, BOOLEAN) => new BooleanResult(x)
        case (null, _) => {
          // There was no such node. We're never going to get an answer for this XPath
          // so fail.
          throw new XPathExpressionException("no node for path " + expressionForErrorMsg)
        }
        case _ => {
          throw new XPathExpressionException("unrecognized evaluation result: " + o + " for target type " + targetType)
        }
      }
      return res
    }
    // Note: removed "retry looking for a string" code. That was not the right approach to using the
    // XPath API. You don't try for a NODE, and if that fails try for a String. You try for a NODE, and 
    // convert the result to a string if you get a node. (Text IS a node).
  }

  /**
   * Whether a string is a DFDL expression (an XPath expression surrounded by brackets).
   *
   * This function does not verify a string conforms to the DFDL subset of XPath
   */
  def isExpression(expression: String): Boolean =
    expression.startsWith("{") && expression.endsWith("}") &&
      (expression(1) != '{')

  /**
   * Returns the XPath expression contained in a DFDL expression (an XPath expression surrounded by brackets).
   *
   * @param expression a valid DFDL expression
   */
  def getExpression(expression: String): String = {
    val v = expression.trim
    v.substring(1, v.length - 1)
  }

}
