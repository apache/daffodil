package edu.illinois.ncsa.daffodil.processors.xpath

/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

//import javax.xml.xpath._
//import javax.xml.xpath.XPathConstants._
//import javax.xml.namespace.{ QName => JQName }
//import org.jdom2.Element
//import org.jdom2.Text
//import org.jdom2.Parent
//import edu.illinois.ncsa.daffodil.exceptions._
//import edu.illinois.ncsa.daffodil.processors.VariableMap
//import edu.illinois.ncsa.daffodil.xml.XMLUtils
//import edu.illinois.ncsa.daffodil.util.Logging
//import edu.illinois.ncsa.daffodil.util._
//import edu.illinois.ncsa.daffodil.util.LogLevel
//import edu.illinois.ncsa.daffodil.processors.RuntimeData
//import scala.xml.NamespaceBinding
//import javax.xml.XMLConstants
//import edu.illinois.ncsa.daffodil.dsom.SchemaComponent
//import java.util.HashMap
//import edu.illinois.ncsa.daffodil.processors.PState
//import scala.util.parsing.combinator.RegexParsers
//import edu.illinois.ncsa.daffodil.dsom._
//import edu.illinois.ncsa.daffodil.Implicits._
//import com.ibm.icu.text.DateFormat
//import com.ibm.icu.text.SimpleDateFormat
//import com.ibm.icu.util.TimeZone
//import com.ibm.icu.util.GregorianCalendar
//import scala.xml.NodeSeq
//import edu.illinois.ncsa.daffodil.processors.InfosetElement
//import scala.math.BigDecimal
//import org.w3c.dom.NodeList
//import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
//import edu.illinois.ncsa.daffodil.dpath._

//object SaxonFunctionState {
//  /**
//   * This thread local must be bound to the current state when an expression is evaluated so that
//   * the DFDL functions which access the state can work.
//   */
//  val currentPState = new ThreadLocal[Option[PState]] {
//    override def initialValue() = None
//  }
//}
//
//abstract class SaxonFunction
//  extends DPathFunction {
//
//  // for use in an error message it is ok to use the prefix (if there is one)
//  // since the error message will also have the lexical context information
//  // that makes the prefix meaningful.
//  private val prefix =
//    if (qName.getPrefix() != XMLConstants.DEFAULT_NS_PREFIX) qName.getPrefix() + ":"
//    else "{" + qName.getNamespaceURI() + "}"
//  private val qualName = prefix + qName.getLocalPart()
//
//  /**
//   * Called by expression evaluation engine to evaluate a function.
//   */
//  def evaluate(args: java.util.List[_]): Object = {
//    if (args.size() < arity)
//      throw new XPathExpressionException("Wrong number of arguments to " + qualName + ". Should be " + arity + ". Args were: " + args)
//
//    // 
//    // here we retrieve the state from the thread-local where it was bound
//    // outside before the whole expression was evaluated. 
//    // 
//    val state = SaxonFunctionState.currentPState.get
//    val res = state match {
//      // None can happen when we're testing if something is a constant.
//      case None => throw new XPathExpressionException("State not bound for use by DFDL expression functions.")
//      case Some(pstate) => {
//        evaluate1(args, pstate)
//      }
//    }
//    res
//  }
//}
//
///**
// * Saxon-specific utility object for compiling evaluating XPath expressions
// */
//object XPathUtil extends Logging {
//
//  private val xpathFactory = new XPathFactoryImpl
//  val config = xpathFactory.getConfiguration
//  config.registerExternalObjectModel(new JDOM2ObjectModel)
//
//  /**
//   * Compile an xpath. It insures functions called actually exist etc.
//   * Should help with performance also.
//   *
//   * Returns a VariableMap=>XPathExpression, that is,
//   * a CompiledExpressionFactory
//   */
//  def compileExpression(dfdlExpressionRaw: String,
//    namespaces: NamespaceBinding,
//    context: ThrowsSDE): CompiledExpressionFactory =
//    // withLoggingLevel(LogLevel.Info) 
//    {
//      log(LogLevel.Debug, "Compiling expression")
//      val dfdlExpression = dfdlExpressionRaw.trim
//      Assert.usage(dfdlExpression != "")
//      // strip leading and trailing {...} if they are there.
//      val expression = if (DPathUtil.isExpression(dfdlExpression)) DPathUtil.getExpression(dfdlExpression) else dfdlExpression
//
//      // Hack around bug in Saxon JAXP support by casting to Saxon-specific class.
//      // -JWC, 27Jul2012.
//      // Is it really a bug, or just lack of a standard API?
//      // -MikeB 03May2013
//      val xpath = xpathFactory.newXPath().asInstanceOf[XPathEvaluator]
//      var variables: VariableMap = new VariableMap() // Closed over. This is modified to supply different variables
//      log(LogLevel.Debug, "Namespaces: %s", namespaces)
//
//      val nsContext = new javax.xml.namespace.NamespaceContext {
//
//        def getNamespaceURI(prefix: String) = {
//          if (prefix == null)
//            throw new IllegalArgumentException("The prefix cannot be null.");
//          val lookup = Option(namespaces.getURI(prefix))
//          lookup match {
//            case None => null
//            case Some(ns) => ns
//          }
//        }
//        def getPrefixes(uri: String) = Assert.invariantFailed("supposed to be unused.")
//        def getPrefix(uri: String): String = Assert.invariantFailed("supposed to be unused.")
//        //      {
//        //        getPrefixList(uri).head
//        //      }
//        //      private def getPrefixList(uri : String) : Seq[String] = {
//        //        val submap = ht.filter{ case (pre, ns) => uri == ns}
//        //        val prefixes = submap.map{ case (pre, ns) => pre }
//        //        prefixes.toSeq
//        //      }
//      }
//
//      xpath setNamespaceContext (nsContext)
//
//      //
//      // Finish the hack by setting the default element namespace (Saxon's API) 
//      // to the default namespace returned by the NamespaceContext (JAXP API).
//      // -JWC, 27Jul2012.
//      val nsForNoPrefix = nsContext.getNamespaceURI(XMLConstants.DEFAULT_NS_PREFIX)
//      val defaultElementNS =
//        if (nsForNoPrefix != null) nsForNoPrefix
//        else XMLConstants.NULL_NS_URI // Null NS aka No Namespace.
//      xpath.getStaticContext().setDefaultElementNamespace(defaultElementNS)
//
//      xpath.setXPathVariableResolver(
//        new XPathVariableResolver() {
//          def resolveVariable(qName: JQName): Object = {
//            // FIXME: PERFORMANCE: readVariable should use the QName object, not require this string to be created every time
//            // we read a variable.
//            val varName = XMLUtils.expandedQName(qName)
//            val (res, newVMap) = variables.readVariable(varName, context)
//            variables = newVMap
//            res
//          }
//        })
//
//      xpath.setXPathFunctionResolver(
//        new XPathFunctionResolver() {
//          def resolveFunction(functionName: JQName, arity: Int): XPathFunction = {
//            val maybeF = DFDLFunctions.get(functionName, arity)
//            maybeF match {
//              case None => throw new XPathExpressionException("no such function: " + functionName + " with arity " + arity)
//              case Some(f) =>
//                f
//            }
//          }
//        })
//
//      val xpathExpr = try {
//        val cexpr = xpath.compile(expression)
//        val saxonExpr = cexpr.asInstanceOf[XPathExpressionImpl]
//        /* This checks to see if there is a problem with the expression that
//         * will cause it to always fail (for example, 2 + 'three' isn't
//         * valid). When saxon finds an expression like this, instead of
//         * throwing an exception, it instead creates an ErrorExpression,
//         * which always fails when executed. Instead of letting if fail at
//         * parse time, we just throw the XPathException that caused the
//         * problem, which is eventually thrown as an SDE
//         */
//        saxonExpr.getInternalExpression match {
//          case ee: ErrorExpression => throw ee.getException
//          case _ =>
//        }
//        cexpr
//      } catch {
//        case e: XPathExpressionException => {
//          val exc = e // debugger never seems to show the case variable itself.
//          val realExc = e.getCause()
//          val forMsg = if (realExc != null) realExc else exc
//          // compilation threw an error. That's a compilation time error, aka a schema definition error
//          context.SDE("Expression compiler reports: %s", forMsg)
//        }
//      }
//
//      // We need to supply the variables late
//      val withoutVariables = new CompiledExpressionFactory(expression) {
//        def getXPathExpr(runtimeVars: VariableMap) = {
//          variables = runtimeVars
//          xpathExpr
//        }
//        // we need to get the variables back at the end of exprsesion evaluation.
//        def getVariables() = variables
//      }
//
//      withoutVariables // return this factory
//    }
//
//  abstract class CompiledExpressionFactory(val expression: String) {
//    def getXPathExpr(runtimeVars: VariableMap): XPathExpression
//    def getVariables(): VariableMap
//  }
//
//  /**
//   * For unit testing only.
//   * Evaluates an XPath 2 expression in one shot, from string to value.
//   *
//   * @param a valid XPath expression - the expression to evaluate (no surrounding braces)
//   * @param variables - the variables in scope
//   * @param contextNode - the context node for this expression
//   * @param namespaces  - the namespaces in scope
//   */
//  private[xpath] def evalExpressionFromString(expression: String, variables: VariableMap,
//    contextNode: Parent, namespaces: NamespaceBinding, targetType: JQName = NODE): XPathResult = {
//
//    val compiledExprExceptVariables = compileExpression(expression, namespaces, null) // null as schema component
//    val res = evalExpression(expression, compiledExprExceptVariables, variables, contextNode, targetType)
//    res
//  }
//
//  /**
//   * Evaluates an XPath expression that has been compiled.
//   *
//   * Issues: expressions like { 3 } are not nodes. Asking for a node throws.
//   * Asking for a string works, or returns "" on illegal things.
//   * If you ask for a number, it will convert to a number or return a NaN on any failure.
//   */
//  def evalExpression(
//    expressionForErrorMsg: String,
//    compiledExprFactory: CompiledExpressionFactory,
//    variables: VariableMap,
//    contextNode: Parent,
//    targetType: JQName): XPathResult = {
//    // withLoggingLevel(LogLevel.Info) 
//    {
//      val ce = compiledExprFactory.getXPathExpr(variables)
//      log(LogLevel.Debug, "Evaluating %s in context %s to get a %s", expressionForErrorMsg, contextNode, targetType) // Careful. contextNode could be null.
//      val (isNumeric, newTargetType) = targetType match {
//        // Represent numeric types as String to prevent loss of precision
//        case NUMBER => (true, STRING)
//        case _ => (false, targetType)
//      }
//      val o = ce.evaluate(contextNode, newTargetType)
//      log(LogLevel.Debug, "Evaluated to: %s", o)
//      val res =
//        try {
//          (o, newTargetType) match {
//            case (_, NUMBER) => {
//              val numStr = o match {
//                case x: Element => x.getContent(0).toString()
//                case x: Text => x.getValue().toString()
//                case x: java.lang.Double if (x.isNaN() && contextNode != null) => {
//                  // We got a NaN. If the path actually exists, then the result is a NaN
//                  // If the path doesn't exist, then we want to fail.
//                  val existingNode = ce.evaluate(contextNode, NODE)
//                  if (existingNode != null) x.toString
//                  else throw new XPathExpressionException("no node for path " + expressionForErrorMsg)
//                }
//                case x: java.lang.Double => x.toString()
//              }
//              // Because XPath converts Number to Double, our output strings
//              // of the evaluated value (numStr) will always have decimals in them.
//              // How can we get around this?
//              BigDecimal(numStr) // Will throw exception if not a valid number
//              new NumberResult(numStr)
//            }
//            case (_, STRING) if isNumeric => {
//              val numStr = o match {
//                case x: Element => x.getContent(0).toString()
//                case x: Text => x.getValue()
//                case "true" => "1" // Because could evaluate checkConstraints which returns true/false
//                case "false" => "0" // Because could evaluate checkConstraints which returns true/false
//                case "" if (contextNode != null) => {
//                  // We got empty string. If the path actually exists, then the result is empty string.
//                  // If the path doesn't exist, then we want to fail.
//                  val existingNode = ce.evaluate(contextNode, NODE)
//                  if (existingNode != null) throw new XPathExpressionException("unrecognized evaluation result: " + o + " (empty string) for target type " + targetType)
//                  else throw new XPathExpressionException("no node for path " + expressionForErrorMsg)
//                }
//                case x: String => x
//                case null => throw new XPathExpressionException("no node for path " + expressionForErrorMsg)
//                case _ => throw new XPathExpressionException("unrecognized evaluation result: " + o + " for target type " + targetType)
//              }
//              BigDecimal(numStr)
//              new NumberResult(numStr)
//            }
//            case (x: NodeList, NODESET) => new NodeSetResult(x)
//            case (x: Element, NODE) => new NodeResult(x)
//            case (x: Element, STRING) => new StringResult(x.getContent(0).toString())
//            case (x: Text, STRING) => new StringResult(x.getValue())
//            case ("", STRING) if (contextNode != null) => {
//              // We got empty string. If the path actually exists, then the result is empty string.
//              // If the path doesn't exist, then we want to fail.
//              val existingNode = ce.evaluate(contextNode, NODE)
//              if (existingNode != null) new StringResult("")
//              else throw new XPathExpressionException("no node for path " + expressionForErrorMsg)
//            }
//            case (x: String, STRING) => new StringResult(x)
//            case (x: java.lang.Boolean, BOOLEAN) => new BooleanResult(x)
//            case (null, _) => {
//              // There was no such node. We're never going to get an answer for this XPath
//              // so fail.
//              throw new XPathExpressionException("no node for path " + expressionForErrorMsg)
//            }
//            case _ => {
//              throw new XPathExpressionException("unrecognized evaluation result: " + o + " for target type " + targetType)
//            }
//          }
//        } catch {
//          case ex: NumberFormatException => new NotANumberResult(o)
//        }
//      return res
//    }
//    // Note: removed "retry looking for a string" code. That was not the right approach to using the
//    // XPath API. You don't try for a NODE, and if that fails try for a String. You try for a NODE, and 
//    // convert the result to a string if you get a node. (Text IS a node).
//  }
//
//}
