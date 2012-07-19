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
import javax.xml.xpath.XPathConstants.NODE
import javax.xml.xpath.XPathConstants.STRING
import javax.xml.namespace.QName
import org.jdom.Element
import org.jdom.Text
import org.jdom.Parent
import net.sf.saxon.om.NamespaceConstant
import net.sf.saxon.jdom.NodeWrapper
import net.sf.saxon.jdom.DocumentWrapper
import net.sf.saxon.Configuration
import net.sf.saxon.om.NodeInfo
import daffodil.xml.Namespaces
import daffodil.exceptions._
import daffodil.processors.VariableMap
import daffodil.xml.XMLUtils
import daffodil.util.Logging
import daffodil.util.Debug
import daffodil.util.LogLevel

/**
 * Utility object for evaluating XPath expressions
 *
 * @version 1
 * @author Alejandro Rodriguez
 */
object XPathUtil extends Logging {

  System.setProperty("javax.xml.xpath.XPathFactory:"+NamespaceConstant.OBJECT_MODEL_JDOM,"net.sf.saxon.xpath.XPathFactoryImpl")
  private val xpathFactory = XPathFactory.newInstance(NamespaceConstant.OBJECT_MODEL_JDOM)

  /**
   * Compile an xpath. It insures functions called actually exist etc.
   * Should help with performance also.
   *
   * Returns a VariableMap=>XPathExpression, which you can think of as
   * a CompiledXPathExpressionFactory, though we didn't create that type name
   */
  def compileExpression(dfdlExpressionRaw: String, namespaces: Namespaces) = {
    val dfdlExpression = dfdlExpressionRaw.trim
    Assert.usage(dfdlExpression != "")
    // strip leading and trailing {...} if they are there.
    val expression = if (isExpression(dfdlExpression)) getExpression(dfdlExpression) else dfdlExpression
        
    val xpath = xpathFactory.newXPath()
    var variables: VariableMap = new VariableMap() // dummy for now.
    xpath setNamespaceContext (namespaces)
    xpath.setXPathVariableResolver(
      new XPathVariableResolver() {
        def resolveVariable(qName: QName): Object =
          variables.readVariable(qName.getNamespaceURI + qName.getLocalPart)
      })

    val xpathExpr = xpath.compile(expression)

    // We need to supply the variables later
    val withoutVariables = new CompiledExpressionFactory(expression) {
       def getXPathExpr(runtimeVars: VariableMap) = {
        variables = runtimeVars
        xpathExpr
      }  
    }

    withoutVariables  // return this factory
  }
  
  abstract class CompiledExpressionFactory(val expression : String){
      def getXPathExpr(runtimeVars: VariableMap) : XPathExpression
  }
                     
  /** Evaluates an XPath 2 expression in one shot, from string to value.
   *
   * @param a valid XPath expression the expression to evaluate (no surrounding brackets)
   * @param variables the variables in scope
   * @param contextNode the context node for this expression
   * @param namespaces the namespaces in scope
   */
  def evalExpression(expression:String,variables:VariableMap,
                     contextNode:Parent,namespaces:Namespaces) : XPathResult = {

    val compiledExprExceptVariables = compileExpression(expression, namespaces)
    val res = evalExpression(expression, compiledExprExceptVariables, variables, contextNode)
    res
  }
    
  def evalExpression(
      expressionForErrorMsg : String, 
      compiledExprFactory : CompiledExpressionFactory, 
      variables:VariableMap, 
      contextNode:Parent) : XPathResult = {
    withLoggingLevel(LogLevel.Debug)
    {
    val ce = compiledExprFactory.getXPathExpr(variables)
    log(Debug("Evaluating %s in context %s", expressionForErrorMsg, contextNode)) // Careful. contextNode could be null.
    log(Debug("Expression eval trying NODE"))
    try{
      val o = ce.evaluate(contextNode, NODE)
      log(Debug("Evaluated to: %s", o))
      val res = o match {
        case x : Element => new NodeResult(x)
        case x : Text => new StringResult(x.getValue())
        case _ => {
           throw new XPathExpressionException("unrecognized NODE (not Element nor Text): " + o) 
        }
      }
      return res
    }catch{
      case e:XPathExpressionException =>
        log(Debug("Didn't work to get NODE due to %s", e))
        //
        // This second try to see if we can evaluate with a STRING
        // as goal should be eliminated by static analysis of
        // the XPath. We should know the type it is intending to return,
        // So we shouln't have to try NODE, then STRING
        // 
        try {
          log(Debug("Expression eval trying STRING."))
          val o = ce.evaluate(contextNode, STRING)
          log(Debug("Evaluated to: '%s'", o))
          new StringResult(o.asInstanceOf[String])
        }catch {
          case e:XPathExpressionException => {
            // doUnknownXPathEvalException(expressionForErrorMsg, e)
            log(Debug("Second try didn't work to get STRING due to %s", e))
            throw e
          }
        }
      case e:Exception => {
        doUnknownXPathEvalException(expressionForErrorMsg, e)
      }
    }
    }
  }

  def doUnknownXPathEvalException(expression : String, exc : Exception) = {
     throw new XPathExpressionException(exc)
  }
  
  /**
   * Whether a string is a DFDL expression (an XPath expression surrounded by brackets).
   *
   * This function does not verify a string conforms to the DFDL subset of XPath
   */
  def isExpression(expression:String) : Boolean =
    expression.startsWith("{") && expression.endsWith("}") &&
      (expression(1) != '{')

  /**
   * Returns the XPath expression contained in a DFDL expression (an XPath expression surrounded by brackets).
   *
   * @param expression a valid DFDL expression
   */
  def getExpression(expression:String) : String = {
    val v = expression.trim
    v.substring(1,v.length-1)
  }


}
