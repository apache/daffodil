package daffodil.processors.internal

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

import org.jdom.Parent
import org.jdom.Element

import daffodil.exceptions.ExpressionDefinitionException
import daffodil.exceptions.DFDLAssertionException
import daffodil.processors.xpath.XPathUtil
import daffodil.processors.xpath.StringResult
import daffodil.processors.xpath.NodeResult
import daffodil.xml.Namespaces
import daffodil.processors.VariableMap

/**
 * Verifies than an XPath expression evaluated in a context node of the output DOM tree evaluates to true, otherwise
 * throws a DFDLAssertionException
 *
 * @param expression the expression to evaluate, including enclosing brackets
 * @param message the message to output if the expression fails
 *
 * @version 1
 * @author Alejandro Rodriguez
 */
class Assert(expression:String,message:String) extends InternalProcessor {

  /** The XPath expression, trimmed */
  val trimmedExpression = if (!XPathUtil.isExpression(expression))
	  throw new ExpressionDefinitionException("not an expression "+expression)
  	else
     XPathUtil getExpression(expression)

  /**
   * Evaluates the expression
   * @param parent the context node in the DOM tree
   * @param variables variables in scope
   * @param target target namespace
   * @param namespaces namespaces in scope
   *
   * @returns the same variables it received
   */
  override def apply(parent:Parent,variables:VariableMap,
                     target:String,namespaces:Namespaces):VariableMap = 
    if (trimmedExpression.length == 0)
      variables
    else{
      
      try{
	      XPathUtil.evalExpression(trimmedExpression,variables,parent.asInstanceOf[Element],namespaces) match {
	        case StringResult(s) => if (s.toBoolean) 
	        	variables 
	        		else 
	        	throw new DFDLAssertionException(getMessage(parent,variables,namespaces),documentContext = parent)
	        case NodeResult(n) => variables
	      }
      } catch {
        case _ => throw new DFDLAssertionException(message,documentContext = parent)
      }
    }
  
  private def getMessage(parent:Parent,variables:VariableMap,namespaces:Namespaces): String = {
    if (XPathUtil.isExpression(message))
      XPathUtil evalExpression(message trim,variables,parent.asInstanceOf[Element],namespaces) match {
        case StringResult(s) => s
        case NodeResult(n) => n toString
      }
    else
        message
  }
    
}
