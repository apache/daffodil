package daffodil.processors.input

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

import daffodil.exceptions.ElementProcessingException
import daffodil.exceptions.ExpressionDefinitionException
import daffodil.processors.xpath.XPathUtil
import daffodil.processors.xpath.NodeResult
import daffodil.processors.xpath.StringResult
import daffodil.xml.Namespaces
import daffodil.xml.XMLUtil.addNewChild
import daffodil.xml.XMLUtil.XSD_NAMESPACE
import org.jdom.{Element, Parent, Text}
import daffodil.parser.RollbackStream
import daffodil.parser.regex.Regex
import daffodil.processors._

class InputExpressionProcessor(expression:String) extends BasicProcessor {

  var typeName:String = _ // should be Option[String]
  
  val trimmedExpression = if (!XPathUtil.isExpression(expression))
	  throw new ExpressionDefinitionException("not an expression "+expression)
  	else
     XPathUtil getExpression(expression)
  
  def this(expression:String,typeName:Option[String]) = {
    this(expression)
    typeName match {
      case Some(s) => this.typeName = s
      case None => this.typeName = null
    }
  }
  
  override def apply(input:RollbackStream,element:Element,
                     variables:VariableMap,
                     namespaces:Namespaces,terminators:List[Regex]):ProcessorResult = {
    if (trimmedExpression.length != 0) {
      val evaluated = XPathUtil.evalExpression(trimmedExpression,variables,element,namespaces) 
      evaluated match {
        case StringResult(s) => element.addContent(new Text(s)) // Note: changed to addContent from setText. Shouldn't matter because the locations should be empty.
        case NodeResult(n) => if ( n != null) element.addContent(n getText) else 
          throw new ElementProcessingException("null result from xpath expression",
            documentContext = element,position = Some(input getPosition))
      }
    }
      if (typeName != null)
        setType(typeName.substring(XSD_NAMESPACE.length),element,namespaces)
    
      Success
    }
  
  
  override def init(input:RollbackStream,element:Element,
                    variables:VariableMap,namespaces:Namespaces) {}
  
  override def terminate(input:RollbackStream,element:Element,variables:VariableMap,
                         namespaces:Namespaces,terminators:List[Regex]) {}
  
  override def findPrefixSeparator(input:RollbackStream,parent:Parent,variables:VariableMap,
                    	namespaces:Namespaces,parentTerminators:List[Regex]):ScanResult = NothingFound
  
  override def findPostfixSeparator(input:RollbackStream,parent:Parent,variables:VariableMap,
                    	namespaces:Namespaces,parentTerminators:List[Regex]):ScanResult = NothingFound
  
  
  def canEqual(o:Any):Boolean = o.isInstanceOf[InputExpressionProcessor]
   
  override def equals(o:Any) =  
    o match {
      case that:InputExpressionProcessor => 
	that.canEqual(this) && this.trimmedExpression == that.trimmedExpression
      case _ => false
    }
}
