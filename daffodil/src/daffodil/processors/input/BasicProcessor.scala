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

import java.io.Serializable

import org.jdom.Element
import org.jdom.Parent

import daffodil.xml.Namespaces
import daffodil.xml.XMLUtil
import daffodil.parser.RollbackStream
import daffodil.parser.regex.Regex
import daffodil.processors.{ScanResult, ProcessorResult, VariableMap}


/**
 * A function that reads from the current position in a stream and sets the value
 * of an Element with a text value parsed from the stream.
 *
 * Returns a ProcessorResult, containing the element and the type of result
 *
 * Post conditions:
 *   the stream is possibly advanced by the amount of bytes read
 *   upon succes.
 *   (stream is NOT rolled back)
 *
 * @author Alejandro Rodriguez
 * @version 1
 *
 */
trait BasicProcessor extends Function5[RollbackStream,Element,VariableMap,
				       Namespaces,List[Regex],ProcessorResult] with Serializable {

  protected var endOfParent:Boolean = _

  def setEndOfParent(b:Boolean) = endOfParent = b
  					 
  def init(input:RollbackStream,element:Element,variables:VariableMap,
	   namespaces:Namespaces)
					 
  def terminate(input:RollbackStream,element:Element,variables:VariableMap,
		namespaces:Namespaces,parentTerminators:List[Regex])
					 
  def findPrefixSeparator(input:RollbackStream,parent:Parent,
			  variables:VariableMap,namespaces:Namespaces,parentTerminators:List[Regex]):ScanResult
  
  def findPostfixSeparator(input:RollbackStream,parent:Parent,
			   variables:VariableMap,namespaces:Namespaces,parentTerminators:List[Regex]):ScanResult

  /**
   * @param input the input stream
   * @param node the element which value should be set
   * @param variables variables in scope
   * @param namespaces namespaces in scope
   * @param parentTerminators all the terminators of parents that should be considered
   */
  def apply(input:RollbackStream,node:Element,variables:VariableMap,
            namespaces:Namespaces,parentTerminators:List[Regex]):ProcessorResult

  /** Sets the type attribute of an element */
  protected def setType(typeName:String,element:Element,namespaces:Namespaces) = {
    val prefix = namespaces getNamespaceByURI(XMLUtil XSD_NAMESPACE) match {
      case Some(n) => n getPrefix
      case None => { var i = 0
		    while(namespaces.getNamespaceByPrefix("xsd"+i)==None) i+=1
		    element addNamespaceDeclaration(namespaces.
						    addNamespace(
						      XMLUtil XSD_NAMESPACE,"xsd"+i))
		    "xsd"+i
		  }
    }	      
    element.setAttribute("type",prefix+":"+typeName)
  } 
}
