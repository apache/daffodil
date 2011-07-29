package daffodil.schema

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

import annotation.enumerations._
import annotation.{AnnotationDefaults, Annotation}
import org.jdom.Parent
import daffodil.processors.{ProcessorFactory, VariableMap}
import daffodil.processors.xpath.{NodeResult, StringResult, XPathUtil}
import daffodil.xml.{XMLUtil, Namespaces}
import daffodil.exceptions._
import daffodil.processors.input.BasicProcessor
import daffodil.parser.{LinkedList, RollbackStream}
import daffodil.parser.regex.Regex

@SerialVersionUID(1)
class Choice(ann:Annotation,target:String,namespaces:Namespaces,c:List[BasicNode])
        extends BasicNodeImpl(target,namespaces,ann) with ComplexType {

  addAll(c)

  private var initiatedContent = annotation.format.initiatedContent match {
    case None => AnnotationDefaults defaultInitiatedContent
    case Some(x) => x
  }


  override def getName(parent:Parent) =
      parent match{
			  case e:org.jdom.Element => e.getName
			  case _ => throw new IllegalStateException("Parent of choice is not an element")
		}

  override protected def findChildren(input:RollbackStream,variables:VariableMap,
                             parent:Parent,maxLength:Int,terminators:List[Regex],
                             processor:BasicProcessor) : ChildResult = {

    var results:LinkedList[org.jdom.Element] = null

    for(child <- children)
      try{
        results = child (input,annotation,variables,parent,maxLength,terminators)
        if (ProcessorFactory isHidden(annotation))
          for(e <- results)
            variables hideElement(e)
        return new ChildSuccess(results)
      } catch {
        case e:InitiatorMissingException => //ignore
        case e:ElementProcessingException =>
          if (initiatedContent)
            throw new ElementNotFoundException("No valid choice",e,annotation element,parent,Some(input getPosition))
          //else ignore
      }
    throw new ElementNotFoundException("No valid choice",
      schemaContext = annotation element,documentContext = parent,position = Some(input getPosition))
  }

  override def canEqual(o:Any):Boolean = o.isInstanceOf[Choice]

  override def equals(o:Any) = o match {
    case that:Choice => {
      that.canEqual(this) && super.equals(that)
    }
  case	 _ => false
	}
}
