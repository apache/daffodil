package daffodil.processors.input.text

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

import java.nio.charset.Charset
import daffodil.processors.{Success, Last, ProcessorResult, VariableMap}
import org.jdom.Element
import daffodil.schema.annotation.{ExpressionValue, ListLiteralValue, EmptyValue, AttributeValue}
import daffodil.xml.{XMLUtil, Namespaces}
import daffodil.exceptions.{ElementNotValidException, DFDLSchemaDefinitionException}
import daffodil.processors.xpath.XPathUtil
import daffodil.parser.RollbackStream
import daffodil.parser.regex.Regex


/**
 * Scans for boolean elements in text representation
 *
 * @version 1
 * @author Alejandro Rodriguez
 */
class BooleanTextProcessor(charset:Charset,
                            acceptEOF:Boolean) extends TextProcessor(charset,acceptEOF){

  private var trueRepresentation:AttributeValue = EmptyValue
  private var falseRepresentation:AttributeValue = EmptyValue

  /**
   * Sets textBooleanTrueRep
   */
  def setTrueRepresentation(trueRepresentation:AttributeValue) =
    this.trueRepresentation = trueRepresentation

  /**
   * Sets textBooleanFalseRep
   */
  def setFalseRepresentation(falseRepresentation:AttributeValue) =
    this.falseRepresentation = falseRepresentation
    
  override def apply(input:RollbackStream,element:Element,
                     variables:VariableMap,namespaces:Namespaces,
                     terminators:List[Regex]):ProcessorResult = {
    val v = super.apply(input,element,variables,namespaces,terminators)
    v match {
      case Success | Last => makeIntoBoolean(element,variables,namespaces)
      case _ =>
    }
    v
  }

  private def makeIntoBoolean(element:Element,variables:VariableMap,namespaces:Namespaces):Unit = {
    val text = element getText()
    trueRepresentation match {
      case EmptyValue => throw new DFDLSchemaDefinitionException("Missing textBooleanTrueRep property",
        documentContext = element)
      case ListLiteralValue(l) => if (l.map{ _ compile(ignoreCase) }. exists { _ matches(text) })
        {element.setText("true"); return}
      case e:ExpressionValue => if (XMLUtil.getListFromExpression(e,variables,element,namespaces).
          map{ _ compile(ignoreCase) }.exists { _ matches(text) })
          {element.setText("true"); return}
    }

    falseRepresentation match {
      case EmptyValue => throw new DFDLSchemaDefinitionException("Missing textBooleanFalseRep property",
        documentContext = element)
      case ListLiteralValue(l) => if (l.map{ _ compile(ignoreCase) }. exists { _ matches(text)})
        {element.setText("false"); return}
      case e:ExpressionValue => if (XMLUtil.getListFromExpression(e,variables,element,namespaces).
              map{ _ compile(ignoreCase) }.exists { _ matches(text) })
          {element.setText("false"); return}
    }

    throw new ElementNotValidException("Cannot parse into xsd:boolean '"+text+"'",
      documentContext = element)
  }
}