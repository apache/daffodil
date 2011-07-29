package daffodil.schema.annotation

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

/**
 * A DFDL annotation of a schema element.
 *
 * This object represents all the combined annotations over the element.
 *
 * @param element the schema node this object annotates
 *
 * @version 1
 * @author Alejandro Rodriguez
 */
@SerialVersionUID(1)
class Annotation(val element:org.jdom.Element) extends Serializable {

  /** The DFDL assertions over the element */
  var assertions:List[Assertion] = List()

  /** The physical properties in scope for this element */
  var format:Format = new Format()

  /** Whether this is a hidden node */
  var hidden:Hidden = null

  /** The definitions of variables in this element */
  var variableDefinitions:List[VariableDefinition] = List()

  /** The variables instantiated and bound in this annotation */
  var variableBindings:List[VariableBinding] = List()

  /** The inputValueCalc annotation */
  var inputValue:InputValue = null

  /** The DFDL discriminator over the element */
  var discriminator:Discrimination = null
  
  def addAssertion(assertion:Assertion) = assertions = assertions ::: List(assertion)
  
  def setFormat(format:Format) = this.format = format
   
  def setHidden(hidden:Hidden) = this.hidden = hidden
  
  def setInputValue(input:InputValue) = this.inputValue = inputValue
  
  def setDiscriminator(discriminator:Discrimination) = this.discriminator = discriminator
    
  def addVariableDefinition(v:VariableDefinition) = 
    variableDefinitions = variableDefinitions ::: List(v)
  
  def addVariableBinding(v:VariableBinding) = 
    variableBindings = variableBindings ::: List(v)
 
  def combine(other:Annotation):Annotation = {
    val newAnnotation = new Annotation(element)
    newAnnotation.assertions = assertions ++ other.assertions
    newAnnotation.format = other.format + format
    newAnnotation.hidden = if (hidden==null) other.hidden else hidden
    newAnnotation.variableDefinitions = variableDefinitions ++ other.variableDefinitions
    newAnnotation.variableBindings = variableBindings ++ other.variableBindings
    newAnnotation.inputValue = if (inputValue==null) other.inputValue else inputValue
    newAnnotation
  }
  
  //TODO override hashcode
   
  override def equals(other:Any) = 
    other match {
      case null => false
      case x:Annotation => 
        assertions == x.assertions &&         
        format == x.format && 
        hidden == x.hidden && 
        inputValue == x.inputValue &&
        variableDefinitions == x.variableDefinitions &&
        variableBindings == x.variableBindings
      case _ => false
    }
  
  override def toString() = {
    val sb = new StringBuilder
    sb append "format = {\n"    
    sb append format.toString
    sb append "}\n"
    if (assertions.length>0)
      sb append("asserts:"+assertions.toString+"\n")
    if (discriminator!=null)
      sb append("discriminator:"+discriminator.toString+"\n")
    if (inputValue!=null)
      sb append("dfdl:inputValueCalc:"+inputValue+"\n")
    if (hidden!=null)
      sb append("hidden:true\n")
        
    sb toString
  }
   
}
