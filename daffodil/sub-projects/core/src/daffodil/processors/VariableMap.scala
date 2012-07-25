package daffodil.processors

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

import scala.collection.mutable.Stack
import scala.collection.mutable.Set
import org.jdom.Element
import daffodil.exceptions.Assert
import daffodil.xml.Namespaces
import daffodil.xml.XMLUtils
// import daffodil.xml.XMLUtil

abstract class VariableState

case object VariableDefined extends VariableState
case object VariableSet extends VariableState
case object VariableRead extends VariableState

class VariableAtom(var value:String,var state:VariableState)

class InstantVariable(val name:String,var state:VariableState,var variableType:String,
               var value:Object)

class Variable(val name:String,var state:VariableState,var variableType:String,
               var value:String){
	
	private val stack:Stack[VariableAtom] = new Stack()
 
 	stack push (new VariableAtom(value,state))
 
 	def save = stack push(stack.top)

  def restore = stack pop

  def set(value:String) = {
    stack.top.value = value
    stack.top.state = VariableSet
  }

  def peek:Object = {
    variableType match {
      case XMLUtils.XSD_STRING | "" | null => stack.top.value
      case XMLUtils.XSD_INT | XMLUtils.XSD_INTEGER | XMLUtils.XSD_UNSIGNED_INT | XMLUtils.XSD_SHORT |
              XMLUtils.XSD_UNSIGNED_SHORT  | XMLUtils.XSD_UNSIGNED_BYTE => Predef int2Integer(stack.top.value.toInt)
      case XMLUtils.XSD_LONG | XMLUtils.XSD_UNSIGNED_LONG => Predef long2Long(stack.top.value.toLong)
      case XMLUtils.XSD_FLOAT => Predef float2Float(stack.top.value.toFloat)
      case XMLUtils.XSD_DOUBLE | XMLUtils.XSD_DECIMAL => Predef double2Double(stack.top.value.toDouble)
      case XMLUtils.XSD_BYTE  => Predef byte2Byte(stack.top.value.toByte)
      case XMLUtils.XSD_BOOLEAN => Predef boolean2Boolean(stack.top.value.toBoolean)
      case XMLUtils.XSD_DATE | XMLUtils.XSD_DATE_TIME | XMLUtils.XSD_TIME =>
        Assert.notYetImplemented()//"date,dateTime,time variable types")
    }
  }

  def get:Object = {
    stack.top.state = VariableRead
    peek
  }
}

class VariableMap(val hiddenNodes:Set[Element],val variables:List[Variable]) {

  def this(parent:VariableMap) =
    this(parent hiddenNodes,parent variables)

  def this() =
    this(Set[Element](),Nil)

  def hideElement(ele:Element) =
    hiddenNodes += ele

  def removeHidden() = {
    for(x <- hiddenNodes)
      x getParent() removeContent (x)
  }

  def defineVariable(name:String,variableType:String,namespaces:Namespaces):VariableMap =
    defineVariable(name,variableType,namespaces,"")

  def defineVariable(name:String,variableType:String,namespaces:Namespaces,defaultValue:String):VariableMap =
    new VariableMap(hiddenNodes,
      new Variable(getLongName(name,namespaces),VariableDefined,variableType,defaultValue) :: variables)

  //TODO FIXME don't set the variable if it is already set
  def setVariable(name:String,value:String,namespaces:Namespaces):Unit = {
    val longName = getLongName(name,namespaces)
    variables find { x:Variable => x.name == longName } match {
      case Some(y) =>  y set(value)
      case None => throw new IllegalArgumentException("unknown variable "+name)
    }
  }

  /**
   * Returns the value of variable
   *
   * @param name the fully qualified name of the variable
   *
   * @throws IllegalArgumentException if the variable is not known
   */
  def readVariable(name:String):Object = {
    variables find { x:Variable => x.name == name } match {
      case Some(y) =>  y get
      case None => throw new IllegalArgumentException("unknown variable "+name)
    }
  }

  def listVariables:List[InstantVariable] =
    for(variable <- variables) yield
      new InstantVariable(variable name,variable state,variable variableType,variable peek)


  def save =
    variables foreach { _ save }

  def restore =
    variables foreach { _ restore }

  private def getLongName(name:String,namespaces:Namespaces) = {
    if (name.contains(":")){
      val parts = name.split(":",2)
      namespaces.getNamespaceURI (parts(0)) + parts(1)
    }else
      name
  }

}
