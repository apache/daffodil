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
//import daffodil.xml.Namespaces
import daffodil.xml.XMLUtils
import javax.xml.namespace.QName
import daffodil.dsom.SchemaDocument
import daffodil.dsom.CompiledExpressionUtil
import daffodil.dsom.DFDLDefineVariable
import daffodil.dsom.DFDLSetVariable
import daffodil.dsom.SchemaComponent

sealed abstract class VariableState

case object VariableUndefined extends VariableState
case object VariableDefined extends VariableState
case object VariableSet extends VariableState
case object VariableRead extends VariableState

class VariableAtom(var value : Option[String], var state : VariableState)

//class InstantVariable(val name:String,var state:VariableState,var variableType:String,
//               var value:Object)

class Variable(varDefinitionContext : SchemaDocument, val name : String, var state : VariableState, val variableType : String,
  var value : Option[String])
  extends WithParseErrorThrowing {

  private val stack : Stack[VariableAtom] = new Stack()

  stack push (new VariableAtom(value, state))

  def save = stack push (stack.top)

  def restore = stack pop

  def set(value : String) = {
    Assert.usage(stack.top.state == VariableUndefined ||
      stack.top.state == VariableDefined) // you can set after a default value
    stack.top.value = Some(value)
    stack.top.state = VariableSet
  }

  def peek : Object = {
    val value = stack.top.value.get
    variableType match {
      case XMLUtils.XSD_STRING => value
      case XMLUtils.XSD_INT | XMLUtils.XSD_INTEGER | XMLUtils.XSD_UNSIGNED_INT | XMLUtils.XSD_SHORT |
        XMLUtils.XSD_UNSIGNED_SHORT | XMLUtils.XSD_UNSIGNED_BYTE => Predef int2Integer (value.toInt)
      case XMLUtils.XSD_LONG | XMLUtils.XSD_UNSIGNED_LONG => Predef long2Long (value.toLong)
      case XMLUtils.XSD_FLOAT => Predef float2Float (value.toFloat)
      case XMLUtils.XSD_DOUBLE | XMLUtils.XSD_DECIMAL => Predef double2Double (value.toDouble)
      case XMLUtils.XSD_BYTE => Predef byte2Byte (value.toByte)
      case XMLUtils.XSD_BOOLEAN => Predef boolean2Boolean (value.toBoolean)
      case XMLUtils.XSD_DATE | XMLUtils.XSD_DATE_TIME | XMLUtils.XSD_TIME =>
        Assert.notYetImplemented() //"date,dateTime,time variable types")
      case _ => Assert.invariantFailed("unknown variable type: " + variableType)
    }
  }
  
  var context : SchemaComponent = null

  def get(contextArg : Option[SchemaComponent]) : Object = {
    context = contextArg.getOrElse(null)
    PECheck(stack.top.state == VariableSet ||
      stack.top.state == VariableDefined ||
      stack.top.state == VariableRead,
      "Variable %s is not readable. State is: %s", name, state.toString)
    stack.top.state = VariableRead
    peek
  }
}

/**
 * Factory for Variable objects
 */
object Variable {
  def apply(defv : DFDLDefineVariable,
    expandedName : String,
    type_ : String,
    defaultValue : Option[String],
    external : Boolean,
    doc : SchemaDocument) = {

    val state = defaultValue match {
      case None => VariableUndefined
      case Some(_) => VariableDefined
    }

    val expTypeName = doc.expressionCompiler.expandedName(type_)
    val typeSym = doc.expressionCompiler.convertTypeString(expTypeName)

    val defaultValExpr = defaultValue.map {
      doc.expressionCompiler.compile(typeSym, _)
    }

    val defaultVal = defaultValExpr.map { _.constantAsString }

    val defaultValIsConstant = {
      val isConst = defaultValExpr.map { _.isConstant }.getOrElse(true)
      defv.schemaDefinition(isConst, "Variable default value %s is not a constant.", defaultValue)
      isConst
    }

    val var_ = new Variable(defv.context.schemaDocument, expandedName, state, expTypeName, defaultVal)
    var_

  }

}

object EmptyVariableMap extends VariableMap()

class VariableMap(val hiddenNodes : Set[Element], val variables : Map[String, Variable]) {
  def this(variables : Map[String, Variable]) = this(Set.empty, variables)
  def this() = this(Set.empty, Map.empty)

  //  def this(parent:VariableMap) =
  //    this(parent hiddenNodes,parent variables)
  //
  //  def this() =
  //    this(Set[Element](),Nil)

  //  def hideElement(ele:Element) =
  //    hiddenNodes += ele
  //
  //  def removeHidden() = {
  //    for(x <- hiddenNodes)
  //      x getParent() removeContent (x)
  //  }

  //  def defineVariable(expandedName:String,variableType:String,namespaces:Namespaces):VariableMap =
  //    defineVariable(expandedName,variableType,namespaces,"")
  //
  //  def defineVariable(expandedName:String, variableType:String,namespaces:Namespaces,defaultValue:String):VariableMap =
  //    new VariableMap(hiddenNodes,
  //      new Variable(expandedName,VariableDefined,variableType,defaultValue) :: variables)

  def setVariable(ann : DFDLSetVariable, expandedName : String, value : String) : Unit = {
    variables.get(expandedName) match {
      case Some(y) => {
        // TODO check for double set, or for already been read.
        y set (value)
      }
      case None => throw new IllegalArgumentException("unknown variable " + expandedName)
    }
  }

  /**
   * Returns the value of variable
   *
   * @param name the fully qualified name of the variable
   *
   * @throws IllegalArgumentException if the variable is not known
   */
  def readVariable(expandedName : String, context : Option[SchemaComponent]) : Object = {
    variables.get(expandedName) match {
      case Some(y) => y get(context)
      case None => {
        val sc = context.getOrElse(throw new IllegalArgumentException("unknown variable " + expandedName))
        sc.schemaDefinitionError("unknown variable %s", expandedName)
      }
    }
  }

  //  def listVariables:List[InstantVariable] =
  //    for(variable <- variables) yield
  //      new InstantVariable(variable name,variable state,variable variableType,variable peek)

//  def save =
//    variables foreach { _ save }
//
//  def restore =
//    variables foreach { _ restore }

}
