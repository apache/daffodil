package edu.illinois.ncsa.daffodil.processors

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
 * Extensively modified - little of the original remains. - Mike Beckerle 2012.
 */

import scala.collection.mutable.Stack
import scala.collection.mutable.Set
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import javax.xml.namespace.QName
import edu.illinois.ncsa.daffodil.dsom.SchemaDocument
import edu.illinois.ncsa.daffodil.dsom.DFDLDefineVariable
import edu.illinois.ncsa.daffodil.dsom.DFDLSetVariable
import edu.illinois.ncsa.daffodil.dsom.SchemaComponent
import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionError

sealed abstract class VariableState

case object VariableUndefined extends VariableState
case object VariableDefined extends VariableState
case object VariableSet extends VariableState
case object VariableRead extends VariableState

/**
 * Core tuple of a pure functional "state" for variables.
 */
case class Variable(state: VariableState, value: Option[AnyRef], defv: DFDLDefineVariable)

object VariableUtil {

  /**
   * Needed to deal with saxon XPath evaluation. Many things are implicitly converted. In JDOM
   * everything is a string, so the only way to add, is to implicitly convert into the specified
   * type of the variable.
   */
  def convert(v: String, extType: String) = {
    extType match {
      case XMLUtils.XSD_STRING => v
      case XMLUtils.XSD_INT | XMLUtils.XSD_INTEGER | XMLUtils.XSD_UNSIGNED_INT | XMLUtils.XSD_SHORT |
        XMLUtils.XSD_UNSIGNED_SHORT | XMLUtils.XSD_UNSIGNED_BYTE => Predef int2Integer (v.toInt)
      case XMLUtils.XSD_LONG | XMLUtils.XSD_UNSIGNED_LONG => Predef long2Long (v.toLong)
      case XMLUtils.XSD_FLOAT => Predef float2Float (v.toFloat)
      case XMLUtils.XSD_DOUBLE | XMLUtils.XSD_DECIMAL => Predef double2Double (v.toDouble)
      case XMLUtils.XSD_BYTE => Predef byte2Byte (v.toByte)
      case XMLUtils.XSD_BOOLEAN => Predef boolean2Boolean (v.toBoolean)
      case XMLUtils.XSD_DATE | XMLUtils.XSD_DATE_TIME | XMLUtils.XSD_TIME =>
        Assert.notYetImplemented("date,dateTime,time variable types")
      case _ => Assert.invariantFailed("unknown variable type: " + extType)
    }
  }
}

//
//  def get(contextArg : SchemaComponent) : (AnyRef, Option[Variable]) = {
//    val context = contextArg
//    PECheck(context,
//      state == VariableSet ||
//        state == VariableDefined ||
//        state == VariableRead,
//      "Variable %s is not readable. State is: %s", defv.extName, state.toString)
//    val newVar = if (state == VariableRead) None else
//      Some(new Variable(VariableRead, value, defv))
//    val res = value
//    (res, newVar)
//  }

/**
 * Factory for Variable objects
 */
object VariableFactory {
  def create(defv: DFDLDefineVariable,
             expandedName: String,
             extType: String,
             defaultValue: Option[String],
             external: Boolean,
             doc: SchemaDocument) = {

    val state = defaultValue match {
      case None => VariableUndefined
      case Some(_) => VariableDefined
    }

    val typeSym = doc.expressionCompiler.convertTypeString(extType)

    val defaultValExpr = defaultValue.map {
      doc.expressionCompiler.compile(typeSym, _)
    }

    val defaultVal = defaultValExpr.map { ce => VariableUtil.convert(ce.constantAsString, extType) }

    val defaultValIsConstant = {
      val isConst = defaultValExpr.map { _.isConstant }.getOrElse(true)
      defv.schemaDefinition(isConst, "Variable default value %s is not a constant.", defaultValue)
      isConst
    }

    val var_ = Variable(state, defaultVal, defv)
    var_

  }
}

object EmptyVariableMap extends VariableMap()

/**
 * Pure functional data structure for implementing DFDL's variables.
 *
 * Key concepts: DFDL variables are single-assignment. Once they have been set, they may not be set again.
 * Furthermore, they have default values, and if the default value has been read, then they may not
 * subsequently be set.
 *
 * These constraints insure that the variables do NOT become a sneaky way to get generalized accumulators and hence turing-complete
 * semantics (and complexity) into designing and debugging DFDL schemas. They also allow for parallel implementations since
 * order of evaluation does not matter.
 *
 * What makes this tricky to implement is that we're using the JDOM representation for the DFDL Infoset. This gives us
 * XPath expression evaluation as part of the implementation; however, that implementation must be made to implement the
 * no-set-after-default-value-has-been-read behavior. This requires that reading the variables causes a state transition.
 * Our "pure functional" desire lives in tension with this.
 */
class VariableMap(private val variables: Map[String, List[List[Variable]]] = Map.empty)
  extends WithParseErrorThrowing {

  lazy val context = Assert.invariantFailed("unused.")

  private def mkVMap(newVar: Variable, firstTier: List[Variable], enclosingScopes: List[List[Variable]]) = {
    val newMap = variables + ((newVar.defv.extName, (newVar :: firstTier) :: enclosingScopes))
    new VariableMap(newMap)
  }

  /**
   * Returns the value of a variable, constructing also a modified variable map which
   * shows that the variable has been read (state VariableRead), when the variable hadn't
   * previously been read yet.
   */
  def readVariable(expandedName: String, referringContext: SchemaComponent): (AnyRef, VariableMap) = {
    val lists = variables.get(expandedName)
    lists match {

      case Some(firstTier :: enclosingScopes) =>
        firstTier match {

          case Variable(VariableRead, Some(v), ctxt) :: rest => (v, this)

          case Variable(st, Some(v), ctxt) :: rest if (st == VariableDefined || st == VariableSet) => {
            val newVar = Variable(VariableRead, Some(v), ctxt)
            val vmap = mkVMap(newVar, firstTier, enclosingScopes)
            val converted = v // already converted
            (converted, vmap)
          }

          case _ => Assert.invariantFailed()
        }

      case Some(Nil) => Assert.invariantFailed()

      case None => referringContext.schemaDefinitionError("unknown variable %s", expandedName)
    }
  }

  /**
   * Assigns a variable, returning a new VariableMap which shows the state of the variable.
   */
  def setVariable(expandedName: String, newValue: Any, referringContext: SchemaComponent): VariableMap = {
    variables.get(expandedName) match {

      case None => referringContext.schemaDefinitionError("unknown variable %s", expandedName)

      // There should always be a list with at least one tier in it (the global tier).
      case x @ Some(firstTier :: enclosingScopes) => {
        firstTier match {

          case Variable(VariableDefined, Some(v), ctxt) :: rest => {
            val newVar = Variable(VariableSet, Some(VariableUtil.convert(newValue.toString, ctxt.extType)), ctxt)
            mkVMap(newVar, firstTier, enclosingScopes)
          }

          case Variable(VariableUndefined, None, ctxt) :: rest => {
            val newVar = Variable(VariableSet, Some(VariableUtil.convert(newValue.toString, ctxt.extType)), ctxt)
            mkVMap(newVar, firstTier, enclosingScopes)
          }

          case Variable(VariableSet, Some(v), ctxt) :: rest => {
            PE(referringContext, "Cannot set variable %s twice. State was: %s. Existing value: %s", ctxt.extName, VariableSet, v)
          }

          case Variable(VariableRead, Some(v), ctxt) :: rest => {
            PE(referringContext, "Cannot set variable %s after reading the default value. State was: %s. Existing value: %s", ctxt.extName, VariableSet, v)
          }

          case _ => Assert.invariantFailed("variable map internal list structure not as expected: " + x)
        }
      }
      case x => Assert.invariantFailed("variables data structure not as expected. Should not be " + x)
    }
  }

}

object VariableMap {
  def create(dvs: Seq[DFDLDefineVariable]): VariableMap = {
    val pairs = dvs.map { dv => (dv.extName, List(List(dv.newVariableInstance))) }
    val hmap = pairs.toMap
    val vmap = new VariableMap(hmap)
    vmap
  }
}
