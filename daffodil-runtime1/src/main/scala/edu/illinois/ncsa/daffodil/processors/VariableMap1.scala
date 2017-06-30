/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

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

import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.externalvars.Binding
import edu.illinois.ncsa.daffodil.infoset.RetryableException
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.xml.GlobalQName
import edu.illinois.ncsa.daffodil.xml.NamedQName

sealed abstract class VariableState extends Serializable

case object VariableUndefined extends VariableState
case object VariableDefined extends VariableState
case object VariableSet extends VariableState
case object VariableRead extends VariableState

/**
 * Used when unparsing. A setVariable or newVariableInstance may be underway
 * in the sense that the value of the variable is being computed but is
 * blocked.
 *
 * Readers in this situation should also block, not get any default value
 * nor any error.
 */
case object VariableInProcess extends VariableState

/**
 * Core tuple of a pure functional "state" for variables.
 */
case class Variable(state: VariableState, value: Maybe[AnyRef], rd: VariableRuntimeData, defaultValueExpr: Maybe[CompiledExpression[AnyRef]]) extends Serializable

object VariableUtils {

  def setExternalVariables(currentVMap: VariableMap, bindings: Seq[Binding], referringContext: ThrowsSDE) = {
    var newVMap = currentVMap
    bindings.foreach { b =>
      val vm = newVMap
      val vqn = b.globalQName
      val vv = b.varValue
      val res = vm.setExtVariable(vqn, vv, referringContext) // NoSuchMethodError thrown here!
      newVMap = res
    }
    newVMap
  }

  def convert(v: String, rd: VariableRuntimeData): AnyRef =
    rd.primType.fromXMLString(v)
  // Infoset.convertToInfosetRepType(rd.primType, v, rd)
}

abstract class VariableException(val qname: NamedQName, val context: VariableRuntimeData, msg: String)
  extends Diagnostic(Maybe(context.schemaFileLocation), Nope, Nope, Maybe(msg)) {
  def isError = true
  def modeName = "Variable"
}

class VariableHasNoValue(qname: NamedQName, context: VariableRuntimeData) extends VariableException(qname, context,
  "Variable map (runtime): variable %s has no value. It was not set, and has no default value.".format(qname))
  with RetryableException

/**
 * Provides one more indirection to the variable map.
 *
 * Needed so that when unparsing multiple clones of a UState can share
 * and modify, the same VMap.
 *
 * This is a new mechanism, which allows for less rel-allocation of VariableMaps.
 * There's a box that the variable map lives in, called vbox for convention.
 * by having two UState items point to the same vbox they can share the
 * variables. This is fine for unparsing, because all shared uses are not
 * alternatives to each other which need independent variables, but are
 * just different parts of the same unparse, which need the same variables.
 *
 * Copying excessively in order to deal with the forward-referencing,
 * and not-yet-computed stuff in unparsing is a battle. This vbox thing is
 * one tiny improvement there. More will be needed.
 */
final class VariableBox(initialVMap: VariableMap) {
  private var vmap_ : VariableMap = initialVMap

  def vmap = vmap_
  def setVMap(newMap: VariableMap) {
    vmap_ = newMap
  }
}

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
 * The DPath implementation must be made to implement the
 * no-set-after-default-value-has-been-read behavior. This requires that reading the variables causes a state transition.
 */
class VariableMap private (vTable: Map[GlobalQName, List[List[Variable]]])
  extends Serializable {

  def this(topLevelVRDs: Seq[VariableRuntimeData] = Nil) =
    this(topLevelVRDs.map {
      vrd =>
        val variab = vrd.newVariableInstance
        (vrd.globalQName, List(List(variab)))
    }.toMap)

  override def toString(): String = {
    "VariableMap(" + vTable.mkString(" | ") + ")"
  }

  def find(qName: GlobalQName): Option[Variable] = {
    val optVrd = getVariableRuntimeData(qName)
    val optLists = optVrd.flatMap { vrd => vTable.get(vrd.globalQName) }
    val variab = optLists.flatMap { lists => lists.flatMap { _.toStream }.headOption }
    variab
  }

  def getVariableRuntimeData(qName: GlobalQName): Option[VariableRuntimeData] = {
    val optLists = vTable.get(qName)
    optLists match {
      case None => None // no such variable.
      case Some(lists) => {
        val flatLists = lists.flatten
        Assert.invariant(flatLists.length > 0)
        val varObj = flatLists.head
        Some(varObj.rd)
      }
    }
  }

  lazy val context = Assert.invariantFailed("unused.")

  private def mkVMap(newVar: Variable, firstTier: List[Variable], enclosingScopes: List[List[Variable]]) = {
    val newMap = vTable + ((newVar.rd.globalQName, (newVar :: firstTier) :: enclosingScopes))
    new VariableMap(newMap)
  }

  /**
   * Convenient method of updating the entry of the Variable and returning a new VMap.
   */
  private def mkVMap(varQName: GlobalQName, updatedFirstTier: List[Variable], enclosingScopes: List[List[Variable]]) = {
    val updatableMap = scala.collection.mutable.Map(vTable.toSeq: _*)
    updatableMap(varQName) = updatedFirstTier :: enclosingScopes
    new VariableMap(updatableMap.toMap)
  }

  /**
   * For testing mostly.
   */
  def getVariableBindings(qn: GlobalQName): List[List[Variable]] = {
    vTable.get(qn).get
  }
  //  def getVariableBindings(vrd: VariableRuntimeData): List[List[Variable]] = {
  //    vTable.get(vrd.globalQName).get
  //  }

  /**
   * Returns the value of a variable, constructing also a modified variable map which
   * shows that the variable has been read (state VariableRead), when the variable hadn't
   * previously been read yet.
   */
  def readVariable(vrd: VariableRuntimeData, referringContext: ThrowsSDE): (AnyRef, VariableMap) = {
    val referringContext: VariableRuntimeData = vrd
    val varQName = vrd.globalQName
    val lists = vTable.get(varQName)
    lists match {

      case Some(firstTier :: enclosingScopes) =>
        firstTier match {

          case Variable(VariableRead, v, ctxt, _) :: rest if (v.isDefined) => (v.get, this)

          case Variable(st, v, ctxt, defExpr) :: rest if ((v.isDefined) && (st == VariableDefined || st == VariableSet)) => {
            val newVar = Variable(VariableRead, One(v.get), ctxt, defExpr)
            val vmap = mkVMap(newVar, firstTier, enclosingScopes)
            val converted = v.get // already converted
            (converted, vmap)
          }

          case _ => {
            // Fix DFDL-766
            throw new VariableHasNoValue(varQName, referringContext)
            // Runtime error:
            // referringContext.SDE(msg, varQName)
          }
        }

      case Some(Nil) => Assert.invariantFailed()

      case None => {
        // Compile time error:
        referringContext.SDE("Variable map (compilation): unknown variable %s", varQName)
      }
    }
  }

  /**
   * Assigns a variable, returning a new VariableMap which shows the state of the variable.
   */
  def setVariable(vrd: VariableRuntimeData, newValue: Any, referringContext: ThrowsSDE, pstate: ParseOrUnparseState): VariableMap = {
    val varQName = vrd.globalQName

    vTable.get(varQName) match {

      case None => referringContext.schemaDefinitionError("unknown variable %s", varQName)

      // There should always be a list with at least one tier in it (the global tier).
      case x @ Some(firstTier :: enclosingScopes) => {
        firstTier match {

          case Variable(VariableDefined, v, ctxt, defaultExpr) :: rest if (v.isDefined) => {
            val newVar = Variable(VariableSet, One(VariableUtils.convert(newValue.toString, ctxt)), ctxt, defaultExpr)
            mkVMap(newVar, firstTier, enclosingScopes)
          }

          case Variable(VariableUndefined, Nope, ctxt, defaultExpr) :: rest => {
            val newVar = Variable(VariableSet, One(VariableUtils.convert(newValue.toString, ctxt)), ctxt, defaultExpr)
            mkVMap(newVar, firstTier, enclosingScopes)
          }

          case Variable(VariableSet, v, ctxt, defaultExpr) :: rest if (v.isDefined) => {
            referringContext.SDE("Cannot set variable %s twice. State was: %s. Existing value: %s", ctxt.globalQName, VariableSet, v.get)
          }

          case Variable(VariableRead, v, ctxt, defaultExpr) :: rest if (v.isDefined) => {
            // referringContext.SDE
            pstate.SDW("Cannot set variable %s after reading the default value. State was: %s. Existing value: %s", ctxt.globalQName, VariableSet, v.get)
            val newVar = Variable(VariableSet, One(VariableUtils.convert(newValue.toString, ctxt)), ctxt, defaultExpr)
            mkVMap(newVar, firstTier, enclosingScopes)
          }

          case _ => Assert.invariantFailed("variable map internal list structure not as expected: " + x)
        }
      }
      case x => Assert.invariantFailed("variables data structure not as expected. Should not be " + x)
    }
  }

  /**
   * Assigns a variable, returning a new VariableMap which shows the state of the variable.
   */
  def setExtVariable(varQName: GlobalQName, newValue: Any, referringContext: ThrowsSDE): VariableMap = {
    vTable.get(varQName) match {

      case None => referringContext.schemaDefinitionError("unknown variable %s", varQName)

      // There should always be a list with at least one tier in it (the global tier).
      case x @ Some(firstTier :: enclosingScopes) => {
        firstTier match {

          case Variable(VariableDefined, v, ctxt, defaultExpr) :: rest if (v.isDefined && ctxt.external) => {
            val newVar = Variable(VariableDefined, One(VariableUtils.convert(newValue.toString, ctxt)), ctxt, defaultExpr)
            val newFirstTier = newVar :: rest
            mkVMap(varQName, newFirstTier, enclosingScopes)
          }
          case Variable(VariableDefined, v, ctxt, defaultExpr) :: rest if (v.isDefined) => {
            referringContext.SDE("Cannot set variable %s externally. State was: %s. Existing value: %s.", ctxt.globalQName, VariableDefined, v.get)
            // this // Unaltered VMap
          }

          case Variable(VariableUndefined, Nope, ctxt, defaultExpr) :: rest if ctxt.external => {
            val newVar = Variable(VariableDefined, One(VariableUtils.convert(newValue.toString, ctxt)), ctxt, defaultExpr)
            val newFirstTier = newVar :: rest
            mkVMap(varQName, newFirstTier, enclosingScopes)
          }

          case Variable(VariableUndefined, Nope, ctxt, defaultExpr) :: rest => {
            referringContext.SDE("Cannot set variable %s externally. State was: %s.", ctxt.globalQName, VariableUndefined)
            // this // Unaltered VMap
          }

          case Variable(VariableSet, v, ctxt, defaultExpr) :: rest if (v.isDefined) => {
            // Shouldn't this be an impossible case? External variables should be defined before parsing.
            // Parsing is the only point at which Set can be called?
            referringContext.SDE("Cannot externally set variable %s twice. State was: %s. Existing value: %s", ctxt.globalQName, VariableSet, v.get)
            // this // Unaltered VMap
          }

          case Variable(VariableRead, v, ctxt, defaultExpr) :: rest if (v.isDefined) => {
            referringContext.SDE("Cannot externally set variable %s after reading the default value. State was: %s. Existing value: %s", ctxt.globalQName, VariableSet, v.get)
            // this // Unaltered VMap
          }

          case _ => Assert.invariantFailed("variable map internal list structure not as expected: " + x)
        }
      }
      case x => Assert.invariantFailed("variables data structure not as expected. Should not be " + x)
    }
  }

}
