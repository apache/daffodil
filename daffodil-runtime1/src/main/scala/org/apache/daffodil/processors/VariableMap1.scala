/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.processors

import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.dsom.CompiledExpression
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.infoset.RetryableException
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe.Nope
import org.apache.daffodil.util.Maybe.One
import org.apache.daffodil.xml.GlobalQName
import org.apache.daffodil.xml.NamedQName
import org.apache.daffodil.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.infoset.DataValue
import org.apache.daffodil.infoset.DataValue.DataValuePrimitiveNullable
import org.apache.daffodil.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.infoset.DataValue.DataValuePrimitive

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
case class Variable(state: VariableState, value: DataValuePrimitiveNullable, rd: VariableRuntimeData, defaultValueExpr: Maybe[CompiledExpression[AnyRef]]) extends Serializable

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

  def convert(v: String, rd: VariableRuntimeData): DataValuePrimitive =
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
  def readVariable(vrd: VariableRuntimeData, referringContext: ThrowsSDE): (DataValuePrimitive, VariableMap) = {
    val referringContext: VariableRuntimeData = vrd
    val varQName = vrd.globalQName
    val lists = vTable.get(varQName)
    lists match {

      case Some(firstTier :: enclosingScopes) =>
        firstTier match {

          case Variable(VariableRead, v, ctxt, _) :: rest if (v.isDefined) => (v.getNonNullable, this)

          case Variable(st, v, ctxt, defExpr) :: rest if ((v.isDefined) && (st == VariableDefined || st == VariableSet)) => {
            val newVar = Variable(VariableRead, v, ctxt, defExpr)
            val vmap = mkVMap(newVar, firstTier, enclosingScopes)
            val converted = v.getNonNullable // already converted
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
  def setVariable(vrd: VariableRuntimeData, newValue: DataValuePrimitive, referringContext: ThrowsSDE, pstate: ParseOrUnparseState): VariableMap = {
    val varQName = vrd.globalQName

    vTable.get(varQName) match {

      case None => referringContext.schemaDefinitionError("unknown variable %s", varQName)

      // There should always be a list with at least one tier in it (the global tier).
      case x @ Some(firstTier :: enclosingScopes) => {
        firstTier match {

          case Variable(VariableDefined, v, ctxt, defaultExpr) :: rest if (v.isDefined) => {
            val newVar = Variable(VariableSet, VariableUtils.convert(newValue.getAnyRef.toString, ctxt), ctxt, defaultExpr)
            mkVMap(newVar, firstTier, enclosingScopes)
          }

          case Variable(VariableUndefined, DataValue.NoValue, ctxt, defaultExpr) :: rest => {
            val newVar = Variable(VariableSet, VariableUtils.convert(newValue.getAnyRef.toString, ctxt), ctxt, defaultExpr)
            mkVMap(newVar, firstTier, enclosingScopes)
          }

          case Variable(VariableSet, v, ctxt, defaultExpr) :: rest if (v.isDefined) => {
            referringContext.SDE("Cannot set variable %s twice. State was: %s. Existing value: %s", ctxt.globalQName, VariableSet, v)
          }

          case Variable(VariableRead, v, ctxt, defaultExpr) :: rest if (v.isDefined) => {
            // referringContext.SDE
            pstate.SDW("Cannot set variable %s after reading the default value. State was: %s. Existing value: %s", ctxt.globalQName, VariableSet, v)
            val newVar = Variable(VariableSet, VariableUtils.convert(newValue.getAnyRef.toString, ctxt), ctxt, defaultExpr)
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
  def setExtVariable(varQName: GlobalQName, newValue: DataValuePrimitive, referringContext: ThrowsSDE): VariableMap = {
    vTable.get(varQName) match {

      case None => referringContext.schemaDefinitionError("unknown variable %s", varQName)

      // There should always be a list with at least one tier in it (the global tier).
      case x @ Some(firstTier :: enclosingScopes) => {
        firstTier match {

          case Variable(VariableDefined, v, ctxt, defaultExpr) :: rest if (v.isDefined && ctxt.external) => {
            val newVar = Variable(VariableDefined, VariableUtils.convert(newValue.getAnyRef.toString, ctxt), ctxt, defaultExpr)
            val newFirstTier = newVar :: rest
            mkVMap(varQName, newFirstTier, enclosingScopes)
          }
          case Variable(VariableDefined, v, ctxt, defaultExpr) :: rest if (v.isDefined) => {
            referringContext.SDE("Cannot set variable %s externally. State was: %s. Existing value: %s.", ctxt.globalQName, VariableDefined, v)
            // this // Unaltered VMap
          }

          case Variable(VariableUndefined, DataValue.NoValue, ctxt, defaultExpr) :: rest if ctxt.external => {
            val newVar = Variable(VariableDefined, VariableUtils.convert(newValue.getAnyRef.toString, ctxt), ctxt, defaultExpr)
            val newFirstTier = newVar :: rest
            mkVMap(varQName, newFirstTier, enclosingScopes)
          }

          case Variable(VariableUndefined, DataValue.NoValue, ctxt, defaultExpr) :: rest => {
            referringContext.SDE("Cannot set variable %s externally. State was: %s.", ctxt.globalQName, VariableUndefined)
            // this // Unaltered VMap
          }

          case Variable(VariableSet, v, ctxt, defaultExpr) :: rest if (v.isDefined) => {
            // Shouldn't this be an impossible case? External variables should be defined before parsing.
            // Parsing is the only point at which Set can be called?
            referringContext.SDE("Cannot externally set variable %s twice. State was: %s. Existing value: %s", ctxt.globalQName, VariableSet, v)
            // this // Unaltered VMap
          }

          case Variable(VariableRead, v, ctxt, defaultExpr) :: rest if (v.isDefined) => {
            referringContext.SDE("Cannot externally set variable %s after reading the default value. State was: %s. Existing value: %s", ctxt.globalQName, VariableSet, v)
            // this // Unaltered VMap
          }

          case _ => Assert.invariantFailed("variable map internal list structure not as expected: " + x)
        }
      }
      case x => Assert.invariantFailed("variables data structure not as expected. Should not be " + x)
    }
  }

}
