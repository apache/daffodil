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

import org.apache.daffodil.api.ThinDiagnostic
import org.apache.daffodil.dsom.CompiledExpression
import org.apache.daffodil.dpath.InvalidPrimitiveDataException
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.infoset.DataValue
import org.apache.daffodil.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.infoset.DataValue.DataValuePrimitiveNullable
import org.apache.daffodil.infoset.RetryableException
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe.Nope
import org.apache.daffodil.util.MStackOf
import org.apache.daffodil.xml.{GlobalQName, UnspecifiedNamespace, NamedQName, RefQName}
import org.apache.daffodil.processors.parsers.PState

import scala.collection.mutable.Map

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
 * Core tuple of a state for variables.
 */
case class VariableInstance(
  var state: VariableState,
  var value: DataValuePrimitiveNullable,
  rd: VariableRuntimeData,
  defaultValueExpr: Maybe[CompiledExpression[AnyRef]],
  var priorState: VariableState = VariableUndefined,
  var priorValue: DataValuePrimitiveNullable = DataValue.NoValue)
  extends Serializable {

  def copy(
    state: VariableState = state,
    value: DataValuePrimitiveNullable = value,
    rd: VariableRuntimeData = rd,
    defaultValueExpr: Maybe[CompiledExpression[AnyRef]] = defaultValueExpr,
    priorState: VariableState = priorState,
    priorValue: DataValuePrimitiveNullable = priorValue) =
      new VariableInstance(state, value, rd, defaultValueExpr, priorState, priorValue)

  def setState(s: VariableState) = {
    this.priorState = this.state
    this.state = s
  }

  def setValue(v: DataValuePrimitiveNullable) = {
    this.priorValue = this.value
    this.value = v
  }

  def reset() = {
    Assert.invariant(this.state != VariableUndefined)
    (this.state, this.priorState, this.defaultValueExpr.isDefined) match {
      case (VariableRead, VariableSet, _) => this.state = VariableSet
      case (VariableRead, _, true) => this.state = VariableDefined
      case (VariableSet, _, _) => {
        this.state = this.priorState
        this.value = this.priorValue
      }
      case (_, _, _) => Assert.impossible("Should have SDE before reaching this")
    }
  }
}

object VariableUtils {

  def setExternalVariables(currentVMap: VariableMap, bindings: Seq[Binding], referringContext: ThrowsSDE) = {
    bindings.foreach { b => currentVMap.setExtVariable(b.varQName, b.varValue, referringContext) }
  }

  def convert(v: String, rd: VariableRuntimeData, referringContext: ThrowsSDE): DataValuePrimitive = {
    try {
      rd.primType.fromXMLString(v)
    } catch {
      case e: InvalidPrimitiveDataException =>
        referringContext.SDE("Error processing variable %s: %s", rd.globalQName, e.getMessage)
    }
  }

}

abstract class VariableException(val qname: NamedQName, val context: VariableRuntimeData, msg: String)
  extends ThinDiagnostic(Maybe(context.schemaFileLocation), Nope, Nope, Maybe(msg)) {
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

  def setVMap(newMap: VariableMap): Unit = {
    vmap_ = newMap
  }
}

/**
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
class VariableMap private(vTable: Map[GlobalQName, MStackOf[VariableInstance]])
  extends Serializable {

  def this(topLevelVRDs: Seq[VariableRuntimeData] = Nil) =
    this(Map(topLevelVRDs.map {
      vrd =>
        val variab = vrd.createVariableInstance
        val stack = new MStackOf[VariableInstance]
        stack.push(variab)
        (vrd.globalQName, stack)
    }: _*))

  override def toString(): String = {
    "VariableMap(" + vTable.mkString(" | ") + ")"
  }

  /**
   * Returns a full, deep copy of the current VariableMap. This is needed as
   * VariableInstances are mutable and cannot safely be shared across threads
   */
  def copy(): VariableMap = {
    val table = Map[GlobalQName, MStackOf[VariableInstance]]()
    vTable.foreach { case (k: GlobalQName, s: MStackOf[VariableInstance]) => {
      val newStack= new MStackOf[VariableInstance]()

      // toList provides a list in LIFO order, so we need to reverse it to
      // maintain order
      s.toList.reverse.foreach { case v: VariableInstance => newStack.push(v.copy()) }
      table(k) = newStack
    }}

    new VariableMap(table)
  }

  def find(qName: GlobalQName): Option[VariableInstance] = {
    val optStack = vTable.get(qName)
    val variab = {
      if (optStack.isDefined)
        Some(optStack.get.top)
      else
        None
    }
    variab
  }

  def getVariableRuntimeData(qName: GlobalQName): Option[VariableRuntimeData] = {
    val optVariable = find(qName)
    if (optVariable.isDefined) Some(optVariable.get.rd) else None
  }

  lazy val context = Assert.invariantFailed("unused.")

  /**
   * For testing mostly.
   */
  def getVariableBindings(qn: GlobalQName): MStackOf[VariableInstance] = {
    vTable.get(qn).get
  }

  /**
   * Returns the value of a variable and sets the state of the variable to be
   * VariableRead.
   */
  def readVariable(vrd: VariableRuntimeData, referringContext: ThrowsSDE, maybePstate: Maybe[ParseOrUnparseState]): DataValuePrimitive = {
    val varQName = vrd.globalQName
    val stack = vTable.get(varQName)
    if (stack.isDefined) {
      val variable = stack.get.top
      variable.state match {
        case VariableRead if (variable.value.isDefined) => variable.value.getNonNullable
        case VariableDefined | VariableSet if (variable.value.isDefined) => {
          if (maybePstate.isDefined && maybePstate.get.isInstanceOf[PState])
            maybePstate.get.asInstanceOf[PState].markVariableRead(vrd)

          variable.setState(VariableRead)
          variable.value.getNonNullable
        }
        case _ => throw new VariableHasNoValue(varQName, vrd)
      }
    } else
      referringContext.SDE("Variable map (compilation): unknown variable %s", varQName) // Fix DFDL-766
  }

  /**
   * Assigns a variable and sets the variables state to VariableSet
   */
  def setVariable(vrd: VariableRuntimeData, newValue: DataValuePrimitive, referringContext: ThrowsSDE, pstate: ParseOrUnparseState) = {
    val varQName = vrd.globalQName
    val stack = vTable.get(varQName)
    if (stack.isDefined) {
      val variable = stack.get.top
      variable.state match {
        case VariableSet => {
          referringContext.SDE("Cannot set variable %s twice. State was: %s. Existing value: %s",
          variable.rd.globalQName, VariableSet, variable.value)
        }

        case VariableRead => {
          /**
           * TODO: This should be an SDE, but due to a bug (DAFFODIL-1443) in
           * the way we evaluate escapeSchemes it could lead us to setting the
           * variable read too early */
          pstate.SDW("Cannot set variable %s after reading the default value. State was: %s. Existing value: %s",
          variable.rd.globalQName, VariableSet, variable.value)
          variable.setValue(VariableUtils.convert(newValue.getAnyRef.toString, variable.rd, referringContext))
          variable.setState(VariableSet)
        }

        case _ => {
          variable.setValue(VariableUtils.convert(newValue.getAnyRef.toString, variable.rd, referringContext))
          variable.setState(VariableSet)
        }
      }
    }
  }

  /**
   * Creates a new instance of a variable
   */
  def newVariableInstance(vrd: VariableRuntimeData) = {
    val varQName = vrd.globalQName
    val stack = vTable.get(varQName)
    Assert.invariant(stack.isDefined)
    stack.get.push(vrd.createVariableInstance)
  }

  def removeVariableInstance(vrd: VariableRuntimeData): Unit = {
    val varQName = vrd.globalQName
    val stack = vTable.get(varQName)
    Assert.invariant(stack.isDefined)
    stack.get.pop
  }


  private lazy val externalVarGlobalQNames: Seq[GlobalQName] =
    vTable.map { case (_, stack) if (stack.top.rd.external) => stack.top.rd.globalQName }.toSeq

  /**
   * Assigns an external variable and sets the variables state to VariableSet
   */
  def setExtVariable(bindingQName: RefQName, newValue: DataValuePrimitive, referringContext: ThrowsSDE) = {
    val varQName: RefQName =
      if (bindingQName.namespace == UnspecifiedNamespace) {
        // The external variable binding was hoping to get away with not specifying a namespace.
        val candidateExtVarsQNames = externalVarGlobalQNames.filter {
          _.local == bindingQName.local
        }
        candidateExtVarsQNames match {
          case Seq() => bindingQName // just pass through. Will fail below when it isn't found
          case Seq(candidate) => candidate.toRefQName
          case _ => {
            // ambiguous. There's multiple such.
            referringContext.SDE(
              "The externally defined variable binding %s is ambiguous.  " +
                "A namespace is required to resolve the ambiguity.\nFound:\t%s",
              bindingQName, candidateExtVarsQNames.mkString(", "))
          }
        }
      } else {
        bindingQName
      }

    val stack = vTable.get(varQName.toGlobalQName)
    if (!stack.isDefined)
      referringContext.schemaDefinitionError("unknown variable %s", varQName)
    else {
      val variable = stack.get.top
      variable.state match {
        case VariableDefined if (!variable.rd.external) => {
          referringContext.SDE("Cannot set variable %s externally. State was: %s. Existing value: %s.",
            variable.rd.globalQName, VariableDefined, variable.value)
        }

        case VariableUndefined if (!variable.rd.external) => {
          referringContext.SDE("Cannot set variable %s externally. State was: %s.", variable.rd.globalQName, VariableUndefined)
        }

        case VariableSet => {
          referringContext.SDE("Cannot externally set variable %s twice. State was: %s. Existing value: %s",
            variable.rd.globalQName, VariableSet, variable.value)
        }

        case VariableRead => {
          referringContext.SDE("Cannot externally set variable %s after reading the default value. State was: %s. Existing value: %s",
            variable.rd.globalQName, VariableSet, variable.value)
        }

        case _ => {
          variable.setValue(VariableUtils.convert(newValue.getAnyRef.toString, variable.rd, referringContext))
          variable.setState(VariableDefined)
        }
      }
    }
  }
}
