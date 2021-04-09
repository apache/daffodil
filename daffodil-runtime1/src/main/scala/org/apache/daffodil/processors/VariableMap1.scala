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
import org.apache.daffodil.xml.{GlobalQName, UnspecifiedNamespace, NamedQName, RefQName}
import org.apache.daffodil.processors.parsers.PState
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.schema.annotation.props.gen.VariableDirection

import scala.collection.mutable.{Map, ArrayBuffer}

sealed abstract class VariableState extends Serializable

/**
 * The variable has no value. Attempting to read the variable will result in an
 * error. This is the initial state of all variables. Variables will remain in
 * this state until their defaultValue expression (if it exists) is evaluated or
 * until the variable is set.
 */
case object VariableUndefined extends VariableState

/**
 * The variable is defined with either a default or externally provided value.
 * Both reading and setting are allowed.
 */
case object VariableDefined extends VariableState

/**
 * We are in the process of setting a default value for a defineVariable
 * statement and this state is used to catch circular definitions of variables.
 */
case object VariableBeingDefined extends VariableState

/**
 * The variable has been set to a value, but has not been read yet. Reading is
 * allowed, but multiple sets will result in an error.
 */
case object VariableSet extends VariableState

/**
 * The variable's value has been read. The value can no longer change. Setting
 * will cause an error.
 */
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
 * Class for maintaining the state of a variable
 */
object VariableInstance {
  def apply(rd: VariableRuntimeData) = {
    new VariableInstance(rd)
  }
}

/**
 * See documentation for object VariableInstance
 */
class VariableInstance private (val rd: VariableRuntimeData)
  extends Serializable {

  var state: VariableState = VariableUndefined
  var priorState: VariableState = VariableUndefined
  var value: DataValuePrimitiveNullable = DataValue.NoValue
  var priorValue: DataValuePrimitiveNullable = DataValue.NoValue

  // This represents the default value at the start of processing, provided by
  // either the defaultValue expression or by an external binding
  var firstInstanceInitialValue: DataValuePrimitiveNullable = DataValue.NoValue

  def setState(s: VariableState) = {
    this.priorState = this.state
    this.state = s
  }

  def setValue(v: DataValuePrimitiveNullable) = {
    this.priorValue = this.value
    this.value = v
  }

  /* This is used to set a default value without assigning a priorValue */
  def setDefaultValue(v: DataValuePrimitiveNullable) = {
    Assert.invariant((this.state == VariableUndefined || this.state == VariableInProcess) && v.isDefined)
    this.state = VariableDefined
    this.value = v
  }

  override def toString: String = "VariableInstance(%s,%s,%s,%s,%s,%s)".format(
                                                    state,
                                                    value,
                                                    rd,
                                                    rd.maybeDefaultValueExpr,
                                                    priorState,
                                                    priorValue)

  def copy(
    state: VariableState = state,
    value: DataValuePrimitiveNullable = value,
    rd: VariableRuntimeData = rd,
    priorState: VariableState = priorState,
    priorValue: DataValuePrimitiveNullable = priorValue) = {
      val inst = new VariableInstance(rd)
      inst.state = state
      inst.priorState = priorState
      inst.value = value
      inst.priorValue = priorValue
      inst
  }

  def reset() = {
    Assert.invariant(this.state != VariableUndefined)
    (this.state, this.priorState, this.rd.maybeDefaultValueExpr.isDefined) match {
      case (VariableRead, VariableSet, _) => this.setState(VariableSet)
      case (VariableRead, _, true) => this.setState(VariableDefined)
      case (VariableSet, _, _) => {
        this.setState(this.priorState)
        this.setValue(this.priorValue)
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

class VariableSuspended(qname: NamedQName, context: VariableRuntimeData) extends VariableException(qname, context,
  "Variable map (runtime): variable %s is currently suspended".format(qname))
  with RetryableException

/**
 * This expression can be thrown either at the start of parsing is the
 * expressions in a defineVariable are circular, or later during parsing if
 * newVariableInstance contains a circular expression
 */
class VariableCircularDefinition(qname: NamedQName, context: VariableRuntimeData) extends VariableException(qname, context,
  "Variable map (runtime): variable %s is part of a circular definition with other variables".format(qname))

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

  def cloneForSuspension(): VariableBox = new VariableBox(vmap.topLevelInstances)
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
 *
 * The implementation of the VariabeMap uses ArrayBuffers essentially as a
 * stack, as they allow for easy serialization unlike the custom MStack classes
 * we use elsewhere. Scala's mutable Stack is deprecated in 2.12
 */
class VariableMap private(vTable: Map[GlobalQName, ArrayBuffer[VariableInstance]])
  extends Serializable {

  def this(topLevelVRDs: Seq[VariableRuntimeData] = Nil) =
    this(Map(topLevelVRDs.map {
      vrd =>
        val variab = vrd.createVariableInstance()
        val variableInstances = new ArrayBuffer[VariableInstance]
        variableInstances += variab
        (vrd.globalQName, variableInstances)
    }: _*))

  override def toString(): String = {
    "VariableMap(" + vTable.mkString(" | ") + ")"
  }

  /**
   * Returns a full, deep copy of the current VariableMap. This is needed as
   * VariableInstances are mutable and cannot safely be shared across threads
   */
  def copy(): VariableMap = {
    val table = vTable.map { case (k: GlobalQName, variableInstances: ArrayBuffer[VariableInstance]) => {
      val newBuf = variableInstances.map { _.copy() }
      (k, newBuf)
    }}

    new VariableMap(table)
  }

  def topLevelInstances(): VariableMap = {
    val table = vTable.map { case (k: GlobalQName, variableInstances: ArrayBuffer[VariableInstance]) => {
      val newBuf = new ArrayBuffer[VariableInstance]()
      newBuf.append(variableInstances.last)
      (k, newBuf)
    }}

    new VariableMap(table)
  }

  // For defineVariable's with non-constant expressions for default values, it
  // is necessary to force the evaluation of the expressions after the
  // VariableMap has been created and initialized, but before parsing begins. We
  // must also ensure that the expressions only reference other variables that
  // have default value expressions or are defined externally.
  def forceExpressionEvaluations(state: ParseOrUnparseState): Unit = {
    vTable.foreach { case (_, variableInstances) => { variableInstances.foreach { inst => {
      (inst.state, inst.rd.maybeDefaultValueExpr.isDefined) match {
        // Evaluate defineVariable statements with non-constant default value expressions
        case (VariableUndefined, true) => {
          val res = inst.rd.maybeDefaultValueExpr.get.evaluate(state)
          inst.setDefaultValue(DataValue.unsafeFromAnyRef(res))
        }
        case (_, _) => // Do nothing
      }
    }}}}
  }


  /**
   * This function is called immediately after forceExpressionEvaluations in
   * order to set the firstInstanceInitialValue for each variable instance.
   * These initial values will be inherited by future new variable instances if
   * the newVariableInstance statement does not provide a default value
   * expression
   */
  def setFirstInstanceInitialValues(): Unit = {
    vTable.foreach { case (_, variableInstances) => {
      variableInstances(0).firstInstanceInitialValue = variableInstances(0).value
    }}
  }

  def find(qName: GlobalQName): Option[VariableInstance] = {
    val optBuf = vTable.get(qName)
    val variab = {
      if (optBuf.isDefined)
        Some(optBuf.get.last)
      else
        None
    }
    variab
  }

  def qnames(): Seq[GlobalQName] = {
    vTable.toSeq.map(_._1)
  }

  def getVariableRuntimeData(qName: GlobalQName): Option[VariableRuntimeData] = {
    val optVariable = find(qName)
    if (optVariable.isDefined) Some(optVariable.get.rd) else None
  }

  lazy val context = Assert.invariantFailed("unused.")

  /**
   * Used only for testing.
   */
  def getVariableBindings(qn: GlobalQName): ArrayBuffer[VariableInstance] = {
    vTable.get(qn).get
  }

  /**
   * Returns the value of a variable and sets the state of the variable to be
   * VariableRead.
   */
  def readVariable(vrd: VariableRuntimeData, referringContext: ThrowsSDE, pstate: ParseOrUnparseState): DataValuePrimitive = {
    val varQName = vrd.globalQName
    vrd.direction match {
      case VariableDirection.ParseOnly if (!pstate.isInstanceOf[PState]) =>
        pstate.SDE("Attempting to read variable %s which is marked as parseOnly during unparsing".format(varQName))
      case VariableDirection.UnparseOnly if (!pstate.isInstanceOf[UState]) =>
        pstate.SDE("Attempting to read variable %s which is marked as unparseOnly during parsing".format(varQName))
      case _ => // Do nothing
    }

    val variableInstances = vTable.get(varQName)
    if (variableInstances.isDefined) {
      val variable = variableInstances.get.last
      variable.state match {
        case VariableRead if (variable.value.isDefined) => variable.value.getNonNullable
        case VariableDefined | VariableSet if (variable.value.isDefined) => {
          if (pstate.isInstanceOf[PState])
            pstate.asInstanceOf[PState].markVariableRead(vrd)

          variable.setState(VariableRead)
          variable.value.getNonNullable
        }
        // This case is only hit for defineVariable's who's expression reference
        // other defineVariables with expressions. It will be hit at the start
        // of parsing, before an infoset is generated, via the
        // forceExpressionEvaluations function after which all variables should
        // have a defined value
        case VariableUndefined if (variable.rd.maybeDefaultValueExpr.isDefined) => {
          variable.setState(VariableBeingDefined)
          val res = DataValue.unsafeFromAnyRef(variable.rd.maybeDefaultValueExpr.get.evaluate(pstate))

          // Need to update the variable's value with the result of the
          // expression
          variable.setState(VariableRead)
          variable.setValue(res)

          res
        }
        case VariableBeingDefined => throw new VariableCircularDefinition(varQName, vrd)
        case VariableInProcess => throw new VariableSuspended(varQName, vrd)
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
    val variableInstances = vTable.get(varQName)
    if (variableInstances.isDefined) {
      val variable = variableInstances.get.last
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
          vrd.direction match {
            /**
             * Due to potential race conditions regarding the setting of
             * variables via setVariable and default values in cominbation with
             * suspesions during unparsing, we only allow the use of either
             * setVariable statements or a default value when unparsing a
             * variable.
             */
            case VariableDirection.UnparseOnly | VariableDirection.Both if (vrd.maybeDefaultValueExpr.isDefined && variableInstances.get.size > 1) => {
              // Variable has an unparse direction, a default value, and a
              // newVariableInstance
              pstate.SDE("Variable %s has an unparse direction and a default value, setting the variable may cause race conditions when combined with a forward referencing expression.", varQName)
            }
            case _ => // Do nothing
          }
          variable.setValue(VariableUtils.convert(newValue.getAnyRef.toString, variable.rd, referringContext))
          variable.setState(VariableSet)
        }
      }
    }
  }

  /**
   * Creates a new instance of a variable without default value
   */
  def newVariableInstance(vrd: VariableRuntimeData): VariableInstance = {
    val varQName = vrd.globalQName
    val variableInstances = vTable.get(varQName)
    Assert.invariant(variableInstances.isDefined)
    val nvi = vrd.createVariableInstance()
    nvi.firstInstanceInitialValue = variableInstances.get(0).firstInstanceInitialValue
    variableInstances.get += nvi
    nvi
  }

  def removeVariableInstance(vrd: VariableRuntimeData): Unit = {
    val varQName = vrd.globalQName
    val variableInstances = vTable.get(varQName)
    Assert.invariant(variableInstances.isDefined && variableInstances.get.nonEmpty)
    variableInstances.get.trimEnd(1)
  }


  private lazy val externalVarGlobalQNames: Seq[GlobalQName] =
    vTable.map { case (_, variableInstances) if (variableInstances.last.rd.external) => variableInstances.last.rd.globalQName }.toSeq

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

    val variableInstances = vTable.get(varQName.toGlobalQName)
    if (!variableInstances.isDefined)
      referringContext.schemaDefinitionError("unknown variable %s", varQName)
    else {
      val variable = variableInstances.get.last
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
          val value = VariableUtils.convert(newValue.getAnyRef.toString, variable.rd, referringContext)
          variable.setValue(value)
          variable.setState(VariableDefined)
        }
      }
    }
  }
}
