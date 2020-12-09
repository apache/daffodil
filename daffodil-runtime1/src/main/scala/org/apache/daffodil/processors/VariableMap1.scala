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
import org.apache.daffodil.util.PreSerialization
import org.apache.daffodil.util.TransientParam
import org.apache.daffodil.xml.{GlobalQName, UnspecifiedNamespace, NamedQName, RefQName}
import org.apache.daffodil.processors.parsers.PState
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.schema.annotation.props.gen.VariableDirection

import scala.collection.mutable.{Map, ArrayBuffer}

sealed abstract class VariableState extends Serializable

case object VariableUndefined extends VariableState

case object VariableDefined extends VariableState

/**
 * Used to detect circular definitions of variables
 */
case object VariableBeingDefined extends VariableState

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
 * Class for maintaining the state of a variable
 *
 * Note: stateArg, valueArg, and defaultValueExprArg are pass by name/lazy
 * values as it is necessary to postpone their evaluation beyond when the
 * VariableInstance is created. Attempting to evaluate these arguments will
 * likely trigger an expression to be evaluated, which will query the
 * VariableMap if the expression references another variable. If this is
 * occurring within a defineVariable call, the VariableMap hasn't been fully
 * created and attempting to evaluate such an expression will result in an OOLAG
 * Circular Definition Exception
 */
object VariableInstance {
  def apply(
    stateArg: => VariableState,
    valueArg: => DataValuePrimitiveNullable,
    rd: VariableRuntimeData,
    defaultValueExprArg: => Maybe[CompiledExpression[AnyRef]],
    priorState: VariableState = VariableUndefined,
    priorValue: DataValuePrimitiveNullable = DataValue.NoValue) = {

    new VariableInstance(stateArg, valueArg, rd, defaultValueExprArg, priorState, priorValue)
  }
}

/**
 * See documentation for object VariableInstance
 */
class VariableInstance private (
  @TransientParam stateArg: => VariableState,
  @TransientParam valueArg: => DataValuePrimitiveNullable,
  val rd: VariableRuntimeData,
  @TransientParam defaultValueExprArg: => Maybe[CompiledExpression[AnyRef]],
  var priorState: VariableState = VariableUndefined,
  var priorValue: DataValuePrimitiveNullable = DataValue.NoValue)
  extends Serializable
  with PreSerialization {

  lazy val defaultValueExpr = defaultValueExprArg

  private var _value: Option[DataValuePrimitiveNullable] = None
  private var _state: VariableState = null

  /**
   * Returns the current value of the VariableInstance
   *
   * This function allows value to be set while also enabling valueArg to be
   * lazily evaluated. This allows the VariableInstance to support setting the
   * value later and allows the construction of the VariableMap containing the
   * VariableInstance before evaluating the default value expression
   */
  def value = {
    if (_value.isEmpty) {
      _value = Some(valueArg)
    }

    _value.get
  }

  /**
   * Returns the current state of the VariableInstance
   *
   * This function allows state to be set while also enabling stateArg to be
   * lazily evaluated. This allows the VariableInstance to support setting the
   * state later and allows the construction of the VariableMap containing the
   * VariableInstance before evaluating the default value expression
   */
  def state = {
    if (_state == null) {
      _state = stateArg
    }

    _state
  }

  def setState(s: VariableState) = {
    this.priorState = this.state
    this._state = s
  }

  def setValue(v: DataValuePrimitiveNullable) = {
    this.priorValue = this.value
    this._value = Some(v)
  }

  def setDefaultValue(v: DataValuePrimitiveNullable) = this._value = Some(v)

  override def preSerialization: Unit = {
    defaultValueExpr
    value
    state
  }

  override def toString: String = "VariableInstance(%s,%s,%s,%s,%s,%s,%s)".format(
                                                    state,
                                                    value,
                                                    rd,
                                                    defaultValueExpr,
                                                    priorState,
                                                    priorValue,
                                                    this.hashCode)

  def copy(
    state: VariableState = state,
    value: DataValuePrimitiveNullable = value,
    rd: VariableRuntimeData = rd,
    defaultValueExpr: Maybe[CompiledExpression[AnyRef]] = defaultValueExpr,
    priorState: VariableState = priorState,
    priorValue: DataValuePrimitiveNullable = priorValue) = {
      val inst = new VariableInstance(state, value, rd, defaultValueExpr, priorState, priorValue)
      inst.init()
      inst
  }

  def init() = preSerialization

  def reset() = {
    Assert.invariant(this.state != VariableUndefined)
    (this.state, this.priorState, this.defaultValueExpr.isDefined) match {
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
      (inst.state, inst.value, inst.defaultValueExpr.isDefined) match {
        // Evaluate defineVariable statements with non-constant default value expressions
        case (VariableDefined, DataValue.NoValue, true) => {
          val res = inst.defaultValueExpr.get.evaluate(state)
          inst.setValue(DataValue.unsafeFromAnyRef(res))
        }
        case (_, _, _) => // Do nothing
      }
    }}}}
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
        case VariableDefined if (!variable.value.isDefined && variable.defaultValueExpr.isDefined) => {
          variable.setState(VariableBeingDefined)
          val res = DataValue.unsafeFromAnyRef(variable.defaultValueExpr.get.evaluate(pstate))

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
   * Creates a new instance of a variable with default value
   */
  def newVariableInstance(vrd: VariableRuntimeData, defaultValue: DataValuePrimitive) = {
    val varQName = vrd.globalQName
    val variableInstances = vTable.get(varQName)
    Assert.invariant(variableInstances.isDefined)

    variableInstances.get += vrd.createVariableInstance(VariableUtils.convert(defaultValue.getAnyRef.toString, vrd, vrd))
  }

  /**
   * Creates a new instance of a variable without default value
   */
  def newVariableInstance(vrd: VariableRuntimeData) = {
    val varQName = vrd.globalQName
    val variableInstances = vTable.get(varQName)
    Assert.invariant(variableInstances.isDefined)

    val v = vrd.createVariableInstance()
    variableInstances.get += v
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
          variable.setValue(VariableUtils.convert(newValue.getAnyRef.toString, variable.rd, referringContext))
          variable.setState(VariableDefined)
        }
      }
    }
  }
}
