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

package org.apache.daffodil.runtime1.processors

import org.apache.daffodil.api.exceptions.{
  ExternalVariableException => JExternalVariableException
}
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.externalvars.Binding
import org.apache.daffodil.lib.iapi.ThinDiagnostic
import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.schema.annotation.props.gen.VariableDirection
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.xml.GlobalQName
import org.apache.daffodil.lib.xml.NamedQName
import org.apache.daffodil.lib.xml.RefQName
import org.apache.daffodil.lib.xml.UnspecifiedNamespace
import org.apache.daffodil.runtime1.dpath.InvalidPrimitiveDataException
import org.apache.daffodil.runtime1.infoset.DataValue
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitiveNullable
import org.apache.daffodil.runtime1.infoset.RetryableException
import org.apache.daffodil.runtime1.processors.parsers.PState
import org.apache.daffodil.runtime1.processors.unparsers.UState

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
class VariableInstance private (val rd: VariableRuntimeData) extends Serializable {

  var state: VariableState = VariableUndefined
  var value: DataValuePrimitiveNullable = DataValue.NoValue

  // This represents the default value at the start of processing, provided by
  // either the defaultValue expression or by an external binding
  var firstInstanceInitialValue: DataValuePrimitiveNullable = DataValue.NoValue

  def setState(s: VariableState): Unit = {
    this.state = s
  }

  def setValue(v: DataValuePrimitiveNullable): Unit = {
    this.value = v
  }

  /* This is used to set a default value with the appropriate state */
  def setDefaultValue(v: DataValuePrimitiveNullable): Unit = {
    Assert.invariant(
      (this.state == VariableUndefined || this.state == VariableInProcess) && v.isDefined
    )
    this.state = VariableDefined
    this.value = v
  }

  override def toString: String =
    "VariableInstance(%s,%s,%s,%s)".format(state, value, rd, rd.maybeDefaultValueExpr)

  def copy(
    state: VariableState = state,
    value: DataValuePrimitiveNullable = value,
    rd: VariableRuntimeData = rd
  ): VariableInstance = {
    val inst = new VariableInstance(rd)
    inst.state = state
    inst.value = value
    inst
  }

}

object VariableUtils {

  def setExternalVariables(
    currentVMap: VariableMap,
    bindings: Seq[Binding],
    referringContext: ThrowsSDE
  ): Unit = {
    bindings.foreach { b =>
      currentVMap.setExtVariable(b.varQName, b.varValue, referringContext)
    }
  }

}

abstract class VariableException(
  val qname: NamedQName,
  val context: VariableRuntimeData,
  msg: String
) extends ThinDiagnostic(Maybe(context.schemaFileLocation), Nope, Nope, Maybe(msg)) {
  def isError = true

  def modeName = "Variable"
}

class VariableHasNoValue(qname: NamedQName, context: VariableRuntimeData)
  extends VariableException(
    qname,
    context,
    "Variable map (runtime): variable %s has no value. It was not set, and has no default value."
      .format(qname)
  )
  with RetryableException

class VariableSuspended(qname: NamedQName, context: VariableRuntimeData)
  extends VariableException(
    qname,
    context,
    "Variable map (runtime): variable %s is currently suspended".format(qname)
  )
  with RetryableException

/**
 * This expression can be thrown either at the start of parsing is the
 * expressions in a defineVariable are circular, or later during parsing if
 * newVariableInstance contains a circular expression
 */
class VariableCircularDefinition(qname: NamedQName, context: VariableRuntimeData)
  extends VariableException(
    qname,
    context,
    "Variable map (runtime): variable %s is part of a circular definition with other variables"
      .format(qname)
  )

/**
 * Provides one more indirection to the variable map.
 *
 * Needed so that when unparsing multiple clones of a UState can share
 * and modify, the same VMap.
 *
 * This is a new mechanism, which allows for less re-allocation of VariableMaps.
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

  def vmap: VariableMap = vmap_

  def setVMap(newMap: VariableMap): Unit = {
    vmap_ = newMap
  }

  def cloneForSuspension(): VariableBox = new VariableBox(vmap.cloneForSuspension())
}

object VariableMap {
  def apply(vrds: Seq[VariableRuntimeData] = Nil): VariableMap = {
    val table = new Array[Seq[VariableInstance]](vrds.size)
    vrds.foreach { vrd =>
      table(vrd.vmapIndex) = Seq(vrd.createVariableInstance())
    }
    new VariableMap(vrds, table)
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
 *
 * The VariableMap implementation essentially uses Seq as a stack, as they allow for easy
 * serialization unlike the custom MStack classes we use elsewhere. Note that Scala's mutable
 * Stack is deprecated in 2.12. Additionally, this stack is rarely changed (only when
 * newVariableInstance occurs) so limiting allocations is not critical. Further, suspensions
 * create a copy of the vTable, which is a relatively expensive operation if using something
 * else like an ArrayBuffer or MStack, since Seq's are immutable and copies are virtually free.
 *
 * Note that each index in the vTable array contains the VariableInstance stack for a specific
 * variable, with that index defined by the "vmapIndex" member of the variables
 * VariableRuntimeData. This implementation allows for constant lookups by indexing into the
 * vTable array and accessing the head of the VariableInstance sequence. Additionally, in some
 * cases (like with suspensions) we have to make a copy of the array. This is the primary reason
 * for using an Array as opposed to another data structure that allows constant lookups like a
 * Map--array copies are quite fast, and we know the exact size since all variables must be
 * defined at compile time. Although this adds extra complexity compared to a Map since we must
 * carry around an index, the performance gains are worth it.
 */
class VariableMap private (
  val vrds: Seq[VariableRuntimeData],
  vTable: Array[Seq[VariableInstance]]
) extends Serializable {

  override def toString: String = {
    "VariableMap(" + vTable.mkString(" | ") + ")"
  }

  /**
   * Returns a full, deep copy of the current VariableMap. This is needed as
   * VariableInstances are mutable and cannot safely be shared across threads
   */
  def copy(): VariableMap = {
    val table = vTable.map { variableInstances =>
      val newBuf = variableInstances.map { _.copy() }
      newBuf
    }

    new VariableMap(vrds, table)
  }

  def cloneForSuspension(): VariableMap = {
    val newTable = new Array[Seq[VariableInstance]](vTable.length)
    Array.copy(vTable, 0, newTable, 0, vTable.length)
    new VariableMap(vrds, newTable)
  }

  // For defineVariable's with non-constant expressions for default values, it
  // is necessary to force the evaluation of the expressions after the
  // VariableMap has been created and initialized, but before parsing begins. We
  // must also ensure that the expressions only reference other variables that
  // have default value expressions or are defined externally.
  def forceExpressionEvaluations(state: ParseOrUnparseState): Unit = {
    vTable.foreach { variableInstances =>
      val inst = variableInstances.head
      if ((inst.state eq VariableUndefined) && inst.rd.maybeDefaultValueExpr.isDefined) {
        val res = inst.rd.maybeDefaultValueExpr.get.evaluate(state)
        inst.setDefaultValue(DataValue.unsafeFromAnyRef(res))
      }
    }
  }

  /**
   * This function is called immediately after forceExpressionEvaluations in
   * order to set the firstInstanceInitialValue for each variable instance.
   * These initial values will be inherited by future new variable instances if
   * the newVariableInstance statement does not provide a default value
   * expression
   */
  def setFirstInstanceInitialValues(): Unit = {
    vTable.foreach { variableInstances =>
      variableInstances.head.firstInstanceInitialValue = variableInstances.head.value
    }
  }

  /**
   * Performance of this is linear in number of variables, this should not be used in
   * performance critical sections.
   */
  def find(qName: RefQName): Option[VariableInstance] = {
    getVariableRuntimeData(qName).map { vrd => vTable(vrd.vmapIndex).head }
  }
  def find(qName: GlobalQName): Option[VariableInstance] = find(qName.toRefQName)

  /**
   * Performance of this is linear in number of variables, this should not be used in
   * performance critical sections.
   */
  lazy val qnames: Seq[GlobalQName] = {
    vrds.map { _.globalQName }
  }

  /**
   * Performance of this is linear in number of variables, this should not be used in
   * performance critical sections.
   */
  def getVariableRuntimeData(qName: RefQName): Option[VariableRuntimeData] = {
    vrds.find { _.globalQName.matches(qName) }
  }

  lazy val context = Assert.invariantFailed("unused.")

  /**
   * Determine if a call to readVariable will change any state in this
   * VariableMap. This should only be used for optimizations and should not
   * prevent readVariable from actually being called. It does not do any
   * checking related to SDE's or invariant checking that readVariable might
   * perform
   */
  def readVariableWillChangeState(vrd: VariableRuntimeData): Boolean = {
    val variable = vTable(vrd.vmapIndex).head
    val variableRead = (variable.state eq VariableRead) && variable.value.isDefined
    !variableRead
  }

  /**
   * Returns the value of a variable and sets the state of the variable to be
   * VariableRead.
   */
  def readVariable(
    vrd: VariableRuntimeData,
    referringContext: ThrowsSDE,
    state: ParseOrUnparseState
  ): DataValuePrimitive = {
    val varQName = vrd.globalQName
    vrd.direction match {
      case VariableDirection.ParseOnly if (!state.isInstanceOf[PState]) =>
        state.SDE(
          "Attempting to read variable %s which is marked as parseOnly during unparsing".format(
            varQName
          )
        )
      case VariableDirection.UnparseOnly if (!state.isInstanceOf[UState]) =>
        state.SDE(
          "Attempting to read variable %s which is marked as unparseOnly during parsing".format(
            varQName
          )
        )
      case _ => // Do nothing
    }

    val variable = {
      // The vrd.vmapIndex cannot be out of range of the vTable, because the vTable size
      // accommodates as many variables as are in the top-level schema.
      val varAtIndex = vTable(vrd.vmapIndex).head
      val varAtIndexVRD = varAtIndex.rd
      if (varAtIndexVRD != vrd) {
        // The variable at that index does not have our VRD.
        // A newVariableInstance must be in place for this variable, but we're trying to access
        // using a different VRD (probably the top-level VRD you get from the schemaSet variable map.
        // These MUST, however, be for the same variable, and so will have the same index
        // into the vtable.
        Assert.invariant(
          (varAtIndexVRD eq vrd) || (varAtIndexVRD.globalQName eq vrd.globalQName)
        )
      }
      varAtIndex
    }
    variable.state match {
      case VariableRead if (variable.value.isDefined) => variable.value.getNonNullable
      case VariableDefined | VariableSet if (variable.value.isDefined) => {
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
        val res =
          DataValue.unsafeFromAnyRef(variable.rd.maybeDefaultValueExpr.get.evaluate(state))

        // Need to update the variable's value with the result of the
        // expression
        variable.setState(VariableRead)
        variable.setValue(res)

        res
      }
      case VariableBeingDefined => throw new VariableCircularDefinition(varQName, vrd)
      case VariableInProcess => throw new VariableSuspended(varQName, vrd)
      case _ =>
        throw new VariableHasNoValue(varQName, vrd)
    }
  }

  /**
   * Assigns a variable and sets the variables state to VariableSet
   */
  def setVariable(
    vrd: VariableRuntimeData,
    newValue: DataValuePrimitive,
    referringContext: ThrowsSDE,
    pstate: ParseOrUnparseState
  ): Unit = {
    val varQName = vrd.globalQName
    val variableInstances = vTable(vrd.vmapIndex)
    val variable = variableInstances.head
    variable.state match {
      case VariableSet => {
        referringContext.SDE(
          "Cannot set variable %s twice. State was: %s. Existing value: %s",
          variable.rd.globalQName,
          VariableSet,
          variable.value
        )
      }

      case VariableRead => {

        /**
         * TODO: This should be an SDE, but due to a bug (DAFFODIL-1443) in
         * the way we evaluate escapeSchemes it could lead us to setting the
         * variable read too early */
        pstate.SDW(
          WarnID.VariableSet,
          "Cannot set variable %s after reading the default value. State was: %s. Existing value: %s",
          variable.rd.globalQName,
          VariableRead,
          variable.value
        )

        variable.setValue(newValue)
        variable.setState(VariableSet)
      }

      case _ => {
        vrd.direction match {
          /**
           * Due to potential race conditions regarding the setting of
           * variables via setVariable and default values in combination with
           * suspensions during unparsing, we only allow the use of either
           * setVariable statements or a default value when unparsing a
           * variable.
           */
          case VariableDirection.UnparseOnly | VariableDirection.Both
              if (vrd.maybeDefaultValueExpr.isDefined && variableInstances.size > 1) => {
            // Variable has an unparse direction, a default value, and a
            // newVariableInstance
            pstate.SDE(
              "Variable %s has an unparse direction and a default value, setting the variable may cause race conditions when combined with a forward referencing expression.",
              varQName
            )
          }
          case _ => // Do nothing
        }
        variable.setValue(newValue)
        variable.setState(VariableSet)
      }
    }
  }

  /**
   * Creates a new instance of a variable without default value
   */
  def newVariableInstance(vrd: VariableRuntimeData): VariableInstance = {
    val variableInstances = vTable(vrd.vmapIndex)
    val nvi = vrd.createVariableInstance()
    nvi.firstInstanceInitialValue = variableInstances.head.firstInstanceInitialValue
    vTable(vrd.vmapIndex) = nvi +: variableInstances
    nvi
  }

  def removeVariableInstance(vrd: VariableRuntimeData): Unit = {
    val variableInstances = vTable(vrd.vmapIndex)
    Assert.invariant(variableInstances.nonEmpty)
    vTable(vrd.vmapIndex) = variableInstances.tail
  }

  /**
   * Assigns an external variable and sets the variables state to VariableSet
   */
  def setExtVariable(
    bindingQName: RefQName,
    newValue: String,
    referringContext: ThrowsSDE
  ): Unit = {

    val optVariableInstances =
      if (bindingQName.namespace == UnspecifiedNamespace) {
        // The external variable binding was hoping to get away with not
        // specifying a namespace. Let's try to find an unambiguous variable
        // using just the local name. Note that this ignores the external value
        // of variables. If we find a single variable, we'll check it's
        // external value later. If we find two or more variables with the same
        // name, we just consider that an ambiguity error. There is a chance of
        // solving this ambiguity if only one of those varibles was external,
        // but it's safer to err on the side of caution and require the user to
        // be explicit about which variable they intended to set.
        val candidates = vrds.filter { vrd =>
          vrd.globalQName.local == bindingQName.local
        }
        candidates.size match {
          case 0 => None
          case 1 => Some(vTable(candidates.head.vmapIndex))
          case _ => {
            val msg =
              "External variable binding %s is ambiguous. A namespace is required to resolve the ambiguity. Found variables: %s"
                .format(bindingQName, candidates.map(_.globalQName.toString).mkString(", "))
            throw new ExternalVariableException(msg)
          }
        }
      } else {
        val optVrd = vrds.find { _.globalQName == bindingQName.toGlobalQName }
        optVrd.map { vrd => vTable(vrd.vmapIndex) }
      }

    optVariableInstances match {
      case None =>
        throw new ExternalVariableException("Variable definition not found: " + bindingQName)
      case Some(variableInstances) => {
        // This array of VariableInstances comes from the VariableMap that is
        // part of the DataProcessor before being copied to pstate/ustate when
        // a parse/unparse is started. So this array should contain only a
        // single instance
        Assert.invariant(variableInstances.size == 1)
        val variable = variableInstances.head
        if (!variable.rd.external) {
          throw new ExternalVariableException(
            "Variable cannot be set externally: " + variable.rd.globalQName
          )
        }
        variable.state match {
          case VariableDefined | VariableUndefined => {
            val value =
              try {
                variable.rd.primType.fromXMLString(newValue)
              } catch {
                case e: InvalidPrimitiveDataException => {
                  val msg = "Value for variable %s is not a valid %s: %s".format(
                    variable.rd.globalQName,
                    variable.rd.primType.globalQName,
                    newValue
                  )
                  throw new ExternalVariableException(msg)
                }
              }
            variable.setValue(value)
            variable.setState(VariableDefined)
          }
          // $COVERAGE-OFF$
          case _ => Assert.impossible()
          // $COVERAGE-ON$
        }
      }
    }
  }
}

class ExternalVariableException(message: String) extends JExternalVariableException(message)
