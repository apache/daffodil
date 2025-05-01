/*
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.apache.daffodil.runtime1.layers

import java.lang.reflect.Constructor
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method

import org.apache.daffodil.lib.calendar.DFDLDate
import org.apache.daffodil.lib.calendar.DFDLDateTime
import org.apache.daffodil.lib.calendar.DFDLTime
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType
import org.apache.daffodil.runtime1.infoset.DataValue
import org.apache.daffodil.runtime1.layers.api.Layer
import org.apache.daffodil.runtime1.processors.VariableRuntimeData

import com.ibm.icu.util.Calendar

/**
 * Enables fast construction of the layer instance passing all parameter vars values
 * as arguments to the parameter setter method.
 *
 * Also contains the data structures which facilitate fast invocation of the getters for
 * any return result values, and assignment of those values to the layer result variables.
 *
 * This object is NOT serializable. It is transient. It is created and discarded
 * when a DFDL schema that uses a layer is compiled. It is re-created at runtime when
 * such a schema is used for parse/unparse.
 */
class LayerVarsRuntime(
  constructor: Constructor[_],
  optParamSetter: Option[Method],
  paramVRDs: Seq[VariableRuntimeData],
  resultVarPairs: Seq[(VariableRuntimeData, Method)]
) {

  def constructInstance(): Layer = constructor.newInstance().asInstanceOf[Layer]

  /**
   * Assembles the parameter variables in the proper order, and gets all their values, then
   * calls the parameter setter to initialize the layer instance, providing all the
   * parameter variables as args to the constructor.
   *
   * Note that zero parameters is an allowed case. In that case this calls the setter
   * just for initialization of the layer.
   *
   * @param layer the layer instance to initialize
   */
  def callParamSetterWithParameterVars(layer: Layer): Unit = {
    val state = layer.getLayerRuntime.state
    val args = paramVRDs.map { vrd =>
      vrd.primType match {
        case PrimType.Date =>
          state.getVariable(vrd, state).value.asInstanceOf[DFDLDate].calendar
        case PrimType.Time =>
          state.getVariable(vrd, state).value.asInstanceOf[DFDLTime].calendar
        case PrimType.DateTime =>
          state.getVariable(vrd, state).value.asInstanceOf[DFDLDateTime].calendar
        case _ => {
          val v: AnyRef = state.getVariable(vrd, state).value
          v
        }
      }
    }
    optParamSetter.foreach { paramSetter =>
      try {
        paramSetter.invoke(layer, args: _*)
      } catch {
        case ite: InvocationTargetException =>
          // unwrap and re-throw. We don't care if it was reflective call.
          val cause = ite.getCause
          throw cause
      }
    }
  }

  /**
   * Calls getter methods on the layer for the return value variables, and
   * assigns the gotten result values to the return value variables.
   *
   * @param layer the layer from which we are getting the result values
   *
   *              When parsing this is called in the unwinding when we remove the layer.
   *
   *              When unparsing it's trickier. We call this from the close of the data output stream
   *              that underlies the layer. That is, from the close() method of
   *              `runtime1.layers.JavaIOOutputStream`.
   */
  def callGettersToPopulateResultVars(layer: Layer): Unit = {
    val state = layer.getLayerRuntime.state
    resultVarPairs.foreach { case (vrd, method) =>
      val value: AnyRef = {
        val raw =
          try {
            method.invoke(layer)
          } catch {
            case ite: InvocationTargetException =>
              // unwrap because we don't care if it was a reflective call.
              val cause = ite.getCause
              throw cause
          }

        vrd.primType match {
          case PrimType.Date => {
            val d = raw.asInstanceOf[Calendar]
            DFDLDate(d, hasTimeZone = false)
          }
          case PrimType.Time => {
            val d = raw.asInstanceOf[Calendar]
            DFDLTime(d, hasTimeZone = false)
          }
          case PrimType.DateTime => {
            val d = raw.asInstanceOf[Calendar]
            DFDLDateTime(d, hasTimeZone = false)
          }
          case _ => raw
        }
      }
      val dv = DataValue.unsafeFromAnyRef(value)
      state.setVariable(vrd, dv, state)
    }
  }
}
