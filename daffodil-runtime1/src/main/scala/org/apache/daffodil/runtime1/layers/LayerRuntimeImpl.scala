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
package org.apache.daffodil.runtime1.layers

import java.nio.charset.Charset

import org.apache.daffodil.lib.exceptions.SchemaFileLocation
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.runtime1.infoset.DataValue
import org.apache.daffodil.runtime1.layers.api.LayerRuntime
import org.apache.daffodil.runtime1.layers.api.LayerVariable
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.SequenceRuntimeData
import org.apache.daffodil.runtime1.processors.VariableRuntimeData
import org.apache.daffodil.runtime1.processors.parsers.PState
import org.apache.daffodil.runtime1.processors.parsers.ParseError
import org.apache.daffodil.runtime1.processors.unparsers.UState
import org.apache.daffodil.runtime1.processors.unparsers.UnparseError

class LayerRuntimeImpl(val state: ParseOrUnparseState, val srd: SequenceRuntimeData)
  extends LayerRuntime {

  override def processingError(cause: Throwable): Nothing =
    processingError(Maybe(cause), Maybe.Nope)

  override def processingError(msg: String): Nothing =
    processingError(Maybe.Nope, Maybe(msg))

  private def processingError(mCause: Maybe[Throwable], mMsg: Maybe[String]): Nothing = {
    val diagnostic = state match {
      case ps: PState =>
        new ParseError(
          rd = Maybe(srd.schemaFileLocation),
          loc = Maybe(state.currentLocation),
          causedBy = mCause,
          kind = mMsg,
        )
      case us: UState =>
        new UnparseError(
          rd = Maybe(srd.schemaFileLocation),
          loc = Maybe(state.currentLocation),
          causedBy = mCause,
          kind = mMsg,
        )
    }
    throw diagnostic
  }

  override def runtimeSchemaDefinitionError(msg: String, args: AnyRef*): Nothing =
    state.SDE(msg, args: _*)

  override def runtimeSchemaDefinitionError(cause: Throwable): Nothing =
    state.SDE(cause)

  override def getString(variable: LayerVariable): String =
    state
      .getVariable(variable.asInstanceOf[VariableRuntimeData], srd)
      .getString

  override def setString(variable: LayerVariable, s: String): Unit =
    state.setVariable(
      variable.asInstanceOf[VariableRuntimeData],
      DataValue.toDataValue(s),
      srd,
    )

  override def getInt(variable: LayerVariable): Int =
    state
      .getVariable(variable.asInstanceOf[VariableRuntimeData], srd)
      .getInt

  override def setInt(variable: LayerVariable, v: Int): Unit =
    state.setVariable(
      variable.asInstanceOf[VariableRuntimeData],
      DataValue.toDataValue(v),
      srd,
    )

  override def schemaDefinitionError(msg: String, args: AnyRef*): Unit =
    runtimeSchemaDefinitionError(msg, args)

  override def getCharset(layerEncoding: String): Charset = Charset.forName(layerEncoding)

  override def layerVariable(namespaceURI: String, varName: String): LayerVariable =
    srd.layerVariable(namespaceURI, varName)

  override def getLong(variable: LayerVariable): Long =
    state
      .getVariable(variable.asInstanceOf[VariableRuntimeData], srd)
      .getLong

  override def setLong(variable: LayerVariable, v: Long): Unit =
    state.setVariable(
      variable.asInstanceOf[VariableRuntimeData],
      DataValue.toDataValue(v),
      srd,
    )

  override def layerName(): String = srd.layerName()

  override def schemaFileLocation(): SchemaFileLocation = srd.schemaFileLocation
}
