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

import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.lib.api.Diagnostic
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.runtime1.dsom.RuntimeSchemaDefinitionError
import org.apache.daffodil.runtime1.layers.api.LayerException
import org.apache.daffodil.runtime1.layers.api.LayerRuntime
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.ProcessingError
import org.apache.daffodil.runtime1.processors.parsers.PState
import org.apache.daffodil.runtime1.processors.parsers.ParseError
import org.apache.daffodil.runtime1.processors.unparsers.UState
import org.apache.daffodil.runtime1.processors.unparsers.UnparseError

class LayerRuntimeImpl(val state: ParseOrUnparseState, lrd: LayerRuntimeData)
  extends LayerRuntime {

  final override def layerName(): String = lrd.localName

  final def layerRuntimeData: LayerRuntimeData = lrd

  final def finfo: FormatInfo = state

  override def processingError(cause: Throwable): Nothing =
    lrd.toss(toProcessingError(new LayerException(this, cause)))

  override def processingError(msg: String): Nothing =
    lrd.toss(toProcessingError(new LayerException(this, msg)))

  def toProcessingError(layerException: LayerException): ProcessingError = {
    val mCause = Maybe(layerException.getCause)
    val mMsg = Maybe(layerException.getMessage)
    val diagnostic = state match {
      case ps: PState =>
        new ParseError(
          rd = Maybe(lrd.schemaFileLocation),
          loc = Maybe(state.currentLocation),
          causedBy = mCause,
          kind = mMsg,
        )
      case us: UState =>
        new UnparseError(
          rd = Maybe(lrd.schemaFileLocation),
          loc = Maybe(state.currentLocation),
          causedBy = mCause,
          kind = mMsg,
        )
    }
    diagnostic
  }

  def toSchemaDefinitionError(mCause: Maybe[Throwable], mMsg: Maybe[String]): Diagnostic = {
    val ctxt = state.getContext()
    val rsde = new RuntimeSchemaDefinitionError(
      ctxt.schemaFileLocation,
      state,
      mCause.orNull,
      mMsg.orNull,
    )
    rsde
  }

  override def runtimeSchemaDefinitionError(msg: String, args: AnyRef*): Nothing =
    state.SDE(msg, args: _*)

  override def runtimeSchemaDefinitionError(cause: Throwable): Nothing =
    state.SDE(cause)
}
