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
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.ProcessingError
import org.apache.daffodil.runtime1.processors.parsers.PState
import org.apache.daffodil.runtime1.processors.parsers.ParseError
import org.apache.daffodil.runtime1.processors.unparsers.UState
import org.apache.daffodil.runtime1.processors.unparsers.UnparseError

/**
 * Glues together the state and the static layer runtime data
 */
class LayerRuntime(val state: ParseOrUnparseState, val layerRuntimeData: LayerRuntimeData) {

  private def lrd = layerRuntimeData

  final def finfo: FormatInfo = state

  def processingError(cause: Throwable): Nothing =
    state.toss(toProcessingError(cause))

  def processingError(msg: String): Nothing =
    state.toss(toProcessingError(msg))

  def toProcessingError(msg: String): ProcessingError = {
    val diagnostic = state match {
      case ps: PState =>
        new ParseError(
          rd = Maybe(lrd.schemaFileLocation),
          loc = Maybe(state.currentLocation),
          causedBy = Maybe.Nope,
          kind = Maybe(msg),
        )
      case us: UState =>
        new UnparseError(
          rd = Maybe(lrd.schemaFileLocation),
          loc = Maybe(state.currentLocation),
          causedBy = Maybe.Nope,
          kind = Maybe(msg),
        )
    }
    diagnostic
  }

  def toProcessingError(e: Throwable): ProcessingError = {
    val diagnostic = state match {
      case ps: PState =>
        new ParseError(
          rd = Maybe(lrd.schemaFileLocation),
          loc = Maybe(state.currentLocation),
          causedBy = Maybe(e),
          kind = Maybe.Nope,
        )
      case us: UState =>
        new UnparseError(
          rd = Maybe(lrd.schemaFileLocation),
          loc = Maybe(state.currentLocation),
          causedBy = Maybe(e),
          kind = Maybe.Nope,
        )
    }
    diagnostic
  }

  def runtimeSchemaDefinitionError(msg: String): Nothing =
    state.SDE(msg)

  def runtimeSchemaDefinitionError(cause: Throwable): Nothing =
    state.SDE(cause)
}
