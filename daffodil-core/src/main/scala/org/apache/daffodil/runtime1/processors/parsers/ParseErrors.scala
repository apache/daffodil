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

package org.apache.daffodil.runtime1.processors.parsers

import org.apache.daffodil.api
import org.apache.daffodil.io.processors.charset.BitsCharsetDecoderUnalignedCharDecodeException
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.SchemaFileLocation
import org.apache.daffodil.lib.iapi.DataLocation
import org.apache.daffodil.lib.iapi.Diagnostic
import org.apache.daffodil.lib.iapi.ThinDiagnostic
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.runtime1.dsom.RuntimeSchemaDefinitionError
import org.apache.daffodil.runtime1.dsom.SchemaDefinitionDiagnosticBase
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.ProcessingError

class ParseError(
  rd: Maybe[SchemaFileLocation],
  val loc: Maybe[DataLocation],
  causedBy: Maybe[Throwable],
  kind: Maybe[String],
  args: Any*
) extends ProcessingError("Parse", rd, loc, causedBy, kind, args: _*) {
  def this(rd: Maybe[SchemaFileLocation], loc: Maybe[DataLocation], kind: String, args: Any*) =
    this(rd, loc, Maybe.Nope, Maybe(kind), args: _*)

  override def toParseError = this
}

final class CharsetNotByteAlignedError(
  pstate: PState,
  cause: BitsCharsetDecoderUnalignedCharDecodeException
) extends ParseError(
    Maybe.toMaybe(pstate.maybeERD.toOption.map { _.schemaFileLocation }),
    One(pstate.currentLocation),
    One(cause),
    Nope
  )

class AssertionFailed(
  rd: SchemaFileLocation,
  state: PState,
  msg: String,
  details: Maybe[String] = Nope
) extends ParseError(One(rd), One(state.currentLocation), "Assertion failed: %s", msg) {
  override def componentText: String = {

    if (details.isDefined) "\nDetails: " + details.get
    else ""

  }
}

class ChoiceBranchFailed(rd: SchemaFileLocation, state: PState, val errors: Seq[api.Diagnostic])
  extends ParseError(
    One(rd),
    One(state.currentLocation),
    "Alternative failed. Reason(s): %s",
    errors
  )

class EntireChoiceFailed(rd: SchemaFileLocation, state: PState, diags: Seq[Diagnostic])
  extends ParseError(
    One(rd),
    One(state.currentLocation),
    "All choice alternatives failed. Reason(s): %s",
    diags
  ) {

  override def getLocationsInSchemaFiles: java.util.List[api.LocationInSchemaFile] =
    diags.flatMap {
      _.getLocationsInSchemaFiles
    }

  override def getDataLocations: java.util.List[api.DataLocation] = {
    // all should have the same starting location if they are alternatives.
    val dataLocs = diags.flatMap { _.getDataLocations }
    // TBD: what is the idiom for "insert a equals sign between all the elements of the list...??"
    // Well, this works, but isn't there a one-liner for this idiom.
    val allAreSame = dataLocs match {
      case f :: r => !r.exists { _ != f }
      case _ => true
    }
    Assert.invariant(allAreSame)
    diags.head.getDataLocations
  }
}

class ChoiceDispatchNoMatch(rd: SchemaFileLocation, state: PState, val key: String)
  extends ParseError(
    One(rd),
    One(state.currentLocation),
    "Choice dispatch key (%s) failed to match any of the branch keys.",
    key
  )

class ChoiceDispatchFailed(
  rd: SchemaFileLocation,
  state: PState,
  val errors: Seq[api.Diagnostic]
) extends ParseError(
    One(rd),
    One(state.currentLocation),
    "Choice dispatch branch failed: %s",
    errors
  )

class GeneralParseFailure(msg: String) extends ThinDiagnostic(Nope, Nope, Nope, Maybe(msg)) {
  Assert.usage(msg != null && msg != "")
  override def isError = true
  override def modeName = "Parse"
}

/**
 * Mixin for signaling Schema Definition Errors at runtime.
 *
 * Some SDE cannot be detected until runtime. Classes that need to signal them
 * mixin this trait.
 */
trait DoSDEMixin {

  protected final def doSDE(e: Throwable, state: ParseOrUnparseState) = {
    e match {
      case sde: SchemaDefinitionDiagnosticBase => {
        state.setFailed(sde)
        throw sde
      }
      case other => {
        val sde =
          new RuntimeSchemaDefinitionError(
            state.getContext().schemaFileLocation,
            e,
            e.getMessage
          )
        state.setFailed(sde)
        state.toss(sde)
      }
    }
  }
}
