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

package org.apache.daffodil.runtime1.dsom

import org.apache.daffodil.lib.exceptions.*
import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.lib.iapi.Diagnostic
import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.*
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState

class SchemaDefinitionError(
  schemaContext: Option[SchemaFileLocation],
  annotationContext: Option[SchemaFileLocation],
  fmtString: String,
  args: Any*
) extends SchemaDefinitionDiagnosticBase(
    schemaContext,
    None,
    annotationContext,
    Nope,
    Maybe(fmtString),
    args*
  ) {

  def this(sc: SchemaFileLocation, kind: String, args: Any*) =
    this(Some(sc), None, kind, args*)

  def isError = true
  def modeName = "Schema Definition"

}

class SchemaDefinitionErrorFromWarning(sdw: SchemaDefinitionWarning)
  extends SchemaDefinitionWarning(
    sdw.warnID,
    sdw.schemaContext,
    sdw.annotationContext,
    sdw.kind,
    sdw.args*
  ) {

  override def isError = true
  override def modeName = super.modeName + " Warning Escalated"

}

/**
 * Specific class used for this specific error, because we need to pick this off
 * in the debugger for special handling.
 */
class RelativePathPastRootError(schemaContext: SchemaFileLocation, kind: String, args: Any*)
  extends SchemaDefinitionError(Some(schemaContext), None, kind, args*)

class RuntimeSchemaDefinitionError(
  schemaContext: SchemaFileLocation,
  causedBy: Throwable,
  fmtString: String,
  args: Any*
) extends SchemaDefinitionError(
    Option(schemaContext),
    None,
    fmtString,
    args*
  ) {

  def this(
    schemaContext: SchemaFileLocation,
    fmtString: String,
    args: Any*
  ) =
    this(schemaContext, null, fmtString, args*)

  override def modeName = "Runtime Schema Definition"

  override def getCause: Throwable = causedBy
}

class RuntimeSchemaDefinitionWarning(
  warnID: WarnID,
  schemaContext: SchemaFileLocation,
  kind: String,
  args: Any*
) extends SchemaDefinitionWarning(
    warnID,
    Some(schemaContext),
    None,
    kind,
    args*
  ) {

  override def modeName = "Runtime Schema Definition"

}

class SchemaDefinitionWarning(
  val warnID: WarnID,
  val schemaContext: Option[SchemaFileLocation],
  val annotationContext: Option[SchemaFileLocation],
  val kind: String,
  val args: Any*
) extends SchemaDefinitionDiagnosticBase(
    schemaContext,
    None,
    annotationContext,
    Nope,
    Maybe(kind + s" (id: ${warnID})"),
    args*
  ) {

  def this(w: WarnID, sc: SchemaFileLocation, kind: String, args: Any*) =
    this(w, Some(sc), None, kind, args*)

  override def isError = false
  def modeName = "Schema Definition"

}

class ValidationError(
  maybeSchemaContext: Maybe[SchemaFileLocation],
  maybeRuntimeContext: Maybe[ParseOrUnparseState],
  maybeCause: Maybe[Throwable],
  maybeFormatString: Maybe[String],
  args: Any*
) extends SchemaDefinitionDiagnosticBase(
    maybeSchemaContext,
    maybeRuntimeContext,
    None,
    maybeCause,
    maybeFormatString,
    args*
  ) {

  def this(
    schemaContext: SchemaFileLocation,
    runtimeContext: ParseOrUnparseState,
    formatString: String,
    args: Any*
  ) =
    this(Maybe(schemaContext), Maybe(runtimeContext), Nope, Maybe(formatString), args*)

  def this(runtimeContext: ParseOrUnparseState, cause: Throwable) =
    this(
      if (runtimeContext.maybeERD.isDefined)
        Maybe(runtimeContext.maybeERD.get.schemaFileLocation)
      else
        Nope,
      Maybe(runtimeContext),
      Maybe(cause),
      Nope
    )

  def this(formatString: String, args: Any*) =
    this(Nope, Nope, Nope, Maybe(formatString), args*)

  override def isError = true

  override def isValidation = true
  val modeName = "Validation"

}

final class TunableLimitExceededError(
  annotationContext: SchemaFileLocation,
  kind: String,
  args: Any*
) extends SchemaDefinitionDiagnosticBase(
    Maybe(annotationContext),
    Nope,
    None,
    None,
    Maybe(kind),
    args*
  ) {

  override def isError = true
  override def modeName = "Tunable Limit Exceeded"
}

abstract class SchemaDefinitionDiagnosticBase(
  sc: Maybe[SchemaFileLocation],
  runtimeContext: Maybe[ParseOrUnparseState],
  private val annotationContext: Option[SchemaFileLocation],
  mc: Maybe[Throwable],
  mfmt: Maybe[String],
  args: Any*
) extends Diagnostic(
    sc,
    if (runtimeContext.isDefined) Maybe(runtimeContext.get.currentLocation) else Nope,
    mc,
    mfmt,
    args*
  ) {

  override def equals(other: Any) = {
    super.equals(other) && {
      other match {
        case sddb: SchemaDefinitionDiagnosticBase => {
          annotationContext == sddb.annotationContext
        }
        case _ => false
      }
    }
  }

  override def hashCode = {
    super.hashCode() +
      annotationContext.hashCode()
  }

  override protected def schemaContextString =
    super.schemaContextString + annotationContextString

  private def annotationContextString =
    annotationContext.map { " " + _.locationDescription + "." }.getOrElse("")
}

trait ImplementsThrowsSDE extends ThrowsSDE {

  def NoAnnotationContext: Option[SchemaFileLocation] = None

  def SDE(id: String, args: Any*): Nothing = {
    val sde =
      new SchemaDefinitionError(Some(schemaFileLocation), NoAnnotationContext, id, args*)
    toss(sde)
  }
}

trait ImplementsThrowsOrSavesSDE extends ImplementsThrowsSDE with SavesErrorsAndWarnings {

  def tunable: DaffodilTunables
  def localSuppressSchemaDefinitionWarnings: Seq[WarnID]

  def error(th: Diagnostic): Unit
  def warn(th: Diagnostic): Unit

  def SDEButContinue(id: String, args: Any*): Unit = {
    val sde =
      new SchemaDefinitionError(Some(schemaFileLocation), NoAnnotationContext, id, args*)
    error(sde) // calls the error routine which records the error, but doesn't throw/toss it.
  }

  /**
   * Issue a warning. The WarnID allows suppression of warning messages.
   */
  def SDW(warnID: WarnID, fmt: String, args: Any*): Unit = {
    val lssdw = localSuppressSchemaDefinitionWarnings
    val tssdw = tunable.suppressSchemaDefinitionWarnings
    val suppress = lssdw.contains(warnID) || lssdw.contains(WarnID.All) ||
      tssdw.contains(warnID) || tssdw.contains(WarnID.All)
    if (!suppress) {
      val sdw = new SchemaDefinitionWarning(
        warnID,
        Some(schemaFileLocation),
        NoAnnotationContext,
        fmt,
        args*
      )
      if (tunable.escalateWarningsToErrors) {
        val sde = new SchemaDefinitionErrorFromWarning(sdw)
        toss(sde)
      } else {
        warn(sdw)
      }
    }
  }
}
