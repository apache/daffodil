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

package org.apache.daffodil.dsom

import org.apache.daffodil.exceptions._
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.processors.CompileState
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.exceptions.SchemaFileLocation
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.processors.CompileState
import org.apache.daffodil.api.DaffodilTunables
import org.apache.daffodil.api.WarnID

class SchemaDefinitionError(schemaContext: Option[SchemaFileLocation],
  annotationContext: Option[SchemaFileLocation],
  fmtString: String,
  args: Any*)
  extends SchemaDefinitionDiagnosticBase(schemaContext, None, annotationContext, Nope, Maybe(fmtString), args: _*) {

  def this(sc: SchemaFileLocation, kind: String, args: Any*) = this(Some(sc), None, kind, args: _*)

  def isError = true
  def modeName = "Schema Definition"

}

/**
 * Specific class used for this specific error, because we need to pick this off
 * in the debugger for special handling.
 */
class RelativePathPastRootError(schemaContext: SchemaFileLocation, kind: String, args: Any*)
  extends SchemaDefinitionError(Some(schemaContext), None, kind, args: _*)

class RuntimeSchemaDefinitionError(schemaContext: SchemaFileLocation,
  runtimeContext: ParseOrUnparseState,
  causedBy: Maybe[Throwable],
  fmtString: Maybe[String],
  args: Any*)
  extends SchemaDefinitionDiagnosticBase(
    Maybe(schemaContext),
    (runtimeContext match { // TODO: this is ugly.
      case cs: CompileState => Nope
      case _ => Maybe(runtimeContext)
    }),
    None, causedBy, fmtString, args: _*) {

  override def isError = true
  override def modeName = "Runtime Schema Definition"

  def this(schemaContext: SchemaFileLocation, runtimeContext: ParseOrUnparseState, fmtString: String, args: Any*) =
    this(schemaContext, runtimeContext, Nope, Maybe(fmtString), args: _*)
}

class RuntimeSchemaDefinitionWarning(schemaContext: SchemaFileLocation,
  runtimeContext: ParseOrUnparseState,
  kind: String,
  args: Any*)
  extends SchemaDefinitionDiagnosticBase(
    Some(schemaContext), Some(runtimeContext), None, Nope, Maybe(kind), args: _*) {

  override def isError = false
  override def modeName = "Runtime Schema Definition"

}

class SchemaDefinitionWarning(schemaContext: Option[SchemaFileLocation],
  annotationContext: Option[SchemaFileLocation],
  kind: String,
  args: Any*)
  extends SchemaDefinitionDiagnosticBase(schemaContext, None, annotationContext, Nope, Maybe(kind), args: _*) {

  def this(sc: SchemaFileLocation, kind: String, args: Any*) = this(Some(sc), None, kind, args: _*)

  override def isError = false
  def modeName = "Schema Definition"
}

class ValidationError(maybeSchemaContext: Maybe[SchemaFileLocation],
                      maybeRuntimeContext: Maybe[ParseOrUnparseState],
                      maybeCause: Maybe[Throwable],
                      maybeFormatString: Maybe[String],
                      args: Any*)
  extends SchemaDefinitionDiagnosticBase(
    maybeSchemaContext, maybeRuntimeContext, None, maybeCause, maybeFormatString, args: _*) {

  def this(schemaContext: SchemaFileLocation,
           runtimeContext: ParseOrUnparseState, formatString: String, args: Any*) =
    this(Maybe(schemaContext), Maybe(runtimeContext), Nope, Maybe(formatString), args: _*)

  def this(runtimeContext: ParseOrUnparseState, cause: Throwable) =
    this(
      if (runtimeContext.maybeERD.isDefined)
        Maybe(runtimeContext.maybeERD.get.schemaFileLocation)
      else
        Nope,
      Maybe(runtimeContext), Maybe(cause), Nope)

  def this(formatString: String, args: Any*) =
    this(Nope, Nope, Nope, Maybe(formatString), args: _*)

  override def isError = true
  
  override def isValidation = true
  val modeName = "Validation"

}

final class TunableLimitExceededError(
  annotationContext: SchemaFileLocation,
  kind: String,
  args: Any*)
  extends SchemaDefinitionDiagnosticBase(
    Maybe(annotationContext),
    Nope, None, None,
    Maybe(kind), args: _*) {

  override def isError = true
  override def modeName = "Tunable Limit Exceeded"
}

abstract class SchemaDefinitionDiagnosticBase(
  sc: Maybe[SchemaFileLocation],
  runtimeContext: Maybe[ParseOrUnparseState],
  private val annotationContext: Option[SchemaFileLocation],
  mc: Maybe[Throwable],
  mfmt: Maybe[String],
  args: Any*)
  extends Diagnostic(sc,
    if (runtimeContext.isDefined) Maybe(runtimeContext.get.currentLocation) else Nope,
    mc, mfmt, args: _*) {

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

trait ImplementsThrowsSDE
  extends ThrowsSDE {

  def NoAnnotationContext: Option[SchemaFileLocation] = None

  def SDE(id: String, args: Any*): Nothing = {
    val sde = new SchemaDefinitionError(Some(schemaFileLocation), NoAnnotationContext, id, args: _*)
    toss(sde)
  }
}

trait ImplementsThrowsOrSavesSDE
  extends ImplementsThrowsSDE with SavesErrorsAndWarnings {

  def tunable: DaffodilTunables

  def error(th: Diagnostic): Unit
  def warn(th: Diagnostic): Unit

  def SDEButContinue(id: String, args: Any*): Unit = {
    val sde = new SchemaDefinitionError(Some(schemaFileLocation), NoAnnotationContext, id, args: _*)
    error(sde) // calls the error routine which records the error, but doesn't throw/toss it.
  }

  /**
   * Issue a warning. The WarnID allows suppression of warning messages.
   */
  def SDW(warnID: WarnID, fmt: String, args: Any*): Unit = {
    if (tunable.notSuppressedWarning(warnID)) {
      val sdw = new SchemaDefinitionWarning(Some(schemaFileLocation), NoAnnotationContext, fmt, args: _*)
      warn(sdw)
    }
  }

  /**
   * Use for cases where it is an SDE because of something we've chosen
   * not to implement. Not merely short term (haven't coded it yet, but intending to),
   * more like things we've chosen to defer intentionally to some future release.
   */
  def subset(testThatWillThrowIfFalse: Boolean, msg: String, args: Any*) = {
    if (!testThatWillThrowIfFalse) subsetError(msg, args: _*)
  }

  def subsetError(msg: String, args: Any*) = {
    val msgTxt = msg.format(args: _*)
    SDE("Subset " + msgTxt)
  }

}
