/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.processors.ParseOrUnparseState
import edu.illinois.ncsa.daffodil.processors.CompileState
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation
import edu.illinois.ncsa.daffodil.processors.ParseOrUnparseState
import edu.illinois.ncsa.daffodil.processors.CompileState
import edu.illinois.ncsa.daffodil.api.DaffodilTunables
import edu.illinois.ncsa.daffodil.api.WarnID

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

class ValidationError(schemaContext: Maybe[SchemaFileLocation],
  runtimeContext: ParseOrUnparseState,
  maybeCause: Maybe[Throwable],
  maybeFormatString: Maybe[String],
  args: Any*)
  extends SchemaDefinitionDiagnosticBase(
    (if (schemaContext.isDefined) schemaContext else {
      val mERD = runtimeContext.maybeERD
      if (mERD.isDefined) Maybe(mERD.get.schemaFileLocation)
      else Nope
    }),
    Maybe(runtimeContext), None, maybeCause, maybeFormatString, args: _*) {

  def this(runtimeContext: ParseOrUnparseState, formatString: String, args: Any*) =
    this(Nope, runtimeContext, Nope, Maybe(formatString), args: _*)

  def this(scheamContext: Maybe[SchemaFileLocation],
    runtimeContext: ParseOrUnparseState, formatString: String, args: Any*) =
    this(Nope, runtimeContext, Nope, Maybe(formatString), args: _*)

  def this(runtimeContext: ParseOrUnparseState, cause: Throwable) =
    this(Nope, runtimeContext, Maybe(cause), Nope)

  override def isError = true
  val modeName = "Validation"

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
    ExecutionMode.requireCompilerMode
    val sde = new SchemaDefinitionError(Some(schemaFileLocation), NoAnnotationContext, id, args: _*)
    error(sde) // calls the error routine which records the error, but doesn't throw/toss it.
  }

  /**
   * This form allows All warnings to be suppressed centrally but not individually.
   */
  def SDW(id: String, args: Any*): Unit = {
    if (tunable.notSuppressedWarning(WarnID.All)) {
      ExecutionMode.requireCompilerMode
      val sdw = new SchemaDefinitionWarning(Some(schemaFileLocation), NoAnnotationContext, id, args: _*)
      warn(sdw)
    }
  }

  /**
   * Use this form if you need to be able to suppress the warning
   * individually selectively.
   */
  def SDW(warnID: WarnID, fmt: String, args: Any*): Unit = {
    if (tunable.notSuppressedWarning(warnID)) {
      ExecutionMode.requireCompilerMode
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
