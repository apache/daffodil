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

package edu.illinois.ncsa.daffodil.exceptions

import edu.illinois.ncsa.daffodil.dsom.LookupLocation
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils
import edu.illinois.ncsa.daffodil.api.DaffodilTunableParameters.WarnID

/**
 * ThrowsSDE has *only* termination semantics. I.e., SDE just throws. This
 * makes it independent of context, i.e., we don't have to pass or otherwise
 * arrange for things that throw SDE to use the right compile-time or
 * runtime context to do their SDE throwing. Rather, we use the appropriate
 * compile-time or runtime mechanism for dealing with these thrown SDEs at the
 * place where they are caught. I.e., the context is on the catch-side
 * only.
 *
 * This turns out to be important for avoiding big modularity problems where
 * everything would end up parameterized by what kind of context is to be used in case
 * there is some sort of error. That makes it very hard to share code across
 * compile time (when errors are accumulated into lists) vs runtime (when SDEs
 * are usually fatal)
 *
 * Both true "compilation" i.e., SchemaComponent will mix this in, as well as
 * runtime data structures.
 */
trait ThrowsSDE {

  def SDE(id: String, args: Any*): Nothing
  final def SDE(th: Throwable): Nothing = SDE(DiagnosticUtils.getSomeMessage(th).get)

  def ThrowSDE = PartialFunction[Throwable, Nothing] { case th: Throwable => SDE(th) }

  def schemaFileLocation: SchemaFileLocation

  /**
   * Centralize throwing for debug convenience
   */
  final def toss(th: Throwable) = {
    throw th // good place for a breakpoint
  }

  final def schemaDefinitionError(str: String, args: Any*): Nothing = SDE(str, args: _*) // long form synonym

  final def schemaDefinitionUnless(testThatWillThrowIfFalse: Boolean, str: String, args: Any*) {
    if (!testThatWillThrowIfFalse)
      SDE(str, args: _*)
  }

  final def schemaDefinitionWhen(testThatWillThrowIfTrue: Boolean, str: String, args: Any*) {
    schemaDefinitionUnless(!testThatWillThrowIfTrue, str, args: _*)
  }

  final def notYetImplemented(msg: String, args: Any*): Nothing = SDE("Feature not yet implemented: " + msg, args: _*)

}

/**
 * This trait for true "compilation" when there is a mechanism for accumulating
 * multiple errors and/or warnings, and we are trying (someplace) to keep going
 * after an error.
 *
 * Also for runtime warnings.
 *
 */
trait SavesErrorsAndWarnings {

  def SDE(id: String, args: Any*): Nothing
  def SDW(str: String, args: Any*): Unit
  /**
   * Use this form if you need to be able to suppress the warning
   */
  def SDW(warnID: WarnID, str: String, args: Any*): Unit
  def SDEButContinue(str: String, args: Any*): Unit

  def schemaDefinitionErrorButContinue(str: String, args: Any*): Unit = SDEButContinue(str, args: _*)

  def schemaDefinitionWarningUnless(testThatWillWarnIfFalse: Boolean, str: String, args: Any*) {
    if (!testThatWillWarnIfFalse) SDW(str, args: _*)
  }

  /**
   * Use this form if you need to be able to suppress the warning
   */
  def schemaDefinitionWarningUnless(warnID: WarnID, testThatWillWarnIfFalse: Boolean, str: String, args: Any*) {
    if (!testThatWillWarnIfFalse) SDW(warnID, str, args: _*)
  }

  def schemaDefinitionWarningWhen(testThatWillWarnIfTrue: Boolean, str: String, args: Any*) {
    schemaDefinitionWarningUnless(!testThatWillWarnIfTrue, str, args: _*)
  }

  /**
   * Use this form if you need to be able to suppress the warning
   */
  def schemaDefinitionWarningWhen(warnID: WarnID, testThatWillWarnIfTrue: Boolean, str: String, args: Any*) {
    schemaDefinitionWarningUnless(warnID, !testThatWillWarnIfTrue, str, args: _*)
  }

  /**
   * SDE special case when we're blaming the error on the value of a property.
   * If the location where the property value is defined is different
   * from the current context, then we inform about both the context
   * location, and the location where the property value comes from.
   */
  def schemaDefinitionErrorDueToPropertyValue(
    propertyName: String,
    propertyValue: String,
    propertyLocation: LookupLocation,
    otherPropertyLocation: LookupLocation,
    str: String, args: Any*): Nothing = {
    //
    // only if there is more than one location to discuss, do we
    // output that information as well.
    //
    if (propertyLocation.locationDescription != otherPropertyLocation.locationDescription) {
      SDEButContinue(str, args: _*)
      SDE("Property %s defined as '%s'.", propertyName, propertyValue)
    } else {
      SDE(str, args: _*)
    }
  }
}
