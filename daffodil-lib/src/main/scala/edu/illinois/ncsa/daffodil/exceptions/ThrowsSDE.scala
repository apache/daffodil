package edu.illinois.ncsa.daffodil.exceptions

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

trait ThrowsSDE extends SchemaFileLocatable {

  def SDE(str: String, args: Any*): Nothing
  def SDW(str: String, args: Any*): Unit
  def SDEButContinue(str: String, args: Any*): Unit

  def rethrowAsDiagnostic(th: Throwable) = SDE("%s", th)

  def schemaDefinitionErrorButContinue(str: String, args: Any*): Unit = SDEButContinue(str, args: _*)

  def schemaDefinitionError(str: String, args: Any*): Nothing = SDE(str, args: _*) // long form synonym

  /**
   * Nobody gets the sense of the boolean test right here. So rename to
   * schemaDefinitionWhen, and schemaDefinitionUnless
   */
  @deprecated(message = "use schemaDefinitionUnless(...) or schemaDefinitionWhen(...) instead", since = "2013-03-25")
  def schemaDefinition(testThatWillThrowIfFalse: => Boolean, str: String, args: Any*) {
    schemaDefinitionUnless(testThatWillThrowIfFalse, str, args: _*)
  }

  def schemaDefinitionUnless(testThatWillThrowIfFalse: => Boolean, str: String, args: Any*) {
    if (!testThatWillThrowIfFalse)
      SDE(str, args: _*)
  }

  def schemaDefinitionWhen(testThatWillThrowIfTrue: => Boolean, str: String, args: Any*) {
    schemaDefinitionUnless(!testThatWillThrowIfTrue, str, args: _*)
  }

  @deprecated(message = "use schemaDefinitionWarningUnless(...) or schemaDefinitionWarningWhen(...) instead", since = "2013-03-25")
  def schemaDefinitionWarning(testThatWillWarnIfFalse: => Boolean, str: String, args: Any*) {
    schemaDefinitionWarningUnless(testThatWillWarnIfFalse, str, args: _*)
  }

  def schemaDefinitionWarningUnless(testThatWillWarnIfFalse: => Boolean, str: String, args: Any*) {
    if (!testThatWillWarnIfFalse) SDW(str, args: _*)
  }

  def schemaDefinitionWarningWhen(testThatWillWarnIfTrue: => Boolean, str: String, args: Any*) {
    schemaDefinitionWarningUnless(!testThatWillWarnIfTrue, str, args: _*)
  }

  def notYetImplemented(msg: String, args: Any*): Nothing = SDE("Feature not yet implemented: " + msg, args: _*)

  /**
   * SDE special case when we're blaming the error on the value of a property.
   * If the location where the property value is defined is different
   * from the current context, then we inform about both the context
   * location, and the location where the property value comes from.
   */
  def schemaDefinitionErrorDueToPropertyValue(
    propertyName: String,
    propertyValue: String,
    propertyLocation: ThrowsSDE,
    str: String, args: Any*): Nothing = {
    //
    // only if there is more than one location to discuss, do we 
    // output that information as well.
    // 
    if (propertyLocation.locationDescription != this.locationDescription) {
      SDEButContinue(str, args: _*)
      propertyLocation.SDE("Property %s defined as '%s'.", propertyName, propertyValue)
    } else {
      SDE(str, args: _*)
    }
  }
}

