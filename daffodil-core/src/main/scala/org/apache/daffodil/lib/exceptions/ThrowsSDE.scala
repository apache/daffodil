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

package org.apache.daffodil.lib.exceptions

import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.schema.annotation.props.LookupLocation
import org.apache.daffodil.lib.util.Misc

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
trait ThrowsSDE extends SDEUsingMacrosMixin {

  def SDE(id: String, args: Any*): Nothing
  final def SDE(th: Throwable): Nothing = SDE(Misc.getSomeMessage(th).get)

  def ThrowSDE: PartialFunction[Throwable, Nothing] = { case th: Throwable => SDE(th) }

  def schemaFileLocation: SchemaFileLocation

  /**
   * Centralize throwing for debug convenience
   */
  final def toss(th: Throwable) = {
    throw th // good place for a breakpoint
  }

  final def schemaDefinitionError(str: String, args: Any*): Nothing =
    SDE(str, args: _*) // long form synonym

  final def notYetImplemented(msg: String, args: Any*): Nothing =
    SDE("Feature not yet implemented: " + msg, args: _*)

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
    SDE("Subset: " + msgTxt)
  }
}

/**
 * This trait for true "compilation" when there is a mechanism for accumulating
 * multiple errors and/or warnings, and we are trying (someplace) to keep going
 * after an error.
 *
 * Also for runtime warnings.
 *
 */
trait SavesErrorsAndWarnings extends SDWUsingMacrosMixin {

  def SDE(id: String, args: Any*): Nothing

  /**
   * Issue a warning. The WarnID enables suppression of warning messages.
   */
  def SDW(warnID: WarnID, str: String, args: Any*): Unit
  def SDEButContinue(str: String, args: Any*): Unit

  def schemaDefinitionErrorButContinue(str: String, args: Any*): Unit =
    SDEButContinue(str, args: _*)

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
    str: String,
    args: Any*
  ): Nothing = {
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
