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

trait SDEUsingMacrosMixin {
  final def schemaDefinitionUnless(
    testThatWillThrowIfFalse: Boolean,
    str: String,
    args: Any*
  ): Unit = macro SDEMacros.schemaDefinitionUnlessMacro

  final def schemaDefinitionWhen(
    testThatWillThrowIfTrue: Boolean,
    str: String,
    args: Any*
  ): Unit = macro SDEMacros.schemaDefinitionWhenMacro
}

trait SDWUsingMacrosMixin {
  /*
   * These functions are now macros as the original code:
   * final def schemaDefinitionUnless(warnID: WarnID, testThatWillThrowIfFalse: Boolean, str: => String, args: => Any*) =  if (!testThatWillThrowIfFalse) SDE(warnID, str, args: _*)
   * would cause expensive object allocation, even when the
   * test would be true and even when the function was inlined
   */

  /**
   * Conditionally issue a warning. The WarnID allows warning suppression.
   */
  def schemaDefinitionWarningUnless(
    warnID: WarnID,
    testThatWillWarnIfFalse: Boolean,
    str: String,
    args: Any*
  ): Unit = macro SDEMacros.schemaDefinitionWarningUnlessSuppressMacro

  /**
   * Conditionally issue a warning. The WarnID allows warning suppression.
   */
  def schemaDefinitionWarningWhen(
    warnID: WarnID,
    testThatWillWarnIfTrue: Boolean,
    str: String,
    args: Any*
  ): Unit = macro SDEMacros.schemaDefinitionWarningWhenSuppressMacro
}
