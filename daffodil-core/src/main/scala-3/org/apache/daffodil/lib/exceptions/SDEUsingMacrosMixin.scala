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
  this: org.apache.daffodil.lib.exceptions.ThrowsSDE =>

  /**
   * We are hoping that the inline combined with
   * lazy by-name args allows these methods
   * to avoid constructing the message string or
   * args list items unless the test has been
   * evaluated and indicates we WILL issue the SDE.
   *
   * That way when we're not issuing an SDE there
   * will not be a bunch of overhead associated with
   * gathering error/diagnostic info.
   */
  final inline def schemaDefinitionUnless(
    testThatWillThrowIfFalse: Boolean,
    inline str: String,
    inline args: Any*
  ): Unit = {
    if (!(testThatWillThrowIfFalse)) {
      SDE(str, args: _*)
    }
  }

  final inline def schemaDefinitionWhen(
    testThatWillThrowIfTrue: Boolean,
    inline str: String,
    inline args: Any*
  ): Unit = {
    if (testThatWillThrowIfTrue) {
      SDE(str, args: _*)
    }
  }
}

trait SDWUsingMacrosMixin {
  this: org.apache.daffodil.lib.exceptions.SavesErrorsAndWarnings =>

  /*
   * These functions are now macros as the original code:
   * final def schemaDefinitionUnless(warnID: WarnID, testThatWillThrowIfFalse: Boolean, str: => String, args: => Any*) =  if (!testThatWillThrowIfFalse) SDE(warnID, str, args: _*)
   * would cause expensive object allocation, even when the
   * test would be true and even when the function was inlined
   */

  /**
   * Conditionally issue a warning. The WarnID allows warning suppression.
   */
  inline def schemaDefinitionWarningUnless(
    warnID: WarnID,
    testThatWillWarnIfFalse: Boolean,
    inline str: String,
    inline args: Any*
  ): Unit = {
    if (!(testThatWillWarnIfFalse)) {
      SDW(warnID, str, args: _*)
    }
  }

  /**
   * Conditionally issue a warning. The WarnID allows warning suppression.
   */
  inline def schemaDefinitionWarningWhen(
    warnID: WarnID,
    testThatWillWarnIfTrue: Boolean,
    inline str: String,
    inline args: Any*
  ): Unit = {
    if (testThatWillWarnIfTrue) {
      SDW(warnID, str, args: _*)
    }
  }
}
