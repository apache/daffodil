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

package org.apache.daffodil.util

import org.apache.daffodil.exceptions.Assert

/**
 * Things that are named need some common methods.
 *
 * These are is isolated in a separate trait because it is hard to mix
 * together all the various overrides of these without
 * shared inheritance from a common trait.
 */
trait NamedMixinBase {

  /**
   * Flag used to prevent re-entry of diagnosticDebugName
   */
  private var isAlreadyComputing: Boolean = false

  /**
   * For diagnostics/trace/debug purposes
   *
   * We cannot call diagnosticDebugName inside another call to diagnosticDebugName
   * of the same object. This catches stack overflow situations.
   */
  final lazy val diagnosticDebugName: String = {
    if (isAlreadyComputing == true) {
      Assert.invariantFailed("Reentry of computation")
    } else {
      try {
        isAlreadyComputing = true
        diagnosticDebugNameImpl
      } finally {
        isAlreadyComputing = false
      }
    }
  }

  /**
   * Override this to implement a diagnostic debug name.
   *
   * Note that this cannot throw exceptions because it is used to create
   * diagnostic messages.
   */
  protected def diagnosticDebugNameImpl = className

  private lazy val className = Misc.getNameFromClass(this)
}