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

package org.apache.daffodil.lib.util

import org.apache.daffodil.lib.exceptions.Assert

/**
 * Define an OK or Error type.
 *
 * This is almost exactly a Maybe[String], but the sense of
 * 'isDefined' is backward in a Maybe type. The Nope would represent Ok, so
 * isEmpty would mean Ok, and isDefined would mean error, and that's so
 * unintuitive, that we define our own Maybe-like value class here
 * so that isOK and isError are the tests.
 */
object OKOrError {
  private val okValue: String = null

  val OK = new OKOrError(okValue)

  def Error(s: String) = {
    Assert.usage(s ne null)
    Assert.usage(s != okValue)
    new OKOrError(s)
  }
}

class OKOrError private (val errMsg: String) extends AnyVal {
  @inline def isOK = this.errMsg eq OKOrError.okValue
  @inline def isError = !isOK
}
