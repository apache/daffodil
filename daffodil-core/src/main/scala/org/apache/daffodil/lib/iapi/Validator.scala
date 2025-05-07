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

package org.apache.daffodil.lib.iapi

import org.apache.daffodil.api
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.runtime1.validation.ValidationResultImpl

object ValidationResult {

  /**
   * an empty [[org.apache.daffodil.api.validation.ValidationResult]]
   */
  val empty: api.validation.ValidationResult = ValidationResultImpl.empty

  def apply(
    w: Seq[api.validation.ValidationWarning],
    e: Seq[api.validation.ValidationFailure]
  ): api.validation.ValidationResult = {
    new ValidationResultImpl(w, e)
  }
}
