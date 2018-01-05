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

package org.apache.daffodil.api

import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.util.Maybe

/**
 * Exceeding these limits is not a back-trackable parse error. It's more severe
 * than that. But it is not a schema-definition error really either.
 */
final class TunableLimitExceededError(limitName: String, cause: Option[Throwable], formatString: String, val args: Any*)
  extends Diagnostic(Nope, Nope, cause, Maybe(formatString), args: _*) {

  def isError = true
  def modeName = "Tunable Limit"

  def this(limitName: String, msg: String, args: Any*) = this(limitName, None, msg, args: _*)

  def this(limitName: String, cause: Throwable) = this(limitName, Some(cause), "")

}
