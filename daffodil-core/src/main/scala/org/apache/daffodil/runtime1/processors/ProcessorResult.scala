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

package org.apache.daffodil.runtime1.processors

import org.apache.daffodil.api.{ Diagnostic => JDiagnostic }
import org.apache.daffodil.lib.util.Misc

/**
 * The resulting state after invoking a Processor
 */
sealed abstract class ProcessorResult {
  def isSuccess: Boolean
  final def isFailure: Boolean = !isSuccess
}

case object Success extends ProcessorResult {
  override def isSuccess = true
}

case class Failure(cause: JDiagnostic) extends ProcessorResult {
  override def isSuccess = false
  lazy val msg = Misc.getSomeMessage(cause).get
  override def toString = "Failure(" + msg + ")"
}
