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

package org.apache.daffodil.validation.schematron

import java.util
import scala.jdk.CollectionConverters._

import org.apache.daffodil.api
import org.apache.daffodil.runtime1.validation.ValidationResultImpl

/**
 * Captures the output of Schematron validation as a Daffodil ValidationResult
 *
 * @param warnings Schematron warnings parsed into ValidationWarning objects
 * @param errors   Schematron errors parsed into ValidationFailure objects
 * @param svrl     Full SVRL output
 */
case class SchematronResult(
  warnings: util.Collection[api.validation.ValidationWarning],
  errors: util.Collection[api.validation.ValidationFailure],
  svrl: String
) extends ValidationResultImpl {
  override def getWarnings: util.Collection[api.validation.ValidationWarning] = warnings

  override def getErrors: util.Collection[api.validation.ValidationFailure] = errors
}

object SchematronResult {
  def apply(
    w: Seq[api.validation.ValidationWarning],
    e: Seq[api.validation.ValidationFailure],
    svrl: String
  ): SchematronResult =
    SchematronResult(w.asJavaCollection, e.asJavaCollection, svrl)
}
