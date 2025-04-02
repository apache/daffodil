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

import org.apache.daffodil.lib.iapi.ValidationFailure
import org.apache.daffodil.lib.iapi.ValidationResult
import org.apache.daffodil.lib.iapi.ValidationWarning

/**
 * Captures the output of Schematron validation as a Daffodil ValidationResult
 *
 * @param warnings Schematron warnings parsed into ValidationWarning objects
 * @param errors   Schematron errors parsed into ValidationFailure objects
 * @param svrl     Full SVRL output
 */
case class SchematronResult(
  warnings: util.Collection[ValidationWarning],
  errors: util.Collection[ValidationFailure],
  svrl: String
) extends ValidationResult

object SchematronResult {
  def apply(
    w: Seq[ValidationWarning],
    e: Seq[ValidationFailure],
    svrl: String
  ): SchematronResult =
    SchematronResult(w.asJavaCollection, e.asJavaCollection, svrl)
}
