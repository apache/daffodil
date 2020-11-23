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

package org.apache.daffodil.validation

import java.io.InputStream

import com.typesafe.config.Config
import org.apache.daffodil.api.ValidationFailure
import org.apache.daffodil.api.ValidationResult
import org.apache.daffodil.api.ValidationWarning
import org.apache.daffodil.api.Validator
import org.apache.daffodil.api.ValidatorFactory

class PassingValidatorFactory extends ValidatorFactory {
  def name(): String = PassingValidator.name
  def make(config: Config): Validator = new TestingValidatorSPI(Seq.empty, Seq.empty)
}
object PassingValidator {
  val name = "passing-validator"
}

class FailingValidatorFactory extends ValidatorFactory {
  def name(): String = FailingValidator.name
  def make(config: Config): Validator = new TestingValidatorSPI(Seq.empty, Seq(ValFail("boom")))
}
object FailingValidator {
  val name = "failing-validator"
}

class TestingValidatorSPI(w: Seq[ValidationWarning], f: Seq[ValidationFailure]) extends Validator {
  def validateXML(document: InputStream): ValidationResult = ValidationResult(w, f)
}

case class ValFail(getMessage: String) extends ValidationFailure
