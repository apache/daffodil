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

package org.apache.daffodil.lib.validation

import java.io.InputStream

import org.apache.daffodil.api

import com.typesafe.config.Config

class PassingValidatorFactory extends api.validation.ValidatorFactory {
  def name(): String = PassingValidator.name
  def make(config: Config): api.validation.Validator =
    new TestingValidatorSPI(Seq.empty)
}
object PassingValidator {
  val name = "passing-validator"
}

class FailingValidatorFactory extends api.validation.ValidatorFactory {
  def name(): String = FailingValidator.name
  def make(config: Config): api.validation.Validator =
    new TestingValidatorSPI(Seq("boom"))
}
object FailingValidator {
  val name = "failing-validator"
}

class TestingValidatorSPI(s: Seq[String]) extends api.validation.Validator {
  def validateXML(document: InputStream, vh: api.validation.ValidationHandler): Unit =
    s.foreach(vh.validationError(_))
}

class TestingValidationHandler extends api.validation.ValidationHandler {
  var errors = Seq.empty[String]
  var warnings = Seq.empty[String]
  def validationError(msg: String, args: Object*): Unit = errors = errors :+ msg
  def validationWarning(msg: String): Unit = warnings = warnings :+ msg
  override def validationErrorNoContext(cause: Throwable): Unit = errors :+ cause.getMessage
}
