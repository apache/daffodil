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

package org.apache.daffodil.sexample

import java.io.File
import java.io.FileInputStream
import java.io.InputStream
import scala.util.Using

import org.apache.daffodil.api.Daffodil
import org.apache.daffodil.api.DataProcessor
import org.apache.daffodil.api.InputSourceDataInputStream
import org.apache.daffodil.api.infoset.Infoset
import org.apache.daffodil.api.validation.ValidationFailure
import org.apache.daffodil.api.validation.ValidationResult
import org.apache.daffodil.api.validation.ValidationWarning
import org.apache.daffodil.api.validation.Validator
import org.apache.daffodil.api.validation.ValidatorFactory
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.runtime1.validation.ValidationResultImpl

import com.typesafe.config.Config

abstract class ValidatorExamplesSupport {
  private def fileFromResource(path: String): File = new File(getClass.getResource(path).toURI)
  val `/dev/null` = Infoset.getNullInfosetOutputter

  def withSchema(name: String)(f: DataProcessor => Unit): Unit = {
    val c = Daffodil.compiler()
    val schemaFile = fileFromResource(name)
    val pf = c.compileFile(schemaFile)
    f(pf.onPath("/"))
  }

  def withInput(name: String)(f: InputSourceDataInputStream => Unit): Unit = {
    Using.resource(
      Infoset.getInputSourceDataInputStream(new FileInputStream(fileFromResource(name)))
    ) { s =>
      f(s)
    }
  }
}

class CustomValidator extends Validator {
  def validateXML(document: InputStream): ValidationResult =
    ValidationResultImpl.empty
}

class CustomValidatorFactory extends ValidatorFactory {
  def name(): String = "sapi-custom"
  def make(config: Config): Validator = new CustomValidator
}

class AlwaysValidator(w: Seq[ValidationWarning], e: Seq[ValidationFailure]) extends Validator {
  def validateXML(document: InputStream): ValidationResult =
    new ValidationResultImpl(w, e)
}

object Boom extends ValidationFailure {
  def getMessage: String = "boom"
}

object Always {
  def fails: Validator = (_: InputStream) => new ValidationResultImpl(Seq.empty, Seq(Boom))
  def passes: Validator = (_: InputStream) => ValidationResultImpl.empty
}

class PassingValidatorFactory extends ValidatorFactory {
  def name(): String = PassingValidator.name
  def make(config: Config): Validator = new TestingValidatorSPI(Seq.empty, Seq.empty)
}
object PassingValidator {
  val name = "passing-sapi-validator"
}

class FailingValidatorFactory extends ValidatorFactory {
  def name(): String = FailingValidator.name
  def make(config: Config): Validator = new TestingValidatorSPI(Seq.empty, Seq(ValFail("boom")))
}
object FailingValidator {
  val name = "failing-sapi-validator"
}

class TestingValidatorSPI(w: Seq[ValidationWarning], f: Seq[ValidationFailure])
  extends Validator {
  def validateXML(document: InputStream): ValidationResult = new ValidationResultImpl(w, f)
}

case class ValFail(getMessage: String) extends ValidationFailure
