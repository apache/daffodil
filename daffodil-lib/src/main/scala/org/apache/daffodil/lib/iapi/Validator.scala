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

import org.apache.daffodil.lib.util.SimpleNamedLoadableService

import com.typesafe.config.Config

/**
 * Implement this trait to provide custom validation logic
 *
 * The Validator implementations are expected to be thread safe
 *
 * @see [[org.apache.daffodil.lib.validation.XercesValidator]] for example of using ThreadLocal for thread safety
 */
trait Validator {
  def validateXML(document: java.io.InputStream): ValidationResult
}

/**
 * Implement this trait and register with SPI to provide runtime discovery of custom Validator implementations
 *
 * The factory implementations are expected to be thread safe
 */
trait ValidatorFactory extends SimpleNamedLoadableService {

  /**
   * Unique name of this Validator service
   * @return registered name of the validator factory
   */
  def name(): String

  /**
   * The factory method to generate the Validator instance
 *
   * @param config com.typesafe.config.Config to pass to validator instance
   * @return [[org.apache.daffodil.lib.iapi.Validator]] instance ready to execute
   * @throws org.apache.daffodil.lib.iapi.ValidatorInitializationException when initialization fails
   */
  @throws(classOf[ValidatorInitializationException])
  def make(config: Config): Validator
}

/**
 * Results of a validation execution
 */
trait ValidationResult {
  val warnings: java.util.Collection[ValidationWarning]
  val errors: java.util.Collection[ValidationFailure]
}

object ValidationResult {

  /**
   * an empty [[org.apache.daffodil.lib.iapi.ValidationResult]]
   */
  val empty: ValidationResult = ValidationResult(Seq.empty, Seq.empty)

  def apply(w: Seq[ValidationWarning], e: Seq[ValidationFailure]): ValidationResult = {
    import scala.jdk.CollectionConverters._
    new ValidationResult {
      val warnings: java.util.Collection[ValidationWarning] = w.asJavaCollection
      val errors: java.util.Collection[ValidationFailure] = e.asJavaCollection
    }
  }
}

/**
 * Represents a non-fatal validation notification
 */
trait ValidationWarning {
  def getMessage: String
}

/**
 * Represents a fatal validation notification
 */
trait ValidationFailure {
  def getMessage: String
}

/**
 * Represents a ValidationFailure that is backed by a Throwable
 */
trait ValidationException extends ValidationFailure {
  def getCause: Throwable
}

/**
 * Thrown when a validator fails to initialize
 * @param message used in the underlying Exception
 */
final case class ValidatorInitializationException(message: String) extends Exception(message)
