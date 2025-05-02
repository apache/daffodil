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

import org.apache.daffodil.api.validation.{ ValidationException => JValidationException }
import org.apache.daffodil.api.validation.{ ValidationFailure => JValidationFailure }
import org.apache.daffodil.api.validation.{ ValidationResult => JValidationResult }
import org.apache.daffodil.api.validation.{ ValidationWarning => JValidationWarning }
import org.apache.daffodil.api.validation.{ Validator => JValidator }
import org.apache.daffodil.api.validation.{ ValidatorFactory => JValidatorFactory }
import org.apache.daffodil.api.validation.{
  ValidatorInitializationException => JValidatorInitializationException
}
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.util.SimpleNamedLoadableService

/**
 * Implement this trait to provide custom validation logic
 *
 * The Validator implementations are expected to be thread safe
 *
 * @see [[org.apache.daffodil.lib.validation.XercesValidator]] for example of using ThreadLocal for thread safety
 */
trait Validator extends JValidator

/**
 * Implement this trait and register with SPI to provide runtime discovery of custom Validator implementations
 *
 * The factory implementations are expected to be thread safe
 */
trait ValidatorFactory extends JValidatorFactory with SimpleNamedLoadableService

/**
 * Results of a validation execution
 */
trait ValidationResult extends JValidationResult

object ValidationResult {

  /**
   * an empty [[org.apache.daffodil.lib.iapi.ValidationResult]]
   */
  val empty: JValidationResult = JValidationResult.empty

  def apply(w: Seq[ValidationWarning], e: Seq[ValidationFailure]): JValidationResult = {
    new JValidationResult(
      w.asInstanceOf[Seq[JValidationWarning]],
      e.asInstanceOf[Seq[JValidationFailure]]
    )
  }
}

/**
 * Represents a non-fatal validation notification
 */
trait ValidationWarning extends JValidationWarning

/**
 * Represents a fatal validation notification
 */
trait ValidationFailure extends JValidationFailure

/**
 * Represents a ValidationFailure that is backed by a Throwable
 */
trait ValidationException extends ValidationFailure with JValidationException

/**
 * Thrown when a validator fails to initialize
 * @param message used in the underlying Exception
 */
final case class ValidatorInitializationException(message: String)
  extends JValidatorInitializationException(message)
