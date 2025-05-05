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
import org.apache.daffodil.lib.util.SimpleNamedLoadableService
import org.apache.daffodil.runtime1.validation.ValidationResultImpl

/**
 * Implement this trait to provide custom validation logic
 *
 * The Validator implementations are expected to be thread safe
 *
 * @see [[org.apache.daffodil.lib.validation.XercesValidator]] for example of using ThreadLocal for thread safety
 */
trait Validator extends api.validation.Validator

/**
 * Implement this trait and register with SPI to provide runtime discovery of custom Validator implementations
 *
 * The factory implementations are expected to be thread safe
 */
trait ValidatorFactory extends api.validation.ValidatorFactory with SimpleNamedLoadableService

/**
 * Results of a validation execution
 */
trait ValidationResult extends api.validation.ValidationResult

object ValidationResult {

  /**
   * an empty [[org.apache.daffodil.lib.iapi.ValidationResult]]
   */
  val empty: api.validation.ValidationResult = ValidationResultImpl.empty

  def apply(
    w: Seq[ValidationWarning],
    e: Seq[ValidationFailure]
  ): api.validation.ValidationResult = {
    new ValidationResultImpl(
      w.asInstanceOf[Seq[api.validation.ValidationWarning]],
      e.asInstanceOf[Seq[api.validation.ValidationFailure]]
    )
  }
}

/**
 * Represents a non-fatal validation notification
 */
trait ValidationWarning extends api.validation.ValidationWarning

/**
 * Represents a fatal validation notification
 */
trait ValidationFailure extends api.validation.ValidationFailure

/**
 * Represents a ValidationFailure that is backed by a Throwable
 */
trait ValidationException extends ValidationFailure with api.validation.ValidationException

/**
 * Thrown when a validator fails to initialize
 * @param message used in the underlying Exception
 */
final case class ValidatorInitializationException(message: String)
  extends api.validation.ValidatorInitializationException(message)
