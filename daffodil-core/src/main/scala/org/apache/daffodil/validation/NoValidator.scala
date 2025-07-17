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
import java.util.Properties

import org.apache.daffodil.api
import org.apache.daffodil.api.validation.ValidationHandler
import org.apache.daffodil.api.validation.Validator

object NoValidator extends Validator {
  override def validateXML(
    document: InputStream,
    validationHandler: ValidationHandler
  ): Unit = {
    // do nothing
  }
}

/**
 * SPI service name: off
 * Configuration properties
 * - none; empty Properties file or null is sufficient
 */
class NoValidatorFactory extends api.validation.ValidatorFactory {

  override def name(): String = "off"

  override def make(config: Properties = new Properties()): Validator = {
    NoValidator
  }
}
