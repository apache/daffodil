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

package org.apache.daffodil.api.validation;

import org.apache.daffodil.lib.util.SimpleNamedLoadableService;

import java.util.Properties;

/**
 * Implement this trait and register with SPI to provide runtime discovery of Validator implementations
 * <p>
 * The factory implementations must be thread safe
 */
public interface ValidatorFactory extends SimpleNamedLoadableService {

  /**
   * Unique name of this Validator service
   *
   * @return registered name of the validator factory
   */
  String name();

  /**
   * The factory method to generate the Validator instance
   *
   * @param config java.util.Properties to pass to validator instance
   * @return {@link org.apache.daffodil.api.validation.Validator} instance ready to execute
   * @throws org.apache.daffodil.api.validation.ValidatorInitializationException when initialization fails
   */
  Validator make(Properties config) throws ValidatorInitializationException;
}