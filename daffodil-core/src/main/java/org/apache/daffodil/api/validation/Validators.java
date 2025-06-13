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

import org.apache.daffodil.util.SimpleNamedServiceLoader;

import java.util.Optional;

/**
 * Access SPI registered [[org.apache.daffodil.api.validation.ValidatorFactory]] instances.
 * <p>
 * Registered instances provide a unique name for lookup.
 */
public class Validators {
  private static final Validators INSTANCE = new Validators();
  private final java.util.Map<String, ValidatorFactory> impls =
      SimpleNamedServiceLoader.loadClass(ValidatorFactory.class);

  private Validators() {
  } // private constructor

  public static Validators getInstance() {
    return INSTANCE;
  }

  /**
   * Get the factory by name or throw
   *
   * @param name registered name of the validator factory
   * @return {@link org.apache.daffodil.api.validation.ValidatorFactory} the factory instance
   * @throws ValidatorNotRegisteredException when factory is not found in the registered services
   */
  public ValidatorFactory get(String name) throws ValidatorNotRegisteredException {
    if (isRegistered(name)) {
      return impls.get(name);
    } else {
      throw new ValidatorNotRegisteredException(name);
    }
  }

  /**
   * Optionally find the factory
   *
   * @param name registered name of the validator factory
   * @return [[org.apache.daffodil.api.validation.ValidatorFactory]] optional factory instance
   */
  public Optional<ValidatorFactory> find(String name) {
    return Optional.ofNullable(impls.get(name));
  }

  /**
   * Check for registration of named factory
   *
   * @param name registered name of the validator factory
   * @return is factory registered
   */
  public boolean isRegistered(String name) {
    return impls.containsKey(name);
  }
}
