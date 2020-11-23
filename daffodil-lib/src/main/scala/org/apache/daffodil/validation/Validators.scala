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

import java.util.ServiceLoader

import org.apache.daffodil.api.ValidatorFactory

import scala.collection.JavaConverters._

/**
 * Access SPI registered [[org.apache.daffodil.api.ValidatorFactory]] instances.
 *
 * Registered instances provide a unique name for lookup.
 */
object Validators {
  private lazy val impls: Map[String, ValidatorFactory] =
    ServiceLoader
      .load(classOf[ValidatorFactory])
      .iterator()
      .asScala
      .map(v => v.name() -> v)
      .toMap

  /**
   * Get the factory by name or throw
   * @param name registered name of the validator factory
   * @throws ValidatorNotRegisteredException when factory is not found in the registered services
   * @return [[org.apache.daffodil.api.ValidatorFactory]] the factory instance
   */
  @throws(classOf[ValidatorNotRegisteredException])
  def get(name: String): ValidatorFactory = impls.getOrElse(name, throw ValidatorNotRegisteredException(name))

  /**
   * Optionally find the factory
   * @param name registered name of the validator factory
   * @return [[org.apache.daffodil.api.ValidatorFactory]] optional factory instance
   */
  def find(name: String): Option[ValidatorFactory] = impls.get(name)

  /**
   * Check for registration of named factory
   * @param name registered name of the validator factory
   * @return is factory registered
   */
  def isRegistered(name: String): Boolean = impls.contains(name)
}

/**
 * Thrown when the by-name lookup of a validator fails
 * @param name the requested validator factory name
 */
case class ValidatorNotRegisteredException(name: String)
  extends Exception(s"No ValidatorFactory is registered as $name")
