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

package org.apache.daffodil.validation.schematron

import java.io.FileInputStream
import java.io.InputStream
import java.nio.file.Paths
import com.typesafe.config.Config

import javax.xml.transform.URIResolver
import net.sf.saxon.TransformerFactoryImpl
import org.apache.daffodil.api.Validator
import org.apache.daffodil.api.ValidatorFactory
import org.apache.daffodil.api.ValidatorInitializationException

import java.nio.file.Files

/**
 * Daffodil ValidatorFactory implementation for ISO schematron
 */
object SchematronValidatorFactory {
  def makeValidator(config: Config): SchematronValidator = {
    if(!config.hasPath(SchematronValidator.name))
      throw ValidatorInitializationException("invalid configuration: missing schematron path")

    val schPath = Paths.get(config.getString(SchematronValidator.name))
    val schStream = if(Files.exists(schPath)) new FileInputStream(schPath.toFile)
    else Option(getClass.getClassLoader.getResourceAsStream(schPath.toString)).getOrElse(
      throw ValidatorInitializationException(s"schematron resource not found: $schPath")
    )
    makeValidator(schStream, SchSource.from(schPath), None)
  }

  def makeValidator(schematron: InputStream, srcfmt: SchSource, fallback: Option[URIResolver] = None): SchematronValidator = {
    val factory = new TransformerFactoryImpl()
    factory.setURIResolver(Schematron.isoTemplateResolver(fallback))
    val rules = Transforms.from(schematron, srcfmt, factory)
    new SchematronValidator(Schematron.fromRules(rules))
  }
}

final class SchematronValidatorFactory extends ValidatorFactory {
  def name(): String = SchematronValidator.name
  def make(config: Config): Validator = SchematronValidatorFactory.makeValidator(config)
}
