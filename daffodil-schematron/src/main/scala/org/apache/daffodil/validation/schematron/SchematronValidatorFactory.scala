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
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import org.apache.daffodil.lib.iapi.Validator
import org.apache.daffodil.lib.iapi.ValidatorFactory
import org.apache.daffodil.lib.iapi.ValidatorInitializationException
import org.apache.daffodil.lib.xml.DFDLCatalogResolver

import com.typesafe.config.Config
import com.typesafe.config.ConfigValueType
import net.sf.saxon.TransformerFactoryImpl

/**
 * Daffodil ValidatorFactory implementation for ISO schematron
 */
object SchematronValidatorFactory {
  def makeValidator(config: Config): SchematronValidator = {
    if (!config.hasPath(SchematronValidator.name)) {
      throw ValidatorInitializationException("invalid configuration: missing schematron path")
    }

    val schPathValue = config.getValue(SchematronValidator.name)
    val schPath = Paths.get(schPathValue.valueType() match {
      case ConfigValueType.OBJECT => config.getString(SchematronValidator.ConfigKeys.schPath)
      case ConfigValueType.STRING => config.getString(SchematronValidator.name)
      case _ =>
        throw ValidatorInitializationException(
          "invalid configuration: schematron path was not an object or string"
        )
    })
    val schStream =
      if (Files.exists(schPath)) new FileInputStream(schPath.toFile)
      else
        Option(getClass.getClassLoader.getResourceAsStream(schPath.toString)).getOrElse(
          throw ValidatorInitializationException(s"schematron resource not found: $schPath")
        )
    val svrlOutPath: Option[Path] =
      if (config.hasPath(SchematronValidator.ConfigKeys.svrlOutputFile))
        Some(Paths.get(config.getString(SchematronValidator.ConfigKeys.svrlOutputFile)))
      else None

    makeValidator(schStream, schPath.toString, SchSource.from(schPath), svrlOutPath)
  }

  def makeValidator(
    schematron: InputStream,
    schematronId: String,
    srcfmt: SchSource
  ): SchematronValidator =
    makeValidator(schematron, schematronId, srcfmt, None)

  def makeValidator(
    schematron: InputStream,
    schematronID: String,
    srcfmt: SchSource,
    svrlPath: Option[Path]
  ): SchematronValidator = {
    val factory = new TransformerFactoryImpl()
    factory.setURIResolver(DFDLCatalogResolver.get)
    val rules = Transforms.from(schematron, schematronID, srcfmt, factory)
    new SchematronValidator(Schematron.fromRules(rules), svrlPath)
  }
}

final class SchematronValidatorFactory extends ValidatorFactory {
  def name(): String = SchematronValidator.name
  def make(config: Config): Validator = SchematronValidatorFactory.makeValidator(config)
}
