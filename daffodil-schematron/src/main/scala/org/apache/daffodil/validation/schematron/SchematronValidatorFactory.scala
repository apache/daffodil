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
import java.util.Properties

import org.apache.daffodil.api
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.xml.DFDLCatalogResolver

import net.sf.saxon.TransformerFactoryImpl

/**
 * Daffodil ValidatorFactory implementation for ISO schematron
 */
object SchematronValidatorFactory {
  def makeValidator(config: Properties): SchematronValidator = {

    if (config == null) {
      throw new api.validation.ValidatorInitializationException(
        "invalid configuration: missing schematron path"
      )
    }

    val schPathValue = config.getProperty(SchematronValidator.name)
    lazy val schemaPathValue = config.getProperty(SchematronValidator.ConfigKeys.schPath)
    lazy val defaultSchema = config.getProperty(SchematronValidator.ConfigKeys.rootSchema)
    val schPath = Paths.get(schPathValue match {
      case e if e == null && !Misc.isNullOrBlank(schemaPathValue) => schemaPathValue
      case e if e != null => schPathValue
      case _ if !Misc.isNullOrBlank(defaultSchema) => defaultSchema
      case _ =>
        throw new api.validation.ValidatorInitializationException(
          "invalid configuration: schematron path was not an object or string"
        )
    })
    val schStream =
      if (Files.exists(schPath)) new FileInputStream(schPath.toFile)
      else
        Option(getClass.getClassLoader.getResourceAsStream(schPath.toString)).getOrElse(
          throw new api.validation.ValidatorInitializationException(
            s"schematron resource not found: $schPath"
          )
        )
    val svrlOutPath: Option[Path] = {
      val svrl = config.getProperty(SchematronValidator.ConfigKeys.svrlOutputFile)
      if (svrl != null)
        Some(Paths.get(svrl))
      else None
    }

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

final class SchematronValidatorFactory extends api.validation.ValidatorFactory {
  def name(): String = SchematronValidator.name
  def make(config: Properties): api.validation.Validator =
    SchematronValidatorFactory.makeValidator(config)
}
