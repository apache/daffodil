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

import java.io.InputStream
import java.net.URI
import java.util.Properties

import org.apache.daffodil.api
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.xml.DFDLCatalogResolver

import net.sf.saxon.TransformerFactoryImpl

/**
 * Daffodil ValidatorFactory implementation for ISO schematron.
 * svrl.file is optional, and if provided a schematron report will be written there.
 * If schematron is defined it will use that and expect it to be a .sch file.
 * If that's not defined, it will use daffodil.rootSchema and attempt to extract embedded schematron rules
 *
 * Configuration
 *
 * <ul>
 *  <li>schematron=uri_string_to_schematron_file</li>
 *  <li>schematron.svrl.file=uri_string_to_output_file</li>
 *  <li>daffodil.rootSchema=schema_file_uri_string</li>
 * </ul>
 */
object SchematronValidatorFactory {
  def makeValidator(config: Properties): SchematronValidator = {

    if (config == null) {
      throw new api.validation.ValidatorInitializationException(
        "invalid configuration: missing schematron path"
      )
    }

    val schPathValue = config.getProperty(SchematronValidator.name)
    lazy val defaultSchema = config.getProperty(api.validation.Validator.rootSchemaKey)
    val schUri = new URI({
      if (!Misc.isNullOrBlank(schPathValue)) schPathValue
      else if (!Misc.isNullOrBlank(defaultSchema)) defaultSchema
      else
        throw new api.validation.ValidatorInitializationException(
          "invalid configuration: schematron path was not an object or string"
        )
    })
    val schStream =
      try {
        schUri.toURL.openStream()
      } catch {
        case _: Exception =>
          throw new api.validation.ValidatorInitializationException(
            s"schematron resource not found: $schUri"
          )
      }
    val svrlOutPath: Option[URI] = {
      val svrl = config.getProperty(SchematronValidator.ConfigKeys.svrlOutputFile)
      if (svrl != null)
        Some(new URI(svrl))
      else None
    }

    makeValidator(schStream, schUri.toString, SchSource.from(schUri), svrlOutPath)
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
    svrlPath: Option[URI]
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
