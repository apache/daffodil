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
 * schematron.svrl.file is optional, and if provided a schematron report will be written there. Note that it is not threadsafe, and will be overwritten in a multithreaded environment.
 * schematron property must be defined, which can either be a .sch file or a DFDL file with
 * embedded schematron rules.
 *
 * Configuration
 *
 * <ul>
 *  <li>schematron=uri_string_to_schematron_file</li>
 *  <li>schematron.svrl.file=uri_string_to_output_file</li>
 * </ul>
 */
object SchematronValidatorFactory {
  def makeValidator(config: Properties): SchematronValidator = {
    val schPathValue = config.getProperty(SchematronValidator.name)
    val schUri = new URI({
      if (!Misc.isNullOrBlank(schPathValue)) schPathValue
      else
        throw new api.validation.ValidatorInitializationException(
          "invalid configuration: schematron property is empty or not defined"
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
    try {
      val rules = Transforms.from(schematron, schematronID, srcfmt, factory)
      new SchematronValidator(rules, svrlPath)
    } catch {
      case e: Exception =>
        throw new api.validation.ValidatorInitializationException(
          s"failed to create schematron validator: " + e.getMessage
        )
    }
  }
}

final class SchematronValidatorFactory extends api.validation.ValidatorFactory {
  def name(): String = SchematronValidator.name
  def make(config: Properties): api.validation.Validator =
    SchematronValidatorFactory.makeValidator(config)
}
