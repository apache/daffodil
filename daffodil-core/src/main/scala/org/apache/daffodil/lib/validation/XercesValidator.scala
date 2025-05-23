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

package org.apache.daffodil.lib.validation

import java.net.URI
import java.nio.file.Paths
import java.util.Properties
import javax.xml.XMLConstants
import javax.xml.transform.stream.StreamSource

import org.apache.daffodil.api
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.validation.XercesValidator.XercesValidatorImpl
import org.apache.daffodil.lib.xml.DFDLCatalogResolver
import org.apache.daffodil.lib.xml.XMLUtils

import org.xml.sax.ErrorHandler
import org.xml.sax.SAXParseException

/**
 * Provides a XercesValidator instance
 *
 * SPI service name: xerces
 * Configuration requirements:
 *   - daffodil.rootSchema=schema_file_uri_string
 */
class XercesValidatorFactory extends api.validation.ValidatorFactory {
  def name(): String = XercesValidator.name

  def make(config: Properties): api.validation.Validator =
    XercesValidatorFactory.makeValidator(config)
}

object XercesValidatorFactory {
  def makeValidator(uriString: String): api.validation.Validator = {
    val config = XercesValidatorFactory.makeConfig(uriString)
    makeValidator(config)
  }

  def makeValidator(config: Properties): api.validation.Validator = {
    val schemaPath = config.getProperty(XercesValidator.name)
    val schemaFile =
      if (schemaPath != null) schemaPath
      else config.getProperty(XercesValidator.ConfigKeys.rootSchema)
    if (Misc.isNullOrBlank(schemaFile)) null
    else XercesValidator.fromFile(schemaFile)
  }

  def makeConfig(uri: String): Properties = {
    val config = new Properties()
    config.setProperty(XercesValidator.ConfigKeys.rootSchema, uri)
    config
  }
}

/**
 * Use this for extra validation passes in the TDML Runner
 * to do a validation pass on the TDML expected Infoset w.r.t. the model and to
 * do a validation pass on the actual result w.r.t. the model as an XML document.
 */
class XercesValidator(schemaSource: javax.xml.transform.Source)
  extends api.validation.Validator {

  private val factory = new org.apache.xerces.jaxp.validation.XMLSchemaFactory()
  private val resolver = DFDLCatalogResolver.get
  factory.setResourceResolver(resolver)

  private val schema = factory.newSchema(schemaSource)

  private val validator = new ThreadLocal[XercesValidatorImpl] {
    override def initialValue(): XercesValidatorImpl =
      initializeValidator(schema.newValidator, resolver)
  }

  def validateXML(
    document: java.io.InputStream,
    validationHandler: api.validation.ValidationHandler
  ): Unit =
    validateXML(document, new XercesErrorHandler(validationHandler))

  def validateXML(
    document: java.io.InputStream,
    eh: ErrorHandler
  ): Unit = {

    val documentSource = new StreamSource(document)

    // get the validator instance for this thread
    val xv = validator.get()

    xv.setErrorHandler(eh)

    // validate the document
    try {
      xv.validate(documentSource)
    } catch {
      // can be thrown by the resolver if it cannot
      // resolve the schemaLocation of an include/import.
      // Regular Xerces doesn't report this as an error.
      case spe: SAXParseException => eh.error(spe)
    }
  }

  private def initializeValidator(
    validator: XercesValidatorImpl,
    resolver: DFDLCatalogResolver
  ): XercesValidatorImpl = {
    validator.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
    validator.setFeature(XMLUtils.XML_DISALLOW_DOCTYPE_FEATURE, true)
    validator.setFeature("http://xml.org/sax/features/validation", true)
    validator.setFeature("http://apache.org/xml/features/validation/schema", true)
    validator.setFeature("http://apache.org/xml/features/validation/schema-full-checking", true)
    validator.setResourceResolver(resolver)
    validator
  }
}

object XercesValidator {
  private type XercesValidatorImpl = javax.xml.validation.Validator
  val name = "xerces"

  def fromURI(schemaURI: URI) = new XercesValidator({
    val is = schemaURI.toURL.openStream()
    val stream = new StreamSource(is)
    stream.setSystemId(
      schemaURI.toString
    ) // must set this so that relative URIs will be created for import/include files.
    stream
  })

  def fromFile(schemaFileName: String) = {
    val uri = new URI(schemaFileName)
    if (uri.isAbsolute) {
      fromURI(uri)
    } else {
      fromURI(Paths.get(schemaFileName).toUri)
    }
  }

  object ConfigKeys {
    val rootSchema = api.validation.Validator.rootSchemaKey
  }
}

private class XercesErrorHandler(validationHandler: api.validation.ValidationHandler)
  extends ErrorHandler {
  override def warning(spe: SAXParseException): Unit =
    validationHandler.validationErrorNoContext(spe)
  override def error(spe: SAXParseException): Unit =
    validationHandler.validationErrorNoContext(spe)
  override def fatalError(spe: SAXParseException): Unit =
    validationHandler.validationErrorNoContext(spe)
}
