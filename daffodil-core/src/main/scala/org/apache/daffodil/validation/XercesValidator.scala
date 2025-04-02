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

import java.io.IOException
import java.net.URI
import java.util.Properties
import javax.xml.XMLConstants
import javax.xml.transform.stream.StreamSource

import org.apache.daffodil.api
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.xml.DFDLCatalogResolver
import org.apache.daffodil.lib.xml.XMLUtils

import XercesValidator.XercesValidatorImpl
import org.xml.sax.ErrorHandler
import org.xml.sax.SAXParseException

/**
 * Provides a XercesValidator instance
 *
 * SPI service name: xerces
 * 
 * Configuration requirements (only one must be present, but the value of xerces takes precedence if both are present):
 *   <ul>
 *    <li>xerces=schema_file_uri_string</li>
 *    <li>daffodil.rootSchema=schema_file_uri_string</li>
 *   </ul>
 */
class XercesValidatorFactory extends api.validation.ValidatorFactory {
  def name(): String = XercesValidator.name

  def make(config: Properties): api.validation.Validator =
    XercesValidatorFactory.makeValidator(config)
}

object XercesValidatorFactory {
  def makeValidator(config: Properties): api.validation.Validator = {
    if (config == null) {
      throw new api.validation.ValidatorInitializationException(
        "invalid configuration: missing xerces path"
      )
    }

    val schemaPath = config.getProperty(XercesValidator.name)
    lazy val defaultSchema = config.getProperty(api.validation.Validator.rootSchemaKey)
    val schemaFile = {
      if (!Misc.isNullOrBlank(schemaPath)) schemaPath
      else if (!Misc.isNullOrBlank(defaultSchema)) defaultSchema
      else
        throw new api.validation.ValidatorInitializationException(
          "invalid configuration: xerces validator path was empty"
        )
    }
    val uri = new URI(schemaFile)
    XercesValidator.fromURI(uri)
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
  ): Unit = {
    val eh = new XercesErrorHandler(validationHandler)
    validateXML(document, eh)
  }

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
    val is =
      try {
        schemaURI.toURL.openStream()
      } catch {
        case e: IOException =>
          throw new api.validation.ValidatorInitializationException(e.getMessage)
      }
    val stream = new StreamSource(is)
    stream.setSystemId(
      schemaURI.toString
    ) // must set this so that relative URIs will be created for import/include files.
    stream
  })
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
