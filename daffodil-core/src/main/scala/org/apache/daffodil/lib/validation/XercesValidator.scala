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
import javax.xml.XMLConstants
import javax.xml.transform.stream.StreamSource
import scala.jdk.CollectionConverters._
import scala.xml.SAXException

import org.apache.daffodil.api
import org.apache.daffodil.lib.iapi.ValidationResult
import org.apache.daffodil.lib.validation.XercesValidator.XercesValidatorImpl
import org.apache.daffodil.lib.xml.DFDLCatalogResolver
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.validation.ValidationException

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigValueFactory
import org.xml.sax.ErrorHandler
import org.xml.sax.SAXParseException

/**
 * Provides a XercesValidator instance
 *
 * SPI service name: xerces
 * Configuration requirements:
 *   - xerces: List[String] -  schema file names
 */
class XercesValidatorFactory extends api.validation.ValidatorFactory {
  def name(): String = XercesValidator.name
  def make(config: Config): api.validation.Validator =
    XercesValidatorFactory.makeValidator(config)
}

object XercesValidatorFactory {
  def makeValidator(uriString: String): api.validation.Validator = {
    val config = XercesValidatorFactory.makeConfig(Seq(uriString))
    makeValidator(config)
  }

  def makeValidator(config: Config): api.validation.Validator = {
    val schemaFiles =
      if (config.hasPath(XercesValidator.name))
        config.getStringList(XercesValidator.name).asScala
      else Seq.empty
    XercesValidator.fromFiles(schemaFiles.toSeq)
  }

  def makeConfig(uris: Seq[String]): Config = {
    val v = ConfigValueFactory.fromIterable(uris.asJava)
    ConfigFactory.parseMap(Map(XercesValidator.name -> v).asJava)
  }
}

/**
 * Use this for extra validation passes in the TDML Runner
 * to do a validation pass on the TDML expected Infoset w.r.t. the model and to
 * do a validation pass on the actual result w.r.t. the model as an XML document.
 */
class XercesValidator(schemaSources: Seq[javax.xml.transform.Source])
  extends api.validation.Validator {

  private val factory = new org.apache.xerces.jaxp.validation.XMLSchemaFactory()
  private val resolver = DFDLCatalogResolver.get
  factory.setResourceResolver(resolver)

  private val schema = factory.newSchema(schemaSources.toArray)

  private val validator = new ThreadLocal[XercesValidatorImpl] {
    override def initialValue(): XercesValidatorImpl =
      initializeValidator(schema.newValidator, resolver)
  }

  def validateXML(document: java.io.InputStream): api.validation.ValidationResult =
    validateXML(document, new XercesErrorHandler)

  def validateXML(
    document: java.io.InputStream,
    eh: ErrorHandler
  ): api.validation.ValidationResult = {

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

    eh match {
      case xeh: XercesErrorHandler => ValidationResult(xeh.warnings, xeh.errors)
      case _ => {
        // When the validator is called by DaffodilXMLLoader, the
        // error handler is not the standard Xerces one, but one that
        // is supplied by the loader. We don't need the validation result
        // in that case, because the diagnostics have already been gathered
        // by callback to the error handler, so we don't construct a ValidationResult.
        ValidationResult.empty
      }
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

  def fromURIs(schemaURIs: Seq[URI]) = new XercesValidator(schemaURIs.map { uri =>
    val is = uri.toURL.openStream()
    val stream = new StreamSource(is)
    stream.setSystemId(
      uri.toString
    ) // must set this so that relative URIs will be created for import/include files.
    stream
  })

  def fromFiles(schemaFileNames: Seq[String]) =
    fromURIs(schemaFileNames.map { new URI(_) })
}

private class XercesErrorHandler extends ErrorHandler {
  private var e = List.empty[api.validation.ValidationFailure]
  private var w = List.empty[api.validation.ValidationWarning]

  def errors: Seq[api.validation.ValidationFailure] = e
  def warnings: Seq[api.validation.ValidationWarning] = w

  override def warning(spe: SAXParseException): Unit = w :+= SaxValidationWarning(spe)
  override def error(spe: SAXParseException): Unit = e :+= SaxValidationError(spe)
  override def fatalError(spe: SAXParseException): Unit = e :+= SaxValidationError(spe)
}

sealed abstract class SaxValidationResult(e: SAXException)
  extends Exception(e)
  with ValidationException
case class SaxValidationError(e: SAXException)
  extends SaxValidationResult(e)
  with api.validation.ValidationFailure
  with ValidationException
case class SaxValidationWarning(e: SAXException)
  extends SaxValidationResult(e)
  with api.validation.ValidationWarning
  with ValidationException
