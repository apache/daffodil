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

import java.net.URI

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigValueFactory
import javax.xml.XMLConstants
import javax.xml.transform.stream.StreamSource
import org.apache.daffodil.api.ValidationException
import org.apache.daffodil.api.ValidationFailure
import org.apache.daffodil.api.ValidationResult
import org.apache.daffodil.api.ValidationWarning
import org.apache.daffodil.api.Validator
import org.apache.daffodil.api.ValidatorFactory
import org.apache.daffodil.validation.XercesValidator.XercesValidatorImpl
import org.apache.daffodil.xml.DFDLCatalogResolver
import org.xml.sax.ErrorHandler
import org.xml.sax.SAXParseException

import scala.collection.JavaConverters._
import scala.xml.SAXException

/**
 * Provides a XercesValidator instance
 *
 * SPI service name: xerces
 * Configuration requirements:
 *   - xerces: List[String] -  schema file names
 */
class XercesValidatorFactory extends ValidatorFactory {
  def name(): String = XercesValidator.name
  def make(config: Config): Validator = XercesValidatorFactory.makeValidator(config)
}

object XercesValidatorFactory {
  def makeValidator(config: Config): Validator = {
    val schemaFiles =
      if(config.hasPath(XercesValidator.name))
        config.getStringList(XercesValidator.name).asScala
      else Seq.empty
    new XercesValidator(schemaFiles)
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
class XercesValidator(schemaFileNames: Seq[String])
  extends Validator {

  private val schemaSources: Seq[javax.xml.transform.Source] = schemaFileNames.map { fn =>
    val uri = new URI(fn)
    val is = uri.toURL.openStream()
    val stream = new StreamSource(is)
    stream.setSystemId(uri.toString) // must set this so that relative URIs will be created for import/include files.
    stream
  }

  private val factory = new org.apache.xerces.jaxp.validation.XMLSchemaFactory()
  private val resolver = DFDLCatalogResolver.get
  factory.setResourceResolver(resolver)

  private val schema = factory.newSchema(schemaSources.toArray)

  private val validator = new ThreadLocal[XercesValidatorImpl] {
    override def initialValue(): XercesValidatorImpl =
      initializeValidator(schema.newValidator(), resolver)
  }

  def validateXML(document: java.io.InputStream): ValidationResult = {
    val documentSource = new StreamSource(document)

    // get the validator instance for this thread
    val xv = validator.get()

    // create a new error handler for this execution
    val eh = new XercesErrorHandler
    xv.setErrorHandler(eh)

    // validate the document
    xv.validate(documentSource)

    // error handler contents as daffodil result
    ValidationResult(eh.warnings, eh.errors)
  }

  private def initializeValidator(validator: XercesValidatorImpl, resolver: DFDLCatalogResolver): XercesValidatorImpl = {
    validator.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
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
}

private class XercesErrorHandler extends ErrorHandler {
  private var e = List.empty[ValidationFailure]
  private var w = List.empty[ValidationWarning]

  def errors: Seq[ValidationFailure] = e
  def warnings: Seq[ValidationWarning] = w

  override def warning(spe: SAXParseException): Unit =  w :+= SaxValidationWarning(spe)
  override def error(spe: SAXParseException): Unit = e :+= SaxValidationError(spe)
  override def fatalError(spe: SAXParseException): Unit = e :+= SaxValidationError(spe)
}

sealed abstract class SaxValidationResult(e: SAXException) extends Exception(e) with ValidationException
case class SaxValidationError(e: SAXException) extends SaxValidationResult(e) with ValidationFailure with ValidationException
case class SaxValidationWarning(e: SAXException) extends SaxValidationResult(e) with  ValidationWarning with ValidationException
