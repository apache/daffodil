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
import java.io.StringWriter
import javax.xml.parsers.ParserConfigurationException
import javax.xml.parsers.SAXParserFactory
import javax.xml.transform.Templates
import javax.xml.transform.URIResolver
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.stream.StreamResult
import org.apache.daffodil.api.ValidatorInitializationException
import org.apache.daffodil.xml.XMLUtils
import org.xml.sax.InputSource
import org.xml.sax.SAXException
import org.xml.sax.XMLReader


/**
 * Schematron engine implementation
 */
object Schematron {
  val templatesRootDir = "iso-schematron-xslt2"

  private val featuress = Array(
    Feature.on(javax.xml.XMLConstants.FEATURE_SECURE_PROCESSING),
    Feature.off("http://xml.org/sax/features/external-general-entities"),
    Feature.off("http://xml.org/sax/features/external-parameter-entities"),
    Feature.off("http://apache.org/xml/features/nonvalidating/load-external-dtd"))

  def fromRules(rules: Templates) = new Schematron(xmlReader.get(), rules)

  def isoTemplateResolver(child: Option[URIResolver]) = new ClassPathUriResolver(templatesRootDir, child)

  // reduce overhead by caching the xml reader, but the SAXParser class is not thread safe so use a thread local
  private val xmlReader = new ThreadLocal[XMLReader] {
    override def initialValue(): XMLReader = {
      val f = try featuress.foldLeft(SAXParserFactory.newInstance){ (fac, ft) =>
        fac.setFeature(ft.name, ft.value); fac
      }
      catch {
        case ex@(_: ParserConfigurationException | _: SAXException) =>
          throw ValidatorInitializationException(s"Error setting feature on parser: ${ex.getMessage}")
      }
      f.setValidating(false)
      val xr = f.newSAXParser.getXMLReader
      XMLUtils.setSecureDefaults(xr)
      xr
    }
  }

  private case class Feature(name: String, value: Boolean)
  private object Feature {
    def on(name: String) = Feature(name, true)
    def off(name: String) = Feature(name, false)
  }
}

final class Schematron private(reader: XMLReader, templates: Templates) {
  private lazy val transformer = templates.newTransformer

  def validate(is: InputStream): String = {
    val writer = new StringWriter
    transformer.transform(new SAXSource(reader, new InputSource(is)), new StreamResult(writer))
    writer.toString
  }
}
