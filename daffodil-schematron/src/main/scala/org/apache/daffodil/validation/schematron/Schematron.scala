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
import javax.xml.transform.Templates
import javax.xml.transform.URIResolver
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.stream.StreamResult
import org.apache.daffodil.xml.DaffodilSAXParserFactory
import org.xml.sax.InputSource
import org.xml.sax.XMLReader


/**
 * Schematron engine implementation
 */
object Schematron {
  val templatesRootDir = "iso-schematron-xslt2"

  def fromRules(rules: Templates) = new Schematron(xmlReader.get(), rules)

  def isoTemplateResolver(child: Option[URIResolver]) = new ClassPathUriResolver(templatesRootDir, child)

  // reduce overhead by caching the xml reader, but the SAXParser class is not thread safe so use a thread local
  private val xmlReader = new ThreadLocal[XMLReader] {
    override def initialValue(): XMLReader = {
      val f = DaffodilSAXParserFactory()
      f.setValidating(false)
      val xr = f.newSAXParser.getXMLReader
      xr
    }
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
