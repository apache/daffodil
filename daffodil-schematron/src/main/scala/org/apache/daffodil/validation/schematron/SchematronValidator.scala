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

import java.io.File
import java.io.FileWriter
import java.io.InputStream
import java.io.StringWriter
import java.net.URI
import javax.xml.transform.Templates
import javax.xml.transform.Transformer
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.stream.StreamResult
import scala.util.Try
import scala.xml.Elem
import scala.xml.XML

import org.apache.daffodil.api
import org.apache.daffodil.lib.util.ThreadSafePool
import org.apache.daffodil.lib.xml.DaffodilSAXParserFactory

import org.xml.sax.InputSource
import org.xml.sax.XMLReader

/**
 * Daffodil Validator implementation for ISO schematron
 */
final class SchematronValidator(
  templates: Templates,
  svrlPath: Option[URI]
) extends api.validation.Validator {

  // XMLReader and Transformer are not thread safe, so we use a ThreadSafePool. The use of a
  // pool allows reuse of objects that are expensive to create while ensuring each Thread gets
  // their own instance. Note that we do not use ThreadLocal, since that can lead to memory
  // leaks that cannot be easily cleaned up
  val readerTransformerPool = new ThreadSafePool[(XMLReader, Transformer)] {
    override def allocate(): (XMLReader, Transformer) = {
      val factory = DaffodilSAXParserFactory()
      factory.setValidating(false)
      val reader = factory.newSAXParser.getXMLReader
      val transformer = templates.newTransformer
      (reader, transformer)
    }
  }

  def validateXML(
    document: InputStream,
    handler: api.validation.ValidationHandler
  ): Unit = {
    val writer = new StringWriter

    readerTransformerPool.withInstance { rt =>
      val (reader, transformer) = rt
      val source = new SAXSource(reader, new InputSource(document))
      val result = new StreamResult(writer)
      transformer.transform(source, result)
    }

    val svrlString = writer.toString
    val svrl = XML.loadString(svrlString)
    svrl.child.collect { case f @ Elem("svrl", "failed-assert", _, _, msg*) =>
      handler.validationError(msg.text.trim, (f \ "@location").text)
    }
    svrlPath.foreach { uri =>
      Try {
        val writer = new FileWriter(new File(uri))
        writer.write(svrlString)
        writer.flush()
      }.failed.foreach(handler.validationErrorNoContext)
    }
  }
}

object SchematronValidator {
  val name = "schematron"
  val templatesRootDir = "iso-schematron-xslt2"

  object ConfigKeys {
    val schPath = s"$name"
    val svrlOutputFile = s"$name.svrl.file"
  }
}
