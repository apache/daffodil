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

package org.apache.daffodil.util

import javax.xml.transform.stream.StreamSource
import javax.xml.XMLConstants
import scala.xml.parsing.NoBindingFactoryAdapter
import java.net.URI
import org.apache.daffodil.xml.DFDLCatalogResolver
import scala.collection.mutable
import org.xml.sax.ErrorHandler

/**
 * Use this for extra validation passes in the TDML Runner
 * to do a validation pass on the TDML expected Infoset w.r.t. the model and to
 * do a validation pass on the actual result w.r.t. the model as an XML document.
 */
object Validator extends NoBindingFactoryAdapter {

  private type CacheType = mutable.HashMap[Seq[String], javax.xml.validation.Validator]

  private val validationSchemaCache =
    new ThreadLocal[CacheType] {
      override def initialValue =
        new CacheType
    }

  def validateXMLSources(schemaFileNames: Seq[String], document: java.io.InputStream, errHandler: ErrorHandler): Unit = {
    val cache = validationSchemaCache.get()
    val validator = {
      val optCachedValidator = cache.get(schemaFileNames)
      optCachedValidator match {
        case Some(validator) => {
          validator.reset()
          validator
        }
        case None => {
          val schemaSources: Seq[javax.xml.transform.Source] = schemaFileNames.map { fn =>
            {
              val uri = new URI(fn)
              val is = uri.toURL.openStream()
              val stream = new StreamSource(is)
              stream.setSystemId(uri.toString) // must set this so that relative URIs will be created for import/include files.
              stream
            }
          }

          val factory = new org.apache.xerces.jaxp.validation.XMLSchemaFactory()
          factory.setErrorHandler(errHandler)
          val resolver = DFDLCatalogResolver.get
          factory.setResourceResolver(resolver)
          val schema = factory.newSchema(schemaSources.toArray)
          val validator = schema.newValidator()
          validator.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
          //
          validator.setFeature("http://xml.org/sax/features/validation", true)

          // If you enable the feature below, it seems to do no validation at all. Just passes.
          //          validator.setFeature("http://apache.org/xml/features/validation/dynamic", true)

          validator.setFeature("http://apache.org/xml/features/validation/schema", true)
          validator.setFeature("http://apache.org/xml/features/validation/schema-full-checking", true)
          validator.setErrorHandler(errHandler)
          validator.setResourceResolver(resolver)
          cache.put(schemaFileNames, validator)
          validator
        }
      }
    }
    val documentSource = new StreamSource(document)
    validator.validate(documentSource)
  }
}

