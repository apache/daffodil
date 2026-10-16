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

package org.apache.daffodil.lib.xml

import java.net.URI
import java.util.concurrent.ConcurrentHashMap
import javax.xml.XMLConstants
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.Schema
import javax.xml.validation.SchemaFactory

import org.apache.daffodil.validation.XercesValidator

/**
 * Holds the compiled schemas, and the validators for them, that loaders use to validate the
 * documents they load.
 *
 * These are Daffodil's own schemas, which are the same for every document and always resolve
 * against Daffodil's built-in catalog, never against a custom entity resolver. A compiled
 * schema is thread safe and a validator is pooled, so loaders that share one of these create
 * each only once, even when they run at the same time.
 */
final class SchemaValidationCache {

  private val validators = new ConcurrentHashMap[URI, XercesValidator]()
  private val schemas = new ConcurrentHashMap[URI, Schema]()

  private def createValidator(schemaURI: URI): XercesValidator =
    XercesValidator.fromURL(schemaURI.toURL(), Some(EntityResolverProviders.noUserCatalogs))

  private def compileSchema(schemaURI: URI): Schema = {
    val sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
    sf.setErrorHandler(RethrowSchemaErrorHandler)
    sf.setResourceResolver(EntityResolverProviders.noUserCatalogs)
    sf.newSchema(new StreamSource(schemaURI.toString))
  }

  def validator(schemaURI: URI): XercesValidator =
    validators.computeIfAbsent(schemaURI, uri => createValidator(uri))

  def schema(schemaURI: URI): Schema =
    schemas.computeIfAbsent(schemaURI, uri => compileSchema(uri))
}
