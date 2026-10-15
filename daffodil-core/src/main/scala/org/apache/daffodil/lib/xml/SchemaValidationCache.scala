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

import org.apache.daffodil.api.DaffodilXMLEntityResolver
import org.apache.daffodil.lib.util.ThreadSafePool
import org.apache.daffodil.validation.XercesValidator

import org.apache.xerces.xni.parser.XMLEntityResolver
import org.w3c.dom.ls.LSResourceResolver

/**
 * Holds what loaders share instead of each creating their own: the compiled schemas, and the
 * validators for them, that are used to validate the documents they load, and the resolvers
 * they use to find schema files.
 *
 * A compiled schema is thread safe and a validator is pooled, so they can be shared by loaders
 * that run at the same time. A resolver is borrowed from a pool for the duration of one
 * operation, so no two threads use the same resolver at once.
 */
final class SchemaValidationCache {

  private val validators = new ConcurrentHashMap[URI, XercesValidator]()
  private val schemas = new ConcurrentHashMap[URI, Schema]()

  private val xmlResolvers = new ThreadSafePool[XMLEntityResolver] {
    override def allocate(): XMLEntityResolver = DaffodilXMLEntityResolver.getXMLEntityResolver
  }

  private val lsResolvers = new ThreadSafePool[LSResourceResolver] {
    override def allocate(): LSResourceResolver =
      DaffodilXMLEntityResolver.getLSResourceResolver
  }

  def withXMLResolver[A](f: XMLEntityResolver => A): A = xmlResolvers.withInstance(f)

  def withLSResolver[A](f: LSResourceResolver => A): A = lsResolvers.withInstance(f)

  private def createValidator(schemaURI: URI): XercesValidator =
    XercesValidator.fromURL(schemaURI.toURL())

  private def compileSchema(schemaURI: URI): Schema =
    withLSResolver { lsResolver =>
      val sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
      sf.setErrorHandler(RethrowSchemaErrorHandler)
      sf.setResourceResolver(lsResolver)
      sf.newSchema(new StreamSource(schemaURI.toString))
    }

  def validator(schemaURI: URI): XercesValidator =
    validators.computeIfAbsent(schemaURI, uri => createValidator(uri))

  def schema(schemaURI: URI): Schema =
    schemas.computeIfAbsent(schemaURI, uri => compileSchema(uri))
}
