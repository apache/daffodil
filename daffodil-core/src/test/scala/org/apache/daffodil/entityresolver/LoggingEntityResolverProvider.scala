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
package org.apache.daffodil.entityresolver

import javax.xml.transform.URIResolver

import org.apache.daffodil.api.DaffodilEntityResolverProvider
import org.apache.daffodil.lib.xml.DFDLCatalogResolver

import org.apache.xerces.xni.parser.XMLEntityResolver
import org.w3c.dom.ls.LSInput
import org.w3c.dom.ls.LSResourceResolver
import org.xml.sax.EntityResolver

/**
 * Provides the default resolvers, except for an LSResourceResolver that prints a marker to
 * stderr when asked to resolve a resource, so tests can tell the provider was used.
 */
class LoggingEntityResolverProvider extends DaffodilEntityResolverProvider {
  override def name(): String = "logging"
  override def getEntityResolver(): EntityResolver = DFDLCatalogResolver.get
  override def getXMLEntityResolver(): XMLEntityResolver = DFDLCatalogResolver.get
  override def getLSResourceResolver(): LSResourceResolver = new LoggingLSResourceResolver
  override def getURIResolver(): URIResolver = DFDLCatalogResolver.get
}

class DuplicateEntityResolverProvider extends LoggingEntityResolverProvider {
  override def name(): String = "duplicate"
}

object LoggingLSResourceResolver {
  val marker = "[logging-entity-resolver]"
}

class LoggingLSResourceResolver extends LSResourceResolver {
  override def resolveResource(
    resourceType: String,
    namespaceURI: String,
    publicId: String,
    systemId: String,
    baseURI: String
  ): LSInput = {
    System.err.println(LoggingLSResourceResolver.marker)
    DFDLCatalogResolver.get.resolveResource(
      resourceType,
      namespaceURI,
      publicId,
      systemId,
      baseURI
    )
  }
}
