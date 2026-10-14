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

import javax.xml.transform.Source

import org.apache.daffodil.api.DaffodilEntityResolver
import org.apache.daffodil.api.DaffodilEntityResolverFactory
import org.apache.daffodil.lib.xml.DFDLCatalogResolver

import org.apache.xerces.xni.XMLResourceIdentifier
import org.apache.xerces.xni.parser.XMLInputSource
import org.w3c.dom.ls.LSInput
import org.xml.sax.InputSource

/**
 * Makes resolvers that behave like the default resolver and print a marker to stderr
 * when asked to resolve a resource, so tests can tell they were used.
 */
class LoggingEntityResolverFactory extends DaffodilEntityResolverFactory {
  override def name(): String = "logging"
  override def make(): DaffodilEntityResolver = new LoggingEntityResolver
}

class DuplicateEntityResolverFactory extends LoggingEntityResolverFactory {
  override def name(): String = "duplicate"
}

object LoggingEntityResolver {
  val marker = "[logging-entity-resolver]"
}

class LoggingEntityResolver extends DaffodilEntityResolver {

  private def default = DFDLCatalogResolver.get

  override def resolveEntity(ri: XMLResourceIdentifier): XMLInputSource = {
    default.resolveEntity(ri)
  }

  override def resolveResource(
    resourceType: String,
    namespaceURI: String,
    publicId: String,
    systemId: String,
    baseURI: String
  ): LSInput = {
    System.err.println(LoggingEntityResolver.marker)
    default.resolveResource(resourceType, namespaceURI, publicId, systemId, baseURI)
  }

  override def resolveEntity(
    name: String,
    publicId: String,
    baseURI: String,
    systemId: String
  ): InputSource = {
    default.resolveEntity(name, publicId, baseURI, systemId)
  }

  override def resolve(href: String, base: String): Source = {
    default.resolve(href, base)
  }

  override def resolveURI(uri: String): String = {
    default.resolveURI(uri)
  }
}
