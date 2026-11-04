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

import org.apache.xerces.xni.XMLResourceIdentifier
import org.apache.xerces.xni.parser.XMLEntityResolver
import org.apache.xerces.xni.parser.XMLInputSource
import org.w3c.dom.ls.LSInput
import org.w3c.dom.ls.LSResourceResolver
import org.xml.sax.EntityResolver

object StrictEntityResolverProvider {

  /**
   * The namespaces of Daffodil's own schemas, which Daffodil must resolve itself.
   */
  val builtInNamespaces: Set[String] = Set(
    "http://www.w3.org/2001/XMLSchema",
    "http://www.w3.org/XML/1998/namespace",
    "http://www.ogf.org/dfdl/dfdl-1.0/",
    "http://www.ogf.org/dfdl/dfdl-1.0/extensions",
    "urn:ogf:dfdl:2013:imp:daffodil.apache.org:2018:int",
    "urn:ogf:dfdl:2013:imp:daffodil.apache.org:2018:ext"
  )

  def check(namespace: String): Unit = {
    if (namespace != null && builtInNamespaces.contains(namespace)) {
      throw new AssertionError(
        s"strict provider was asked for the built-in namespace $namespace"
      )
    }
  }
}

/**
 * Provides the default resolvers, but its XML and LS resolvers fail if they are asked for a
 * namespace that belongs to Daffodil's own schemas.
 */
class StrictEntityResolverProvider extends DaffodilEntityResolverProvider {
  override def name(): String = "strict"
  override def getEntityResolver(): EntityResolver = new DFDLCatalogResolver()
  override def getXMLEntityResolver(): XMLEntityResolver = new StrictXMLEntityResolver
  override def getLSResourceResolver(): LSResourceResolver = new StrictLSResourceResolver
  override def getURIResolver(): URIResolver = new DFDLCatalogResolver()
}

class StrictXMLEntityResolver extends XMLEntityResolver {

  private val delegate = new DFDLCatalogResolver()

  override def resolveEntity(ri: XMLResourceIdentifier): XMLInputSource = {
    StrictEntityResolverProvider.check(ri.getNamespace())
    delegate.resolveEntity(ri)
  }
}

class StrictLSResourceResolver extends LSResourceResolver {

  private val delegate = new DFDLCatalogResolver()

  override def resolveResource(
    resourceType: String,
    namespaceURI: String,
    publicId: String,
    systemId: String,
    baseURI: String
  ): LSInput = {
    StrictEntityResolverProvider.check(namespaceURI)
    delegate.resolveResource(resourceType, namespaceURI, publicId, systemId, baseURI)
  }
}
