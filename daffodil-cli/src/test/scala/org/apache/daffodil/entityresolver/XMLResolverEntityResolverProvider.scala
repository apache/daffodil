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

import java.util.List as JList
import javax.xml.transform.URIResolver

import org.apache.daffodil.api.DaffodilEntityResolverProvider

import org.apache.xerces.xni.parser.XMLEntityResolver
import org.w3c.dom.ls.LSResourceResolver
import org.xml.sax.EntityResolver
import org.xmlresolver.ResolverFeature
import org.xmlresolver.XMLResolver
import org.xmlresolver.XMLResolverConfiguration

/**
 * Provides the resolvers of an org.xmlresolver.XMLResolver that reads the catalog named
 * xmlresolver-test-catalog.xml on the classpath, to show an off-the-shelf resolver can be
 * registered as the provider.
 */
class XMLResolverEntityResolverProvider extends DaffodilEntityResolverProvider {

  private def newResolver(): XMLResolver = {
    val catalog = classOf[XMLResolverEntityResolverProvider].getResource(
      "/xmlresolver-test-catalog.xml"
    )
    val config = new XMLResolverConfiguration()
    config.setFeature(ResolverFeature.CATALOG_FILES, JList.of(catalog.toString))
    new XMLResolver(config)
  }

  override def name(): String = "xmlresolver"
  override def getEntityResolver(): EntityResolver = newResolver().getEntityResolver()
  override def getXMLEntityResolver(): XMLEntityResolver = newResolver().getXMLEntityResolver()
  override def getLSResourceResolver(): LSResourceResolver =
    newResolver().getLSResourceResolver()
  override def getURIResolver(): URIResolver = newResolver().getURIResolver()
}
