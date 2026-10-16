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

import javax.xml.transform.Source
import javax.xml.transform.URIResolver
import javax.xml.transform.stream.StreamSource
import scala.jdk.CollectionConverters.*

import org.apache.daffodil.api.DaffodilEntityResolverProvider
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.util.SimpleNamedServiceLoader

import org.apache.xerces.xni.XMLResourceIdentifier
import org.apache.xerces.xni.parser.XMLEntityResolver
import org.apache.xerces.xni.parser.XMLInputSource
import org.w3c.dom.ls.LSInput
import org.w3c.dom.ls.LSResourceResolver
import org.xml.sax.EntityResolver
import org.xml.sax.InputSource

/**
 * The provider Daffodil registers with SPI. Every kind of resolver it provides uses the
 * built-in catalog and any catalogs the user configured, which are read when the resolver is
 * created.
 */
final class DefaultEntityResolverProvider extends DaffodilEntityResolverProvider {
  override def name(): String = "default-entity-resolver"
  override def getEntityResolver(): EntityResolver = new DFDLCatalogResolver()
  override def getXMLEntityResolver(): XMLEntityResolver =
    new DFDLCatalogResolver()
  override def getLSResourceResolver(): LSResourceResolver =
    new DFDLCatalogResolver()
  override def getURIResolver(): URIResolver = new DFDLCatalogResolver()
}

/**
 * Wraps a custom provider so that its resolvers are only asked for what Daffodil does not
 * resolve itself. A namespace of Daffodil's own schemas is always resolved by noUserCatalogs,
 * the resolver that ignores user catalogs, which also honors a schemaLocation that names one
 * of them. Anything else is looked up by usesUserCatalogs, in the built-in catalog and then
 * any the user configured, and only the rest goes to the provider.
 */
private[xml] final class CatalogFirstProvider(
  noUserCatalogs: DFDLCatalogResolver,
  provider: DaffodilEntityResolverProvider
) extends DaffodilEntityResolverProvider {
  override def name(): String = provider.name()

  override def getEntityResolver(): EntityResolver =
    new CatalogFirstEntityResolver(new DFDLCatalogResolver(), provider.getEntityResolver())

  override def getXMLEntityResolver(): XMLEntityResolver =
    new CatalogFirstXMLEntityResolver(
      new DFDLCatalogResolver(),
      noUserCatalogs,
      provider.getXMLEntityResolver()
    )

  override def getLSResourceResolver(): LSResourceResolver =
    new CatalogFirstLSResourceResolver(
      new DFDLCatalogResolver(),
      noUserCatalogs,
      provider.getLSResourceResolver()
    )

  override def getURIResolver(): URIResolver =
    new CatalogFirstURIResolver(new DFDLCatalogResolver(), provider.getURIResolver())
}

private[xml] final class CatalogFirstEntityResolver(
  usesUserCatalogs: DFDLCatalogResolver,
  user: EntityResolver
) extends EntityResolver {
  override def resolveEntity(publicId: String, systemId: String): InputSource =
    usesUserCatalogs.resolveFromCatalog(null, systemId) match {
      case Some(uri) => {
        val source = new InputSource()
        source.setPublicId(publicId)
        source.setSystemId(uri.toString)
        source
      }
      case None => user.resolveEntity(publicId, systemId)
    }
}

private[xml] final class CatalogFirstXMLEntityResolver(
  usesUserCatalogs: DFDLCatalogResolver,
  noUserCatalogs: DFDLCatalogResolver,
  user: XMLEntityResolver
) extends XMLEntityResolver {
  override def resolveEntity(ri: XMLResourceIdentifier): XMLInputSource = {
    val ns = ri.getNamespace()
    val resolved =
      if (noUserCatalogs.mapsNamespace(ns)) {
        noUserCatalogs.resolveToURI(ns, ri.getLiteralSystemId(), ri.getBaseSystemId())
      } else {
        usesUserCatalogs.resolveFromCatalog(ns, ri.getLiteralSystemId())
      }
    resolved match {
      case Some(uri) => new XMLInputSource(null, uri.toString, null)
      case None => user.resolveEntity(ri)
    }
  }
}

private[xml] final class CatalogFirstLSResourceResolver(
  usesUserCatalogs: DFDLCatalogResolver,
  noUserCatalogs: DFDLCatalogResolver,
  user: LSResourceResolver
) extends LSResourceResolver {
  override def resolveResource(
    resourceType: String,
    namespaceURI: String,
    publicId: String,
    systemId: String,
    baseURI: String
  ): LSInput = {
    val resolved =
      if (noUserCatalogs.mapsNamespace(namespaceURI)) {
        noUserCatalogs.resolveToURI(namespaceURI, systemId, baseURI)
      } else {
        usesUserCatalogs.resolveFromCatalog(namespaceURI, systemId)
      }
    resolved match {
      case Some(uri) => new InputStreamLSInput(publicId, uri.toString, null)
      case None => user.resolveResource(resourceType, namespaceURI, publicId, systemId, baseURI)
    }
  }
}

private[xml] final class CatalogFirstURIResolver(
  usesUserCatalogs: DFDLCatalogResolver,
  user: URIResolver
) extends URIResolver {
  override def resolve(href: String, base: String): Source =
    usesUserCatalogs.resolveFromCatalog(null, href) match {
      case Some(uri) => new StreamSource(uri.toString)
      case None => user.resolve(href, base)
    }
}

object EntityResolverProviders {

  /**
   * Resolves only through the built-in catalog, for Daffodil's own schemas. It is thread
   * safe, so it is shared. It ignores user catalogs, so nothing configured later can change it.
   */
  lazy val noUserCatalogs: DFDLCatalogResolver = new DFDLCatalogResolver(userCatalogs = false)

  /**
   * The custom provider if there is exactly one besides the default, otherwise the default.
   */
  private[xml] def selectProvider(
    providers: Seq[DaffodilEntityResolverProvider]
  ): DaffodilEntityResolverProvider = {
    val (defaults, custom) = providers.partition {
      _.isInstanceOf[DefaultEntityResolverProvider]
    }
    custom match {
      case Seq(p) => p
      case Seq() => {
        if (defaults.isEmpty) {
          throw new IllegalStateException(
            "No DaffodilEntityResolverProvider found. Check that the META-INF/services entries " +
              "are on the classpath. Providers registered with duplicate names are ignored."
          )
        }
        defaults.head
      }
      case _ =>
        throw new IllegalStateException(
          "At most one DaffodilEntityResolverProvider may be registered besides the default, " +
            "but found: " + custom.map { Misc.getNameFromClass(_) }.mkString(", ")
        )
    }
  }

  private lazy val provider: DaffodilEntityResolverProvider = {
    val loaded = SimpleNamedServiceLoader
      .loadClass(classOf[DaffodilEntityResolverProvider])
      .values()
      .asScala
      .toSeq
    selectProvider(loaded) match {
      case default: DefaultEntityResolverProvider => default
      case custom => new CatalogFirstProvider(noUserCatalogs, custom)
    }
  }

  /**
   * The provider Daffodil uses for user schemas: the default provider, or the custom one
   * behind the catalogs. Resolvers can be expensive to create and need not be thread safe, so
   * callers should request one per operation, keep it for the whole operation, and not use it
   * from more than one thread at a time.
   */
  def get(): DaffodilEntityResolverProvider = provider
}
