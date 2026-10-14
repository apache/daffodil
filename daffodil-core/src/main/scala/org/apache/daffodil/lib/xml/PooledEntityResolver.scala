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
import scala.jdk.CollectionConverters.*

import org.apache.daffodil.api.DaffodilEntityResolver
import org.apache.daffodil.api.DaffodilEntityResolverFactory
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.util.SimpleNamedServiceLoader
import org.apache.daffodil.lib.util.ThreadSafePool

import org.apache.xerces.xni.XMLResourceIdentifier
import org.apache.xerces.xni.parser.XMLInputSource
import org.w3c.dom.ls.LSInput
import org.xml.sax.InputSource

/**
 * A resolver that is safe to use from any thread, built from a factory of resolvers that
 * are not. Each call runs on a resolver that no other thread is using at that moment.
 */
final class PooledEntityResolver(factory: DaffodilEntityResolverFactory)
  extends DaffodilEntityResolver {

  private val pool = new ThreadSafePool[DaffodilEntityResolver] {
    override def allocate(): DaffodilEntityResolver = factory.make()
  }

  override def resolveEntity(ri: XMLResourceIdentifier): XMLInputSource =
    pool.withInstance { _.resolveEntity(ri) }

  override def resolveResource(
    resourceType: String,
    namespaceURI: String,
    publicId: String,
    systemId: String,
    baseURI: String
  ): LSInput =
    pool.withInstance {
      _.resolveResource(resourceType, namespaceURI, publicId, systemId, baseURI)
    }

  override def resolveEntity(publicId: String, systemId: String): InputSource =
    pool.withInstance { _.resolveEntity(publicId, systemId) }

  override def getExternalSubset(name: String, baseURI: String): InputSource =
    pool.withInstance { _.getExternalSubset(name, baseURI) }

  override def resolveEntity(
    name: String,
    publicId: String,
    baseURI: String,
    systemId: String
  ): InputSource =
    pool.withInstance { _.resolveEntity(name, publicId, baseURI, systemId) }

  override def resolve(href: String, base: String): Source =
    pool.withInstance { _.resolve(href, base) }

  override def resolveURI(uri: String): String =
    pool.withInstance { _.resolveURI(uri) }

  override def getCatalogFiles: java.util.List[String] =
    pool.withInstance { _.getCatalogFiles }
}

object PooledEntityResolver {

  private lazy val custom: Option[DaffodilEntityResolver] =
    fromFactories(
      SimpleNamedServiceLoader
        .loadClass(classOf[DaffodilEntityResolverFactory])
        .values()
        .asScala
        .toSeq
    )

  /**
   * The resolver to use: one built from the registered factory if there is one, otherwise
   * the default resolver for the calling thread.
   */
  def resolver(): DaffodilEntityResolver = custom match {
    case Some(r) => r
    case None => DFDLCatalogResolver.get
  }

  /**
   * None if there are no factories, a pooled resolver if there is one, and an error
   * if there is more than one.
   */
  private def fromFactories(
    factories: Seq[DaffodilEntityResolverFactory]
  ): Option[DaffodilEntityResolver] = factories match {
    case Seq() => None
    case Seq(factory) => Some(new PooledEntityResolver(factory))
    case _ =>
      throw new IllegalStateException(
        "At most one DaffodilEntityResolverFactory may be registered, but found: " +
          factories.map { Misc.getNameFromClass(_) }.mkString(", ")
      )
  }
}
