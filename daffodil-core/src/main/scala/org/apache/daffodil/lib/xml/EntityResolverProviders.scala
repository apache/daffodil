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

import javax.xml.transform.URIResolver
import scala.jdk.CollectionConverters.*

import org.apache.daffodil.api.DaffodilEntityResolverProvider
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.util.SimpleNamedServiceLoader

import org.apache.xerces.xni.parser.XMLEntityResolver
import org.w3c.dom.ls.LSResourceResolver
import org.xml.sax.EntityResolver

/**
 * The provider Daffodil registers with SPI. It provides a new resolver on each call, so
 * callers should keep the one they get for the whole operation.
 */
final class DefaultEntityResolverProvider extends DaffodilEntityResolverProvider {
  override def name(): String = "default-entity-resolver"
  override def getEntityResolver(): EntityResolver = new DFDLCatalogResolver()
  override def getXMLEntityResolver(): XMLEntityResolver = new DFDLCatalogResolver()
  override def getLSResourceResolver(): LSResourceResolver = new DFDLCatalogResolver()
  override def getURIResolver(): URIResolver = new DFDLCatalogResolver()
}

object EntityResolverProviders {

  private lazy val provider: DaffodilEntityResolverProvider = {
    val all = SimpleNamedServiceLoader
      .loadClass(classOf[DaffodilEntityResolverProvider])
      .values()
      .asScala
      .toSeq
    val (defaults, custom) = all.partition { _.isInstanceOf[DefaultEntityResolverProvider] }
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

  /**
   * The provider registered with SPI besides the default if there is one, otherwise the
   * default provider. Resolvers can be expensive to create and need not be thread safe, so
   * callers should request one per operation, keep it for the whole operation, and not use it
   * from more than one thread at a time.
   */
  def get(): DaffodilEntityResolverProvider = provider
}
