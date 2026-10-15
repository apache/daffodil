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
package org.apache.daffodil.api;

import org.apache.daffodil.lib.util.SimpleNamedLoadableService;
import org.apache.xerces.xni.parser.XMLEntityResolver;
import org.w3c.dom.ls.LSResourceResolver;
import org.xml.sax.EntityResolver;

import javax.xml.transform.URIResolver;

/**
 * Implement this interface and register it with SPI, by listing the implementation in
 * META-INF/services/org.apache.daffodil.api.DaffodilEntityResolverProvider, to replace the
 * resolvers that Daffodil uses to resolve import/include schemaLocations.
 * <p>
 * Implementations must have a zero-argument constructor. At most one provider may be
 * registered besides Daffodil's own default provider, which is used when there is none, and
 * providers registered with the same name are ignored.
 * <p>
 * Daffodil requests a resolver for an operation, keeps it for the whole operation, and never
 * uses a resolver from more than one thread at a time. Daffodil may use a resolver from
 * different threads one after another, so a resolver must not assume that only the thread that
 * created it uses it. A provider can therefore return a new resolver from each method, which
 * need not be thread safe. A provider that returns the same
 * resolver to every caller must make that resolver thread safe. The provider itself must be
 * thread safe.
 * <p>
 * Daffodil keeps the resolvers it gets in pools that last as long as the compiler that uses
 * them, and reuses them for many operations. A resolver must therefore work correctly when it
 * is used again, including after an operation that threw an exception. Anything a resolver
 * caches stays in memory for as long as the compiler does.
 * <p>
 * Each method returns the kind of resolver Daffodil needs for one operation, and they need not
 * return the same object. A provider that does not support a kind of resolver may throw an
 * UnsupportedOperationException, in which case the operations that need it will fail.
 */
public interface DaffodilEntityResolverProvider extends SimpleNamedLoadableService {

  /**
   * Unique name of this provider. Providers that share a name are all ignored with a warning,
   * so the name must not clash with any other provider on the classpath. The name
   * "default-entity-resolver" is used by Daffodil's own provider.
   *
   * @return name of the provider
   */
  String name();

  /**
   * @return entity resolver
   */
  EntityResolver getEntityResolver();

  /**
   * @return xml entity resolver
   */
  XMLEntityResolver getXMLEntityResolver();

  /**
   * @return ls resource resolver
   */
  LSResourceResolver getLSResourceResolver();

  /**
   * @return uri resolver
   */
  URIResolver getURIResolver();
}
