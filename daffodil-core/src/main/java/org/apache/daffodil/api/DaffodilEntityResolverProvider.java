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
 * Daffodil requests a resolver from the thread that is going to use it and never shares a
 * resolver between threads. A provider can therefore return a new resolver or a per-thread
 * resolver from each method, which need not be thread safe. A provider that returns the same
 * resolver to every caller must make that resolver thread safe. The provider itself must be
 * thread safe.
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
