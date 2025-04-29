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

import org.apache.xerces.xni.parser.XMLEntityResolver;
import org.w3c.dom.ls.LSResourceResolver;
import org.xml.sax.EntityResolver;
import org.apache.daffodil.lib.xml.DFDLCatalogResolver;

/**
 * Returns the EntityResolver used by Daffodil to resolve import/include
 * schemaLocations.
 * <p>
 * The entity resolver attempts to resolve namespaces and systemId's in the
 * following order:
 * <p>
 * 1. Use an org.apache.xml.resolver.Catalog/CatalogManager. By default the
 * Catalog only includes the daffodil-built-in-catalog.xml, but additional
 * catalogs can be added by putting CatalogManager.properties on the
 * classpath when daffodil is run.
 * <p>
 * 2. If not resolved in step 1, schemaLocations are resolved relative to the
 * importing schema URI, which could either be a file on the filesystem or in
 * a jar on the classpath.
 * <p>
 * The EntityResolver isn't thread safe, but it also is expensive and stateful,
 * so we use ThreadLocal to only create one instance per thread.
 */
public final class DaffodilXMLEntityResolver {

  private DaffodilXMLEntityResolver() {
    // private constructor to prevent instantiation
  }

  public static EntityResolver getEntityResolver() {
    return DFDLCatalogResolver.get();
  }

  public static XMLEntityResolver getXMLEntityResolver() {
    return DFDLCatalogResolver.get();
  }

  public static LSResourceResolver getLSResourceResolver() {
    return DFDLCatalogResolver.get();
  }
}
