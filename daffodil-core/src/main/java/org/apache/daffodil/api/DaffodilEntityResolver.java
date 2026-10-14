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

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import javax.xml.transform.URIResolver;
import org.apache.xerces.xni.parser.XMLEntityResolver;
import org.w3c.dom.ls.LSResourceResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.ext.EntityResolver2;

/**
 * An entity resolver that can be used wherever Daffodil, SAX, Xerces, DOM
 * load/ls, or javax.xml.transform needs to resolve a schema or entity. A
 * single implementation serves all of these roles.
 */
public interface DaffodilEntityResolver
    extends EntityResolver2, XMLEntityResolver, LSResourceResolver, URIResolver {

  /**
   * Resolves a namespace or other URI string to the URI of a schema.
   *
   * @return the resolved URI, or null if it cannot be resolved
   */
  String resolveURI(String uri);

  /**
   * Daffodil does not use DTDs, so by default there is no external subset.
   */
  @Override
  default InputSource getExternalSubset(String name, String baseURI)
      throws SAXException, IOException {
    return null;
  }

  /**
   * By default resolves with no entity name or base URI.
   */
  @Override
  default InputSource resolveEntity(String publicId, String systemId)
      throws SAXException, IOException {
    return resolveEntity(null, publicId, null, systemId);
  }

  /**
   * @return the catalog files consulted when resolving, used in diagnostics
   */
  default List<String> getCatalogFiles() {
    return Collections.emptyList();
  }
}
