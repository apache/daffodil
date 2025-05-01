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

import org.apache.daffodil.api.exceptions.DaffodilUnhandledSAXException;
import org.apache.daffodil.api.exceptions.DaffodilUnparseErrorSAXException;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;
import scala.Array;

/**
 * Accepts SAX callback events from any SAX XMLReader for unparsing
 */
public interface DaffodilUnparseContentHandler extends ContentHandler {
  /**
   * Returns the result of the SAX unparse containing diagnostic information. In the case of an
   * DaffodilUnhandledSAXException, this will return null.
   */
  UnparseResult getUnparseResult();

  void setDocumentLocator(Locator locator);

  void startDocument() throws DaffodilUnparseErrorSAXException, DaffodilUnhandledSAXException;

  void endDocument() throws DaffodilUnparseErrorSAXException, DaffodilUnhandledSAXException;

  void startPrefixMapping(String prefix, String uri);

  void endPrefixMapping(String prefix);

  void startElement(String uri, String localName, String qName, Attributes attributes) throws DaffodilUnparseErrorSAXException, DaffodilUnhandledSAXException;

  void endElement(String uri, String localName, String qName) throws DaffodilUnparseErrorSAXException, DaffodilUnhandledSAXException;

  void characters(Array<scala.Char> ch, int start, int length);

  void ignorableWhitespace(Array<scala.Char> ch, int start, int length);

  void processingInstruction(String target, String data);

  void skippedEntity(String name);
}
