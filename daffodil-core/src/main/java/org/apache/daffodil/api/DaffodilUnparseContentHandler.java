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

import org.apache.daffodil.api.exceptions.DaffodilUnparseErrorSAXException;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;

/**
 * Accepts SAX callback events from any SAX XMLReader for unparsing
 */
public interface DaffodilUnparseContentHandler extends ContentHandler {
  /**
   * Returns the result of the SAX unparse containing diagnostic information. The {@code finish()}
   * method should be called prior to calling this function.
   *
   * If the XMLReader parse method throws a DaffodilUnhandledSAXException, which generally indicates
   * a bug, this will return null.
   *
   * @return result of the SAX unparse containing diagnostic information
   */
  UnparseResult getUnparseResult();

  /**
   * Ensure calls to {@code getUnparseResult()} return a value and clean up internal state. This
   * should be called after XMLReader parsing has ended, even if the XMLReader throws an exception.
   */
  void finish();

  void setDocumentLocator(Locator locator);

  void startDocument() throws DaffodilUnparseErrorSAXException;

  void endDocument() throws DaffodilUnparseErrorSAXException;

  void startPrefixMapping(String prefix, String uri);

  void endPrefixMapping(String prefix);

  void startElement(String uri, String localName, String qName, Attributes attributes) throws DaffodilUnparseErrorSAXException;

  void endElement(String uri, String localName, String qName) throws DaffodilUnparseErrorSAXException;

  void characters(char[] ch, int start, int length);

  void ignorableWhitespace(char[] ch, int start, int length);

  void processingInstruction(String target, String data);

  void skippedEntity(String name);
}
