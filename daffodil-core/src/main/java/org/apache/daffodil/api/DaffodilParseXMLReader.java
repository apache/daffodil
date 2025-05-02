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

import org.apache.daffodil.lib.xml.XMLUtils;
import org.xml.sax.ContentHandler;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.XMLReader;
import org.xml.sax.EntityResolver;
import org.xml.sax.DTDHandler;

import java.io.InputStream;

/**
 * SAX Method of parsing schema and getting the DFDL Infoset via designated
 * org.xml.sax.ContentHandler, based on the org.xml.sax.XMLReader interface
 */
public interface DaffodilParseXMLReader extends XMLReader {
  /**
   * The full URIs needed for setting/getting properties for the {@link DaffodilParseXMLReader}
   */
  /**
   * Property name to get the {@link ParseResult} from the {@link DaffodilParseXMLReader}. This property is read only.
   */
  String DAFFODIL_SAX_URN_PARSERESULT = XMLUtils.DAFFODIL_SAX_URN_PARSERESULT();

  /**
   * Property name to get/set blob directory as String from the {@link DaffodilParseXMLReader}
   */
  String DAFFODIL_SAX_URN_BLOBDIRECTORY = XMLUtils.DAFFODIL_SAX_URN_BLOBDIRECTORY();

  /**
   * Property name to get/set blob prefix as String from the {@link DaffodilParseXMLReader}
   */
  String DAFFODIL_SAX_URN_BLOBPREFIX = XMLUtils.DAFFODIL_SAX_URN_BLOBPREFIX();

  /**
   * Property name to get/set blob suffix as String from the {@link DaffodilParseXMLReader}
   */
  String DAFFODIL_SAX_URN_BLOBSUFFIX = XMLUtils.DAFFODIL_SAX_URN_BLOBSUFFIX();

  /**
   * Get the value of the feature flag
   *
   * @param name feature flag whose value is to be retrieved
   * @return value of the feature flag
   */
  boolean getFeature(String name);

  /**
   * Set the value of the feature flag
   *
   * @param name  feature flag to be set
   * @param value value to be assigned to feature flag
   */
  void setFeature(String name, boolean value);

  /**
   * Get the value of the property
   *
   * @param name property whose value is to be retrieved
   * @return value of the property
   */
  Object getProperty(String name);

  /**
   * Set the value of the property
   *
   * @param name  property whose value is to be set
   * @param value value to be assigned to the property
   */
  void setProperty(String name, Object value);

  /**
   * Register an entity resolver
   *
   * @param resolver entity resolver to be registered
   */
  void setEntityResolver(EntityResolver resolver);

  /**
   * Return the registered entity resolver
   *
   * @return registered entity resolver or null
   */
  EntityResolver getEntityResolver();

  /**
   * Register a DTD Handler
   *
   * @param handler DTD Handler to be registered
   */
  void setDTDHandler(DTDHandler handler);

  /**
   * Retrieve registered DTD Handler
   *
   * @return registered DTD Handler or null
   */
  DTDHandler getDTDHandler();

  /**
   * Register a content handler
   *
   * @param handler content handler to be registered
   */
  void setContentHandler(ContentHandler handler);

  /**
   * Retrieve registered content handler
   *
   * @return registered content handler or null
   */
  ContentHandler getContentHandler();

  /**
   * Register an error handler
   *
   * @param handler error handler to be registered
   */
  void setErrorHandler(ErrorHandler handler);

  /**
   * Retrieve registered error handler
   *
   * @return registered error handler or null
   */
  ErrorHandler getErrorHandler();

  /**
   * Parse input data from an InputSource. Infoset can be retrieved via the registered
   * contentHandler and diagnostics via the registered errorHandler
   *
   * @param input data to be parsed
   */
  void parse(InputSource input);

  /**
   * Parse data from a system identifier/URI. This method is not supported by the API.
   *
   * @param systemId URI for data to be parsed
   */
  void parse(String systemId);

  /**
   * Parse input data from an InputSourceDataInputStream. Infoset can retrieved via the registered
   * contentHandler and diagnostics via the registered errorHandler
   *
   * @param isdis data to be parsed
   */
  void parse(InputSourceDataInputStream isdis);

  /**
   * Parse input data from an InputStream. Infoset can retrieved via the registered contentHandler
   * and diagnostics via the registered errorHandler
   *
   * @param stream data to be parsed
   */
  void parse(InputStream stream);

  /**
   * Parse input data from an array of bytes. Infoset can be retrieved via the registered
   * contentHandler and diagnostics via the registered errorHandler
   *
   * @param arr data to be parsed
   */
  void parse(byte[] arr);
}

