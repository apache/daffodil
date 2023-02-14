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

import javax.xml.XMLConstants
import javax.xml.parsers.SAXParserFactory
import scala.xml.SAXParser

import org.apache.xerces.jaxp.SAXParserFactoryImpl

class DaffodilSAXParserFactory private () extends SAXParserFactoryImpl() {

  override def newSAXParser(): SAXParser = {
    val parser = super.newSAXParser()
    //
    // Note that each time getXMLReader() is called on a SAXParser,
    // the exact same instance is returned. It doesn't allocate new ones.
    //
    val xrdr = parser.getXMLReader()
    XMLUtils.setSecureDefaults(xrdr)
    parser
  }
}

/**
 * This factory ensures secure defaults are used for all our SAX parsers and
 * their underlying XMLReader objects.
 *
 * It also ensures we're using the exact SAXParser implementation we need,
 * not some library substituted/plugged by way of java properties settings.
 */
object DaffodilSAXParserFactory {

  def apply(): SAXParserFactory = {
    val spf = new DaffodilSAXParserFactory()
    spf.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
    spf.setFeature(XMLUtils.XML_EXTERNAL_GENERAL_ENTITIES_FEATURE, false)
    spf.setFeature(XMLUtils.XML_EXTERNAL_PARAMETER_ENTITIES_FEATURE, false)
    spf.setFeature(XMLUtils.XML_LOAD_EXTERNAL_DTD_FEATURE, false)
    spf
  }
}
