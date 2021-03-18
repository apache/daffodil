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

package org.apache.daffodil.externalvars

import org.apache.daffodil.util.Misc
import org.apache.daffodil.xml.DFDLCatalogResolver
import org.apache.daffodil.xml.XMLUtils
import org.apache.xerces.jaxp.validation.XMLSchemaFactory

import javax.xml.transform.stream.StreamSource
import org.xml.sax.SAXException

import java.io.File

object ExternalVariablesValidator {

  final val extVarXsd = {
    val uri = Misc.getRequiredResource("org/apache/daffodil/xsd/dafext.xsd")
    val stream = uri.toURL.openStream()
    stream
  }

  def validate(xmlFile: File): Either[java.lang.Throwable, _] = {
    try {
      val factory = new XMLSchemaFactory()
      factory.setResourceResolver(DFDLCatalogResolver.get)
      val schema = factory.newSchema(new StreamSource(extVarXsd))
      val validator = schema.newValidator()
      validator.setFeature(XMLUtils.XML_DISALLOW_DOCTYPE_FEATURE, true)
      validator.validate(new StreamSource(xmlFile))
    } catch {
      case ex: SAXException => Left(ex)
    }
    Right(true)
  }

}
