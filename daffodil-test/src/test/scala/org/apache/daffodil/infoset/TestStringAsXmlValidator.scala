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
package org.apache.daffodil.infoset

import java.io.InputStream
import java.net.URL

import org.apache.daffodil.api.validation.ValidationHandler
import org.apache.daffodil.api.validation.Validator
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.validation.XercesValidator

object TestStringAsXmlNamespacedValidator {
  val name = "TestStringAsXmlNamespacedValidator"
}

class TestStringAsXmlNamespacedValidator extends Validator {

  val schemaURL: URL = Misc
    .getRequiredResource(
      "/org/apache/daffodil/infoset/stringAsXml/namespaced/xsd/binMessageWithXmlPayload.xsd"
    )
    .toURL

  override def validateXML(document: InputStream, vh: ValidationHandler): Unit = {
    val v = XercesValidator.fromURL(schemaURL)
    v.validateXML(document, vh)
  }
}

object TestStringAsXmlNoNamespaceValidator {
  val name = "TestStringAsXmlNoNamespaceValidator"
}

class TestStringAsXmlNoNamespaceValidator extends Validator {

  val schemaURL: URL = Misc
    .getRequiredResource(
      "/org/apache/daffodil/infoset/stringAsXml/nonamespace/xsd/binMessageWithXmlPayload.xsd"
    )
    .toURL

  override def validateXML(document: InputStream, vh: ValidationHandler): Unit = {
    val v = XercesValidator.fromURL(schemaURL)
    v.validateXML(document, vh)
  }
}
