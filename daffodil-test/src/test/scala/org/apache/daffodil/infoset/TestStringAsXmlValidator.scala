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
import java.util.Properties

import org.apache.daffodil.api.validation.ValidationHandler
import org.apache.daffodil.api.validation.Validator
import org.apache.daffodil.api.validation.ValidatorFactory
import org.apache.daffodil.validation.XercesValidator

object TestStringAsXmlValidator {
  val name = "TestStringAsXmlValidator"
}

class TestStringAsXmlValidator(schemaURL: String) extends Validator {
  private lazy val xercesValidator = XercesValidator.fromURL(new URL(schemaURL))

  override def validateXML(document: InputStream, vh: ValidationHandler): Unit = {
    xercesValidator.validateXML(document, vh)
  }
}

class TestStringAsXmlValidatorFactory extends ValidatorFactory {

  override def name: String = TestStringAsXmlValidator.name

  override def make(config: Properties) = {
    val dfdlSchema = config.getProperty(name)
    // assumes the validation XSD path is in the same as the DFDL schema but with a different suffix
    val xsdSchema = dfdlSchema.replace(".dfdl.xsd", "WithXmlPayload.xsd")
    new TestStringAsXmlValidator(xsdSchema)
  }
}
