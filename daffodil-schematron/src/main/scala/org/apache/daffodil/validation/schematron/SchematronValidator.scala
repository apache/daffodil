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

package org.apache.daffodil.validation.schematron

import java.io.InputStream

import org.apache.daffodil.api.ValidationFailure
import org.apache.daffodil.api.ValidationResult
import org.apache.daffodil.api.Validator

import scala.xml.XML

/**
 * Daffodil Validator implementation for ISO schematron
 */
final class SchematronValidator(engine: Schematron) extends Validator {
  def validateXML(document: InputStream): ValidationResult = {
    val svrl = XML.loadString(engine.validate(document))
    val err = for(f @ <svrl:failed-assert>{ msg @ _* }</svrl:failed-assert> <- svrl.child) yield {
      SchematronValidationError(msg.text.trim, { f \\ "@location" }.text)
    }
    ValidationResult(Seq.empty, err)
  }
}

object SchematronValidator {
  val name = "schematron"
}

/**
 * Represents an error reported by the Schematron validation
 * @param text the failed constraint text
 * @param location the failed constraint location
 */
case class SchematronValidationError(text: String, location: String) extends ValidationFailure {
  def getMessage: String = text
}
