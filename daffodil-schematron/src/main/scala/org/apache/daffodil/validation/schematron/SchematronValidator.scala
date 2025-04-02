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

import java.io.FileOutputStream
import java.io.InputStream
import java.nio.file.Path
import scala.util.Try
import scala.xml.Elem
import scala.xml.XML

import org.apache.daffodil.lib.iapi.ValidationException
import org.apache.daffodil.lib.iapi.ValidationFailure
import org.apache.daffodil.lib.iapi.ValidationResult
import org.apache.daffodil.lib.iapi.Validator

/**
 * Daffodil Validator implementation for ISO schematron
 */
final class SchematronValidator(engine: Schematron, svrlPath: Option[Path]) extends Validator {
  def validateXML(document: InputStream): ValidationResult = {
    val svrl = XML.loadString(engine.validate(document))
    val valErr: Seq[ValidationFailure] =
      for (f @ Elem("svrl", "failed-assert", _, _, msg @ _*) <- svrl.child) yield {
        SchematronValidationError(msg.text.trim, { f \\ "@location" }.text)
      }

    val svrlString = svrl.mkString
    val svrlOutputFailure = svrlPath.flatMap { path =>
      Try {
        val os = new FileOutputStream(path.toFile)
        os.write(svrlString.getBytes)
        os.close()
      }.failed.map(SvrlOutputException(_)).toOption
    }

    val err = svrlOutputFailure.fold(valErr)(f => valErr :+ f)
    SchematronResult(Seq.empty, err, svrlString)
  }
}

object SchematronValidator {
  val name = "schematron"

  object ConfigKeys {
    val schPath = s"$name.path"
    val svrlOutputFile = s"$name.svrl.file"
  }
}

/**
 * Represents an error reported by the Schematron validation
 * @param text the failed constraint text
 * @param location the failed constraint location
 */
case class SchematronValidationError(text: String, location: String) extends ValidationFailure {
  def getMessage: String = text
}

/**
 * Thrown when raw SVRL output is requested but cannot be written
 * @param e the cause
 */
final case class SvrlOutputException(e: Throwable) extends Exception(e) with ValidationException
