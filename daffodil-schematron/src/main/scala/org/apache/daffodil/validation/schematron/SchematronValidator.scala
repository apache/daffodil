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

import org.apache.daffodil.api

/**
 * Daffodil Validator implementation for ISO schematron
 */
final class SchematronValidator(
  engine: Schematron,
  svrlPath: Option[Path]
) extends api.validation.Validator {
  def validateXML(
    document: InputStream,
    handler: api.validation.ValidationHandler
  ): Unit = {
    val svrl = XML.loadString(engine.validate(document))
    for (f @ Elem("svrl", "failed-assert", _, _, msg @ _*) <- svrl.child) yield {
      handler.validationError(msg.text.trim, { f \\ "@location" }.text)
    }

    val svrlString = svrl.mkString
    svrlPath.foreach { path =>
      Try {
        val os = new FileOutputStream(path.toFile)
        os.write(svrlString.getBytes)
        os.close()
      }.failed.foreach(handler.validationErrorNoContext)
    }
  }
}

object SchematronValidator {
  val name = "schematron"

  object ConfigKeys {
    val schPath = s"$name.path"
    val svrlOutputFile = s"$name.svrl.file"
  }
}
