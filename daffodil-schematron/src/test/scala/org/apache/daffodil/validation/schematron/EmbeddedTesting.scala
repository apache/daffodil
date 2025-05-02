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

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File

import org.apache.daffodil.api.Daffodil
import org.apache.daffodil.api.DataProcessor
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.api.ParseResult
import org.apache.daffodil.api.infoset.Infoset
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.validation.schematron.SchSource.Xsd

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue

trait EmbeddedTesting {
  case class PR(r: ParseResult) {
    def validated: Boolean = !r.isValidationError()
    def diagnostics: Seq[Diagnostic] = r.getDiagnostics
  }

  sealed trait PrintInfosetMode
  case object Quiet extends PrintInfosetMode
  case object AnyError extends PrintInfosetMode
  case object ValError extends PrintInfosetMode
  case object ProcError extends PrintInfosetMode
  case object Always extends PrintInfosetMode

  case class Validation(dp: DataProcessor) {
    def parse(str: String, verbose: PrintInfosetMode = Quiet): PR =
      withBytes(str.getBytes, verbose)

    def withBytes(bytes: Array[Byte], verbose: PrintInfosetMode = Quiet): PR = {
      val bos = new ByteArrayOutputStream()
      val r1 = dp.parse(
        Infoset.getInputSourceDataInputStream(new ByteArrayInputStream(bytes)),
        Infoset.getXMLTextInfosetOutputter(bos, true)
      )
      verbose match {
        case Always | AnyError if r1.isError() => r1.getDiagnostics.foreach(println)
        case Always => println(bos.toString)
        case ValError if r1.isValidationError() => r1.getDiagnostics.foreach(println)
        case ProcError if r1.isProcessingError() => r1.getDiagnostics.foreach(println)
        case _ =>
      }

      PR(r1)
    }
  }

  def withSchema(xsd: String)(f: Validation => Unit): Unit = {
    val schema = Misc.getRequiredResource(xsd)
    val c = Daffodil.compiler()
    val pf = c.compileFile(new File(schema))

    if (pf.isError()) pf.getDiagnostics.foreach(println)
    assertFalse("Schema did not compile", pf.isError())

    val v = SchematronValidatorFactory.makeValidator(
      schema.toURL.openStream(),
      schema.toURL.toString(),
      Xsd
    )
    val dp = pf.onPath("/").withValidator(v)

    f(Validation(dp))
  }

  def shouldPass(pr: PR): Unit = check(pr)(assertTrue)
  def shouldFail(pr: PR): Unit = check(pr)(assertFalse)

  def check(pr: PR)(f: Boolean => Unit): Unit = {
    if (!pr.validated) pr.diagnostics.foreach(println)
    f(pr.validated)
  }
}
