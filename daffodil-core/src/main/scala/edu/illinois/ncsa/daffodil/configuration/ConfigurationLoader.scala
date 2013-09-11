package edu.illinois.ncsa.daffodil.configuration

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

import java.io.File
import java.net.URI

import scala.io.Codec.string2codec
import scala.xml.Node
import scala.xml.parsing.ConstructingParser

import org.jdom2.Element
import org.xml.sax.SAXException

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Misc.determineEncoding
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.SchemaFactory

object ConfigurationLoader {

  def getConfiguration(file: File): Node = {
    Assert.usage(file != null, "getConfiguration expects 'file' to not be null!")
    ConfigurationValidator.validate(file) match {
      case Left(ex) => Assert.abort(ex)
      case Right(_) => // Success
    }
    val enc = determineEncoding(file)
    val input = scala.io.Source.fromURI(file.toURI)(enc)
    val node = ConstructingParser.fromSource(input, true).document.docElem
    node
  }

  def getConfiguration(fileName: String): Node = {
    Assert.usage(fileName != null, "getConfiguration expects 'fileName' to not be null!")
    val f = new File(fileName)
    getConfiguration(f)
  }

  def getConfiguration(uri: URI): Node = {
    Assert.usage(uri != null, "getConfiguration expects 'uri' to not be null!")
    val file = new File(uri)
    getConfiguration(file)
  }

}

object ConfigurationValidator {

  final val configXsd = {
    val stream = this.getClass().getResourceAsStream("/xsd/dfdl-config-format.xsd")
    stream
  }

  def validate(xmlFile: File): Either[java.lang.Throwable, _] = {
    try {
      val schemaLang = "http://www.w3.org/2001/XMLSchema"
      val factory = SchemaFactory.newInstance(schemaLang)
      val schema = factory.newSchema(new StreamSource(configXsd))
      val validator = schema.newValidator()
      validator.validate(new StreamSource(xmlFile))
    } catch {
      case ex: SAXException => Left(ex)
      case ex: Exception => Left(ex)
    }
    Right(true)
  }

}
