/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.configuration

import java.io.File
import java.net.URI
import scala.xml.Node
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.xml.DaffodilXMLLoader
import org.apache.daffodil.api.URISchemaSource

/**
 * TODO: This is all overkill. ConfigurationLoader doesn't need its own validator,
 * nor does it need to use the ConstructingParser which is used for TDML only because
 * it deals with CDATA nodes differently than the regular loader (DaffodilXMLLoader,
 * which is based on scala.xml.XML's loader, which is based on the sax parser
 * built into the JVM (which is xerces?), and which validates.
 *
 * For the configuration loader, we only need plain old loading.
 *
 * I suspect this was just cloned from the TDML loader code, and so copied its
 * more complicated arrangements.
 */
object ConfigurationLoader {

  def getConfiguration(loader: DaffodilXMLLoader, uri: URI): Node = {
    Assert.usage(uri != null, "getConfiguration expects 'uri' to not be null!")
    val node = loader.load(new URISchemaSource(uri))
    scala.xml.Utility.trim(node)
  }

  def getConfiguration(loader: DaffodilXMLLoader, fileName: String): Node = {
    Assert.usage(fileName != null, "getConfiguration expects 'fileName' to not be null!")
    val f = new File(fileName)
    getConfiguration(loader, f.toURI)
  }

}
//
//  def validate(uri: URI): Either[java.lang.Throwable, _] = {
//    try {
//      val schemaLang = "http://www.w3.org/2001/XMLSchema"
//      val factory = SchemaFactory.newInstance(schemaLang)
//      val schema = factory.newSchema(new StreamSource(configXsd))
//      val validator = schema.newValidator()
//      validator.validate(new StreamSource(uri.toURL.openStream()))
//    } catch {
//      case ex: SAXException => Left(ex)
//      case ex: Exception => Left(ex)
//    }
//    Right(true)
//  }
//
//}
