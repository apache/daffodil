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
    val node = loader.load(URISchemaSource(uri))
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
