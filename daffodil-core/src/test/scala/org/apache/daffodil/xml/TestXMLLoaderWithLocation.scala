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

package org.apache.daffodil.xml

import org.junit.Assert._
import org.junit.Test
import org.apache.daffodil.Implicits._
import java.io.File
import org.apache.daffodil.api.URISchemaSource

class TestXMLLoaderWithLocation {

  @Test def testFile1(): Unit = {
    val tmpXMLFileName = getClass.getName() + ".xml"
    // Our loader looks for xs:schema node, and appends a file attribute
    // if it can.
    val testXML = <xs:schema xmlns:xs={ XMLUtils.XSD_NAMESPACE }><xs:annotation/></xs:schema>
    try {
      using(new java.io.FileWriter(tmpXMLFileName)) {
        fw =>
          fw.write(testXML.toString())
      }
      val res = URISchemaSource(new File(tmpXMLFileName).toURI)
      val eh = new BasicErrorHandler
      val node = (new DaffodilXMLLoader(eh)).load(res)
      assertTrue(node.toString.toLowerCase.contains("dafint:file"))
      assertFalse(eh.hasError)
      assertEquals(0, eh.diagnostics.length)
    } finally {
      val t = new java.io.File(tmpXMLFileName)
      t.delete()
    }
  }

  @Test def testCatalogResolver(): Unit = {
    val baseURI: String = new File(".").toURI().toString
    // val ldr = new DaffodilXMLLoader(BasicErrorHandler)
    val pId: String = null
    val sId: String = null
    val resolver = DFDLCatalogResolver.get
    // val resolved =
    resolver.resolveResource(XMLUtils.XSD_NAMESPACE, XMLUtils.XSD_NAMESPACE, pId, sId, baseURI)
    // println(resolved)
  }

  @Test def testFileValidation(): Unit = {
    val tmpXMLFileName = getClass.getName() + ".xml"
    // Our loader looks for xs:schema node, and appends a file attribute
    // if it can.
    val testXML = <xs:schema xmlns:xs={ XMLUtils.XSD_NAMESPACE }><xs:illegal/></xs:schema>
    try {
      using(new java.io.FileWriter(tmpXMLFileName)) {
        fw =>
          fw.write(testXML.toString())
      }
      val res = URISchemaSource(new File(tmpXMLFileName).toURI)
      val eh = new BasicErrorHandler
      val node = (new DaffodilXMLLoader(eh)).load(res)
      assertTrue(eh.hasError)
      val msgs = eh.diagnostics.map { _.getMessage() }.mkString("\n")
      assertTrue(msgs.contains(":illegal"))
      assertTrue(node.toString.toLowerCase.contains("dafint:file"))
    } finally {
      val t = new java.io.File(tmpXMLFileName)
      t.delete()
    }
  }
}
