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

package org.apache.daffodil.core.iapi

import java.io.File

import org.apache.daffodil.core.compiler._
import org.apache.daffodil.core.dsom.DFDLElement
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.schema.annotation.props.gen._
import org.apache.daffodil.lib.util._
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class TestDsomCompiler3 {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testTmpDirProvided(): Unit = {
    val sc = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={dfdl}>
            <dfdl:element encoding="US-ASCII" alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separator="">
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>
    )

    val tmpDir = new File("./dfdl_tmp")
    if (tmpDir.exists) {
      tmpDir.listFiles.foreach(_.delete)
      tmpDir.delete
    }
    try {
      tmpDir.mkdirs
      val sset = Compiler().compileNode(sc, Some(tmpDir)).sset

      val list = tmpDir.list()
      assertEquals(1, list.length)

      val fileName = list(0)
      assertTrue(fileName.contains(".dfdl.xsd"))

      // Verify things still work using specified tmpDir
      //
      val Seq(schema) = sset.schemas
      val schemaDoc = schema.schemaDocuments.head
      val Seq(decl) = schemaDoc.globalElementDecls.map { _.asRoot }
      val Seq(ct) = schemaDoc.globalComplexTypeDefs
      assertEquals("example1", ct.name)

      decl.formatAnnotation.asInstanceOf[DFDLElement]
      assertEquals(AlignmentUnits.Bytes, decl.alignmentUnits)
    } finally {
      if (tmpDir.exists) {
        tmpDir.listFiles.foreach(_.delete)
        tmpDir.delete
      }
    }
  }

}
