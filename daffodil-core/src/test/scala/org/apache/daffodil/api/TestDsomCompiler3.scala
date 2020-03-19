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

package org.apache.daffodil.api

import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.util._
import org.apache.daffodil.Implicits._
import org.apache.daffodil.compiler._
import org.apache.daffodil.schema.annotation.props.gen._
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import java.io.File
import org.junit.Test
import org.apache.daffodil.dsom.DFDLElement

class TestDsomCompiler3 {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testTmpDirProvided() {
    val sc = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,

      <xs:element name="list" type="tns:example1">
        <xs:annotation>
          <xs:appinfo source={ dfdl }>
            <dfdl:element encoding="US-ASCII" alignmentUnits="bytes"/>
          </xs:appinfo>
        </xs:annotation>
      </xs:element>
      <xs:complexType name="example1">
        <xs:sequence dfdl:separator="">
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>)

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
      val Seq(schemaDoc, _) = schema.schemaDocuments
      val Seq(decl) = schemaDoc.globalElementDecls.map{ _.asRoot }
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
