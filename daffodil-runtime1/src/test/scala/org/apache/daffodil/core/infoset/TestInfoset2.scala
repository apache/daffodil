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

package org.apache.daffodil.core.infoset

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.util._
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Assert._
import org.junit.Test

object INoWarn8 { ImplicitsSuppressUnusedImportWarning() }

class TestInfoset2 {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val ex = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testXMLToInfoset1(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchemaUnqualified(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="b">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="c" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
            <xs:element minOccurs="0" maxOccurs="unbounded" name="a" type="xs:string" dfdl:length="1" dfdl:lengthKind="explicit" dfdl:occursCountKind="expression" dfdl:occursCount="{ ../c }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )

    try {
      // Debugger.setDebugging(true)
      val (_, xml) = TestUtils.testString(testSchema, "2AB")
      val xmlStr = xml.toString
      // element b is defined as being in the example.com namespace, but with
      // no prefix defined. That means for be xmlns="example.com". But since
      // the schema is unqualified, that means elements c and x do not have a
      // namespace, so for those elements xmlns="". There was a bug where
      // NoNamespace was displayed as xmlns="No_Namespace", so this checks to
      // make sure that is resolved.
      assertFalse(xmlStr.contains("No_Namespace"))
      assertTrue(xmlStr.contains("xmlns=\"\""))
      XMLUtils.compareAndReport(<b><c>2</c><a>A</a><a>B</a></b>, xml)
    } finally {
      // Debugger.setDebugging(false)
    }

  }
}
