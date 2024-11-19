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

package org.apache.daffodil.core.dsom

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Test

class TestMiddleEndAttributes2 {

  @Test def testNestedSequencePrefixSep() = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" lengthKind="delimited" encoding="US-ASCII"/>,
      <xs:element name="e1">
        <xs:complexType>
          <xs:sequence dfdl:separator="/" dfdl:separatorPosition="prefix">
            <xs:sequence>
              <xs:element name="x" type="xs:int"/>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )

    val sset = SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val sd = sch.schemaDocuments.head

    // Explore global element decl
    val Seq(e1) = sd.globalElementDecls
    val e1ct = e1.complexType
    val seq1 = e1ct.sequence
    val mems = seq1.groupMembers
    val Seq(t1: Term) = mems
    t1.asInstanceOf[LocalSequence]
    val (_, actual) = TestUtils.testString(testSchema, "/5")
    val expected = <e1><x>5</x></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

}
