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
import org.apache.daffodil.lib.util._
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Test

class TestInteriorAlignmentElimination {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  /**
   * This test illustrates a complex situation involving where
   * interior alignment can be optimized out inappropriately by
   * the schema compiler.
   */
  @Test def testInteriorAlign1(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" representation="binary" lengthKind="implicit" lengthUnits="bytes" alignmentUnits="bytes"/>,
      <xs:element name="r" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="int3" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="3"/>
            <xs:element ref="ex:e1"/>
            <xs:element name="int1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element ref="ex:e2"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element ref="ex:eShared"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="e2" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            >
            <xs:element ref="ex:eShared"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="eShared" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="int1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <!--
               Schema compilation will compile eShared once.

               Depending on the amount of knowledge the schema compiler keeps about alignment, it
               may know that eShared starts at bit 24 the first time, and it will then know that after the first
               int1, the alignment is now 4-byte, meeting the requirements for the 'aligned' field.

               Hence, it will optimize out the alignmentFill parser that would skip some bytes to achieve
               the 4 byte alignment.

               The second occurrence of eShared is on a 9 byte boundary. After int1 it is on a 10-byte boundary
               and needs 2-bytes of alignmentFill region in order to parse the aligned element.

               But compilation happens only once, and so if the alignment region is optimized out; so we
               won't get the 2-bytes of alignment, and the parser will fail to parse properly.

               Correct compilation will not assume it knows the 32 bits are satisfied the first time
               around, and will assume it only knows of 8-bit alignment, so it will keep AlignmentFill,
               and this test will pass.
              -->
            <xs:element name="aligned" type="xs:int" dfdl:alignment="4" dfdl:lengthKind="explicit" dfdl:length="4"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    //
    // in the below hex string, it is the 6666 that is the alignment fill.
    //
    val (_, actual) =
      TestUtils.testBinary(testSchema, "00000102000000030405666600000007", areTracing = false)
    val expected =
      <ex:r>
        <int3>1</int3>
        <ex:e1><ex:eShared><int1>2</int1><aligned>3</aligned></ex:eShared></ex:e1>
        <int1>4</int1>
        <ex:e2><ex:eShared><int1>5</int1><aligned>7</aligned></ex:eShared></ex:e2>
      </ex:r>
    XMLUtils.compareAndReport(expected, actual)
  }

}
