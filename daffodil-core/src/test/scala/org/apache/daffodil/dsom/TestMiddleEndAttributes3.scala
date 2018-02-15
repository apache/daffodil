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

package org.apache.daffodil.dsom

import org.apache.daffodil.util.SchemaUtils
import org.junit.Test
import org.junit.Assert._

class TestMiddleEndAttributes3 {

  @Test def testNextParentElements() = {
    val testSchema = SchemaUtils.dfdlTestSchema(

      <dfdl:format ref="tns:GeneralFormat" lengthKind="delimited" encoding="US-ASCII"/>,

      <xs:group name="IPSrcGrp">
        <xs:sequence>
          <xs:element name="byte1" type="xs:int" dfdl:outputValueCalc="{ ../IPSrc }"/>
        </xs:sequence>
      </xs:group>
      <xs:element name="e">
        <xs:complexType>
          <xs:sequence>
            <xs:element name='IPv4Header'>
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Checksum" type="xs:int"/>
                  <xs:element name="IPSrc" type="xs:int"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="TransportLayer" type="xs:byte" dfdl:lengthKind="explicit" dfdl:lengthUnits="bytes" dfdl:length="{ ../PayloadLength }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    // Explore global element decl
    val Seq(ef) = sd.globalElementDecls
    val e = ef.forRoot()
    val ect = e.complexType
    val seq = ect.sequence
    val mems = seq.groupMembers
    val Seq(ile: ElementBase, _: ElementBase) = mems
    val ile_nextSibling = ile.nextSibling
    assertNotNull(ile_nextSibling)
    val ilct = ile.complexType
    val ilseq = ilct.sequence
    val Seq(_: ElementBase, ipsrcle: ElementBase) = ilseq.groupMembers
    val nextSiblings = ipsrcle.nextSibling
    assertNotNull(nextSiblings)
    val nextParents = ipsrcle.nextParentElements
    assertNotNull(nextParents)
    assertEquals(0, nextParents.length)
    // assertEquals("TransportLayer", tlle.name)

    //    val actual = TestUtils.testString(testSchema, "/5").result
    //    actual.toString
    //    val expected = <e1><x>5</x></e1>
    //    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testNextParentElements2() = {
    val testSchema = SchemaUtils.dfdlTestSchema(

      <dfdl:format ref="tns:GeneralFormat" lengthKind="delimited" encoding="US-ASCII"/>,

      <xs:group name="IPSrcGrp">
        <xs:sequence>
          <xs:element name="byte1" type="xs:int" dfdl:outputValueCalc="{ ../IPSrc }"/>
        </xs:sequence>
      </xs:group>
      <xs:element name="e">
        <xs:complexType>
          <xs:sequence>
            <xs:element name='IPv4Header'>
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Checksum" type="xs:int"/>
                  <xs:sequence dfdl:hiddenGroupRef="ex:IPSrcGrp"/>
                  <xs:element name="IPSrc" type="xs:int" dfdl:inputValueCalc="{ ../byte1 + ../byte1 }"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="PayloadLength" type="xs:int" dfdl:inputValueCalc="{ ../IPv4Header/Checksum }"/>
            <xs:choice>
              <xs:element name="TransportLayer" type="xs:byte" dfdl:lengthKind="explicit" dfdl:lengthUnits="bytes" dfdl:length="{ ../PayloadLength }"/>
            </xs:choice>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    // Explore global element decl
    val Seq(ef) = sd.globalElementDecls
    val e = ef.forRoot()
    val ect = e.complexType
    val seq = ect.sequence
    val mems = seq.groupMembers
    val Seq(ile: ElementBase, _: ElementBase, _: Choice) = mems
    val ile_nextSibling = ile.nextSibling
    assertNotNull(ile_nextSibling)
    val ilct = ile.complexType
    val ilseq = ilct.sequence
    val Seq(_: ElementBase, _: SequenceGroupRef, ipsrcle: ElementBase) = ilseq.groupMembers
    val nextSiblings = ipsrcle.nextSibling
    assertNotNull(nextSiblings)
    val nextParents = ipsrcle.nextParentElements
    // println("nextParents: " + nextParents)
    assertEquals(0, nextParents.length)
    //    val actual = TestUtils.testString(testSchema, "/5").result
    //    actual.toString
    //    val expected = <e1><x>5</x></e1>
    //    TestUtils.assertEqualsXMLElements(expected, actual)
  }

}
