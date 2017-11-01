/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.dsom

import org.apache.daffodil.util.SchemaUtils
import org.junit.Test
import org.junit.Assert._

class TestMiddleEndAttributes3 {

  @Test def testNextParentElements() = {
    val testSchema = SchemaUtils.dfdlTestSchema(

      <dfdl:format ref="tns:daffodilTest1" lengthKind="delimited" encoding="US-ASCII"/>,

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

      <dfdl:format ref="tns:daffodilTest1" lengthKind="delimited" encoding="US-ASCII"/>,

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
    val Seq(_: ElementBase, _: Sequence, ipsrcle: ElementBase) = ilseq.groupMembers
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
