/*
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.illinois.ncsa.daffodil.dsom

import org.junit.Test
import edu.illinois.ncsa.daffodil.compiler._

import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.util.SchemaUtils
import org.junit.Test

class TestIsScannable extends Logging {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testIsScannableAllText1() {
    val sc = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="utf-8"/>,

      <xs:element name="list">
        <xs:complexType>
          <xs:sequence dfdl:separator="">
            <xs:sequence dfdl:hiddenGroupRef="ex:g"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
      <xs:group name="g">
        <xs:choice>
          <xs:element ref="ex:w"/>
          <xs:element name="x" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:choice>
      </xs:group>)

    val sset = Compiler().compileNode(sc).sset

    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val listDecl = schemaDoc.globalElementDecls.head
    val list = listDecl.forRoot()
    assertTrue(list.isScannable)
    val Seq(child) = list.termChildren
    assertTrue(child.isScannable)
    assertEquals(NamedEncoding("UTF-8"), child.summaryEncoding)
  }

  @Test def testIsScannableHasBinary1() {
    val sc = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,

      <xs:element name="list">
        <xs:complexType>
          <xs:sequence dfdl:separator="">
            <xs:sequence dfdl:hiddenGroupRef="ex:g"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="w" dfdl:representation="binary" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
      <xs:group name="g">
        <xs:choice>
          <xs:element ref="ex:w"/>
          <xs:element name="x" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:choice>
      </xs:group>)

    val sset = Compiler().compileNode(sc).sset

    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val listDecl = schemaDoc.globalElementDecls.head
    val list = listDecl.forRoot()
    assertFalse(list.isScannable)
    val Seq(child) = list.termChildren
    assertFalse(child.isScannable)
    val Seq(s1) = child.termChildren
    assertFalse(s1.isScannable)
    val Seq(gr1) = s1.termChildren
    assertEquals(Mixed, gr1.summaryEncoding)
    val Seq(e1, e2) = gr1.termChildren
    assertEquals(Binary, e1.summaryEncoding)
    assertEquals(NamedEncoding("US-ASCII"), e2.summaryEncoding)
  }

  @Test def testIsScannableDifferentEncodings1() {
    val sc = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="utf-8"/>,
      <xs:element name="list">
        <xs:complexType>
          <xs:sequence dfdl:separator="">
            <xs:sequence dfdl:hiddenGroupRef="ex:g"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="w" dfdl:encoding="ascii" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
      <xs:group name="g">
        <xs:choice>
          <xs:element ref="ex:w"/>
          <xs:element name="x" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:choice>
      </xs:group>)

    val sset = Compiler().compileNode(sc).sset

    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val listDecl = schemaDoc.globalElementDecls.head
    val list = listDecl.forRoot()
    assertFalse(list.isScannable)
    val Seq(child) = list.termChildren
    assertFalse(child.isScannable)
    val Seq(s1) = child.termChildren
    assertFalse(s1.isScannable)
    val Seq(gr1) = s1.termChildren
    assertEquals(Mixed, gr1.summaryEncoding)
    val Seq(e1, e2) = gr1.termChildren
    assertEquals(NamedEncoding("US-ASCII"), e1.summaryEncoding)
    assertEquals(NamedEncoding("UTF-8"), e2.summaryEncoding)
    assertTrue(e1.isScannable)
    assertTrue(e2.isScannable)
    assertFalse(gr1.isScannable)
  }

}
