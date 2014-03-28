package edu.illinois.ncsa.daffodil.dsom

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util._
import scala.xml._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import junit.framework.Assert.assertEquals
import org.junit.Test
import org.junit.Test

class TestDsomCompilerNew extends Logging {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  val dummyGroupRef = null // just because otherwise we have to construct too many things.

  def FindValue(collection: Map[String, String], key: String, value: String): Boolean = {
    val found: Boolean = Option(collection.find(x => x._1 == key && x._2 == value)) match {
      case Some(_) => true
      case None => false
    }
    found
  }

  @Test def testHasPatternFacets() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" type="tns:st1" dfdl:lengthKind="explicit" dfdl:length="1"/>
      <xs:simpleType name="st1">
        <xs:restriction base="xs:string">
          <xs:pattern value="1"/>
          <xs:pattern value="2"/>
          <xs:pattern value="3"/>
        </xs:restriction>
      </xs:simpleType>)

    val compiler = Compiler()
    val (sset, _) = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.forRoot()

    assertEquals(1, decl.patternValues.length)
    val (facetName, pattern) = decl.patternValues(0)
    assertEquals("1|2|3", pattern.toString())
  }

  @Test def testPatternFacetsInheritance() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" type="tns:st1" dfdl:lengthKind="explicit" dfdl:length="1"/>
      <xs:simpleType name="st1">
        <xs:restriction base="tns:st2">
          <xs:pattern value="1"/>
          <xs:pattern value="2"/>
          <xs:pattern value="3"/>
        </xs:restriction>
      </xs:simpleType>
      <xs:simpleType name="st2">
        <xs:restriction base="tns:st3">
          <xs:pattern value="4"/>
          <xs:pattern value="5"/>
          <xs:pattern value="6"/>
        </xs:restriction>
      </xs:simpleType>
      <xs:simpleType name="st3">
        <xs:restriction base="xs:string">
          <xs:pattern value="7"/>
          <xs:pattern value="8"/>
          <xs:pattern value="9"/>
        </xs:restriction>
      </xs:simpleType>)

    val compiler = Compiler()
    val (sset, _) = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val decl = declf.forRoot()

    assertEquals(3, decl.patternValues.length)
    val (_, st1) = decl.patternValues(0)
    val (_, st2) = decl.patternValues(1)
    val (_, st3) = decl.patternValues(2)

    assertEquals("1|2|3", st1.toString())
    assertEquals("4|5|6", st2.toString())
    assertEquals("7|8|9", st3.toString())
  }

  /**
   * Here we just want to test that we can detect next
   * elements across sequences.  All elements are 'required'.
   */
  @Test def test_could_be_next_method_01() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="one" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="a" type="xs:string"/>
                  <xs:element name="b" type="xs:string"/>
                  <xs:element name="c" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="two" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="d" type="xs:string"/>
                  <xs:element name="e" type="xs:string"/>
                  <xs:element name="f" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="three" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="g" type="xs:string"/>
                  <xs:element name="h" type="xs:string"/>
                  <xs:element name="i" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:sequence>
              <xs:element name="four" dfdl:terminator=":">
                <xs:complexType>
                  <xs:sequence dfdl:separator=",">
                    <xs:element name="j" type="xs:string"/>
                    <xs:element name="k" type="xs:string"/>
                    <xs:element name="l" type="xs:string"/>
                  </xs:sequence>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val compiler = Compiler()
    val (sset, _) = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val root = declf.forRoot()
    val rootCT = root.immediateType.get.asInstanceOf[LocalComplexTypeDef]

    // Verify that nothing follows the root, as it is the root.
    val elemsFollowingRoot = root.couldBeNext
    assertEquals(0, elemsFollowingRoot.length)

    val rootCTSeq = rootCT.modelGroup.asInstanceOf[Sequence] //... which is a sequence

    // Verify that nothing follows the sequence of the root.
    val elemsFollowingRootSeq = rootCTSeq.couldBeNext
    assertEquals(0, elemsFollowingRootSeq.length)

    val Seq(one: LocalElementBase, two: LocalElementBase, three: LocalElementBase, seq: Sequence) =
      rootCTSeq.groupMembers // has an element and a sub-sequence as its children.

    val elemsFollowingOne = one.couldBeNext
    assertEquals(1, elemsFollowingOne.length)
    assertEquals("two", elemsFollowingOne(0).asInstanceOf[LocalElementBase].name)

    val elemsFollowingTwo = two.couldBeNext
    assertEquals(1, elemsFollowingTwo.length)
    assertEquals("three", elemsFollowingTwo(0).asInstanceOf[LocalElementBase].name)

    val elemsFollowingThree = three.couldBeNext
    assertEquals(1, elemsFollowingThree.length)
    val seqFollowingThree = elemsFollowingThree(0).asInstanceOf[Sequence]

    val Seq(four: LocalElementBase) = seqFollowingThree.allSelfContainedTermsTerminatedByRequiredElement
    assertEquals("four", four.name)

  }

  /**
   * Here we want to detect that element 'three' is optional.
   * As such, the list of possible elements after 'two' should
   * contain 'three' and 'four'.
   */
  @Test def test_could_be_next_method_02() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="one" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="a" type="xs:string"/>
                  <xs:element name="b" type="xs:string"/>
                  <xs:element name="c" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="two" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="d" type="xs:string"/>
                  <xs:element name="e" type="xs:string"/>
                  <xs:element name="f" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="three" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="g" type="xs:string"/>
                  <xs:element name="h" type="xs:string"/>
                  <xs:element name="i" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:sequence>
              <xs:element name="four" dfdl:terminator=":">
                <xs:complexType>
                  <xs:sequence dfdl:separator=",">
                    <xs:element name="j" type="xs:string"/>
                    <xs:element name="k" type="xs:string"/>
                    <xs:element name="l" type="xs:string"/>
                  </xs:sequence>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val compiler = Compiler()
    val (sset, _) = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val root = declf.forRoot()
    val rootCT = root.immediateType.get.asInstanceOf[LocalComplexTypeDef]

    // Verify that nothing follows the root, as it is the root.
    val elemsFollowingRoot = root.couldBeNext
    assertEquals(0, elemsFollowingRoot.length)

    val rootCTSeq = rootCT.modelGroup.asInstanceOf[Sequence] //... which is a sequence

    // Verify that nothing follows the sequence of the root.
    val elemsFollowingRootSeq = rootCTSeq.couldBeNext
    assertEquals(0, elemsFollowingRootSeq.length)

    val Seq(one: LocalElementBase, two: LocalElementBase, three: LocalElementBase, seq: Sequence) =
      rootCTSeq.groupMembers // has an element and a sub-sequence as its children.

    val elemsFollowingOne = one.couldBeNext
    assertEquals(1, elemsFollowingOne.length)
    assertEquals("two", elemsFollowingOne(0).asInstanceOf[LocalElementBase].name)

    val elemsFollowingTwo = two.couldBeNext
    assertEquals(2, elemsFollowingTwo.length)
    assertEquals("three", elemsFollowingTwo(0).asInstanceOf[LocalElementBase].name)
    val seqFollowingThree = elemsFollowingTwo(1).asInstanceOf[Sequence]
    val Seq(four: LocalElementBase) = seqFollowingThree.groupMembers
    assertEquals("four", four.name)

    val elemsFollowingThree = three.couldBeNext
    assertEquals(1, elemsFollowingThree.length)
    val seqFollowingThree2 = elemsFollowingThree(0).asInstanceOf[Sequence]
    val Seq(four2: LocalElementBase) = seqFollowingThree2.groupMembers
    assertEquals("four", four2.name)
  }

  /**
   * Here because 'two' is optional, we expect to see
   * 'two' and 'three' in 'one's couldBeNext list.
   */
  @Test def test_could_be_next_method_03() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="one" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="a" type="xs:string"/>
                  <xs:element name="b" type="xs:string"/>
                  <xs:element name="c" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="two" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="d" type="xs:string"/>
                  <xs:element name="e" type="xs:string"/>
                  <xs:element name="f" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="three" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="g" type="xs:string"/>
                  <xs:element name="h" type="xs:string"/>
                  <xs:element name="i" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:sequence>
              <xs:element name="four" dfdl:terminator=":">
                <xs:complexType>
                  <xs:sequence dfdl:separator=",">
                    <xs:element name="j" type="xs:string"/>
                    <xs:element name="k" type="xs:string"/>
                    <xs:element name="l" type="xs:string"/>
                  </xs:sequence>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val compiler = Compiler()
    val (sset, _) = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val root = declf.forRoot()
    val rootCT = root.immediateType.get.asInstanceOf[LocalComplexTypeDef]

    // Verify that nothing follows the root, as it is the root.
    val elemsFollowingRoot = root.couldBeNext
    assertEquals(0, elemsFollowingRoot.length)

    val rootCTSeq = rootCT.modelGroup.asInstanceOf[Sequence] //... which is a sequence

    // Verify that nothing follows the sequence of the root.
    val elemsFollowingRootSeq = rootCTSeq.couldBeNext
    assertEquals(0, elemsFollowingRootSeq.length)

    val Seq(one: LocalElementBase, two: LocalElementBase, three: LocalElementBase, seq: Sequence) =
      rootCTSeq.groupMembers // has an element and a sub-sequence as its children.

    val elemsFollowingOne = one.couldBeNext
    assertEquals(2, elemsFollowingOne.length)
    assertEquals("two", elemsFollowingOne(0).asInstanceOf[LocalElementBase].name)
    assertEquals("three", elemsFollowingOne(1).asInstanceOf[LocalElementBase].name)

    val elemsFollowingTwo = two.couldBeNext
    assertEquals(1, elemsFollowingTwo.length)
    assertEquals("three", elemsFollowingTwo(0).asInstanceOf[LocalElementBase].name)

    val elemsFollowingThree = three.couldBeNext
    assertEquals(1, elemsFollowingThree.length)
    val seqFollowingThree = elemsFollowingThree(0).asInstanceOf[Sequence]
    val Seq(four: LocalElementBase) = seqFollowingThree.groupMembers
    assertEquals("four", four.name)

  }

  /**
   * Here because 'one' is optional, we expect to see
   * 'two' in 'one's couldBeNext list.
   */
  @Test def test_could_be_next_method_04() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="one" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="a" type="xs:string"/>
                  <xs:element name="b" type="xs:string"/>
                  <xs:element name="c" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="two" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="d" type="xs:string"/>
                  <xs:element name="e" type="xs:string"/>
                  <xs:element name="f" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="three" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="g" type="xs:string"/>
                  <xs:element name="h" type="xs:string"/>
                  <xs:element name="i" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:sequence>
              <xs:element name="four" dfdl:terminator=":">
                <xs:complexType>
                  <xs:sequence dfdl:separator=",">
                    <xs:element name="j" type="xs:string"/>
                    <xs:element name="k" type="xs:string"/>
                    <xs:element name="l" type="xs:string"/>
                  </xs:sequence>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val compiler = Compiler()
    val (sset, _) = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val root = declf.forRoot()
    val rootCT = root.immediateType.get.asInstanceOf[LocalComplexTypeDef]

    // Verify that nothing follows the root, as it is the root.
    val elemsFollowingRoot = root.couldBeNext
    assertEquals(0, elemsFollowingRoot.length)

    val rootCTSeq = rootCT.modelGroup.asInstanceOf[Sequence] //... which is a sequence

    // Verify that nothing follows the sequence of the root.
    val elemsFollowingRootSeq = rootCTSeq.couldBeNext
    assertEquals(0, elemsFollowingRootSeq.length)

    val Seq(one: LocalElementBase, two: LocalElementBase, three: LocalElementBase, seq: Sequence) =
      rootCTSeq.groupMembers // has an element and a sub-sequence as its children.

    val elemsFollowingOne = one.couldBeNext
    assertEquals(1, elemsFollowingOne.length)
    assertEquals("two", elemsFollowingOne(0).asInstanceOf[LocalElementBase].name)

    val elemsFollowingTwo = two.couldBeNext
    assertEquals(1, elemsFollowingTwo.length)
    assertEquals("three", elemsFollowingTwo(0).asInstanceOf[LocalElementBase].name)

    val elemsFollowingThree = three.couldBeNext
    assertEquals(1, elemsFollowingThree.length)
    val seqFollowingThree = elemsFollowingThree(0).asInstanceOf[Sequence]
    val Seq(four: LocalElementBase) = seqFollowingThree.groupMembers
    assertEquals("four", four.name)

  }

  /**
   * Here because 'two', 'three', and 'four' are optional...
   *
   * name	couldBeNext
   * =======================
   * one	two, three, four
   * two	three, four
   * three	four
   * four	-empty-
   */
  @Test def test_could_be_next_method_05() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="one" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="a" type="xs:string"/>
                  <xs:element name="b" type="xs:string"/>
                  <xs:element name="c" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="two" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="d" type="xs:string"/>
                  <xs:element name="e" type="xs:string"/>
                  <xs:element name="f" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="three" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="g" type="xs:string"/>
                  <xs:element name="h" type="xs:string"/>
                  <xs:element name="i" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:sequence>
              <xs:element name="four" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
                <xs:complexType>
                  <xs:sequence dfdl:separator=",">
                    <xs:element name="j" type="xs:string"/>
                    <xs:element name="k" type="xs:string"/>
                    <xs:element name="l" type="xs:string"/>
                  </xs:sequence>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val compiler = Compiler()
    val (sset, _) = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val root = declf.forRoot()
    val rootCT = root.immediateType.get.asInstanceOf[LocalComplexTypeDef]

    // Verify that nothing follows the root, as it is the root.
    val elemsFollowingRoot = root.couldBeNext
    assertEquals(0, elemsFollowingRoot.length)

    val rootCTSeq = rootCT.modelGroup.asInstanceOf[Sequence] //... which is a sequence

    // Verify that nothing follows the sequence of the root.
    val elemsFollowingRootSeq = rootCTSeq.couldBeNext
    assertEquals(0, elemsFollowingRootSeq.length)

    val Seq(one: LocalElementBase, two: LocalElementBase, three: LocalElementBase, seq: Sequence) =
      rootCTSeq.groupMembers // has an element and a sub-sequence as its children.

    val Seq(four: LocalElementBase) = seq.groupMembers

    val elemsFollowingOne = one.couldBeNext
    val Seq(eTwo: LocalElementBase, eThree: LocalElementBase, seqFollowingThree: Sequence) = one.couldBeNext
    assertEquals(3, elemsFollowingOne.length)
    assertEquals("two", eTwo.name)
    assertEquals("three", eThree.name)
    val Seq(eFour: LocalElementBase) = seqFollowingThree.groupMembers
    assertEquals("four", eFour.name)

    val elemsFollowingTwo = two.couldBeNext
    val Seq(eThree_2: LocalElementBase, seqFollowingThree_2: Sequence) = two.couldBeNext
    assertEquals(2, elemsFollowingTwo.length)
    assertEquals("three", eThree_2.name)
    val Seq(eFour_2: LocalElementBase) = seqFollowingThree_2.groupMembers
    assertEquals("four", eFour_2.name)

    val elemsFollowingThree = three.couldBeNext
    val Seq(seqFollowingThree_3: Sequence) = three.couldBeNext
    assertEquals(1, elemsFollowingThree.length)
    val Seq(eFour_3: LocalElementBase) = seqFollowingThree_3.groupMembers
    assertEquals("four", eFour_3.name)

    val elemsFollowingFour = four.couldBeNext
    assertEquals(0, elemsFollowingFour.length)

  }

  /**
   * Here because 'two', and 'three' are optional...
   *
   * name	couldBeNext
   * =======================
   * one	two, three, four
   * two	three, four
   * three	four
   * four	-empty-
   */
  @Test def test_could_be_next_method_06() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="one" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="a" type="xs:string"/>
                  <xs:element name="b" type="xs:string"/>
                  <xs:element name="c" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="two" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="d" type="xs:string"/>
                  <xs:element name="e" type="xs:string"/>
                  <xs:element name="f" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="three" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="g" type="xs:string"/>
                  <xs:element name="h" type="xs:string"/>
                  <xs:element name="i" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:sequence>
              <xs:element name="four" dfdl:terminator=":">
                <xs:complexType>
                  <xs:sequence dfdl:separator=",">
                    <xs:element name="j" type="xs:string"/>
                    <xs:element name="k" type="xs:string"/>
                    <xs:element name="l" type="xs:string"/>
                  </xs:sequence>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val compiler = Compiler()
    val (sset, _) = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val root = declf.forRoot()
    val rootCT = root.immediateType.get.asInstanceOf[LocalComplexTypeDef]

    // Verify that nothing follows the root, as it is the root.
    val elemsFollowingRoot = root.couldBeNext
    assertEquals(0, elemsFollowingRoot.length)

    val rootCTSeq = rootCT.modelGroup.asInstanceOf[Sequence] //... which is a sequence

    // Verify that nothing follows the sequence of the root.
    val elemsFollowingRootSeq = rootCTSeq.couldBeNext
    assertEquals(0, elemsFollowingRootSeq.length)

    val Seq(one: LocalElementBase, two: LocalElementBase, three: LocalElementBase, seq: Sequence) =
      rootCTSeq.groupMembers // has an element and a sub-sequence as its children.

    val Seq(four: LocalElementBase) = seq.groupMembers

    val elemsFollowingOne = one.couldBeNext
    val Seq(two_1: LocalElementBase, three_1: LocalElementBase, seqFollowingThree_1: Sequence) = one.couldBeNext
    val Seq(four_1: LocalElementBase) = seqFollowingThree_1.groupMembers
    assertEquals(3, elemsFollowingOne.length)
    assertEquals("two", two_1.name)
    assertEquals("three", three_1.name)
    assertEquals("four", four_1.name)

    val elemsFollowingTwo = two.couldBeNext
    val Seq(three_2: LocalElementBase, seqFollowingThree_2: Sequence) = two.couldBeNext
    val Seq(four_2: LocalElementBase) = seqFollowingThree_2.groupMembers
    assertEquals(2, elemsFollowingTwo.length)
    assertEquals("three", three_2.name)
    assertEquals("four", four_2.name)

    val elemsFollowingThree = three.couldBeNext
    val Seq(seqFollowingThree_3: Sequence) = three.couldBeNext
    val Seq(four_3: LocalElementBase) = seqFollowingThree_3.groupMembers
    assertEquals(1, elemsFollowingThree.length)
    assertEquals("four", four_3.name)

    val elemsFollowingFour = four.couldBeNext
    assertEquals(0, elemsFollowingFour.length)

  }

  /**
   * Here because 'two', and 'three' are optional...
   *
   * name	couldBeNext
   * =======================
   * one	two, three, four
   * a		b
   * b		c
   * c		two, three, four
   * two	three, four
   * three	four
   * four	-empty-
   */
  @Test def test_could_be_next_method_07() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="one" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="a" type="xs:string"/>
                  <xs:element name="b" type="xs:string"/>
                  <xs:element name="c" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="two" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="d" type="xs:string"/>
                  <xs:element name="e" type="xs:string"/>
                  <xs:element name="f" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="three" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="g" type="xs:string"/>
                  <xs:element name="h" type="xs:string"/>
                  <xs:element name="i" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:sequence>
              <xs:element name="four" dfdl:terminator=":">
                <xs:complexType>
                  <xs:sequence dfdl:separator=",">
                    <xs:element name="j" type="xs:string"/>
                    <xs:element name="k" type="xs:string"/>
                    <xs:element name="l" type="xs:string"/>
                  </xs:sequence>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val compiler = Compiler()
    val (sset, _) = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val root = declf.forRoot()
    val rootCT = root.immediateType.get.asInstanceOf[LocalComplexTypeDef]

    // Verify that nothing follows the root, as it is the root.
    val elemsFollowingRoot = root.couldBeNext
    assertEquals(0, elemsFollowingRoot.length)

    val rootCTSeq = rootCT.modelGroup.asInstanceOf[Sequence] //... which is a sequence

    // Verify that nothing follows the sequence of the root.
    val elemsFollowingRootSeq = rootCTSeq.couldBeNext
    assertEquals(0, elemsFollowingRootSeq.length)

    val Seq(one: LocalElementDecl, two: LocalElementDecl, three: LocalElementDecl, seq: Sequence) =
      rootCTSeq.groupMembers // has an element and a sub-sequence as its children.

    val Seq(four: LocalElementDecl) = seq.groupMembers

    val oneCT = one.immediateType.get.asInstanceOf[LocalComplexTypeDef]
    val oneCTSeq = oneCT.modelGroup.asInstanceOf[Sequence]
    val elemsFollowingOneCTSeq = oneCTSeq.couldBeNext

    val Seq(two_1: LocalElementBase, three_1: LocalElementBase, seqFollowingThree_1: Sequence) = oneCTSeq.couldBeNext
    val Seq(four_1: LocalElementBase) = seqFollowingThree_1.groupMembers
    assertEquals(3, elemsFollowingOneCTSeq.length)
    assertEquals("two", two_1.name)
    assertEquals("three", three_1.name)
    assertEquals("four", four_1.name)

    val Seq(a: LocalElementDecl, b: LocalElementDecl, c: LocalElementDecl) = oneCTSeq.groupMembers

    val elemsFollowingA = a.couldBeNext
    assertEquals(1, elemsFollowingA.length)
    assertEquals("b", elemsFollowingA(0).asInstanceOf[LocalElementBase].name)

    val elemsFollowingB = b.couldBeNext
    assertEquals(1, elemsFollowingB.length)
    assertEquals("c", elemsFollowingB(0).asInstanceOf[LocalElementBase].name)

    val elemsFollowingC = c.couldBeNext
    val Seq(two_2: LocalElementBase, three_2: LocalElementBase, seqFollowingThree_2: Sequence) = c.couldBeNext
    val Seq(four_2: LocalElementBase) = seqFollowingThree_2.groupMembers
    assertEquals(3, elemsFollowingC.length)
    assertEquals("two", two_2.name)
    assertEquals("three", three_2.name)
    assertEquals("four", four_2.name)

    val elemsFollowingTwo = two.couldBeNext
    val Seq(three_3: LocalElementBase, seqFollowingThree_3: Sequence) = two.couldBeNext
    val Seq(four_3: LocalElementBase) = seqFollowingThree_3.groupMembers
    assertEquals(2, elemsFollowingTwo.length)
    assertEquals("three", three_3.name)
    assertEquals("four", four_3.name)

    val elemsFollowingThree = three.couldBeNext
    val Seq(seqFollowingThree_4: Sequence) = three.couldBeNext
    val Seq(four_4: LocalElementBase) = seqFollowingThree_4.groupMembers
    assertEquals(1, elemsFollowingThree.length)
    assertEquals("four", four_4.name)

    val elemsFollowingFour = four.couldBeNext
    assertEquals(0, elemsFollowingFour.length)

  }

  /**
   * Here we've added three levels of sequences
   * that contain optional elements.
   *
   * The innermost sequence is in the last position.
   *
   * name	couldBeNext
   * =======================
   * one	two, three, four
   * a		b, c, s-1, s-2, s-3
   * b		c, s-1, s-2, s-3
   * c		s-1, s-2, s-3
   * two	three, four
   * three	four
   * four	-empty-
   */
  @Test def test_could_be_next_method_08() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="one" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="a" type="xs:string"/>
                  <xs:element name="b" type="xs:string" minOccurs="0" maxOccurs="1"/>
                  <xs:element name="c" type="xs:string" minOccurs="0" maxOccurs="1"/>
                  <xs:sequence dfdl:separator="-">
                    <xs:element name="s-1" type="xs:string" minOccurs="0" maxOccurs="1"/>
                    <xs:element name="s-2" type="xs:string" minOccurs="0" maxOccurs="1"/>
                    <xs:element name="s-3" type="xs:string"/>
                  </xs:sequence>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="two" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="d" type="xs:string"/>
                  <xs:element name="e" type="xs:string"/>
                  <xs:element name="f" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="three" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="g" type="xs:string"/>
                  <xs:element name="h" type="xs:string"/>
                  <xs:element name="i" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:sequence>
              <xs:element name="four" dfdl:terminator=":">
                <xs:complexType>
                  <xs:sequence dfdl:separator=",">
                    <xs:element name="j" type="xs:string"/>
                    <xs:element name="k" type="xs:string"/>
                    <xs:element name="l" type="xs:string"/>
                  </xs:sequence>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val compiler = Compiler()
    val (sset, _) = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val root = declf.forRoot()
    val rootCT = root.immediateType.get.asInstanceOf[LocalComplexTypeDef]

    // Verify that nothing follows the root, as it is the root.
    val elemsFollowingRoot = root.couldBeNext
    assertEquals(0, elemsFollowingRoot.length)

    val rootCTSeq = rootCT.modelGroup.asInstanceOf[Sequence] //... which is a sequence

    // Verify that nothing follows the sequence of the root.
    val elemsFollowingRootSeq = rootCTSeq.couldBeNext
    assertEquals(0, elemsFollowingRootSeq.length)

    val Seq(one: LocalElementDecl, two: LocalElementDecl, three: LocalElementDecl, seq: Sequence) =
      rootCTSeq.groupMembers // has an element and a sub-sequence as its children.

    val Seq(four: LocalElementDecl) = seq.groupMembers

    val oneCT = one.immediateType.get.asInstanceOf[LocalComplexTypeDef]
    val oneCTSeq = oneCT.modelGroup.asInstanceOf[Sequence]
    val elemsFollowingOneCTSeq = oneCTSeq.couldBeNext

    val Seq(two_1: LocalElementBase, three_1: LocalElementBase, seqFollowingThree_1: Sequence) = oneCTSeq.couldBeNext
    val Seq(four_1: LocalElementBase) = seqFollowingThree_1.groupMembers
    assertEquals(3, elemsFollowingOneCTSeq.length)
    assertEquals("two", two_1.name)
    assertEquals("three", three_1.name)
    assertEquals("four", four_1.name)

    val Seq(a: LocalElementDecl, b: LocalElementDecl, c: LocalElementDecl, one_subseq: Sequence) = oneCTSeq.groupMembers

    val elemsFollowingA = a.couldBeNext
    val Seq(b_1: LocalElementBase, c_1: LocalElementBase, seqFollowingC_1: Sequence) = a.couldBeNext
    val Seq(s1_1: LocalElementBase, s2_1: LocalElementBase, s3_1: LocalElementBase) = seqFollowingC_1.groupMembers
    assertEquals(3, elemsFollowingA.length)
    assertEquals("b", b_1.name)
    assertEquals("c", c_1.name)
    assertEquals("s-1", s1_1.name)
    assertEquals("s-2", s2_1.name)
    assertEquals("s-3", s3_1.name)

    val elemsFollowingB = b.couldBeNext
    val Seq(c_2: LocalElementBase, seqFollowingC_2: Sequence) = b.couldBeNext
    val Seq(s1_2: LocalElementBase, s2_2: LocalElementBase, s3_2: LocalElementBase) = seqFollowingC_2.groupMembers
    assertEquals(2, elemsFollowingB.length)
    assertEquals("c", c_2.name)
    assertEquals("s-1", s1_2.name)
    assertEquals("s-2", s2_2.name)
    assertEquals("s-3", s3_2.name)

    val elemsFollowingC = c.couldBeNext
    val Seq(seqFollowingC_3: Sequence) = c.couldBeNext
    val Seq(s1_3: LocalElementBase, s2_3: LocalElementBase, s3_3: LocalElementBase) = seqFollowingC_3.groupMembers
    assertEquals(1, elemsFollowingC.length)
    assertEquals("s-1", s1_3.name)
    assertEquals("s-2", s2_3.name)
    assertEquals("s-3", s3_3.name)

    val elemsFollowingTwo = two.couldBeNext
    val Seq(three_2: LocalElementBase, seqFollowingThree_2: Sequence) = two.couldBeNext
    val Seq(four_2: LocalElementBase) = seqFollowingThree_2.groupMembers
    assertEquals(2, elemsFollowingTwo.length)
    assertEquals("three", three_2.name)
    assertEquals("four", four_2.name)

    val elemsFollowingThree = three.couldBeNext
    val Seq(seqFollowingThree_3: Sequence) = three.couldBeNext
    val Seq(four_3: LocalElementBase) = seqFollowingThree_3.groupMembers
    assertEquals(1, elemsFollowingThree.length)
    assertEquals("four", four_3.name)

    val elemsFollowingFour = four.couldBeNext
    assertEquals(0, elemsFollowingFour.length)

  }

  /**
   * Here we've added three levels of sequences
   * that contain optional elements.
   *
   * The inner most sequence is in the middle.
   *
   * name	couldBeNext
   * =======================
   * one	two, three, four
   * a		b, s-1, s-2, s-3
   * b		s-1, s-2, s-3
   * c		two, three, four
   * two	three, four
   * three	four
   * four	-empty-
   */
  @Test def test_could_be_next_method_09() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="one" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="a" type="xs:string"/>
                  <xs:element name="b" type="xs:string" minOccurs="0" maxOccurs="1"/>
                  <xs:sequence dfdl:separator="-">
                    <xs:element name="s-1" type="xs:string" minOccurs="0" maxOccurs="1"/>
                    <xs:element name="s-2" type="xs:string" minOccurs="0" maxOccurs="1"/>
                    <xs:element name="s-3" type="xs:string"/>
                  </xs:sequence>
                  <xs:element name="c" type="xs:string" minOccurs="0" maxOccurs="1"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="two" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="d" type="xs:string"/>
                  <xs:element name="e" type="xs:string"/>
                  <xs:element name="f" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="three" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="g" type="xs:string"/>
                  <xs:element name="h" type="xs:string"/>
                  <xs:element name="i" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:sequence>
              <xs:element name="four" dfdl:terminator=":">
                <xs:complexType>
                  <xs:sequence dfdl:separator=",">
                    <xs:element name="j" type="xs:string"/>
                    <xs:element name="k" type="xs:string"/>
                    <xs:element name="l" type="xs:string"/>
                  </xs:sequence>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val compiler = Compiler()
    val (sset, _) = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val root = declf.forRoot()
    val rootCT = root.immediateType.get.asInstanceOf[LocalComplexTypeDef]

    // Verify that nothing follows the root, as it is the root.
    val elemsFollowingRoot = root.couldBeNext
    assertEquals(0, elemsFollowingRoot.length)

    val rootCTSeq = rootCT.modelGroup.asInstanceOf[Sequence] //... which is a sequence

    // Verify that nothing follows the sequence of the root.
    val elemsFollowingRootSeq = rootCTSeq.couldBeNext
    assertEquals(0, elemsFollowingRootSeq.length)

    val Seq(one: LocalElementDecl, two: LocalElementDecl, three: LocalElementDecl, seq: Sequence) =
      rootCTSeq.groupMembers // has an element and a sub-sequence as its children.

    val Seq(four: LocalElementDecl) = seq.groupMembers

    val oneCT = one.immediateType.get.asInstanceOf[LocalComplexTypeDef]
    val oneCTSeq = oneCT.modelGroup.asInstanceOf[Sequence]
    val elemsFollowingOneCTSeq = oneCTSeq.couldBeNext

    val Seq(two_1: LocalElementBase, three_1: LocalElementBase, seqFollowingThree_1: Sequence) = oneCTSeq.couldBeNext
    val Seq(four_1: LocalElementBase) = seqFollowingThree_1.groupMembers
    assertEquals(3, elemsFollowingOneCTSeq.length)
    assertEquals("two", two_1.name)
    assertEquals("three", three_1.name)
    assertEquals("four", four_1.name)

    val Seq(a: LocalElementDecl, b: LocalElementDecl, one_subseq: Sequence, c: LocalElementDecl) = oneCTSeq.groupMembers

    val elemsFollowingA = a.couldBeNext
    val Seq(b_1: LocalElementBase, seqFollowingA: Sequence) = a.couldBeNext
    val Seq(s1_1: LocalElementBase, s2_1: LocalElementBase, s3_1: LocalElementBase) = seqFollowingA.groupMembers
    assertEquals(2, elemsFollowingA.length)
    assertEquals("b", b_1.name)
    assertEquals("s-1", s1_1.name)
    assertEquals("s-2", s2_1.name)
    assertEquals("s-3", s3_1.name)

    val elemsFollowingB = b.couldBeNext
    val Seq(seqFollowingB: Sequence) = b.couldBeNext
    val Seq(s1_2: LocalElementBase, s2_2: LocalElementBase, s3_2: LocalElementBase) = seqFollowingB.groupMembers
    assertEquals(1, elemsFollowingB.length)
    assertEquals("s-1", s1_2.name)
    assertEquals("s-2", s2_2.name)
    assertEquals("s-3", s3_2.name)

    val elemsFollowingC = c.couldBeNext
    val Seq(two_2: LocalElementBase, three_2: LocalElementBase, seqFollowingThree_2: Sequence) = c.couldBeNext
    val Seq(four_2: LocalElementBase) = seqFollowingThree_2.groupMembers
    assertEquals(3, elemsFollowingC.length)
    assertEquals("two", two_2.name)
    assertEquals("three", three_2.name)
    assertEquals("four", four_2.name)

    val elemsFollowingTwo = two.couldBeNext
    val Seq(three_3: LocalElementBase, seqFollowingThree_3: Sequence) = two.couldBeNext
    val Seq(four_3: LocalElementBase) = seqFollowingThree_3.groupMembers
    assertEquals(2, elemsFollowingTwo.length)
    assertEquals("three", three_3.name)
    assertEquals("four", four_3.name)

    val elemsFollowingThree = three.couldBeNext
    val Seq(seqFollowingThree_4: Sequence) = three.couldBeNext
    val Seq(four_4: LocalElementBase) = seqFollowingThree_4.groupMembers
    assertEquals(1, elemsFollowingThree.length)
    assertEquals("four", four_4.name)

    val elemsFollowingFour = four.couldBeNext
    assertEquals(0, elemsFollowingFour.length)

  }

  /**
   * Here we've added three levels of sequences
   * that contain optional elements.
   *
   * The inner most sequence is in the beginning.
   *
   * name	couldBeNext
   * =======================
   * one	two, three, four
   * a		b
   * b		c
   * c		two, three, four
   * two	three, four
   * three	four
   * four	-empty-
   */
  @Test def test_could_be_next_method_10() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="one" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:sequence dfdl:separator="-">
                    <xs:element name="s-1" type="xs:string" minOccurs="0" maxOccurs="1"/>
                    <xs:element name="s-2" type="xs:string" minOccurs="0" maxOccurs="1"/>
                    <xs:element name="s-3" type="xs:string"/>
                  </xs:sequence>
                  <xs:element name="a" type="xs:string"/>
                  <xs:element name="b" type="xs:string" minOccurs="0" maxOccurs="1"/>
                  <xs:element name="c" type="xs:string" minOccurs="0" maxOccurs="1"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="two" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="d" type="xs:string"/>
                  <xs:element name="e" type="xs:string"/>
                  <xs:element name="f" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="three" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="g" type="xs:string"/>
                  <xs:element name="h" type="xs:string"/>
                  <xs:element name="i" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:sequence>
              <xs:element name="four" dfdl:terminator=":">
                <xs:complexType>
                  <xs:sequence dfdl:separator=",">
                    <xs:element name="j" type="xs:string"/>
                    <xs:element name="k" type="xs:string"/>
                    <xs:element name="l" type="xs:string"/>
                  </xs:sequence>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val compiler = Compiler()
    val (sset, _) = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val root = declf.forRoot()
    val rootCT = root.immediateType.get.asInstanceOf[LocalComplexTypeDef]

    // Verify that nothing follows the root, as it is the root.
    val elemsFollowingRoot = root.couldBeNext
    assertEquals(0, elemsFollowingRoot.length)

    val rootCTSeq = rootCT.modelGroup.asInstanceOf[Sequence] //... which is a sequence

    // Verify that nothing follows the sequence of the root.
    val elemsFollowingRootSeq = rootCTSeq.couldBeNext
    assertEquals(0, elemsFollowingRootSeq.length)

    val Seq(one: LocalElementDecl, two: LocalElementDecl, three: LocalElementDecl, seq: Sequence) =
      rootCTSeq.groupMembers // has an element and a sub-sequence as its children.

    val Seq(four: LocalElementDecl) = seq.groupMembers

    val oneCT = one.immediateType.get.asInstanceOf[LocalComplexTypeDef]
    val oneCTSeq = oneCT.modelGroup.asInstanceOf[Sequence]
    val elemsFollowingOneCTSeq = oneCTSeq.couldBeNext

    val Seq(two_1: LocalElementBase, three_1: LocalElementBase, seqFollowingThree_1: Sequence) = oneCTSeq.couldBeNext
    val Seq(four_1: LocalElementBase) = seqFollowingThree_1.groupMembers
    assertEquals(3, elemsFollowingOneCTSeq.length)
    assertEquals("two", two_1.name)
    assertEquals("three", three_1.name)
    assertEquals("four", four_1.name)

    val Seq(one_subseq: Sequence, a: LocalElementDecl, b: LocalElementDecl, c: LocalElementDecl) = oneCTSeq.groupMembers

    val elemsFollowingA = a.couldBeNext
    val Seq(b_2: LocalElementBase, c_2: LocalElementBase, two_2: LocalElementBase, three_2: LocalElementBase, seqFollowingThree_2: Sequence) = a.couldBeNext
    val Seq(four_2: LocalElementBase) = seqFollowingThree_2.groupMembers
    assertEquals(5, elemsFollowingA.length)
    assertEquals("b", b_2.name)
    assertEquals("c", c_2.name)
    assertEquals("two", two_2.name)
    assertEquals("three", three_2.name)
    assertEquals("four", four_2.name)

    val elemsFollowingB = b.couldBeNext
    val Seq(c_3: LocalElementBase, two_3: LocalElementBase, three_3: LocalElementBase, seqFollowingThree_3: Sequence) = b.couldBeNext
    val Seq(four_3: LocalElementBase) = seqFollowingThree_3.groupMembers
    assertEquals(4, elemsFollowingB.length)
    assertEquals("c", c_3.name)
    assertEquals("two", two_3.name)
    assertEquals("three", three_3.name)
    assertEquals("four", four_3.name)

    val elemsFollowingC = c.couldBeNext
    val Seq(two_4: LocalElementBase, three_4: LocalElementBase, seqFollowingThree_4: Sequence) = c.couldBeNext
    val Seq(four_4: LocalElementBase) = seqFollowingThree_4.groupMembers
    assertEquals(3, elemsFollowingC.length)
    assertEquals("two", two_4.name)
    assertEquals("three", three_4.name)
    assertEquals("four", four_4.name)

    val elemsFollowingTwo = two.couldBeNext
    val Seq(three_5: LocalElementBase, seqFollowingThree_5: Sequence) = two.couldBeNext
    val Seq(four_5: LocalElementBase) = seqFollowingThree_5.groupMembers
    assertEquals(2, elemsFollowingTwo.length)
    assertEquals("three", three_5.name)
    assertEquals("four", four_5.name)

    val elemsFollowingThree = three.couldBeNext
    val Seq(seqFollowingThree_6: Sequence) = three.couldBeNext
    val Seq(four_6: LocalElementBase) = seqFollowingThree_6.groupMembers
    assertEquals(1, elemsFollowingThree.length)
    assertEquals("four", four_6.name)

    val elemsFollowingFour = four.couldBeNext
    assertEquals(0, elemsFollowingFour.length)

  }

  /**
   * Here we just want to test that we can detect next
   * elements across sequences (unordered).  All elements are 'required'.
   * However, because these are unordered sequences optionality doesn't matter.
   */
  @Test def test_could_be_next_method_11() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence dfdl:sequenceKind="unordered">
            <xs:element name="one" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator=",">
                  <xs:element name="a" type="xs:string"/>
                  <xs:element name="b" type="xs:string"/>
                  <xs:element name="c" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="two" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator="," dfdl:sequenceKind="unordered">
                  <xs:element name="d" type="xs:string"/>
                  <xs:element name="e" type="xs:string"/>
                  <xs:element name="f" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="three" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator="," dfdl:sequenceKind="unordered">
                  <xs:element name="g" type="xs:string"/>
                  <xs:element name="h" type="xs:string"/>
                  <xs:element name="i" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val compiler = Compiler()
    val (sset, _) = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val root = declf.forRoot()
    val rootCT = root.immediateType.get.asInstanceOf[LocalComplexTypeDef]

    // Verify that nothing follows the root, as it is the root.
    val elemsFollowingRoot = root.couldBeNext
    assertEquals(0, elemsFollowingRoot.length)

    val rootCTSeq = rootCT.modelGroup.asInstanceOf[Sequence] //... which is a sequence

    // Verify that nothing follows the sequence of the root.
    val elemsFollowingRootSeq = rootCTSeq.couldBeNext
    assertEquals(0, elemsFollowingRootSeq.length)

    val Seq(one: LocalElementBase, two: LocalElementBase, three: LocalElementBase) =
      rootCTSeq.groupMembers // has an element and a sub-sequence as its children.

    val elemsFollowingOne = one.couldBeNext
    assertEquals(3, elemsFollowingOne.length)
    assertEquals("one", elemsFollowingOne(0).asInstanceOf[LocalElementBase].name)
    assertEquals("two", elemsFollowingOne(1).asInstanceOf[LocalElementBase].name)
    assertEquals("three", elemsFollowingOne(2).asInstanceOf[LocalElementBase].name)

    val elemsFollowingTwo = two.couldBeNext
    assertEquals(3, elemsFollowingTwo.length)
    assertEquals("one", elemsFollowingTwo(0).asInstanceOf[LocalElementBase].name)
    assertEquals("two", elemsFollowingTwo(1).asInstanceOf[LocalElementBase].name)
    assertEquals("three", elemsFollowingTwo(2).asInstanceOf[LocalElementBase].name)

    val elemsFollowingThree = three.couldBeNext
    assertEquals(3, elemsFollowingThree.length)
    assertEquals("one", elemsFollowingThree(0).asInstanceOf[LocalElementBase].name)
    assertEquals("two", elemsFollowingThree(1).asInstanceOf[LocalElementBase].name)
    assertEquals("three", elemsFollowingThree(2).asInstanceOf[LocalElementBase].name)
  }

  /**
   * Unordered Sequences
   *
   * Here the fact that 'three' is optional doesn't matter.  The
   * elements are in an unordered sequence, so any of the elements:
   * one, two or three could be next.
   */
  @Test def test_could_be_next_method_12() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence dfdl:sequenceKind="unordered">
            <xs:element name="one" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator="," dfdl:sequenceKind="unordered">
                  <xs:element name="a" type="xs:string"/>
                  <xs:element name="b" type="xs:string"/>
                  <xs:element name="c" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="two" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator="," dfdl:sequenceKind="unordered">
                  <xs:element name="d" type="xs:string"/>
                  <xs:element name="e" type="xs:string"/>
                  <xs:element name="f" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="three" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
              <xs:complexType>
                <xs:sequence dfdl:separator="," dfdl:sequenceKind="unordered">
                  <xs:element name="g" type="xs:string"/>
                  <xs:element name="h" type="xs:string"/>
                  <xs:element name="i" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val compiler = Compiler()
    val (sset, _) = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val root = declf.forRoot()
    val rootCT = root.immediateType.get.asInstanceOf[LocalComplexTypeDef]

    // Verify that nothing follows the root, as it is the root.
    val elemsFollowingRoot = root.couldBeNext
    assertEquals(0, elemsFollowingRoot.length)

    val rootCTSeq = rootCT.modelGroup.asInstanceOf[Sequence] //... which is a sequence

    // Verify that nothing follows the sequence of the root.
    val elemsFollowingRootSeq = rootCTSeq.couldBeNext
    assertEquals(0, elemsFollowingRootSeq.length)

    val Seq(one: LocalElementBase, two: LocalElementBase, three: LocalElementBase) =
      rootCTSeq.groupMembers

    val elemsFollowingOne = one.couldBeNext
    assertEquals(3, elemsFollowingOne.length)
    assertEquals("one", elemsFollowingOne(0).asInstanceOf[LocalElementBase].name)
    assertEquals("two", elemsFollowingOne(1).asInstanceOf[LocalElementBase].name)
    assertEquals("three", elemsFollowingOne(2).asInstanceOf[LocalElementBase].name)

    val elemsFollowingTwo = two.couldBeNext
    assertEquals(3, elemsFollowingTwo.length)
    assertEquals("one", elemsFollowingTwo(0).asInstanceOf[LocalElementBase].name)
    assertEquals("two", elemsFollowingTwo(1).asInstanceOf[LocalElementBase].name)
    assertEquals("three", elemsFollowingTwo(2).asInstanceOf[LocalElementBase].name)

    val elemsFollowingThree = three.couldBeNext
    assertEquals(3, elemsFollowingThree.length)
    assertEquals("one", elemsFollowingThree(0).asInstanceOf[LocalElementBase].name)
    assertEquals("two", elemsFollowingThree(1).asInstanceOf[LocalElementBase].name)
    assertEquals("three", elemsFollowingThree(2).asInstanceOf[LocalElementBase].name)
  }

  /**
   * Unordered Sequences
   *
   * Here because 'two', and 'three' are optional...
   *
   * name	couldBeNext
   * =======================
   * one	two, three
   * a		b, c
   * b		a, c
   * c		a, b, one, two, three
   * two	one, three
   * three	one, two
   */
  @Test def test_could_be_next_method_13() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence dfdl:sequenceKind="unordered">
            <xs:element name="one" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator="," dfdl:sequenceKind="unordered">
                  <xs:element name="a" type="xs:string"/>
                  <xs:element name="b" type="xs:string"/>
                  <xs:element name="c" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="two" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
              <xs:complexType>
                <xs:sequence dfdl:separator="," dfdl:sequenceKind="unordered">
                  <xs:element name="d" type="xs:string"/>
                  <xs:element name="e" type="xs:string"/>
                  <xs:element name="f" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="three" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
              <xs:complexType>
                <xs:sequence dfdl:separator="," dfdl:sequenceKind="unordered">
                  <xs:element name="g" type="xs:string"/>
                  <xs:element name="h" type="xs:string"/>
                  <xs:element name="i" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val compiler = Compiler()
    val (sset, _) = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val root = declf.forRoot()
    val rootCT = root.immediateType.get.asInstanceOf[LocalComplexTypeDef]

    // Verify that nothing follows the root, as it is the root.
    val elemsFollowingRoot = root.couldBeNext
    assertEquals(0, elemsFollowingRoot.length)

    val rootCTSeq = rootCT.modelGroup.asInstanceOf[Sequence] //... which is a sequence

    // Verify that nothing follows the sequence of the root.
    val elemsFollowingRootSeq = rootCTSeq.couldBeNext
    assertEquals(0, elemsFollowingRootSeq.length)

    val Seq(one: LocalElementDecl, two: LocalElementDecl, three: LocalElementDecl) =
      rootCTSeq.groupMembers // has an element and a sub-sequence as its children.

    val oneCT = one.immediateType.get.asInstanceOf[LocalComplexTypeDef]
    val oneCTSeq = oneCT.modelGroup.asInstanceOf[Sequence]
    val elemsFollowingOneCTSeq = oneCTSeq.couldBeNext

    val Seq(one_1: LocalElementBase, two_1: LocalElementBase, three_1: LocalElementBase) = oneCTSeq.couldBeNext
    assertEquals(3, elemsFollowingOneCTSeq.length)
    assertEquals("one", one_1.name)
    assertEquals("two", two_1.name)
    assertEquals("three", three_1.name)

    val Seq(a: LocalElementDecl, b: LocalElementDecl, c: LocalElementDecl) = oneCTSeq.groupMembers

    val elemsFollowingA = a.couldBeNext
    assertEquals(6, elemsFollowingA.length)
    assertEquals("a", elemsFollowingA(0).asInstanceOf[LocalElementBase].name)
    assertEquals("b", elemsFollowingA(1).asInstanceOf[LocalElementBase].name)
    assertEquals("c", elemsFollowingA(2).asInstanceOf[LocalElementBase].name)
    assertEquals("one", elemsFollowingA(3).asInstanceOf[LocalElementBase].name)
    assertEquals("two", elemsFollowingA(4).asInstanceOf[LocalElementBase].name)
    assertEquals("three", elemsFollowingA(5).asInstanceOf[LocalElementBase].name)

    val elemsFollowingB = b.couldBeNext
    assertEquals(6, elemsFollowingB.length)
    assertEquals("a", elemsFollowingB(0).asInstanceOf[LocalElementBase].name)
    assertEquals("b", elemsFollowingB(1).asInstanceOf[LocalElementBase].name)
    assertEquals("c", elemsFollowingB(2).asInstanceOf[LocalElementBase].name)
    assertEquals("one", elemsFollowingB(3).asInstanceOf[LocalElementBase].name)
    assertEquals("two", elemsFollowingB(4).asInstanceOf[LocalElementBase].name)
    assertEquals("three", elemsFollowingB(5).asInstanceOf[LocalElementBase].name)

    val elemsFollowingC = c.couldBeNext
    val Seq(a_2: LocalElementBase, b_2: LocalElementBase, c_2: LocalElementBase, one_2: LocalElementBase, two_2: LocalElementBase, three_2: LocalElementBase) = c.couldBeNext
    assertEquals(6, elemsFollowingC.length)
    assertEquals("a", a_2.name)
    assertEquals("b", b_2.name)
    assertEquals("c", c_2.name)
    assertEquals("one", one_2.name)
    assertEquals("two", two_2.name)
    assertEquals("three", three_2.name)

    val elemsFollowingTwo = two.couldBeNext
    val Seq(one_3: LocalElementBase, two_3: LocalElementBase, three_3: LocalElementBase) = two.couldBeNext
    assertEquals(3, elemsFollowingTwo.length)
    assertEquals("one", one_3.name)
    assertEquals("two", two_3.name)
    assertEquals("three", three_3.name)

    val elemsFollowingThree = three.couldBeNext
    val Seq(one_4: LocalElementBase, two_4: LocalElementBase, three_4: LocalElementBase) = three.couldBeNext
    assertEquals(3, elemsFollowingThree.length)
    assertEquals("one", one_4.name)
    assertEquals("two", two_4.name)
    assertEquals("three", three_4.name)
  }

  /**
   * Unordered Sequences
   *
   * Here because 'two', and 'three' are optional...
   *
   * name	couldBeNext
   * =======================
   * one	two, three
   * a		b
   * b		c
   * c		one, two, three
   * two	one, three
   * three	one, two
   */
  @Test def test_could_be_next_method_13_1() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence dfdl:sequenceKind="unordered">
            <xs:element name="one" dfdl:terminator=":">
              <xs:complexType>
                <xs:sequence dfdl:separator="," dfdl:sequenceKind="ordered">
                  <xs:element name="a" type="xs:string"/>
                  <xs:element name="b" type="xs:string"/>
                  <xs:element name="c" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="two" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
              <xs:complexType>
                <xs:sequence dfdl:separator="," dfdl:sequenceKind="unordered">
                  <xs:element name="d" type="xs:string"/>
                  <xs:element name="e" type="xs:string"/>
                  <xs:element name="f" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="three" dfdl:terminator=":" minOccurs="0" maxOccurs="1">
              <xs:complexType>
                <xs:sequence dfdl:separator="," dfdl:sequenceKind="unordered">
                  <xs:element name="g" type="xs:string"/>
                  <xs:element name="h" type="xs:string"/>
                  <xs:element name="i" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val compiler = Compiler()
    val (sset, _) = compiler.frontEnd(testSchema)
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val root = declf.forRoot()
    val rootCT = root.immediateType.get.asInstanceOf[LocalComplexTypeDef]

    // Verify that nothing follows the root, as it is the root.
    val elemsFollowingRoot = root.couldBeNext
    assertEquals(0, elemsFollowingRoot.length)

    val rootCTSeq = rootCT.modelGroup.asInstanceOf[Sequence] //... which is a sequence

    // Verify that nothing follows the sequence of the root.
    val elemsFollowingRootSeq = rootCTSeq.couldBeNext
    assertEquals(0, elemsFollowingRootSeq.length)

    val Seq(one: LocalElementDecl, two: LocalElementDecl, three: LocalElementDecl) =
      rootCTSeq.groupMembers // has an element and a sub-sequence as its children.

    val oneCT = one.immediateType.get.asInstanceOf[LocalComplexTypeDef]
    val oneCTSeq = oneCT.modelGroup.asInstanceOf[Sequence]
    val elemsFollowingOneCTSeq = oneCTSeq.couldBeNext

    val Seq(one_1: LocalElementBase, two_1: LocalElementBase, three_1: LocalElementBase) = oneCTSeq.couldBeNext
    assertEquals(3, elemsFollowingOneCTSeq.length)
    assertEquals("one", one_1.name)
    assertEquals("two", two_1.name)
    assertEquals("three", three_1.name)

    val Seq(a: LocalElementDecl, b: LocalElementDecl, c: LocalElementDecl) = oneCTSeq.groupMembers

    val elemsFollowingA = a.couldBeNext
    assertEquals(1, elemsFollowingA.length)
    assertEquals("b", elemsFollowingA(0).asInstanceOf[LocalElementBase].name)

    val elemsFollowingB = b.couldBeNext
    assertEquals(1, elemsFollowingB.length)
    assertEquals("c", elemsFollowingB(0).asInstanceOf[LocalElementBase].name)

    val elemsFollowingC = c.couldBeNext
    val Seq(one_2: LocalElementBase, two_2: LocalElementBase, three_2: LocalElementBase) = c.couldBeNext
    assertEquals(3, elemsFollowingC.length)
    assertEquals("one", one_2.name)
    assertEquals("two", two_2.name)
    assertEquals("three", three_2.name)

    val elemsFollowingTwo = two.couldBeNext
    val Seq(one_3: LocalElementBase, two_3: LocalElementBase, three_3: LocalElementBase) = two.couldBeNext
    assertEquals(3, elemsFollowingTwo.length)
    assertEquals("one", one_3.name)
    assertEquals("two", two_3.name)
    assertEquals("three", three_3.name)

    val elemsFollowingThree = three.couldBeNext
    val Seq(one_4: LocalElementBase, two_4: LocalElementBase, three_4: LocalElementBase) = three.couldBeNext
    assertEquals(3, elemsFollowingThree.length)
    assertEquals("one", one_4.name)
    assertEquals("two", two_4.name)
    assertEquals("three", three_4.name)
  }

}
