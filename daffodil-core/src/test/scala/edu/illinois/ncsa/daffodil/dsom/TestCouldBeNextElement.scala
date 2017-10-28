/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.dsom

import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.compiler._
import junit.framework.Assert.assertEquals
import edu.illinois.ncsa.daffodil.infoset.ChoiceBranchStartEvent
import edu.illinois.ncsa.daffodil.infoset.ChoiceBranchEndEvent

class TestCouldBeNextElement {

  @Test def testCouldBeNextElement_1() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:choice>
              <xs:sequence>
                <xs:sequence>
                </xs:sequence>
              </xs:sequence>
              <xs:sequence>
              </xs:sequence>
              <xs:element name="bar1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
            </xs:choice>
            <xs:element name="barOpt" minOccurs="0" dfdl:occursCountKind="implicit" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="bar2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val root = declf.forRoot()

    val rootCT = root.complexType
    val rootSeq = rootCT.sequence
    val Seq(choice1: Choice, barOpt: LocalElementBase, bar2: LocalElementBase) = rootSeq.groupMembers
    val Seq(choice1seq1: Sequence, choice1seq2: Sequence, bar1: LocalElementBase) = choice1.groupMembers
    val Seq(gm: Sequence) = choice1seq1.groupMembers
    assertNotNull(gm)

    assertEquals(0, root.possibleNextChildElementsInInfoset.length)
    assertEquals(3, root.possibleFirstChildElementsInInfoset.length)
    assertEquals("bar1", root.possibleFirstChildElementsInInfoset(0).asInstanceOf[LocalElementBase].name)
    assertEquals("barOpt", root.possibleFirstChildElementsInInfoset(1).asInstanceOf[LocalElementBase].name)
    assertEquals("bar2", root.possibleFirstChildElementsInInfoset(2).asInstanceOf[LocalElementBase].name)

    //val bar1PFCEII = bar1.possibleFirstChildElementsInInfoset
    //val bar1PNCEII = bar1.possibleNextChildElementsInInfoset
    assertEquals(0, bar1.possibleFirstChildElementsInInfoset.length)
    assertEquals(2, bar1.possibleNextChildElementsInInfoset.length)
    assertEquals("barOpt", bar1.possibleNextChildElementsInInfoset(0).asInstanceOf[LocalElementBase].name)
    assertEquals("bar2", bar1.possibleNextChildElementsInInfoset(1).asInstanceOf[LocalElementBase].name)

    assertEquals(0, barOpt.possibleFirstChildElementsInInfoset.length)
    assertEquals(1, barOpt.possibleNextChildElementsInInfoset.length)
    assertEquals("bar2", barOpt.possibleNextChildElementsInInfoset(0).asInstanceOf[LocalElementBase].name)

    assertEquals(0, bar2.possibleFirstChildElementsInInfoset.length)
    assertEquals(0, bar2.possibleNextChildElementsInInfoset.length)

    assertEquals(2, choice1seq1.identifyingEventsForChoiceBranch.length)
    assertEquals("barOpt", choice1seq1.identifyingEventsForChoiceBranch(0).qname.local)
    assertEquals("bar2", choice1seq1.identifyingEventsForChoiceBranch(1).qname.local)

    assertEquals(2, choice1seq2.identifyingEventsForChoiceBranch.length)
    assertEquals("barOpt", choice1seq2.identifyingEventsForChoiceBranch(0).qname.local)
    assertEquals("bar2", choice1seq2.identifyingEventsForChoiceBranch(1).qname.local)

    assertEquals(3, choice1.choiceBranchMap.size)
    assertEquals(bar1.runtimeData, choice1.choiceBranchMap(ChoiceBranchStartEvent(bar1.namedQName)))
    assertEquals(choice1seq1.runtimeData, choice1.choiceBranchMap(ChoiceBranchStartEvent(barOpt.namedQName)))
    assertEquals(choice1seq1.runtimeData, choice1.choiceBranchMap(ChoiceBranchStartEvent(bar2.namedQName)))
  }

  @Test def testCouldBeNextElement_2() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="array" minOccurs="0" maxOccurs="unbounded" dfdl:occursCountKind="implicit">
              <xs:complexType>
                <xs:choice>
                  <xs:element name="ch1" type="xs:string"/>
                  <xs:element name="ch2" type="xs:string"/>
                  <xs:element name="array" type="xs:string"/>
                  <xs:sequence/>
                </xs:choice>
              </xs:complexType>
            </xs:element>
            <xs:element name="after" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val root = declf.forRoot()

    val rootCT = root.complexType
    val rootSeq = rootCT.sequence
    val Seq(array: LocalElementBase, after: LocalElementBase) = rootSeq.groupMembers
    val arrayCT = array.complexType
    val choice = arrayCT.modelGroup.asInstanceOf[Choice]
    val Seq(ch1: LocalElementBase, ch2: LocalElementBase, array2: LocalElementBase, choiceSeq: Sequence) = choice.groupMembers

    assertEquals(0, root.possibleNextChildElementsInInfoset.length)
    assertEquals(2, root.possibleFirstChildElementsInInfoset.length)
    assertEquals("array", root.possibleFirstChildElementsInInfoset(0).asInstanceOf[LocalElementBase].name)
    assertEquals("after", root.possibleFirstChildElementsInInfoset(1).asInstanceOf[LocalElementBase].name)

    assertEquals(3, array.possibleFirstChildElementsInInfoset.length)
    assertEquals("ch1", array.possibleFirstChildElementsInInfoset(0).asInstanceOf[LocalElementBase].name)
    assertEquals("ch2", array.possibleFirstChildElementsInInfoset(1).asInstanceOf[LocalElementBase].name)
    assertEquals("array", array.possibleFirstChildElementsInInfoset(2).asInstanceOf[LocalElementBase].name)

    assertEquals(2, array.possibleNextChildElementsInInfoset.length)
    assertEquals("array", array.possibleNextChildElementsInInfoset(0).asInstanceOf[LocalElementBase].name)
    assertEquals("after", array.possibleNextChildElementsInInfoset(1).asInstanceOf[LocalElementBase].name)

    assertEquals(0, after.possibleFirstChildElementsInInfoset.length)
    assertEquals(0, after.possibleNextChildElementsInInfoset.length)

    assertEquals(0, ch1.possibleFirstChildElementsInInfoset.length)
    assertEquals(0, ch1.possibleNextChildElementsInInfoset.length)

    assertEquals(0, ch2.possibleFirstChildElementsInInfoset.length)
    assertEquals(0, ch2.possibleNextChildElementsInInfoset.length)

    assertEquals(0, array2.possibleFirstChildElementsInInfoset.length)
    assertEquals(0, array2.possibleNextChildElementsInInfoset.length)

    assertEquals(4, choice.choiceBranchMap.size)
    assertEquals(ch1.runtimeData, choice.choiceBranchMap(ChoiceBranchStartEvent(ch1.namedQName)))
    assertEquals(ch2.runtimeData, choice.choiceBranchMap(ChoiceBranchStartEvent(ch2.namedQName)))
    assertEquals(array2.runtimeData, choice.choiceBranchMap(ChoiceBranchStartEvent(array2.namedQName)))
    assertEquals(choiceSeq.runtimeData, choice.choiceBranchMap(ChoiceBranchEndEvent(array.namedQName)))
  }

  @Test def testCouldBeNextElement_3() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="e1" type="xs:string"/>
            <xs:element name="e2" type="xs:string"/>
            <xs:element name="e3" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val root = declf.forRoot()

    val rootCT = root.complexType
    val rootSeq = rootCT.sequence
    val Seq(e1: LocalElementBase, e2: LocalElementBase, e3: LocalElementBase) = rootSeq.groupMembers

    assertEquals(0, root.possibleNextChildElementsInInfoset.length)
    assertEquals(1, root.possibleFirstChildElementsInInfoset.length)
    assertEquals("e1", root.possibleFirstChildElementsInInfoset(0).asInstanceOf[LocalElementBase].name)

    assertEquals(1, e1.possibleNextChildElementsInInfoset.length)
    assertEquals("e2", e1.possibleNextChildElementsInInfoset(0).asInstanceOf[LocalElementBase].name)

    assertEquals(1, e2.possibleNextChildElementsInInfoset.length)
    assertEquals("e3", e2.possibleNextChildElementsInInfoset(0).asInstanceOf[LocalElementBase].name)

    assertEquals(0, e3.possibleNextChildElementsInInfoset.length)
  }

  @Test def testCouldBeNextElement_4() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="e1" type="xs:string"/>
            <xs:sequence dfdl:hiddenGroupRef="g1_hidden"/>
            <xs:element name="e2" type="xs:string" dfdl:occursCountKind="parsed"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:group name="g1_hidden">
        <xs:sequence>
          <xs:element name="e2_hidden" type="xs:string" dfdl:occursCountKind="parsed"/>
        </xs:sequence>
      </xs:group>)

    val compiler = Compiler()
    val sset = compiler.compileNode(testSchema).sset
    val Seq(schema) = sset.schemas
    val Seq(schemaDoc, _) = schema.schemaDocuments
    val Seq(declf) = schemaDoc.globalElementDecls
    val root = declf.forRoot()

    assertEquals(0, root.possibleNextChildElementsInInfoset.length)
    assertEquals(1, root.possibleFirstChildElementsInInfoset.length)

    val e1 = root.possibleFirstChildElementsInInfoset(0).asInstanceOf[LocalElementBase]

    assertEquals("e1", e1.name)
    assertEquals(1, e1.possibleNextChildElementsInInfoset.length)
    assertEquals("e2", e1.possibleNextChildElementsInInfoset(0).asInstanceOf[LocalElementBase].name)
  }

}
