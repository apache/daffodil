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

import org.apache.daffodil.lib.util._
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Assert._
import org.junit.Test

class TestMiddleEndAttributes {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def test_hasStaticallyRequiredOccurrencesInDataRepresentation_1(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" terminator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:sequenceKind="ordered">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
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
    val seq = e1ct.sequence
    val Seq(s1, s2) = seq.groupMembers
    assertTrue(s1.hasStaticallyRequiredOccurrencesInDataRepresentation)
    assertTrue(s2.hasStaticallyRequiredOccurrencesInDataRepresentation)
  }

  @Test def test_hasStaticallyRequiredOccurrencesInDataRepresentation_2(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format representation="text" occursCountKind="parsed" lengthUnits="bytes" encoding="US-ASCII" initiator="" terminator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:sequenceKind="ordered">
            <xs:element name="s1" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s2" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
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
    val seq = e1ct.sequence
    val Seq(s1, s2) = seq.groupMembers
    assertFalse(s1.hasStaticallyRequiredOccurrencesInDataRepresentation)
    assertFalse(s2.hasStaticallyRequiredOccurrencesInDataRepresentation)
  }

  @Test def testRequiredSiblings(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format representation="text" occursCountKind="parsed" lengthUnits="bytes" encoding="US-ASCII" initiator="" terminator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:sequenceKind="ordered">
            <xs:element name="s1" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s3" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s4" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s5" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
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
    val seq = e1ct.sequence
    val Seq(s1, s2, s3, s4, s5) = seq.groupMembers
    assertFalse(s1.hasStaticallyRequiredOccurrencesInDataRepresentation)
    assertTrue(s2.hasStaticallyRequiredOccurrencesInDataRepresentation)
    assertFalse(s3.hasStaticallyRequiredOccurrencesInDataRepresentation)
    assertTrue(s4.hasStaticallyRequiredOccurrencesInDataRepresentation)
    assertFalse(s5.hasStaticallyRequiredOccurrencesInDataRepresentation)
  }

  @Test def testStaticallyFirstWithChoice(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format representation="text" occursCountKind="parsed" lengthUnits="bytes" encoding="US-ASCII" initiator="" terminator="" separator="" ignoreCase="no"
        alignment="1" alignmentUnits="bytes" leadingSkip="0" trailingSkip="0"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:sequenceKind="ordered" dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:choice>
              <xs:element name="s1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
              <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            </xs:choice>
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
    val seq = e1ct.sequence
    val Seq(seqMem) = seq.groupMembers
    val cho = seqMem.asInstanceOf[Choice]
    val Seq(s1, s2) = cho.groupMembers
    val es = s1.optLexicalParent.get.optLexicalParent.get.asInstanceOf[LocalSequence]
    assertTrue(es.hasInfixSep)
    assertEquals(1, s1.position)
    assertTrue(s1.isScalar)
    val es2 = s2.optLexicalParent.get.optLexicalParent.get.asInstanceOf[LocalSequence]
    assertEquals(es, es2)
    assertEquals(2, s2.position)
    assertTrue(s2.isScalar)
  }

  @Test def testNearestEnclosingSequenceElementRef(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" representation="text" occursCountKind="parsed" lengthUnits="bytes" encoding="US-ASCII" initiator="" terminator="" separator="" ignoreCase="no" initiatedContent="no"/>,
      <xs:element name="e1" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
      <xs:element name="e2" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:sequenceKind="ordered" dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:element ref="e1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )

    val sset = SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val sd = sch.schemaDocuments.head

    // Explore global element decl
    val Seq(_, e2) = sd.globalElementDecls
    val e2ct = e2.complexType
    val seq = e2ct.sequence
    val mems = seq.groupMembers
    val Seq(t1: Term) = mems
    val e1ref = t1.asInstanceOf[ElementRef]
    val Some(nes: LocalSequence) = e1ref.optLexicalParent
    assertEquals(seq, nes)
  }

  @Test def testImmediatelyEnclosingModelGroup1(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" lengthKind="delimited"/>,
      <xs:element name="doc">
        <xs:complexType>
          <xs:sequence dfdl:sequenceKind="ordered">
            <xs:element name="msg" type="ex:msgType" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:complexType name="msgType">
        <xs:choice dfdl:initiatedContent="yes">
          <xs:element ref="ex:fWithInitiator"/>
          <!-- Only one in the choice -->
        </xs:choice>
      </xs:complexType>
      <xs:element name="fWithInitiator" dfdl:initiator="$">
        <xs:complexType>
          <xs:sequence dfdl:sequenceKind="ordered">
            <xs:element name="f" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )

    val sset = SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val sd = sch.schemaDocuments.head

    // Explore global element decl
    val Seq(e1, _) = sd.globalElementDecls
    val e1ct = e1.complexType
    val e1seq = e1ct.sequence
    val Seq(t1: Term) = e1seq.groupMembers

    val eMsg = t1.asInstanceOf[LocalElementDecl]
    val eMsgct = eMsg.typeDef.asInstanceOf[GlobalComplexTypeDef]
    val eMsgChoice = eMsgct.modelGroup.asInstanceOf[Choice]
    val Seq(t2: Term) = eMsgChoice.groupMembers
    val e2ref = t2.asInstanceOf[ElementRef]
    val e3 = e2ref.referencedElement
    val e3ct = e3.complexType
    val e3seq = e3ct.sequence
    val e3seqImmediatelyEnclosingModelGroup = e3seq.immediatelyEnclosingModelGroup
    // Sequence inside an element doesn't have an immediately enclosing model group
    assertEquals(None, e3seqImmediatelyEnclosingModelGroup)
    // element reference also contained inside same
    assertEquals(Some(eMsgChoice), e2ref.immediatelyEnclosingModelGroup)

  }

}
