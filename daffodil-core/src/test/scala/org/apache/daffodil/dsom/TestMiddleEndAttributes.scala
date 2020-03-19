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

import org.apache.daffodil.xml.XMLUtils
import junit.framework.Assert._
import org.apache.daffodil.util._
import org.junit.Test
import org.junit.Test

class TestMiddleEndAttributes {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testHasPriorRequiredSiblings {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" terminator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:sequenceKind="ordered">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    // Explore global element decl
    val Seq(e1) = sd.globalElementDecls
    val e1ct = e1.complexType
    val seq = e1ct.sequence
    val Seq(s1, s2) = seq.groupMembers
    assertTrue(s1.hasStaticallyRequiredOccurrencesInDataRepresentation)
    assertTrue(s2.hasStaticallyRequiredOccurrencesInDataRepresentation)
    assertTrue(s1.hasLaterRequiredSiblings)
    assertTrue(s2.hasPriorRequiredSiblings)

  }

  @Test def testDoesNotHavePriorRequiredSiblings {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format representation="text" occursCountKind="parsed" lengthUnits="bytes" encoding="US-ASCII" initiator="" terminator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:sequenceKind="ordered">
            <xs:element name="s1" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s2" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    // Explore global element decl
    val Seq(e1) = sd.globalElementDecls
    val e1ct = e1.complexType
    val seq = e1ct.sequence
    val Seq(s1, s2) = seq.groupMembers
    assertFalse(s1.hasStaticallyRequiredOccurrencesInDataRepresentation)
    assertFalse(s2.hasStaticallyRequiredOccurrencesInDataRepresentation)
    assertFalse(s1.hasLaterRequiredSiblings)
    assertFalse(s2.hasPriorRequiredSiblings)

  }

  @Test def testRequiredSiblings {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
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
      </xs:element>)

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

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
    assertTrue(s1.hasLaterRequiredSiblings)
    assertTrue(s2.hasLaterRequiredSiblings)
    assertTrue(s3.hasLaterRequiredSiblings)
    assertFalse(s4.hasLaterRequiredSiblings)
    assertFalse(s5.hasLaterRequiredSiblings)
    assertFalse(s1.hasPriorRequiredSiblings)
    assertFalse(s2.hasPriorRequiredSiblings)
    assertTrue(s3.hasPriorRequiredSiblings)
    assertTrue(s4.hasPriorRequiredSiblings)
    assertTrue(s5.hasPriorRequiredSiblings)
  }

  @Test def testStaticallyFirstWithChoice {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format representation="text" occursCountKind="parsed" lengthUnits="bytes" encoding="US-ASCII" initiator="" terminator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:sequenceKind="ordered" dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:choice>
              <xs:element name="s1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
              <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            </xs:choice>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    // Explore global element decl
    val Seq(e1) = sd.globalElementDecls
    val e1ct = e1.complexType
    val seq = e1ct.sequence
    val Seq(seqMem) = seq.groupMembers
    val cho = seqMem.asInstanceOf[Choice]
    val Seq(s1, s2) = cho.groupMembers
    val es = s1.optLexicalParent.get.optLexicalParent.get.asInstanceOf[Sequence]
    assertTrue(es.hasInfixSep)
    assertEquals(1, s1.position)
    assertTrue(s1.isScalar)
    assertTrue(!s1.hasPriorRequiredSiblings)
    val es2 = s2.optLexicalParent.get.optLexicalParent.get.asInstanceOf[Sequence]
    assertEquals(es, es2)
    assertEquals(2, s2.position)
    assertTrue(s2.isScalar)
    assertTrue(!s2.hasPriorRequiredSiblings)
  }

  @Test def testNearestEnclosingSequenceElementRef() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,

      <dfdl:format ref="tns:GeneralFormat" representation="text" occursCountKind="parsed" lengthUnits="bytes" encoding="US-ASCII" initiator="" terminator="" separator="" ignoreCase="no" initiatedContent="no"/>,

      <xs:element name="e1" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
      <xs:element name="e2" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:sequenceKind="ordered" dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:element ref="e1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    // Explore global element decl
    val Seq(_, e2) = sd.globalElementDecls
    val e2ct = e2.complexType
    val seq = e2ct.sequence
    val mems = seq.groupMembers
    val Seq(t1: Term) = mems
    val e1ref = t1.asInstanceOf[ElementRef]
    val Some(nes: Sequence) = e1ref.optLexicalParent
    assertEquals(seq, nes)
  }

  @Test def testImmediatelyEnclosingModelGroup1() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
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
      </xs:element>)

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

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
