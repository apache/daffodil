package daffodil.dsom

import daffodil.xml.XMLUtils
import scala.xml._
import org.scalatest.junit.JUnitSuite
import daffodil.schema.annotation.props.gen._
import daffodil.schema.annotation.props._
import junit.framework.Assert._
import daffodil.util._
import daffodil.compiler._
import org.junit.Test

class TestMiddleEndAttributes extends JUnitSuite {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testHasPriorRequiredSiblings {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" terminator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val compiler = Compiler()

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    // Explore global element decl
    val Seq(e1f) = sd.globalElementDecls
    val e1 = e1f.forRoot()
    val e1ct = e1.immediateType.get.asInstanceOf[LocalComplexTypeDef]
    val seq = e1ct.modelGroup.asInstanceOf[Sequence]
    val Seq(s1, s2) = seq.groupMembers
    assertTrue(s1.hasStaticallyRequiredInstances)
    assertTrue(s2.hasStaticallyRequiredInstances)
    assertTrue(s1.hasLaterRequiredSiblings)
    assertTrue(s2.hasPriorRequiredSiblings)

  }

  @Test def testDoesNotHavePriorRequiredSiblings {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format representation="text" occursCountKind="parsed" lengthUnits="bytes" encoding="US-ASCII" initiator="" terminator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s2" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val compiler = Compiler()

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    // Explore global element decl
    val Seq(e1f) = sd.globalElementDecls
    val e1 = e1f.forRoot()
    val e1ct = e1.immediateType.get.asInstanceOf[LocalComplexTypeDef]
    val seq = e1ct.modelGroup.asInstanceOf[Sequence]
    val Seq(s1, s2) = seq.groupMembers
    assertFalse(s1.hasStaticallyRequiredInstances)
    assertFalse(s2.hasStaticallyRequiredInstances)
    assertFalse(s1.hasLaterRequiredSiblings)
    assertFalse(s2.hasPriorRequiredSiblings)

  }

  @Test def testRequiredSiblings {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format representation="text" occursCountKind="parsed" lengthUnits="bytes" encoding="US-ASCII" initiator="" terminator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s3" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s4" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s5" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val compiler = Compiler()

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    // Explore global element decl
    val Seq(e1f) = sd.globalElementDecls
    val e1 = e1f.forRoot()
    val e1ct = e1.immediateType.get.asInstanceOf[LocalComplexTypeDef]
    val seq = e1ct.modelGroup.asInstanceOf[Sequence]
    val Seq(s1, s2, s3, s4, s5) = seq.groupMembers
    assertFalse(s1.hasStaticallyRequiredInstances)
    assertTrue(s2.hasStaticallyRequiredInstances)
    assertFalse(s3.hasStaticallyRequiredInstances)
    assertTrue(s4.hasStaticallyRequiredInstances)
    assertFalse(s5.hasStaticallyRequiredInstances)
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
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format representation="text" occursCountKind="parsed" lengthUnits="bytes" encoding="US-ASCII" initiator="" terminator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:choice>
              <xs:element name="s1" type="xs:int"    dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
              <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            </xs:choice>          
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val compiler = Compiler()

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    // Explore global element decl
    val Seq(e1f) = sd.globalElementDecls
    val e1 = e1f.forRoot()
    val e1ct = e1.immediateType.get.asInstanceOf[LocalComplexTypeDef]
    val seq = e1ct.modelGroup.asInstanceOf[Sequence]
    val Seq(seqMem) = seq.groupMembers
    val cho = seqMem.asInstanceOf[Choice]
    val Seq(s1, s2) = cho.groupMembers
    val es = s1.es
    assertTrue(es.hasInfixSep)
    assertEquals(1, s1.positionInNearestEnclosingSequence)
    assertTrue(s1.isScalar)
    assertTrue(!s1.hasPriorRequiredSiblings)
    val es2 = s2.es
    assertEquals(es, es2)
    assertEquals(1, s2.positionInNearestEnclosingSequence)
    assertTrue(s2.isScalar)
    assertTrue(!s2.hasPriorRequiredSiblings)
  }
  
  @Test def testNearestEnclosingSequenceElementRef () {
     val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format representation="text" occursCountKind="parsed" lengthUnits="bytes" encoding="US-ASCII" initiator="" terminator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
      <xs:element name="e2" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
              <xs:element ref="e1"/>      
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val compiler = Compiler()

    val sset = new SchemaSet(testSchema)
    val Seq(sch) = sset.schemas
    val Seq(sd) = sch.schemaDocuments

    // Explore global element decl
    val Seq(e1f, e2f) = sd.globalElementDecls
    val e2 = e2f.forRoot()
    val e2ct = e2.immediateType.get.asInstanceOf[daffodil.dsom.LocalComplexTypeDef]
    val seq = e2ct.modelGroup.asInstanceOf[Sequence]
    val mems = seq.groupMembers
    val Seq(t1 : Term) = mems
    val e1ref = t1.asInstanceOf[daffodil.dsom.ElementRef]
    val nes = e1ref.nearestEnclosingSequence
    nes match {
       case None => fail()
       case Some(nes) => {
           assertEquals(seq, nes)
       }
     }
  }
}