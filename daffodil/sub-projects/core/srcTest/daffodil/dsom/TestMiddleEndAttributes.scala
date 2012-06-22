package daffodil.dsom

import daffodil.xml.XMLUtils
import scala.xml._

import org.scalatest.junit.JUnit3Suite

import daffodil.schema.annotation.props.gen._
import daffodil.schema.annotation.props._
import junit.framework.Assert._
import daffodil.util._

class TestMiddleEndAttributes extends JUnit3Suite {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  def testHasPriorRequiredSiblings {
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

  def testDoesNotHavePriorRequiredSiblings {
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

  def testRequiredSiblings {
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
}