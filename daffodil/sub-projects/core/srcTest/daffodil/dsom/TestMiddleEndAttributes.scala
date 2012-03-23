package daffodil.dsom

import daffodil.xml.XMLUtil
import scala.xml._

import org.scalatest.junit.JUnit3Suite

import daffodil.schema.annotation.props.gen._
import daffodil.schema.annotation.props._
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import daffodil.util._


class TestMiddleEndAttributes extends JUnit3Suite {
  val xsd = XMLUtil.XSD_NAMESPACE
  val dfdl = XMLUtil.DFDL_NAMESPACE
  val xsi = XMLUtil.XSI_NAMESPACE
  val example = XMLUtil.EXAMPLE_NAMESPACE


  def testHasPriorRequiredSiblings {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" terminator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" />
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" />
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
}