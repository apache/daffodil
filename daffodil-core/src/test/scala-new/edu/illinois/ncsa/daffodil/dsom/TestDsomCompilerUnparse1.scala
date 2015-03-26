package edu.illinois.ncsa.daffodil.dsom

import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import org.junit.Test
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.compiler._

class TestDsomCompilerUnparse1 {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testUnparse1() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example }><s1>1</s1><s2>2</s2></ex:e1>
    TestUtils.testUnparsing(sch, infoset, "12")
  }
  
  @Test def testUnparse2() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes" outputNewLine="%CR;%LF;"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:initiator="[" dfdl:separator="," dfdl:terminator="]">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example }><s1>1</s1><s2>2</s2></ex:e1>
    TestUtils.testUnparsing(sch, infoset, "[1,2]")
  }

}