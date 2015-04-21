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

  /**
   * Test emphasis on delimiter unparsers
   */
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

  /**
   * Test emphasis on StringDelimitedUnparser
   */
  @Test def testUnparse3() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes" outputNewLine="%CR;%LF;"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:initiator="[" dfdl:separator="," dfdl:terminator="]">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example }><s1>1</s1><s2>2</s2></ex:e1>
    TestUtils.testUnparsing(sch, infoset, "[1,2]")
  }

  /**
   * Test emphasis on StringDelimitedUnparser w/ padding
   */
  @Test def testUnparse4() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes" outputNewLine="%CR;%LF;"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit" dfdl:initiator="!#" dfdl:terminator="#!">
        <xs:complexType>
          <xs:sequence dfdl:separator=",">
            <xs:element name="s1" type="st" dfdl:lengthKind="implicit" dfdl:textStringPadCharacter="_" dfdl:textPadKind="padChar" dfdl:textStringJustification="left"/>
            <xs:element name="s2" type="st" dfdl:lengthKind="implicit" dfdl:textStringPadCharacter="_" dfdl:textPadKind="padChar" dfdl:textStringJustification="right"/>
            <xs:element name="s3" type="st" dfdl:lengthKind="implicit" dfdl:textStringPadCharacter="_" dfdl:textPadKind="padChar" dfdl:textStringJustification="center"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:simpleType name="st">
        <xs:restriction base="xs:string">
          <xs:maxLength value="3"/>
          <xs:minLength value="3"/>
        </xs:restriction>
      </xs:simpleType>, elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example }><s1>1</s1><s2>2</s2><s3>3</s3></ex:e1>
    TestUtils.testUnparsing(sch, infoset, "!#1__,__2,_3_#!")
  }

  /**
   * Test emphasis on StringDelimitedUnparser w/ escape scheme
   */
  @Test def testUnparse5() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes" outputNewLine="%CR;%LF;"/>
      <dfdl:defineEscapeScheme name="pound">
        <dfdl:escapeScheme escapeCharacter='#' escapeKind="escapeCharacter" escapeEscapeCharacter="" extraEscapedCharacters="" generateEscapeBlock="whenNeeded"/>
      </dfdl:defineEscapeScheme>
      <dfdl:defineEscapeScheme name='cStyleComment'>
        <dfdl:escapeScheme escapeBlockStart='/*' escapeBlockEnd='*/' escapeKind="escapeBlock" escapeEscapeCharacter="#" generateEscapeBlock="whenNeeded" extraEscapedCharacters=""/>
      </dfdl:defineEscapeScheme>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator=",">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited" dfdl:escapeSchemeRef="pound"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited" dfdl:escapeSchemeRef="cStyleComment"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>, elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example }><s1>one, two</s1><s2>, three and four*/</s2></ex:e1>
    TestUtils.testUnparsing(sch, infoset, "one#, two,/*, three and four#*/*/")
  }

}
