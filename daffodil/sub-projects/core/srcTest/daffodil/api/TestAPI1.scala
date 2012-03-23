package daffodil.api

import junit.framework.Assert._

import org.scalatest.junit.JUnit3Suite

import scala.xml._
import daffodil.xml.XMLUtil
import daffodil.xml.XMLUtil._
import daffodil.dsom.Compiler
import daffodil.util._

class TestDFDLParser extends JUnit3Suite {

  def testParseSimple1() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>)
    val actual = Compiler.testString(sch, "5678")
    val actualString = actual.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains(">5678</e1>"))

    val expected : Node = <e1>5678</e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  def testParseSequenceOfJustOneScalar() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "5")
    val actualString = actual.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains("><s1>5</s1></e1>"))
    val expected = <e1><s1>5</s1></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  def testParseSequence1() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "56")
    val actualString = actual.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains("><s1>5</s1><s2>6</s2></e1>"))
    val expected = <e1><s1>5</s1><s2>6</s2></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  def testParseSequence2() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" dfdl:occursCountKind="fixed" minOccurs="2" maxOccurs="2"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "56")
    val actualString = actual.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains("><s1>5</s1><s1>6</s1></e1>"))
    val expected = <e1><s1>5</s1><s1>6</s1></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }
}