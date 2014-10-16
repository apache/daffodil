package edu.illinois.ncsa.daffodil.dpath

import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.util.SchemaUtils
import edu.illinois.ncsa.daffodil.util.TestUtils

class TestDPath {

  val testSchemaNoRef = SchemaUtils.dfdlTestSchemaUnqualified(
    <dfdl:format ref="tns:daffodilTest1" representation="binary" binaryNumberRep="binary" lengthUnits="bytes"/>,
    <xs:element name="a">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="b" type="xs:unsignedInt" dfdl:inputValueCalc="{ 4 }"/>
          <xs:element name="c">
            <xs:complexType>
              <xs:sequence>
                <xs:element name="d" type="xs:int" dfdl:inputValueCalc="{ ../../b }"/>
              </xs:sequence>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
    </xs:element>)

  @Test def test_twoUpwardSteps() = {
    val actual = TestUtils.testString(testSchemaNoRef, "")
  }

  val testSchema = SchemaUtils.dfdlTestSchemaUnqualified(
    <dfdl:format ref="tns:daffodilTest1" representation="binary" binaryNumberRep="binary" lengthUnits="bytes"/>,
    <xs:element name="a">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="b" type="xs:unsignedInt" dfdl:inputValueCalc="{ 4 }"/>
          <xs:element ref="tns:c"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>
    <xs:element name="c">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="d" type="xs:int" dfdl:inputValueCalc="{ ../../b }"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>)

  @Test def test_twoUpwardStepsAcrossElementReference() = {
    val actual = TestUtils.testString(testSchema, "")
  }

  val testSchema2 = SchemaUtils.dfdlTestSchemaUnqualified(
    <dfdl:format ref="tns:daffodilTest1" representation="binary" binaryNumberRep="binary" lengthUnits="bytes"/>,
    <xs:element name="a">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="b" type="xs:unsignedInt" dfdl:inputValueCalc="{ 4 }"/>
          <xs:element ref="tns:c"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>
    <xs:element name="c" type="xs:int" dfdl:inputValueCalc="{ ../b }"/>)

  @Test def test_oneUpwardStepsAcrossElementReference() = {
    val actual = TestUtils.testString(testSchema2, "")
  }

}