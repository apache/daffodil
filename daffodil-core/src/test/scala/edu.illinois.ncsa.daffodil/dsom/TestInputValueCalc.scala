package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.compiler._
import org.scalatest.junit.JUnitSuite
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import org.junit.Test
import scala.xml.Node

class TestInputValueCalc extends JUnitSuite with Logging {

  // @Test
  @Test def testInputValueCalc1() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="data" type="xs:string" dfdl:textNumberRep="standard" dfdl:representation="text" dfdl:terminator="" dfdl:emptyValueDelimiterPolicy="none" dfdl:inputValueCalc="{ 42 }" dfdl:initiator="" dfdl:lengthKind="explicit" dfdl:length="1"/>)
    val actual = Compiler.testString(testSchema, "")
    val actualString = actual.result.toString
    val expected: Node = <data>42</data>
    TestUtils.assertEqualsXMLElements(expected, actual.result)
  }

  // @Test
  @Test def testInputValueCalcString2() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="data">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="e2" type="xs:string" dfdl:inputValueCalc="{ ../tns:e1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val actual = Compiler.testString(testSchema, "A")
    val actualString = actual.result.toString
     val expected: Node = <data><e1>A</e1><e2>A</e2></data>
    TestUtils.assertEqualsXMLElements(expected, actual.result)
  }

  // @Test
  @Test def testInputValueCalcInt3() {
    val testSchema = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="data">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="e1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="e2" type="xs:int" dfdl:inputValueCalc="{ ../tns:e1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    val actual = Compiler.testString(testSchema, "8")
    val actualString = actual.result.toString
    val expected: Node = <data><e1>8</e1><e2>8</e2></data>
    TestUtils.assertEqualsXMLElements(expected, actual.result)
  }
}