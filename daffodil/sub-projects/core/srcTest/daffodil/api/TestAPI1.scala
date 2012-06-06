package daffodil.api

import junit.framework.Assert._

import org.scalatest.junit.JUnit3Suite

import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
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

    val expected: Node = <e1>5678</e1>
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
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" dfdl:occursCountKind="fixed" minOccurs="3" maxOccurs="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "567")
    val actualString = actual.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains("><s1>5</s1><s1>6</s1><s1>7</s1></e1>"))
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  def testParseSequence3() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format separatorPolicy="required" separatorPosition="infix" ref="tns:daffodilTest1"/>,
      <xs:element name="e1">
        <xs:complexType>
          <xs:sequence dfdl:separator=",">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" dfdl:occursCountKind="fixed" minOccurs="3" maxOccurs="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "5,6,7")
    val actualString = actual.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains("><s1>5</s1><s1>6</s1><s1>7</s1></e1>"))
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  def testParseSequence4() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format separatorPolicy="required" separatorPosition="infix" ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:initiator="[" dfdl:terminator="]">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:initiator="{" dfdl:terminator="}">
            <xs:element name="s1" type="xs:string" dfdl:initiator="(" dfdl:terminator=")" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" dfdl:occursCountKind="fixed" minOccurs="3" maxOccurs="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "[{(5),(6),(7)}]")
    val actualString = actual.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains("><s1>5</s1><s1>6</s1><s1>7</s1></e1>"))
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  /**
   * Shows that delimiters longer than 1 character work.
   * Shows that simple expressions like 1 + 1 get evaluated to constants
   * Shows that initiator and terminator work on a sequence as well as on elements.
   */
  def testParseSequence5() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format separatorPolicy="required" separatorPosition="infix" ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:initiator="[[" dfdl:terminator="]]">
        <xs:complexType>
          <xs:sequence dfdl:separator=",," dfdl:initiator="{{" dfdl:terminator="}}">
            <xs:element name="s1" type="xs:string" dfdl:initiator="((" dfdl:terminator="))" dfdl:lengthKind="explicit" dfdl:length="{ 1 + 1 }" dfdl:occursCountKind="fixed" minOccurs="3" maxOccurs="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "[[{{((55)),,((66)),,((77))}}]]")
    val actualString = actual.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains("><s1>55</s1><s1>66</s1><s1>77</s1></e1>"))
    val expected = <e1><s1>55</s1><s1>66</s1><s1>77</s1></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  def testInt1() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="explicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s2" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 5 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)    
    val actual = Compiler.testString(sch, "55,000")
    TestUtils.assertEqualsXMLElements(<e1><s1>5</s1><s2>5000</s2></e1>, actual)
  }

  def testInt2() {
    val sch = TestUtils.dfdlTestSchema(
        <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="explicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
    <xs:element name="s2" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 5 }"/>
    </xs:sequence>
    </xs:complexType>
    </xs:element>)
    val e = intercept[Exception]{
      val actual = Compiler.testString(sch, "55.001")
    }
    //println("ERROR!!!!!" + e.getMessage())
    assertTrue(e.getMessage().contains("xs:int"))
  }

  def testShort1() {
    val sch = TestUtils.dfdlTestSchema(
        <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="explicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:short" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
    <xs:element name="s2" type="xs:short" dfdl:lengthKind="explicit" dfdl:length="{ 5 }"/>
    </xs:sequence>
    </xs:complexType>
    </xs:element>)
    val actual = Compiler.testString(sch, "55,000")
    TestUtils.assertEqualsXMLElements(<e1><s1>5</s1><s2>5000</s2></e1>, actual)
  }

  def testShort2() {
    val sch = TestUtils.dfdlTestSchema(
        <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="explicit">
        <xs:complexType>
          <xs:element name="s" type="xs:short" dfdl:lengthKind="explicit" dfdl:length="{ 5 }"/>
        </xs:complexType>
    </xs:element>)
    val e = intercept[Exception]{
      val actual = Compiler.testString(sch, "70,000")
    }
    //println("ERROR!!!!!" + e.getMessage())
    assertTrue(e.getMessage().contains("xs:short"))
  }

  def testLong1() {
    val sch = TestUtils.dfdlTestSchema(
        <dfdl:format ref="tns:daffodilTest1"/>,
          <xs:element name="s" type="xs:long" dfdl:lengthKind="explicit" dfdl:length="{ 13 }"/>)
    val actual = Compiler.testString(sch, "2,000,000,000")
    TestUtils.assertEqualsXMLElements(<s>2000000000</s>, actual)
  }

  def testByte1() {
    val sch = TestUtils.dfdlTestSchema(
        <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="explicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:byte" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>
    <xs:element name="s2" type="xs:byte" dfdl:lengthKind="explicit" dfdl:length="{ 3 }"/>
    </xs:sequence>
    </xs:complexType>
    </xs:element>)
    val actual = Compiler.testString(sch, "55123")
    TestUtils.assertEqualsXMLElements(<e1><s1>55</s1><s2>123</s2></e1>, actual)
  }

  def testNumber1() {
    val sch = TestUtils.dfdlTestSchema(
        <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="explicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="country" type="xs:byte" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="area" type="xs:short" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
            <xs:element name="region" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
            <xs:element name="number" type="xs:long" dfdl:lengthKind="explicit" dfdl:length="{ 5 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "1-800-555-1212")
    TestUtils.assertEqualsXMLElements(<e1><country>1</country><area>-800</area><region>-555</region><number>-1212</number></e1>, actual)
  }

  def testNumber2() {
    val sch = TestUtils.dfdlTestSchema(
        <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="mersenne" type="xs:byte" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>)
    val actual = Compiler.testString(sch, "-127")
    TestUtils.assertEqualsXMLElements(<mersenne>-127</mersenne>, actual)
  }

  def testNumber3() {
    val sch = TestUtils.dfdlTestSchema(
        <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="perfect" type="xs:byte" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>)
    val actual = Compiler.testString(sch, "+3")
    TestUtils.assertEqualsXMLElements(<perfect>3</perfect>, actual)
  }

  def testBigInteger1() {
    val sch = TestUtils.dfdlTestSchema(
        <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="mersenne" type="xs:integer" dfdl:lengthKind="explicit" dfdl:length="{ 207 }"/>)
    val actual = Compiler.testString(sch, "686,479,766,013,060,971,498,190,079,908,139,321,726,943,530,014,330,540,939,446,345,918,554,318,339,765,605,212,255,964,066,145,455,497,729,631,139,148,085,803,712,198,799,971,664,381,257,402,829,111,505,715")
    TestUtils.assertEqualsXMLElements(<mersenne>686479766013060971498190079908139321726943530014330540939446345918554318339765605212255964066145455497729631139148085803712198799971664381257402829111505715</mersenne>, actual)
  }

  // TEST FAILS - SEE JIRA DFDL-184
//  def testIntTooLong() {
//    val sch = TestUtils.dfdlTestSchema(
//      <dfdl:format ref="tns:daffodilTest1"/>,
//      <xs:element name="e1" dfdl:lengthKind="explicit">
//        <xs:complexType>
//          <xs:sequence>
//            <xs:element name="s1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 20 }"/>
//          </xs:sequence>
//        </xs:complexType>
//      </xs:element>)    
//       val e = intercept[Exception]{
//      val actual = Compiler.testString(sch, "55555555555555555555")
//    }
//    //println("ERROR!!!!!" + e.getMessage())
//    assertTrue(e.getMessage().contains("xs:int"))
//  }

  def testParseSequenceInt() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format separatorPolicy="required" separatorPosition="infix" ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:initiator="[[" dfdl:terminator="]]">
        <xs:complexType>
          <xs:sequence dfdl:separator=",," dfdl:initiator="{{" dfdl:terminator="}}">
            <xs:element name="s1" type="xs:int" dfdl:initiator="((" dfdl:terminator="))" dfdl:lengthKind="explicit" dfdl:length="{ 1 + 1 }" dfdl:occursCountKind="fixed" minOccurs="3" maxOccurs="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "[[{{((55)),,((66)),,((77))}}]]")
    val actualString = actual.toString
    assertTrue(actualString.contains("<e1")) // there might be xsi:type stuff in the tag, and namespace stuff
    assertTrue(actualString.contains("><s1>55</s1><s1>66</s1><s1>77</s1></e1>"))
    val expected = <e1><s1>55</s1><s1>66</s1><s1>77</s1></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }
  
  def testBadInt() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>)
    val e = intercept[Exception]{
      val actual = Compiler.testString(sch, "A")
    }
    //println("ERROR!!!!!" + e.getMessage())
    assertTrue(e.getMessage().contains("xs:int"))
  }
  
  def testParseOccursCountKindOfParsed() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" minOccurs="0" dfdl:occursCountKind="parsed"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "5678A")
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1><s1>8</s1><s2>A</s2></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  def testParseOccursCountKindOfParsedWithTerminator() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "5;6;7;8;A")
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1><s1>8</s1><s2>A</s2></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }
  
  def testBinaryInts() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format representation="binary" byteOrder="bigEndian" binaryNumberRep="binary" ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="implicit"  minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:int" dfdl:byteOrder="littleEndian" dfdl:lengthKind="implicit" />
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testBinary(sch, "000000013bFFFFFFFF3b080402013b000000003bFFFFFF7F")
    val expected = <e1><s1>1</s1><s1>-1</s1><s1>134480385</s1><s1>0</s1><s2>2147483647</s2></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)    
  }
  
  def testBinaryDoubles() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format representation="binary" byteOrder="bigEndian" binaryNumberRep="binary" binaryFloatRep="ieee" ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:double" dfdl:lengthKind="implicit"  minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:double" dfdl:byteOrder="littleEndian" dfdl:lengthKind="implicit" />
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testBinary(sch, "3FF00000000000003bBFF00000000000003b08040201080402013b00000000000000003bFFFFFFFFFFFFFF7F")
    val expected = <e1><s1>1.0</s1><s1>-1.0</s1><s1>4.7340609871421765E-270</s1><s1>0.0</s1><s2>NaN</s2></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  def testTextDoubles() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="explicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:double" dfdl:lengthKind="explicit" dfdl:length="{ 4 }" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:double" dfdl:lengthKind="explicit" dfdl:length="{ 8 }"/>
            <xs:element name="s3" type="xs:double" dfdl:lengthKind="explicit" dfdl:length="{ 7 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "01.0;-1.0;4.15;0.31;8.6E-2001,234.9")
    val expected = <e1><s1>1.0</s1><s1>-1.0</s1><s1>4.15</s1><s1>0.31</s1><s2>8.6E-200</s2><s3>1234.9</s3></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  def testBinaryFloats() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format representation="binary" byteOrder="bigEndian" binaryNumberRep="binary" binaryFloatRep="ieee" ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:float" dfdl:lengthKind="implicit"  minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:float" dfdl:byteOrder="littleEndian" dfdl:lengthKind="implicit" />
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testBinary(sch, "3F8000003bBF8000003b080402013b000000003b0000C07F")
    val expected = <e1><s1>1.0</s1><s1>-1.0</s1><s1>3.972466E-34</s1><s1>0.0</s1><s2>NaN</s2></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  def testTextFloats() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="explicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:float" dfdl:lengthKind="explicit" dfdl:length="{ 4 }" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:float" dfdl:lengthKind="explicit" dfdl:length="{ 6 }"/>
            <xs:element name="s3" type="xs:float" dfdl:lengthKind="explicit" dfdl:length="{ 7 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "01.0;-1.0;4.15;0.31;-7.1E81,234.9")
    val expected = <e1><s1>1.0</s1><s1>-1.0</s1><s1>4.15</s1><s1>0.31</s1><s2>-7.1E8</s2><s3>1234.9</s3></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

}

