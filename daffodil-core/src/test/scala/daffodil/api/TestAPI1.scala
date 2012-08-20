package daffodil.api

import junit.framework.Assert._

import org.scalatest.junit.JUnit3Suite

import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._

class TestDFDLParser extends JUnit3Suite {

  def testParseSimple1() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>)
    val actual = Compiler.testString(sch, "5678").result
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
    val actual = Compiler.testString(sch, "5").result
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
    val actual = Compiler.testString(sch, "56").result
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
    val actual = Compiler.testString(sch, "567").result
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
    val actual = Compiler.testString(sch, "5,6,7").result
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
    val actual = Compiler.testString(sch, "[{(5),(6),(7)}]").result
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
      <xs:element name="e1" dfdl:initiator="[more[" dfdl:terminator="]nomore]">
        <xs:complexType>
          <xs:sequence dfdl:separator=",," dfdl:initiator="{{" dfdl:terminator="}}">
            <xs:element name="s1" type="xs:string" dfdl:initiator="((" dfdl:terminator="))" dfdl:lengthKind="explicit" dfdl:length="{ 1 + 1 }" dfdl:occursCountKind="fixed" minOccurs="3" maxOccurs="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "[more[{{((55)),,((66)),,((77))}}]nomore]").result
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
    val actual = Compiler.testString(sch, "55,000").result
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
    // println("ERROR!!!!!" + e.getMessage())
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
    val actual = Compiler.testString(sch, "55,000").result
    TestUtils.assertEqualsXMLElements(<e1><s1>5</s1><s2>5000</s2></e1>, actual)
  }

  def testShort2() {
    val sch = TestUtils.dfdlTestSchema(
        <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="explicit">
        <xs:complexType>
          <xs:sequence>
          <xs:element name="s" type="xs:short" dfdl:lengthKind="explicit" dfdl:length="{ 6 }"/>
          </xs:sequence>
        </xs:complexType>
    </xs:element>)
    val e = intercept[Exception]{
      val actual = Compiler.testString(sch, "70,000")
    }
    // println("ERROR!!!!!" + e.getMessage())
    assertTrue(e.getMessage().contains("xs:short"))
  }

  def testLong1() {
    val sch = TestUtils.dfdlTestSchema(
        <dfdl:format ref="tns:daffodilTest1"/>,
          <xs:element name="s" type="xs:long" dfdl:lengthKind="explicit" dfdl:length="{ 13 }"/>)
    val actual = Compiler.testString(sch, "2,000,000,000").result
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
    val actual = Compiler.testString(sch, "55123").result
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
    val actual = Compiler.testString(sch, "1-800-555-1212").result
    TestUtils.assertEqualsXMLElements(<e1><country>1</country><area>-800</area><region>-555</region><number>-1212</number></e1>, actual)
  }

  def testNumber2() {
    val sch = TestUtils.dfdlTestSchema(
        <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="mersenne" type="xs:byte" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>)
    val actual = Compiler.testString(sch, "-127").result
    TestUtils.assertEqualsXMLElements(<mersenne>-127</mersenne>, actual)
  }

  def testNumber3() {
    val sch = TestUtils.dfdlTestSchema(
        <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="perfect" type="xs:byte" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>)
    val actual = Compiler.testString(sch, "+3").result
    TestUtils.assertEqualsXMLElements(<perfect>3</perfect>, actual)
  }

  def testUnsignedNumbers1() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format separatorPolicy="required" separatorPosition="infix" ref="tns:daffodilTest1"/>,
      <xs:element name="limits" dfdl:lengthKind="explicit">
        <xs:complexType>
          <xs:sequence dfdl:separator=":">
            <xs:element name="unsigned-byte" type="xs:unsignedByte" dfdl:lengthKind="explicit" dfdl:length="{ 3 }"/>
            <xs:element name="unsigned-short" type="xs:unsignedShort" dfdl:lengthKind="explicit" dfdl:length="{ 6 }"/>
            <xs:element name="unsigned-int" type="xs:unsignedInt" dfdl:lengthKind="explicit" dfdl:length="{ 13 }"/>
            <xs:element name="unsigned-long" type="xs:unsignedLong" dfdl:lengthKind="explicit" dfdl:length="{ 26 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "255:65,535:4,294,967,295:18,446,744,073,709,551,615").result
    TestUtils.assertEqualsXMLElements(<limits><unsigned-byte>255</unsigned-byte><unsigned-short>65535</unsigned-short><unsigned-int>4294967295</unsigned-int><unsigned-long>18446744073709551615</unsigned-long></limits>, actual)
  }

  def testUnsignedLong1() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="needs-square-root" type="xs:unsignedLong" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>)
    val e = intercept[Exception]{
      val actual = Compiler.testString(sch, "-3")
    }
    // println("ERROR!!!!!" + e.getMessage())
    assertTrue(e.getMessage().contains("xs:unsignedLong"))
  }

  def testUnsignedInt1() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="world-population" type="xs:unsignedInt" dfdl:lengthKind="explicit" dfdl:length="{ 13 }"/>)
    val e = intercept[Exception]{
      // As of 18:31 UTC (EST+5) Jun 8, 2012
      val actual = Compiler.testString(sch, "7,018,631,476")
    }
    // println("ERROR!!!!!" + e.getMessage())
    assertTrue(e.getMessage().contains("xs:unsignedInt"))
  }

  def testUnsignedShort1() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="last-element-indicator" type="xs:unsignedShort" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>)
    val e = intercept[Exception]{
      val actual = Compiler.testString(sch, "-1")
    }
    // println("ERROR!!!!!" + e.getMessage())
    assertTrue(e.getMessage().contains("xs:unsignedShort"))
  }

  def testUnsignedByte1() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bits" type="xs:unsignedByte" dfdl:lengthKind="explicit" dfdl:length="{ 3 }"/>)
    val e = intercept[Exception]{
      val actual = Compiler.testString(sch, "256")
    }
    // println("ERROR!!!!!" + e.getMessage())
    assertTrue(e.getMessage().contains("xs:unsignedByte"))
  }

  def testBigInteger1() {
    val sch = TestUtils.dfdlTestSchema(
        <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="mersenne" type="xs:integer" dfdl:lengthKind="explicit" dfdl:length="{ 207 }"/>)
    val actual = Compiler.testString(sch, "686,479,766,013,060,971,498,190,079,908,139,321,726,943,530,014,330,540,939,446,345,918,554,318,339,765,605,212,255,964,066,145,455,497,729,631,139,148,085,803,712,198,799,971,664,381,257,402,829,111,505,715").result
    TestUtils.assertEqualsXMLElements(<mersenne>686479766013060971498190079908139321726943530014330540939446345918554318339765605212255964066145455497729631139148085803712198799971664381257402829111505715</mersenne>, actual)
  }

  def testLengthKindPattern() {
    val sch = TestUtils.dfdlTestSchema(
        <dfdl:format ref="tns:daffodilTest1" separatorPolicy="required" separatorPosition="infix"/>,
      <xs:element name="doctors">
        <xs:complexType>
          <xs:sequence dfdl:separator=",">
              <xs:element name="name" type="xs:string" dfdl:lengthKind="pattern" dfdl:lengthPattern=".*?[^\\](?=,|$)" dfdl:occursCountKind="fixed" minOccurs="11" maxOccurs="11"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, """Hartnell\, William,Troughton\, Patrick,Pertwee\, Jon,Baker\, Tom,Davison\, Peter,Baker\, Colin,McCoy\, Sylvester,McGann\, Paul,Christopher Eccleston,David Tennant,Matt Smith""").result
    TestUtils.assertEqualsXMLElements(<doctors><name>Hartnell\, William</name><name>Troughton\, Patrick</name><name>Pertwee\, Jon</name><name>Baker\, Tom</name><name>Davison\, Peter</name><name>Baker\, Colin</name><name>McCoy\, Sylvester</name><name>McGann\, Paul</name><name>Christopher Eccleston</name><name>David Tennant</name><name>Matt Smith</name></doctors>, actual)
  }

  def testLengthKindPatternCompound() {
    // LoggingDefaults.setLoggingLevel(LogLevel.Debug)
    val sch = TestUtils.dfdlTestSchema(
        <dfdl:format ref="tns:daffodilTest1" separatorPolicy="required" separatorPosition="infix"/>,
        <xs:element name="abc">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="nul" dfdl:lengthKind="explicit" type="xs:int" dfdl:length="{1}"/>
              <xs:element name="ab" dfdl:lengthKind="pattern" dfdl:lengthPattern=".*?//">
                <xs:complexType>
                  <xs:sequence dfdl:separator=",">
                      <xs:element name="name" type="xs:string" dfdl:lengthKind="delimited"/>
                      <xs:element name="item" type="xs:string" dfdl:lengthKind="delimited"/>
                  </xs:sequence>
                </xs:complexType>
              </xs:element>
            <xs:element name="c" dfdl:lengthKind="explicit" type="xs:int" dfdl:length="{1}"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>)
    val actual = Compiler.testString(sch, """0pqr,bb//5""").result
    TestUtils.assertEqualsXMLElements(<abc><nul>0</nul><ab><name>pqr</name><item>bb//</item></ab><c>5</c></abc>, actual)
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
//    //println("ERROR!!!!!" + e.getMessage())//    assertTrue(e.getMessage().contains("xs:int"))
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
    val actual = Compiler.testString(sch, "[[{{((55)),,((66)),,((77))}}]]").result
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
      val actual = Compiler.testString(sch, "A").result
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
    val actual = Compiler.testString(sch, "5678A").result
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
    val actual = Compiler.testString(sch, "5;6;7;8;A").result
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1><s1>8</s1><s2>A</s2></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }
  
  def testParseOccursCountKindOfParsedDelimitedByTerminator() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited" dfdl:terminator="."/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "5;6;7;8;A.").result
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1><s1>8</s1><s2>A</s2></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }
  
  def testParseOccursCountKindOfParsedDelimitedByTerminator2() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit" dfdl:terminator=".">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "5;6;7;8;.").result
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1><s1>8</s1></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }
  
  def testParseOccursCountKindOfParsedDelimitedBySeparator() {
    val sch = TestUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit" dfdl:terminator=".">
        <xs:complexType>
          <xs:sequence dfdl:separator=";" dfdl:terminator=";">
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="delimited" minOccurs="0" dfdl:occursCountKind="parsed"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = Compiler.testString(sch, "5;6;7;8;.").result
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1><s1>8</s1></e1>
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
    val actual = Compiler.testBinary(sch, "000000013bFFFFFFFF3b080402013b000000003bFFFFFF7F").result
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
    val actual = Compiler.testBinary(sch, "3FF00000000000003bBFF00000000000003b08040201080402013b00000000000000003bFFFFFFFFFFFFFF7F").result
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
    val actual = Compiler.testString(sch, "01.0;-1.0;4.15;0.31;8.6E-2001,234.9").result
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
    val actual = Compiler.testBinary(sch, "3F8000003bBF8000003b080402013b000000003b0000C07F").result
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
    val actual = Compiler.testString(sch, "01.0;-1.0;4.15;0.31;-7.1E81,234.9").result
    val expected = <e1><s1>1.0</s1><s1>-1.0</s1><s1>4.15</s1><s1>0.31</s1><s2>-7.1E8</s2><s3>1234.9</s3></e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

}

