/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.core.iapi

import scala.xml._

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.util._
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Assert._
import org.junit.Test

class TestDFDLParser {

  @Test def testParseSimple1(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
    )
    val (_, actual) = TestUtils.testString(sch, "5678")
    val expected: Node = <e1>5678</e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testParseSequenceOfJustOneScalar(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val (_, actual) = TestUtils.testString(sch, "5")
    val expected = <e1><s1>5</s1></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testParseSequence1(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val (_, actual) = TestUtils.testString(sch, "56")
    val expected = <e1><s1>5</s1><s2>6</s2></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testParseSequence2(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" dfdl:occursCountKind="fixed" minOccurs="3" maxOccurs="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val (_, actual) = TestUtils.testString(sch, "567")
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testParseSequence3(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format separatorSuppressionPolicy="never" separatorPosition="infix" ref="tns:GeneralFormat"/>,
      <xs:element name="e1">
        <xs:complexType>
          <xs:sequence dfdl:separator=",">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" dfdl:occursCountKind="fixed" minOccurs="3" maxOccurs="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val (_, actual) = TestUtils.testString(sch, "5,6,7")
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testInt1(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s2" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 5 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val (_, actual) = TestUtils.testString(sch, "55,000")
    XMLUtils.compareAndReport(<e1><s1>5</s1><s2>5000</s2></e1>, actual)
  }

  @Test def testInt2(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s2" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 5 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val e = intercept[Exception] {
      TestUtils.testString(sch, "55.001")
    }
    assertTrue(e.getMessage().contains("xs:int"))
  }

  @Test def testShort1(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:short" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="s2" type="xs:short" dfdl:lengthKind="explicit" dfdl:length="{ 5 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val (_, actual) = TestUtils.testString(sch, "55,000")
    XMLUtils.compareAndReport(<e1><s1>5</s1><s2>5000</s2></e1>, actual)
  }

  @Test def testShort2(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s" type="xs:short" dfdl:lengthKind="explicit" dfdl:length="{ 6 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val e = intercept[Exception] {
      TestUtils.testString(sch, "70,000")
    }
    assertTrue(e.getMessage().contains("short"))
  }

  @Test def testByte1(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:byte" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>
            <xs:element name="s2" type="xs:byte" dfdl:lengthKind="explicit" dfdl:length="{ 3 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val (_, actual) = TestUtils.testString(sch, "55123")
    XMLUtils.compareAndReport(<e1><s1>55</s1><s2>123</s2></e1>, actual)
  }

  @Test def testNumber1(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="country" type="xs:byte" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
            <xs:element name="area" type="xs:short" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
            <xs:element name="region" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
            <xs:element name="number" type="xs:long" dfdl:lengthKind="explicit" dfdl:length="{ 5 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val (_, actual) = TestUtils.testString(sch, "1-800-555-1212")
    XMLUtils.compareAndReport(
      <e1><country>1</country><area>-800</area><region>-555</region><number>-1212</number></e1>,
      actual
    )
  }

  @Test def testNumber2(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="mersenne" type="xs:byte" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
    )
    val (_, actual) = TestUtils.testString(sch, "-127")
    XMLUtils.compareAndReport(<mersenne>-127</mersenne>, actual)
  }

  @Test def testNumber3(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="perfect" type="xs:byte" dfdl:textNumberPattern="+0" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>
    )
    val (_, actual) = TestUtils.testString(sch, "+3")
    XMLUtils.compareAndReport(<perfect>3</perfect>, actual)
  }

  @Test def testUnsignedLong1(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="needs-square-root" type="xs:unsignedLong" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>
    )
    val e = intercept[Exception] {
      TestUtils.testString(sch, "-3")
    }
    assertTrue(e.getMessage().contains("unsignedLong"))
  }

  @Test def testUnsignedInt1(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="world-population" type="xs:unsignedInt" dfdl:lengthKind="explicit" dfdl:length="{ 13 }"/>
    )
    val e = intercept[Exception] {
      // As of 18:31 UTC (EST+5) Jun 8, 2012
      TestUtils.testString(sch, "7,018,631,476")
    }
    assertTrue(e.getMessage().contains("unsignedInt"))
  }

  @Test def testUnsignedShort1(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="last-element-indicator" type="xs:unsignedShort" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>
    )
    val e = intercept[Exception] {
      TestUtils.testString(sch, "-1")
    }
    assertTrue(e.getMessage().contains("unsignedShort"))
  }

  @Test def testUnsignedByte1(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="bits" type="xs:unsignedByte" dfdl:lengthKind="explicit" dfdl:length="{ 3 }"/>
    )
    val e = intercept[Exception] {
      TestUtils.testString(sch, "256")
    }
    assertTrue(e.getMessage().contains("unsignedByte"))
  }

  @Test def testIntTooLong(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 20 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val e = intercept[Exception] {
      TestUtils.testString(sch, "55555555555555555555")
    }
    assertTrue(e.getMessage().contains("int"))
  }

  @Test def testParseSequenceInt(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format separatorSuppressionPolicy="never" separatorPosition="infix" ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:initiator="[[" dfdl:terminator="]]">
        <xs:complexType>
          <xs:sequence dfdl:separator=",," dfdl:initiator="{{{" dfdl:terminator="}}">
            <xs:element name="s1" type="xs:int" dfdl:initiator="((" dfdl:terminator="))" dfdl:lengthKind="explicit" dfdl:length="{ 1 + 1 }" dfdl:occursCountKind="fixed" minOccurs="3" maxOccurs="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val (_, actual) = TestUtils.testString(sch, "[[{{((55)),,((66)),,((77))}}]]")
    val expected = <e1><s1>55</s1><s1>66</s1><s1>77</s1></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testParseSequenceInt3Operands(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format separatorSuppressionPolicy="never" separatorPosition="infix" ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:initiator="[[" dfdl:terminator="]]">
        <xs:complexType>
          <xs:sequence dfdl:separator=",," dfdl:initiator="{{{" dfdl:terminator="}}">
            <xs:element name="s1" type="xs:int" dfdl:initiator="((" dfdl:terminator="))" dfdl:lengthKind="explicit" dfdl:length="{ 1 + 0 + 1 }" dfdl:occursCountKind="fixed" minOccurs="3" maxOccurs="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val (_, actual) = TestUtils.testString(sch, "[[{{((55)),,((66)),,((77))}}]]")
    val expected = <e1><s1>55</s1><s1>66</s1><s1>77</s1></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testBadInt(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
    )
    val e = intercept[Exception] {
      TestUtils.testString(sch, "A")
    }
    assertTrue(e.getMessage().contains("xs:int"))
  }

  @Test def testParseOccursCountKindOfParsed(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" minOccurs="0" dfdl:occursCountKind="parsed"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val (_, actual) = TestUtils.testString(sch, "5678A")
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1><s1>8</s1><s2>A</s2></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testParseOccursCountKindOfParsedWithTerminator(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val (_, actual) = TestUtils.testString(sch, "5;6;7;8;A")
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1><s1>8</s1><s2>A</s2></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testParseOccursCountKindOfParsedDelimitedByTerminator(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited" dfdl:terminator="."/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val areTracing = false
    val (_, actual) = TestUtils.testString(sch, "5;6;7;8;A.", areTracing)
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1><s1>8</s1><s2>A</s2></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testParseOccursCountKindOfParsedDelimitedByTerminator2(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit" dfdl:terminator=".">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val areTracing = false
    val (_, actual) = TestUtils.testString(sch, "5;6;7;8;.", areTracing)
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1><s1>8</s1></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testParseOccursCountKindOfParsedDelimitedBySeparator(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit" dfdl:terminator=".">
        <xs:complexType>
          <xs:sequence dfdl:separator=";" dfdl:terminator=";">
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="delimited" minOccurs="0" dfdl:occursCountKind="implicit" maxOccurs="4"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val areTracing = false
    val (_, actual) = TestUtils.testString(sch, "5;6;7;8;.", areTracing)
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1><s1>8</s1></e1>

    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testParseOccursCountKindOfParsedDelimitedBySeparator3(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit" dfdl:terminator=".">
        <xs:complexType>
          <xs:sequence dfdl:separator=";" dfdl:terminator="!">
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="delimited" minOccurs="0" dfdl:occursCountKind="parsed"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val areTracing = false
    val (_, actual) = TestUtils.testString(sch, "5;6;7;8!.", areTracing)
    val expected = <e1><s1>5</s1><s1>6</s1><s1>7</s1><s1>8</s1></e1>

    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testBinaryIntMinusOne(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format representation="binary" byteOrder="bigEndian" binaryNumberRep="binary" ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="implicit"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val (_, actual) = TestUtils.testBinary(sch, "FFFFFFFF")
    val expected = <e1><s1>-1</s1></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testBinaryInts(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format representation="binary" byteOrder="bigEndian" binaryNumberRep="binary" ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:int" dfdl:lengthKind="implicit" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:int" dfdl:byteOrder="littleEndian" dfdl:lengthKind="implicit"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val areTracing = false
    val (_, actual) =
      TestUtils.testBinary(sch, "000000013bFFFFFFFF3b080402013b000000003bFFFFFF7F", areTracing)
    val expected = <e1><s1>1</s1><s1>-1</s1><s1>134480385</s1><s1>0</s1><s2>2147483647</s2></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testBinaryDoubleOne(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format representation="binary" byteOrder="bigEndian" binaryNumberRep="binary" binaryFloatRep="ieee" ref="tns:GeneralFormat"/>,
      <xs:element name="e1" type="xs:double" dfdl:lengthKind="implicit"/>
    )
    val (_, actual) = TestUtils.testBinary(sch, "3FF0000000000000")
    val expected = <e1>1.0</e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testBinaryDoubleMinusOne(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format representation="binary" byteOrder="bigEndian" binaryNumberRep="binary" binaryFloatRep="ieee" ref="tns:GeneralFormat"/>,
      <xs:element name="e1" type="xs:double" dfdl:lengthKind="implicit"/>
    )
    val (_, actual) = TestUtils.testBinary(sch, "BFF0000000000000")
    val expected = <e1>-1.0</e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testBinaryDoubleLSB(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format representation="binary" byteOrder="bigEndian" binaryNumberRep="binary" binaryFloatRep="ieee" ref="tns:GeneralFormat"/>,
      <xs:element name="e1" type="xs:double" dfdl:lengthKind="implicit"/>
    )
    val (_, actual) = TestUtils.testBinary(sch, "0000000000000001")
    val expected = <e1>4.9E-324</e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testBinaryDoubles(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format representation="binary" byteOrder="bigEndian" binaryNumberRep="binary" binaryFloatRep="ieee" ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:double" dfdl:lengthKind="implicit" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:double" dfdl:byteOrder="littleEndian" dfdl:lengthKind="implicit"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val (_, actual) = TestUtils.testBinary(
      sch,
      "3FF00000000000003bBFF00000000000003b08040201080402013b00000000000000003bFFFFFFFFFFFFFF7F"
    )
    val expected =
      <e1><s1>1.0</s1><s1>-1.0</s1><s1>4.7340609871421765E-270</s1><s1>0.0</s1><s2>NaN</s2></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testTextDoubles(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:double" dfdl:lengthKind="explicit" dfdl:length="{ 4 }" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:double" dfdl:lengthKind="explicit" dfdl:length="{ 8 }"/>
            <xs:element name="s3" type="xs:double" dfdl:lengthKind="explicit" dfdl:length="{ 7 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val (_, actual) = TestUtils.testString(sch, "01.0;-1.0;4.15;0.31;8.6E-2001,234.9")
    val expected =
      <e1><s1>1.0</s1><s1>-1.0</s1><s1>4.15</s1><s1>0.31</s1><s2>8.6E-200</s2><s3>1234.9</s3></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testBinaryFloats(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format representation="binary" byteOrder="bigEndian" binaryNumberRep="binary" binaryFloatRep="ieee" ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:float" dfdl:lengthKind="implicit" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:float" dfdl:byteOrder="littleEndian" dfdl:lengthKind="implicit"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val (_, actual) =
      TestUtils.testBinary(sch, "3F8000003bBF8000003b080402013b000000003b0000C07F")
    val expected =
      <e1><s1>1.0</s1><s1>-1.0</s1><s1>3.972466E-34</s1><s1>0.0</s1><s2>NaN</s2></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testTextFloats(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:float" dfdl:lengthKind="explicit" dfdl:length="{ 4 }" minOccurs="0" dfdl:occursCountKind="parsed" dfdl:terminator=";"/>
            <xs:element name="s2" type="xs:float" dfdl:lengthKind="explicit" dfdl:length="{ 6 }"/>
            <xs:element name="s3" type="xs:float" dfdl:lengthKind="explicit" dfdl:length="{ 7 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val areTracing = false
    val (_, actual) = TestUtils.testString(sch, "01.0;-1.0;4.15;0.31;-7.1E81,234.9", areTracing)
    val expected =
      <e1><s1>1.0</s1><s1>-1.0</s1><s1>4.15</s1><s1>0.31</s1><s2>-7.1E8</s2><s3>1234.9</s3></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

}
