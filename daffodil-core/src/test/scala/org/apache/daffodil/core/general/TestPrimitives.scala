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

package org.apache.daffodil.core.general

import scala.xml._

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Test; object INoWarn9 { ImplicitsSuppressUnusedImportWarning() }
import org.apache.daffodil.lib.util.SchemaUtils

class TestPrimitives {

  @Test def testInitiator(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" representation="text" lengthUnits="bytes" encoding="US-ASCII" terminator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }" dfdl:initiator="abcd">
      </xs:element>
    )
    val areTracing = false
    val (_, actual) = TestUtils.testString(sch, "abcdefgh", areTracing)
    val expected: Node = <e1>efgh</e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testTerminator(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }" dfdl:terminator="efgh">
      </xs:element>
    )
    val areTracing = false
    val (_, actual) = TestUtils.testString(sch, "abcdefgh", areTracing)
    val expected: Node = <e1>abcd</e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testSeparator(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" terminator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val areTracing = false
    val (_, actual) = TestUtils.testString(sch, "abcd,efgh", areTracing)
    val expected: Node = <e1><s1>abcd</s1><s2>efgh</s2></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testLengthKindDelimited(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" terminator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="delimited">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val areTracing = false
    val (_, actual) = TestUtils.testString(sch, "abcd,efgh", areTracing)
    val expected: Node = <e1><s1>abcd</s1><s2>efgh</s2></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testLengthKindDelimited2(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" terminator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="delimited">
        <xs:complexType>
          <xs:sequence dfdl:separator="%WSP;%WSP*;\%NL;%WSP;%WSP*;" dfdl:separatorPosition="infix">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val (_, actual) = TestUtils.testString(sch, "abcd  \\\n  efgh")
    val expected: Node = <e1><s1>abcd</s1><s2>efgh</s2></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testLengthKindDelimited3(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" terminator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="delimited">
        <xs:complexType>
          <xs:sequence dfdl:separator="}}}" dfdl:separatorPosition="infix">
            <xs:element name="s1" dfdl:lengthKind="delimited">
              <xs:complexType>
                <xs:sequence dfdl:separator="}" dfdl:separatorPosition="infix">
                  <xs:element name="ss1" type="xs:string" dfdl:lengthKind="delimited"/>
                  <xs:element name="ss2" type="xs:string" dfdl:lengthKind="delimited"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val areTracing = false
    val (_, actual) = TestUtils.testString(sch, "abcd}efgh}}}ijkl", areTracing)
    val expected: Node = <e1><s1><ss1>abcd</ss1><ss2>efgh</ss2></s1><s2>ijkl</s2></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testDelimiterInheritance(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:defineFormat name="config">
        <dfdl:format ref="tns:GeneralFormat" initiator="" terminator="" leadingSkip="0" trailingSkip="0" truncateSpecifiedLengthString="no" textBidi="no" floating="no" byteOrder="bigEndian" alignment="1" alignmentUnits="bytes" fillByte="f" occursCountKind="implicit" lengthUnits="bytes" initiatedContent="no" sequenceKind="ordered" ignoreCase="no" textPadKind="none" textTrimKind="none" textStandardBase="10" textNumberJustification="right" separatorPosition="infix" separatorSuppressionPolicy="never" escapeSchemeRef="" lengthKind="delimited" documentFinalTerminatorCanBeMissing="no" outputNewLine="%LF;" textNumberRep="standard" nilValueDelimiterPolicy="both" textNumberRounding="pattern"/>
      </dfdl:defineFormat>
      <dfdl:defineFormat name="baseString">
        <dfdl:format ref="tns:GeneralFormat" byteOrder="bigEndian" alignment="1" alignmentUnits="bytes" fillByte="f" lengthUnits="bytes" initiator="" terminator="" leadingSkip="0" trailingSkip="0" truncateSpecifiedLengthString="no" textBidi="no" floating="no" ignoreCase="no" textPadKind="none" textTrimKind="none" textStandardBase="10" textStringJustification="right" escapeSchemeRef="" lengthKind="delimited" occursCountKind="implicit"/>
      </dfdl:defineFormat>
      <dfdl:defineFormat name="inheritance">
        <dfdl:format ref="tns:GeneralFormat" byteOrder="bigEndian" alignment="1" alignmentUnits="bytes" fillByte="f" lengthUnits="bytes" initiator="" terminator="}" leadingSkip="0" trailingSkip="0" truncateSpecifiedLengthString="no" textBidi="no" floating="no" ignoreCase="no" textPadKind="none" textTrimKind="none" textStandardBase="10" textStringJustification="right" escapeSchemeRef="" lengthKind="delimited" occursCountKind="implicit"/>
      </dfdl:defineFormat>
      <dfdl:format ref="tns:GeneralFormat" representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" terminator="" ignoreCase="no" initiatedContent="no"/>,
      <xs:element name="root" dfdl:lengthKind="implicit" dfdl:ref="config" dfdl:initiator="{{">
        <xs:complexType>
          <xs:sequence dfdl:ref="config" dfdl:separator="," dfdl:terminator="::">
            <xs:element name="e1" type="xs:string" dfdl:ref="baseString" />
            <xs:element name="e2" type="xs:string" dfdl:ref="baseString" />
            <xs:element name="e3" dfdl:ref="baseString" dfdl:representation="text">
              <xs:complexType>
                <xs:sequence dfdl:ref="config" dfdl:separator="/" dfdl:terminator="//">
                  <xs:element name="e3_1" type="xs:string" dfdl:ref="baseString" dfdl:terminator="."/>
                  <xs:element name="e3_2" type="xs:string" dfdl:ref="inheritance" />
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val areTracing = false
    val (_, actual) = TestUtils.testString(sch, "{a,b,c./d}//::", areTracing)

    // <root><e1></e1><e2></e2><e3><e3_1></e3_1><e3_2></e3_2></e3></root>
    // a,b,c./d//::

    val expected: Node = <root><e1>a</e1><e2>b</e2><e3><e3_1>c</e3_1><e3_2>d</e3_2></e3></root>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testEntityReplacementSeparator(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" terminator="" ignoreCase="no"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="%NUL;" dfdl:separatorPosition="infix">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val (_, actual) = TestUtils.testString(sch, "abcd\u0000efgh")

    val expected: Node = <e1><s1>abcd</s1><s2>efgh</s2></e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testEntityReplacementInitiator(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" representation="text" lengthUnits="bytes" encoding="US-ASCII" terminator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }" dfdl:initiator="%NUL;">
      </xs:element>
    )
    val (_, actual) = TestUtils.testString(sch, "\u0000efgh")
    val expected: Node = <e1>efgh</e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testEntityReplacementTerminator(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" ignoreCase="no"/>,
      <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }" dfdl:terminator="%NUL;">
      </xs:element>
    )
    val (_, actual) = TestUtils.testString(sch, "abcd\u0000")

    val expected: Node = <e1>abcd</e1>
    XMLUtils.compareAndReport(expected, actual)
  }

}
