package edu.illinois.ncsa.daffodil.processors

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

import junit.framework.Assert._
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.util.TestUtils
import edu.illinois.ncsa.daffodil.dsom.SchemaSet
import org.junit.Test
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.util.SchemaUtils

class TestPrimitives {

  @Test def testInitiator {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" representation="text" lengthUnits="bytes" encoding="US-ASCII" terminator="" separator="" ignoreCase="no"/>,

      <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }" dfdl:initiator="abcd">
      </xs:element>)
    val actual = TestUtils.testString(sch, "abcdefgh")
    assertTrue(actual.canProceed)
    val actualString = actual.result.toString
    val expected: Node = <e1>efgh</e1>
    TestUtils.assertEqualsXMLElements(expected, actual.result)
  }

  @Test def testTerminator {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" ignoreCase="no"/>,

      <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }" dfdl:terminator="efgh">
      </xs:element>)
    val actual = TestUtils.testString(sch, "abcdefgh")
    val actualString = actual.result.toString
    val expected: Node = <e1>abcd</e1>
    TestUtils.assertEqualsXMLElements(expected, actual.result)
  }

  @Test def testSeparator() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" terminator="" ignoreCase="no"/>,

      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = TestUtils.testString(sch, "abcd,efgh")
    val actualString = actual.result.toString
    val expected: Node = <e1><s1>abcd</s1><s2>efgh</s2></e1>
    TestUtils.assertEqualsXMLElements(expected, actual.result)
  }

  @Test def testLengthKindDelimited {
    val sch = SchemaUtils.dfdlTestSchema(

      <dfdl:format ref="tns:daffodilTest1" representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" terminator="" ignoreCase="no"/>,

      <xs:element name="e1" dfdl:lengthKind="delimited">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = TestUtils.testString(sch, "abcd,efgh")
    val actualString = actual.result.toString
    val expected: Node = <e1><s1>abcd</s1><s2>efgh</s2></e1>
    TestUtils.assertEqualsXMLElements(expected, actual.result)
  }

  @Test def testLengthKindDelimited2 {
    val sch = SchemaUtils.dfdlTestSchema(

      <dfdl:format ref="tns:daffodilTest1" representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" terminator="" ignoreCase="no"/>,

      <xs:element name="e1" dfdl:lengthKind="delimited">
        <xs:complexType>
          <xs:sequence dfdl:separator="%WSP;%WSP*;\%NL;%WSP;%WSP*;" dfdl:separatorPosition="infix">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = TestUtils.testString(sch, "abcd  \\\n  efgh")
    val actualString = actual.result.toString
    val expected: Node = <e1><s1>abcd</s1><s2>efgh</s2></e1>
    TestUtils.assertEqualsXMLElements(expected, actual.result)
  }

  @Test def testLengthKindDelimited3 {
    val sch = SchemaUtils.dfdlTestSchema(

      <dfdl:format ref="tns:daffodilTest1" representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" terminator="" ignoreCase="no"/>,

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
      </xs:element>)
    val actual = TestUtils.testString(sch, "abcd}efgh}}}ijkl")
    val actualString = actual.result.toString
    val expected: Node = <e1><s1><ss1>abcd</ss1><ss2>efgh</ss2></s1><s2>ijkl</s2></e1>
    TestUtils.assertEqualsXMLElements(expected, actual.result)
  }

  @Test def testDelimiterInheritance {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:defineFormat name="config">
        <dfdl:format ref="tns:daffodilTest1" initiator="" terminator="" leadingSkip="0" trailingSkip="0" truncateSpecifiedLengthString="no" textBidi="no" floating="no" byteOrder="bigEndian" alignment="1" alignmentUnits="bytes" fillByte="f" occursCountKind="implicit" lengthUnits="bytes" initiatedContent="no" sequenceKind="ordered" ignoreCase="no" textPadKind="none" textTrimKind="none" textStandardBase="10" textNumberJustification="right" separatorPosition="infix" separatorSuppressionPolicy="never" escapeSchemeRef="" lengthKind="delimited" documentFinalTerminatorCanBeMissing="no" outputNewLine="%LF;" textNumberRep="standard" nilValueDelimiterPolicy="both" textNumberRounding="pattern"/>
      </dfdl:defineFormat>
      <dfdl:defineFormat name="baseString">
        <dfdl:format ref="tns:daffodilTest1" byteOrder="bigEndian" alignment="1" alignmentUnits="bytes" fillByte="f" lengthUnits="bytes" initiator="" terminator="" leadingSkip="0" trailingSkip="0" truncateSpecifiedLengthString="no" textBidi="no" floating="no" ignoreCase="no" textPadKind="none" textTrimKind="none" textStandardBase="10" textStringJustification="right" escapeSchemeRef="" lengthKind="delimited" occursCountKind="implicit"/>
      </dfdl:defineFormat>
      <dfdl:defineFormat name="inheritance">
        <dfdl:format ref="tns:daffodilTest1" byteOrder="bigEndian" alignment="1" alignmentUnits="bytes" fillByte="f" lengthUnits="bytes" initiator="" terminator="}" leadingSkip="0" trailingSkip="0" truncateSpecifiedLengthString="no" textBidi="no" floating="no" ignoreCase="no" textPadKind="none" textTrimKind="none" textStandardBase="10" textStringJustification="right" escapeSchemeRef="" lengthKind="delimited" occursCountKind="implicit"/>
      </dfdl:defineFormat>
      <dfdl:format ref="tns:daffodilTest1" representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" terminator="" ignoreCase="no" initiatedContent="no"/>,

      <xs:element name="root" dfdl:lengthKind="implicit" dfdl:ref="config" dfdl:initiator="{{">
        <xs:complexType>
          <xs:sequence dfdl:ref="config" dfdl:separator="," dfdl:terminator="::">
            <xs:element name="e1" type="xs:string" dfdl:ref="baseString" dfdl:representation="text"/>
            <xs:element name="e2" type="xs:string" dfdl:ref="baseString" dfdl:representation="text"/>
            <xs:element name="e3" dfdl:ref="baseString" dfdl:representation="text">
              <xs:complexType>
                <xs:sequence dfdl:ref="config" dfdl:separator="/" dfdl:terminator="//">
                  <xs:element name="e3_1" type="xs:string" dfdl:ref="baseString" dfdl:representation="text" dfdl:terminator="."/>
                  <xs:element name="e3_2" type="xs:string" dfdl:ref="inheritance" dfdl:representation="text"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = TestUtils.testString(sch, "{a,b,c./d}//::")
    val actualString = actual.result.toString

    // <root><e1></e1><e2></e2><e3><e3_1></e3_1><e3_2></e3_2></e3></root>
    // a,b,c./d//::

    val expected: Node = <root><e1>a</e1><e2>b</e2><e3><e3_1>c</e3_1><e3_2>d</e3_2></e3></root>
    TestUtils.assertEqualsXMLElements(expected, actual.result)
  }

  @Test def testEntityReplacementSeparator {
    val sch = SchemaUtils.dfdlTestSchema(

      <dfdl:format ref="tns:daffodilTest1" representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" terminator="" ignoreCase="no"/>,

      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="%NUL;" dfdl:separatorPosition="infix">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val actual = TestUtils.testString(sch, "abcd\u0000efgh")
    val actualString = actual.result.toString

    val expected: Node = <e1><s1>abcd</s1><s2>efgh</s2></e1>
    TestUtils.assertEqualsXMLElements(expected, actual.result)
  }

  @Test def testEntityReplacementInitiator {
    val sch = SchemaUtils.dfdlTestSchema(

      <dfdl:format ref="tns:daffodilTest1" representation="text" lengthUnits="bytes" encoding="US-ASCII" terminator="" separator="" ignoreCase="no"/>,

      <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }" dfdl:initiator="%NUL;">
      </xs:element>)
    val actual = TestUtils.testString(sch, "\u0000efgh")
    assertTrue(actual.canProceed)
    val actualString = actual.result.toString

    val expected: Node = <e1>efgh</e1>
    TestUtils.assertEqualsXMLElements(expected, actual.result)
  }

  @Test def testEntityReplacementTerminator {
    val sch = SchemaUtils.dfdlTestSchema(

      <dfdl:format ref="tns:daffodilTest1" representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" separator="" ignoreCase="no"/>,

      <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 4 }" dfdl:terminator="%NUL;">
      </xs:element>)
    val actual = TestUtils.testString(sch, "abcd\u0000")
    val actualString = actual.result.toString

    val expected: Node = <e1>abcd</e1>
    TestUtils.assertEqualsXMLElements(expected, actual.result)
  }

  // We no longer expect byte alignment errors
  //  @Test def testNotByteAlignedCharactersErrorDetected() {
  //    val sch = SchemaUtils.dfdlTestSchema(
  //      <dfdl:format ref="tns:daffodilTest1"/>,
  //      <xs:element name="data" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 2 }"/>)
  //    val pf = new Compiler().compile(sch)
  //    val dp = pf.onPath("/").asInstanceOf[DataProcessor]
  //
  //    val edecl = dp.rootElem
  //    val d = Misc.stringToReadableByteChannel("42")
  //    val initialState = PState.createInitialState(pf.sset.schemaComponentRegistry,
  //      edecl, d, bitOffset = 3)
  //    // Processing Errors are no longer making it to the top-level to throw the Exception
  //    //    val exc = intercept[Exception] {
  //    //      var resState = dp.parse(initialState)
  //    //      System.err.println(resState.isError)
  //    //      assertTrue(resState.isError)
  //    //    }
  //    //    val err = exc.getMessage()
  //    //    assertTrue(err.toString.contains("8"))
  //    //    assertTrue(err.toString.contains("align"))
  //    var resState = dp.parse(initialState)
  //    assertTrue(resState.isError)
  //    //assertTrue(resState.resultState.status.isInstanceOf[Failure])
  //    val err = resState.resultState.status.asInstanceOf[Failure].msg
  //    assertTrue(err.contains("not byte aligned"))
  //
  //    // val Seq(err) = resState.getDiagnostics
  //    // assertTrue(err.isInstanceOf[ParseError])
  //
  //  }

}
