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

import scala.xml.NodeSeq

import org.apache.daffodil.core.util.TestUtils.compileSchema
import org.apache.daffodil.lib.util._

import org.junit.Assert.fail

/**
 * Test rig that allows capture of heap dumps using jvisualVM, for subsequent
 * analysis.
 *
 * The tests at one time created a jvisualVM heap dump showing
 * retention of large numbers of DFDL schema compiler data structures.
 * These are no longer being detected, but the test rig is here for future needs.
 *
 * Note that the @Test keywords are commented out.
 */
class TestForHeapDump {

  val rootElem =
    <xs:element name="r" dfdl:lengthKind="implicit">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="e1" type="xs:int" dfdl:length="1"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>

  /**
   * Gets a data processor, allowing (in theory) all the other compiler data structures
   * to be reclaimed by the GC.
   *
   * This variant uses a standard include of the DFDLGeneralFormat.
   */
  def getDataProcWithInclude() = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" lengthKind="explicit"/>,
      rootElem,
      elementFormDefault = "unqualified",
      useDefaultNamespace = false
    )
    val p = compileSchema(sch)
    p
  }

  /**
   * Gets a data processor, allowing (in theory) all the other compiler data structures
   * to be reclaimed by the GC.
   *
   * This variant is fully self-contained. There are no import/includes.
   */
  def getDataProcNoInclude() = {
    val sch = SchemaUtils.dfdlTestSchema(
      NodeSeq.Empty,
      <dfdl:format
        alignment="1"
        alignmentUnits="bytes"
        binaryFloatRep="ieee"
        binaryNumberCheckPolicy="lax"
        binaryNumberRep="binary"
        binaryCalendarEpoch="1970-01-01T00:00:00"
        bitOrder="mostSignificantBitFirst"
        byteOrder="bigEndian"
        calendarCenturyStart="53"
        calendarCheckPolicy="strict"
        calendarDaysInFirstWeek="4"
        calendarFirstDayOfWeek="Sunday"
        calendarLanguage="en"
        calendarObserveDST="yes"
        calendarPatternKind="implicit"
        calendarTimeZone=""
        choiceLengthKind="implicit"
        decimalSigned="yes"
        documentFinalTerminatorCanBeMissing="no"
        emptyValueDelimiterPolicy="both"
        encodingErrorPolicy="replace"
        encoding="US-ASCII"
        escapeSchemeRef=""
        fillByte="%#r20;"
        floating="no"
        ignoreCase="no"
        initiatedContent="no"
        initiator=""
        leadingSkip="0"
        lengthKind="explicit"
        lengthUnits="bytes"
        occursCountKind="implicit"
        outputNewLine="%LF;"
        representation="text"
        separator=""
        separatorPosition="infix"
        separatorSuppressionPolicy="anyEmpty"
        sequenceKind="ordered"
        terminator=""
        textBidi="no"
        textBooleanPadCharacter="%SP;"
        textCalendarJustification="left"
        textCalendarPadCharacter="%SP;"
        textNumberCheckPolicy="lax"
        textNumberJustification="right"
        textNumberPadCharacter="%SP;"
        textNumberPattern="#,##0.###;-#,##0.###"
        textNumberRep="standard"
        textNumberRounding="explicit"
        textNumberRoundingIncrement="0"
        textNumberRoundingMode="roundHalfEven"
        textOutputMinLength="0"
        textPadKind="none"
        textStandardBase="10"
        textStandardDecimalSeparator="."
        textStandardExponentRep="E"
        textStandardGroupingSeparator=","
        textStandardInfinityRep="Inf"
        textStandardNaNRep="NaN"
        textStandardZeroRep="0"
        textStringJustification="left"
        textStringPadCharacter="%SP;"
        textTrimKind="none"
        trailingSkip="0"
        truncateSpecifiedLengthString="no"
        utf16Width="fixed"
        />,
      rootElem,
      elementFormDefault = "unqualified",
      useDefaultNamespace = false
    )
    val p = compileSchema(sch)
    p
  }

  def gcAndAllowHeapDump(): Unit = {
    System.gc()
    System.out.println("Take a Heap Dump Now! (You have 10 seconds)")
    Thread.sleep(10000)
    System.out.println("Too late")
  }

  /**
   * Use to get a heap dump for this simple DFDL schema showing that
   * it does NOT contain any DSOM objects E.g., there are zero
   * retained instances of the SchemaSet class, or the LocalElementDecl class, and so on.
   *
   * Numerous "Lambda" objects - code that is output from the Scala compiler - are
   * created but these are small, and in general there is exactly one of each.
   */
  // @Test
  def testMemoryObjects_ForUseWithHeapDump_NoInclude(): Unit = {
    val p = getDataProcNoInclude()
    gcAndAllowHeapDump()
    if (p eq null) fail("no processor")
  }

  /**
   * Use to get a heap dump for this simple DFDL schema
   *
   * The heap dump can be analyzed to show any
   * schema-compiler data structures that are retained, allowing them to be
   * visualized for debugging/etc. This includes
   * DSOM objects, grammar Prod objects, etc.
   */
  // @Test
  def testMemoryObjects_ForUseWithHeapDump_WithInclude(): Unit = {
    val p = getDataProcWithInclude()
    gcAndAllowHeapDump()
    if (p eq null) fail("no processor")
  }
}
