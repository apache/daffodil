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

package org.apache.daffodil.core.outputValueCalc

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

/*
 * These are all tests of OVC and alignment interactions
 */
class TestOutputValueCalcAndAlignment {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  /**
   * Test verifies that if the OVC's length is known, the alignment afterwards
   * works.
   */
  @Test def testOutputValueCalcAlignmentFixed(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes" alignmentUnits="bytes" fillByte="X"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:outputValueCalc="{ ../s2 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:alignment="2"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:e1 xmlns:ex={example}><s2>2</s2></ex:e1>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "2X2", areTracing)
  }

  /**
   * Tests that alignment will cause a suspended alignment region, until
   * the variable-length of the preceding OVC element is determined, at which
   * point then the absolute position is known, the alignment can proceed,
   * and the unparsing will complete.
   */
  @Test def testOutputValueCalcVariableLengthThenAlignment(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes" alignmentUnits="bytes" fillByte="X"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <!--
             corner case: OVC element has variable length. So aligned element
             after it cannot be unparsed (alignment region size isn't known)
             until we know just how big the representation of s1 is, in bytes.
             So the alignment must be deferred.
             -->
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited" dfdl:terminator="T" dfdl:outputValueCalc="{ ../s2 }"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited" dfdl:terminator="S" dfdl:alignment="8"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:e1 xmlns:ex={example}><s2>12345</s2></ex:e1>
    val areTracing = false
    //
    // below, the two XX are inserted to align to an 8 byte boundary. But the
    // number of bytes to insert to achieve alignment can't be computed until
    // the OVC element gets its value, and therefore it can be unparsed, which
    // finishes the suspended unparse for the OVC, the absolute position
    // propagates to the next (buffered) data output stream, so when the
    // alignment is retried, then it can determine 2 bytes are needed.
    //
    TestUtils.testUnparsing(sch, infoset, "12345TXX12345S", areTracing)
  }

  /**
   * Tests that alignment region will cause deadlock if preceding OVC is
   * variable length (such that the absolute position can't be known.
   *
   * This is the outputValueCalc depends on length which depends on interior alignment issue.
   *
   * This is supposed to deadlock.
   */
  @Test def testOutputValueCalcVariableLengthThenAlignmentDeadlock(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes" alignmentUnits="bytes" fillByte="X"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <!--
             corner case: OVC element has variable length.
             So alignment needed for i2 can't be determined since we don't have
             the starting absolute position.
             However, that alignment region length is needed to compute
             the dfdl:contentLength of s2. So we deadlock.
             -->
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited" dfdl:terminator="T" dfdl:outputValueCalc="{ dfdl:valueLength(../s2, 'bytes') }"/>
            <xs:element name="s2" dfdl:lengthKind="delimited" dfdl:terminator="S">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="i1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
                  <xs:element name="i2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1" dfdl:alignment="8"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:e1 xmlns:ex={example}><s2><i1>A</i1><i2>B</i2></s2></ex:e1>
    val areTracing = false
    try {
      TestUtils.testUnparsing(sch, infoset, "ignored", areTracing)
    } catch {
      // Some test utilities, given multiple diagnostics from a run, will just
      // concatenate their messages, and throw a vanilla Exception object.
      case e: Exception => {
        val msg = Misc.getSomeMessage(e).get.toLowerCase
        if (!msg.contains("Schema Definition Error".toLowerCase))
          fail(msg + " did not contain Schema Definition Error")

        assertTrue(msg.contains("Deadlock".toLowerCase))
      }
    }
  }

}
