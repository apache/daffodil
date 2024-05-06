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

package org.apache.daffodil.core.runtime1

import org.apache.daffodil.core.util.StreamParser
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Assert.assertEquals
import org.junit.Test

class TestDelimiterFinalBacktracking {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  val schema1 =
    <xs:element name="r" dfdl:lengthKind="implicit">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="a" type="xs:string"
                      dfdl:lengthKind="delimited"
                      dfdl:terminator="long longer"
                      dfdl:ignoreCase="no" />
        </xs:sequence>
      </xs:complexType>
    </xs:element>

  /**
   * This test shows that even if Daffodil speculates forward to see if a longer
   * delimiter is present, but it isn't, it resets the InputSourceDataInputStream
   * to the position after the last character of the delimiter that was found.
   *
   * Daffodil has to speculate to see if the delimiter is "longer", not just "long", so
   * it has to read the "e" and "R" characters to know that the first delimiter is just "long".
   *
   * If the input stream was a TCP network socket, then Daffodil would need to read the two
   * bytes ("eR") past the first "long", and so would block waiting for those bytes to become available.
   * But it does not consume them from its own InputSourceDataInputStream, which is what this test
   * shows. Rather, daffodil looks at them, but backtracks them so they are not consumed. 
   */
  @Test def testParseTerminatorBacktrack1(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      schema1,
      elementFormDefault = "unqualified"
    )

    //
    // Baseline behavior
    //
    {
      val data = "AlongerBlong" // we should get "A" then "B"
      val res = StreamParser.doStreamTest(testSchema, data)
      val firstBytePos1b = ((res(0).bitPos1b - 1) / 8).toInt
      assertEquals(7, firstBytePos1b)
      assertEquals("Alonger", data.substring(0, firstBytePos1b))
      assertEquals("A", (res(0).message \\ "a").text)
      assertEquals("B", (res(1).message \\ "a").text)
    }
    //
    // Now the actual test
    //
    val data = "AlongeRBlong" // we should get "A" then "eRB"
    val res = StreamParser.doStreamTest(testSchema, data)
    val firstBytePos1b = ((res(0).bitPos1b - 1) / 8).toInt
    assertEquals(5, firstBytePos1b)
    assertEquals("Along", data.substring(0, firstBytePos1b))
    assertEquals("A", (res(0).message \\ "a").text)
    assertEquals("eRB", (res(1).message \\ "a").text)
  }

}
