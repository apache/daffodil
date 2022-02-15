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

package org.apache.daffodil.section11.content_framing_properties

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestContentFramingProperties {

  private val testDir_02 = "/org/apache/daffodil/section11/content_framing_properties/"
  val runner2 = Runner(testDir_02, "ContentFramingProps.tdml")

  @AfterClass def shutdown(): Unit = {
    runner2.reset
  }
}

class TestContentFramingProperties {
  import TestContentFramingProperties._

  // Commented out due to 4byte char decode issue when implementing DFDL-951 - DFDL-965
  // @Test def test_xml_utf8_4byte_chars_01() { runner2.runOneTest("xml_utf8_4byte_chars_01") }
  // @Test def test_xml_utf8_4byte_chars() { runner2.runOneTest("xml_utf8_4byte_chars") }

  @Test def test_UTF_16_01(): Unit = { runner2.runOneTest("UTF_16_01") }
  @Test def test_xml_illegal_chars_01(): Unit = { runner2.runOneTest("xml_illegal_chars_01") }
  @Test def test_xml_illegal_chars_02(): Unit = { runner2.runOneTest("xml_illegal_chars_02") }
  @Test def test_xml_illegal_chars(): Unit = { runner2.runOneTest("xml_illegal_chars") }

  @Test def test_alignmentPacked7BitASCII(): Unit = { runner2.runOneTest("alignmentPacked7BitASCII") }
  @Test def test_alignmentPacked7BitASCII_03(): Unit = { runner2.runOneTest("alignmentPacked7BitASCII_03") }
  @Test def test_alignmentPacked7BitASCII_04(): Unit = { runner2.runOneTest("alignmentPacked7BitASCII_04") }
  //  DFDL-751 - 7-bit ASCII alignment should be 1 bit, complains that it needs to be 8 bits
  //  @Test def test_alignmentPacked7BitASCII_02() { runner2.runOneTest("alignmentPacked7BitASCII_02") }
  //  @Test def test_alignmentPacked7BitASCII_05() { runner2.runOneTest("alignmentPacked7BitASCII_05") }

/*** DFDL-379 X-DFDL-US-ASCII-7-BIT-PACKED text ***/
  @Test def test_packed7BitASCII1(): Unit = { runner2.runOneTest("packed7BitASCII1") }
  @Test def test_packed7BitASCII2(): Unit = { runner2.runOneTest("packed7BitASCII2") }
  @Test def test_packed7BitASCII3() = { runner2.runOneTest("packed7BitASCII3") }
  @Test def test_packed7BitASCII4() = { runner2.runOneTest("packed7BitASCII4") }
  @Test def test_packed7BitASCII5() = { runner2.runOneTest("packed7BitASCII5") }
  @Test def test_packed7BitASCII6() = { runner2.runOneTest("packed7BitASCII6") }

  @Test def test_packed7BitASCII7() = { runner2.runOneTest("packed7BitASCII7") }
  @Test def test_packed7BitASCII8() = { runner2.runOneTest("packed7BitASCII8") }
  @Test def test_packed7BitASCII9() = { runner2.runOneTest("packed7BitASCII9") }
  @Test def test_packed7BitASCII10(): Unit = { runner2.runOneTest("packed7BitASCII10") }
  @Test def test_packed7BitASCII_unparse(): Unit = { runner2.runOneTest("packed7BitASCII_unparse") }
  @Test def test_packed7BitASCII_unparse2(): Unit = { runner2.runOneTest("packed7BitASCII_unparse2") }

  // DFDL-935 to re-enable, needs encodingErrorPolicy="error"
  //@Test def test_packed7BitASCII_unparse3() { runner2.runOneTest("packed7BitASCII_unparse3") }

  @Test def test_encoding_iso_8859_1() = { runner2.runOneTest("encoding_iso-8859-1") }

  @Test def test_encodingErrorReplace(): Unit = { runner2.runOneTest("encodingErrorReplace") }
  // JIRA Ticket DFDL-1386 - 4-byte utf-8 characters/surrogate-pair issue.
  @Test def test_encodingNoError(): Unit = { runner2.runOneTest("encodingNoError") }
  @Test def test_encodingErrorReplace2(): Unit = { runner2.runOneTest("encodingErrorReplace2") }
  @Test def test_encodingErrorReplace3(): Unit = { runner2.runOneTest("encodingErrorReplace3") }
  @Test def test_encodingErrorReplace4(): Unit = { runner2.runOneTest("encodingErrorReplace4") }
  @Test def test_encoding_property_expression(): Unit = { runner2.runOneTest("encoding_property_expression") }

  @Test def test_mixedEncoding1(): Unit = { runner2.runOneTest("mixedEncoding1") }
  @Test def test_mixedEncoding2(): Unit = { runner2.runOneTest("mixedEncoding2") }

  // Added for JIRA Ticket DFDL-1288
  @Test def test_encodingErrorReplace_unparse(): Unit = { runner2.runOneTest("encodingErrorReplace_unparse") }
  @Test def test_packed7BitASCII11(): Unit = { runner2.runOneTest("packed7BitASCII11") }
  @Test def test_packed7BitASCII12(): Unit = { runner2.runOneTest("packed7BitASCII12") }
  @Test def test_packed6BitASCII1(): Unit = { runner2.runOneTest("packed6BitASCII1") }
  @Test def test_encoding_property_expression2(): Unit = { runner2.runOneTest("encoding_property_expression2") }
  @Test def test_encoding_property_expression3(): Unit = { runner2.runOneTest("encoding_property_expression3") }
  @Test def test_encoding_property_expression4(): Unit = { runner2.runOneTest("encoding_property_expression4") }
  @Test def test_encoding_property_expression2_unparse(): Unit = { runner2.runOneTest("encoding_property_expression2_unparse") }
  @Test def test_alignmentPacked6BitASCII(): Unit = { runner2.runOneTest("alignmentPacked6BitASCII") }
  @Test def test_packed6BitASCII_unparse1(): Unit = { runner2.runOneTest("packed6BitASCII_unparse1") }
  @Test def test_packed6BitASCII_unparse2(): Unit = { runner2.runOneTest("packed6BitASCII_unparse2") }
  @Test def test_packed6BitASCII3(): Unit = { runner2.runOneTest("packed6BitASCII3") }
  @Test def test_packed6BitASCII4(): Unit = { runner2.runOneTest("packed6BitASCII4") }
  @Test def test_packed6BitASCII5(): Unit = { runner2.runOneTest("packed6BitASCII5") }
  // DAFFODIL-2659
  @Test def test_packed6BitICAO(): Unit = { runner2.runOneTest("packed6BitICAO") }
  // DAFFODIL-2661
  // @Test def test_packed6BitMSBF(): Unit = { runner2.runOneTest("packed6BitMSBF") }
  @Test def test_packed6BitMSBF2(): Unit = { runner2.runOneTest("packed6BitMSBF2") }
  @Test def test_packed5Bit1(): Unit = { runner2.runOneTest("packed5Bit1") }
  @Test def test_packed5Bit2(): Unit = { runner2.runOneTest("packed5Bit2") }
  @Test def test_packed5Bit3(): Unit = { runner2.runOneTest("packed5Bit3") }
  @Test def test_packed5Bit4(): Unit = { runner2.runOneTest("packed5Bit4") }
  @Test def test_packed5Bit5(): Unit = { runner2.runOneTest("packed5Bit5") }
  @Test def test_packed5Bit6(): Unit = { runner2.runOneTest("packed5Bit6") }
  @Test def test_packed5Bit_unparse1(): Unit = { runner2.runOneTest("packed5Bit_unparse1") }
  @Test def test_packed5Bit_unparse2(): Unit = { runner2.runOneTest("packed5Bit_unparse2") }
  @Test def test_packed5Bit_unparse3(): Unit = { runner2.runOneTest("packed5Bit_unparse3") }
  @Test def test_octalLSBF1(): Unit = { runner2.runOneTest("octalLSBF1") }
  @Test def test_octalLSBF2(): Unit = { runner2.runOneTest("octalLSBF2") }
  @Test def test_octalLSBF_unparse1(): Unit = { runner2.runOneTest("octalLSBF_unparse1") }
  // DFDL-935 to re-enable, needs encodingErrorPolicy="error"
  //@Test def test_octalLSBF_unparse_error() { runner2.runOneTest("octalLSBF_unparse_error") }
  @Test def test_octalLSBF3(): Unit = { runner2.runOneTest("octalLSBF3") }
  @Test def test_hexLSBF1(): Unit = { runner2.runOneTest("hexLSBF1") }
  @Test def test_hexLSBF2(): Unit = { runner2.runOneTest("hexLSBF2") }
  @Test def test_hexLSBF_unparse1(): Unit = { runner2.runOneTest("hexLSBF_unparse1") }

}
