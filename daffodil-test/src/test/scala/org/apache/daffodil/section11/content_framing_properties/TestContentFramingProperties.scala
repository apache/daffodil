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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Ignore
import org.junit.Test

object TestContentFramingProperties extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section11/content_framing_properties/ContentFramingProps.tdml"
}

class TestContentFramingProperties extends TdmlTests {
  val tdmlSuite = TestContentFramingProperties

  // Commented out due to 4byte char decode issue when implementing DFDL-951 - DFDL-965
  @Ignore @Test def xml_utf8_4byte_chars_01 = test
  @Ignore @Test def xml_utf8_4byte_chars = test

  @Test def UTF_16_01 = test
  @Test def xml_illegal_chars_01 = test
  @Test def xml_illegal_chars_02 = test
  @Test def xml_illegal_chars = test

  @Test def alignmentPacked7BitASCII = test
  @Test def alignmentPacked7BitASCII_03 = test
  @Test def alignmentPacked7BitASCII_04 = test
  //  DFDL-751 - 7-bit ASCII alignment should be 1 bit, complains that it needs to be 8 bits
  @Ignore @Test def alignmentPacked7BitASCII_02 = test
  @Ignore @Test def alignmentPacked7BitASCII_05 = test

  /*** DFDL-379 X-DFDL-US-ASCII-7-BIT-PACKED text ***/
  @Test def packed7BitASCII1 = test
  @Test def packed7BitASCII2 = test
  @Test def packed7BitASCII3 = test
  @Test def packed7BitASCII4 = test
  @Test def packed7BitASCII5 = test
  @Test def packed7BitASCII6 = test

  @Test def packed7BitASCII7 = test
  @Test def packed7BitASCII8 = test
  @Test def packed7BitASCII9 = test
  @Test def packed7BitASCII10 = test
  @Test def packed7BitASCII_unparse = test
  @Test def packed7BitASCII_unparse2 = test

  // DFDL-935 to re-enable, needs encodingErrorPolicy="error"
  @Ignore @Test def packed7BitASCII_unparse3 = test

  @Test def encoding_iso_8859_1 = test

  @Test def encodingErrorReplace = test
  // JIRA Ticket DFDL-1386 - 4-byte utf-8 characters/surrogate-pair issue.
  @Test def encodingNoError = test
  @Test def encodingErrorReplace2 = test
  @Test def encodingErrorReplace3 = test
  @Test def encodingErrorReplace4 = test
  @Test def encoding_property_expression = test

  @Test def mixedEncoding1 = test
  @Test def mixedEncoding2 = test

  // Added for JIRA Ticket DFDL-1288
  @Test def encodingErrorReplace_unparse = test
  @Test def packed7BitASCII11 = test
  @Test def packed7BitASCII12 = test
  @Test def packed6BitASCII1 = test
  @Test def encoding_property_expression2 = test
  @Test def encoding_property_expression3 = test
  @Test def encoding_property_expression4 = test
  @Test def encoding_property_expression2_unparse = test
  @Test def alignmentPacked6BitASCII = test
  @Test def packed6BitASCII_unparse1 = test
  @Test def packed6BitASCII_unparse2 = test
  @Test def packed6BitASCII3 = test
  @Test def packed6BitASCII4 = test
  @Test def packed6BitASCII5 = test
  // DAFFODIL-2659
  @Test def packed6BitICAO = test
  // DAFFODIL-2661
  @Ignore @Test def packed6BitMSBF = test
  @Test def packed6BitMSBF2 = test
  @Test def packed5Bit1 = test
  @Test def packed5Bit2 = test
  @Test def packed5Bit3 = test
  @Test def packed5Bit4 = test
  @Test def packed5Bit5 = test
  @Test def packed5Bit6 = test
  @Test def packed5Bit_unparse1 = test
  @Test def packed5Bit_unparse2 = test
  @Test def packed5Bit_unparse3 = test
  @Test def octalLSBF1 = test
  @Test def octalLSBF2 = test
  @Test def octalLSBF_unparse1 = test
  // DFDL-935 to re-enable, needs encodingErrorPolicy="error"
  @Ignore @Test def octalLSBF_unparse_error = test
  @Test def octalLSBF3 = test
  @Test def hexLSBF1 = test
  @Test def hexLSBF2 = test
  @Test def hexLSBF_unparse1 = test
}
