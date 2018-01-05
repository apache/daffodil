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

object TestContentFramingPropertiesDebug {
  private val testDir_01 = "/org/apache/daffodil/ibm-tests/"
  lazy val runner1 = Runner(testDir_01, "dpaext1.tdml")

  private val testDir_02 = "/org/apache/daffodil/section11/content_framing_properties/"
  lazy val runner2 = Runner(testDir_02, "ContentFramingProps.tdml")

  @AfterClass def shutdown {
    runner1.reset
    runner2.reset
  }
}

class TestContentFramingPropertiesDebug {

  import TestContentFramingPropertiesDebug._

  //  DFDL-751 - 7-bit ASCII alignment should be 1 bit, complains that it needs to be 8 bits
  @Test def test_alignmentPacked7BitASCII_02() { runner2.runOneTest("alignmentPacked7BitASCII_02") }
  @Test def test_alignmentPacked7BitASCII_05() { runner2.runOneTest("alignmentPacked7BitASCII_05") }

  // Implementing DFDL-951 caused regression - see DFDL-965
  @Test def test_xml_utf8_4byte_chars() { runner2.runOneTest("xml_utf8_4byte_chars") }
  @Test def test_xml_utf8_4byte_chars_01() { runner2.runOneTest("xml_utf8_4byte_chars_01") }

  // JIRA Ticket DFDL-1386 - 4-byte utf-8 characters/surrogate-pair issue.
  @Test def test_encodingNoError() { runner2.runOneTest("encodingNoError") }

  // DFDL-935 to re-enable, needs encodingErrorPolicy="error"
  @Test def test_packed7BitASCII_unparse3() { runner2.runOneTest("packed7BitASCII_unparse3") }
  @Test def test_octalLSBF_unparse_error() { runner2.runOneTest("octalLSBF_unparse_error") }

}
