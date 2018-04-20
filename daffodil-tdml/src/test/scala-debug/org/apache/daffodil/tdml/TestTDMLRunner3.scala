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

package org.apache.daffodil.tdml

import org.apache.daffodil.xml.XMLUtils
import junit.framework.Assert.assertEquals
import org.apache.daffodil.util._
import org.junit.Test

class TestTDMLRunner3 {

  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example

  /**
   * Test illustrates problem with multiple document parts having a RTL byte order
   * not being assembled properly.
   *
   * That or the document bitOrder is causing the parts bitOrders to be assembled incorrectly.
   *
   * There are many other tests that use RTL byte order to assemble bits together, so it is
   * something about mixing byteOrder RTL with LTR that is causing the problem.
   *
   * Bug DAFFODIL-1898
   */
  @Test def testMixedBigEndianMSBFWithLittleEndianLSBF() {
    val xml = <document bitOrder="MSBFirst" xmlns="http://www.ibm.com/xmlns/dfdl/testData">
                <documentPart type="byte" bitOrder="MSBFirst" byteOrder="LTR">AA                  </documentPart>
                <documentPart type="bits" bitOrder="LSBFirst" byteOrder="RTL">XXXX X001</documentPart>
                <documentPart type="bits" bitOrder="LSBFirst" byteOrder="RTL">1111 1XXX</documentPart>
                <!-- The above is AAF9 -->
              </document>
    val doc = new Document(xml, null)
    val bytes = doc.documentBytes
    val hexDigits = Misc.bytes2Hex(bytes)
    val expected = "AAF9".replace(" ", "")
    assertEquals(expected, hexDigits)
  }

  val testDir = "/test/tdml/"
  val runner = Runner(testDir, "tdmlQuoting.tdml", validateTDMLFile = false)

  /**
   * Test illustrates problem with tdml runner correctly processes apostrophes (') 
   * in the html format (&apos;) within the document or infoset data. The aposrophes are  
   * stripped out of actual or expected values causing the comparison to fail.
   *
   * Bug DAFFODIL-1928
   */
  @Test def test_apos_test1() { runner.runOneTest("apos_test1") }
  @Test def test_apos_test2() { runner.runOneTest("apos_test2") }

}
