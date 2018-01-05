/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.tdml

import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.util._
import org.junit.Test
import org.apache.daffodil.Implicits._

class TestTDMLRunner2 {

  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example
  // val sub = XMLUtils.DFDL_XMLSCHEMASUBSET_NAMESPACE

  @Test def testTDMLUnparse() {
    val testSuite = <ts:testSuite xmlns:ts={ tdml } xmlns:tns={ tns } xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:xsi={ xsi } suiteName="theSuiteName">
                      <ts:defineSchema name="unparseTestSchema1">
                        <dfdl:format ref="tns:daffodilTest1"/>
                        <xs:element name="data" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 9 }"/>
                      </ts:defineSchema>
                      <ts:unparserTestCase ID="some identifier" name="testTDMLUnparse" root="data" model="unparseTestSchema1">
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <data xmlns={ example }>123456789</data>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                        <ts:document>123456789</ts:document>
                      </ts:unparserTestCase>
                    </ts:testSuite>
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testTDMLUnparse")
  }

  val testDir = "/test/tdml/"
  val t0 = testDir + "tdmlNamespaces.tdml"
  lazy val r = new DFDLTestSuite(Misc.getRequiredResource(t0))

  val aa = testDir + "tdmlQuoting.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_quote_test1() = {
    runner.setDebugging(true)
    runner.runOneTest("quote_test1")
  }

}
