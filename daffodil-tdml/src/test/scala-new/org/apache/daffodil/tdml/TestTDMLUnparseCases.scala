/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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
import junit.framework.Assert.assertEquals
import org.junit.Test
import org.apache.daffodil.Implicits._

class TestTDMLUnparseCases {

  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example

  @Test def testUnparseSuite1() {

    val testSuite = <ts:testSuite xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:ex={ example } xmlns:ts={ tdml } suiteName="theSuiteName">
                      <ts:defineSchema name="s">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/built-in-formats.xsd"/>
                        <dfdl:format ref="ex:daffodilTest1"/>
                        <xs:element name="bar" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
                      </ts:defineSchema>
                      <ts:unparserTestCase ID="test1" name="test1" root="bar" model="s">
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <ex:bar>Hello</ex:bar>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                        <ts:document>Hello</ts:document>
                      </ts:unparserTestCase>
                    </ts:testSuite>
    lazy val ts = new DFDLTestSuite(testSuite)
    val tc: UnparserTestCase = ts.unparserTestCases.find { utc => utc.name == "test1" }.get
    // println(tc)
    tc.document
    val is = tc.inputInfoset
    // println(is)
    val bar = is.dfdlInfoset.rawContents
    val scala.xml.Elem(pre, label, _, _, _) = bar
    assertEquals("ex", pre)
    assertEquals("bar", label)
    // ts.trace
    ts.runOneTest("test1")
  }

}
