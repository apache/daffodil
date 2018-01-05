package org.apache.daffodil.tdml

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

import org.apache.daffodil.xml.XMLUtils
import org.junit.Test
import org.apache.daffodil.xml.NS.implicitNStoString

class TestTDMLRunnerTutorial {

  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example
  val xhtml = XMLUtils.XHTML_NAMESPACE

  val tdmlWithTutorial =
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:xsi={ xsi } xmlns={ xhtml } xmlns:ex={ example }>
      <tdml:tutorial>
        <p>1</p>
      </tdml:tutorial>
      <tdml:defineSchema name="mySchema">
        <dfdl:format ref="tns:daffodilTest1"/>
        <xs:element name="data" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(2) }"/>
      </tdml:defineSchema>
      <tdml:tutorial>
        <p>2</p>
      </tdml:tutorial>
      <tdml:parserTestCase name="testTutorialElementsParse" root="data" model="mySchema">
        <tdml:tutorial>
          <p>3</p>
        </tdml:tutorial>
        <tdml:document>37</tdml:document>
        <tdml:tutorial>
          <p>4</p>
        </tdml:tutorial>
        <tdml:infoset>
          <tdml:dfdlInfoset>
            <ex:data>37</ex:data>
          </tdml:dfdlInfoset>
        </tdml:infoset>
        <tdml:tutorial>
          <p>5</p>
        </tdml:tutorial>
      </tdml:parserTestCase>
      <tdml:tutorial>
        <p>6</p>
      </tdml:tutorial>
      <tdml:defineSchema name="s">
        <xs:include schemaLocation="org/apache/daffodil/xsd/built-in-formats.xsd"/>
        <dfdl:format ref="ex:daffodilTest1"/>
        <xs:element name="bar" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
      </tdml:defineSchema>
      <tdml:tutorial>
        <p>7</p>
      </tdml:tutorial>
      <tdml:unparserTestCase name="testTutorialElementsUnparse" root="bar" model="s">
        <tdml:tutorial>
          <p>8</p>
        </tdml:tutorial>
        <tdml:infoset>
          <tdml:dfdlInfoset>
            <ex:bar>Hello</ex:bar>
          </tdml:dfdlInfoset>
        </tdml:infoset>
        <tdml:tutorial>
          <p>9</p>
        </tdml:tutorial>
        <tdml:document>Hello</tdml:document>
        <tdml:tutorial>
          <p>10</p>
        </tdml:tutorial>
      </tdml:unparserTestCase>
      <tdml:tutorial>
        <p>11</p>
      </tdml:tutorial>
    </tdml:testSuite>

  @Test def testTutorialElementsParse() {
    val testSuite = tdmlWithTutorial
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testTutorialElementsParse")
  }

  @Test def testTutorialElementsUnparse() {
    val testSuite = tdmlWithTutorial
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testTutorialElementsUnparse")
  }
}
