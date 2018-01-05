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

package edu.illinois.ncsa.daffodil.general

import org.junit.Test
import edu.illinois.ncsa.daffodil.util.SchemaUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util.TestUtils

class TestPrimitives2 {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testUnparseNilValueEntities() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="e1" nillable="true" dfdl:nilKind="literalValue" dfdl:lengthKind="delimited" type="xs:string" dfdl:nilValue="%WSP;nil%NL; foobar" dfdl:outputNewline="%LF;"/>,
      elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example } xmlns:xsi={ XMLUtils.XSI_NAMESPACE.toString() } xsi:nil="true"/>
    TestUtils.testUnparsing(sch, infoset, " nil\u000a")
  }

  @Test def testUnparseNilValueEntities2() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="e1" dfdl:nilValue="start%WSP;bla%%WSP;;;;foo%WSP*;bar%WSP+;baz%ES;quux%NL;boo%%baz%%NL;end" dfdl:outputNewline="%LF;" nillable="true" dfdl:nilKind="literalValue" dfdl:lengthKind="delimited" type="xs:string"/>,
      elementFormDefault = "unqualified")
    val infoset = <ex:e1 xmlns:ex={ example } xmlns:xsi={ XMLUtils.XSI_NAMESPACE.toString() } xsi:nil="true"/>
    TestUtils.testUnparsing(sch, infoset, "start bla%WSP;;;;foobar bazquux\u000aboo%baz%NL;end")
  }

}
