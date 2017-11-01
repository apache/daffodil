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

package org.apache.daffodil.infoset

import org.junit.Test
import org.junit.Assert._
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.Implicits._
import org.apache.daffodil.equality._; object ENoWarnU1 { EqualitySuppressUnusedImportWarning() }
import org.apache.daffodil.util.SchemaUtils
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.xml._

/*
 * These are all tests of default-value insertion.
 * But I determined that the approach to defaulting,
 * which was a schema-aware separate pass, can't really work
 * and isn't worth a whole pass on its own.
 *
 * So these tests will have to be revisited someday when we
 * have defaulting working.
 *
 * The way these tests work is broken. They are assuming that
 * defaulting is being inserted by the XMLEventCursor. It will
 * not be there. Defaulting will be done as part of Unparsing.
 */
class TestInfosetDefaultingInUnparser {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testDefaultable = {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator=",">
            <xs:element name="foo" dfdl:lengthKind="delimited" type="xs:string" default="abcde"/>
            <xs:element name="afterFoo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val compiler = Compiler()
    val pf = compiler.compileNode(sch)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    val u = pf.onPath("/").asInstanceOf[DataProcessor]
    if (u.isError) {
      val msgs = u.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }

    val xml = <bar xmlns={ XMLUtils.EXAMPLE_NAMESPACE }><afterFoo>Hello</afterFoo></bar>

    val rootERD = u.ssrd.elementRuntimeData

    val inputter = new ScalaXMLInfosetInputter(xml)
    inputter.initialize(rootERD, u.getTunables())
    val is = Adapter(inputter)

    val Start(bar_s: DIComplex) = is.next
    assertNotNull(bar_s)
    val Start(foo_s: DISimple) = is.next
    val End(foo_e: DISimple) = is.next
    assertNotNull(foo_e)
    val Start(afterFoo_s: DISimple) = is.next
    val End(afterFoo_e: DISimple) = is.next
    assertNotNull(afterFoo_e)
    val End(bar_e: DIComplex) = is.next
    assertNotNull(bar_e)

    assertEquals("abcde", foo_s.dataValue)
    assertEquals("Hello", afterFoo_s.dataValue)
  }

}
