/* Copyright (c) 2014 Tresys Technology, LLC. All rights reserved.
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

import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.util._
import org.apache.daffodil.Implicits._;
import org.junit.Test
import org.junit.Assert._

object INoWarn8 { ImplicitsSuppressUnusedImportWarning() }

class TestInfoset2 {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val ex = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testXMLToInfoset1() {
    val testSchema = SchemaUtils.dfdlTestSchemaUnqualified(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="b">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="c" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
            <xs:element minOccurs="0" maxOccurs="unbounded" name="a" type="xs:string" dfdl:length="1" dfdl:lengthKind="explicit" dfdl:occursCountKind="expression" dfdl:occursCount="{ ../c }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)

    try {
      // Debugger.setDebugging(true)
      val (_, xml) = TestUtils.testString(testSchema, "2AB")
      val xmlStr = xml.toString
      // element b is defined as being in the example.com namespace, but with
      // no prefix defined. That means for be xmlns="example.com". But since
      // the schema is unqualified, that means elements c and x do not have a
      // namespace, so for those elements xmlns="". There was a bug where
      // NoNamespace was displayed as xmlns="No_Namespace", so this checks to
      // make sure that is resolved.
      assertFalse(xmlStr.contains("No_Namespace"))
      assertTrue(xmlStr.contains("xmlns=\"\""))
      TestUtils.assertEqualsXMLElements(<b><c>2</c><a>A</a><a>B</a></b>, xml)
    } finally {
      // Debugger.setDebugging(false)
    }

  }
}
