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

package edu.illinois.ncsa.daffodil.util

import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.exceptions._
import org.junit.Test
import edu.illinois.ncsa.daffodil.Implicits._
import scala.xml._

class TestSchemaUtils {

  /**
   * Just some random TDML-like DFDL fragments.
   */
  val test1 = <surround xmlns:ex="http://example.com" xmlns:ct="http://w3.ibm.com/xmlns/dfdl/ctInfoset" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tdml="http://www.ibm.com/xmlns/dfdl/testData">
                <dfdl:format ref="ex:daffodilTest1"/>
                <xs:element dfdl:lengthKind="delimited" name="intRestrict">
                  <xs:simpleType>
                    <xs:restriction base="xs:int">
                      <xs:totalDigits value="1"/>
                      <xs:minInclusive value="1"/>
                      <xs:maxInclusive value="10"/>
                    </xs:restriction>
                  </xs:simpleType>
                </xs:element>
                <xs:element dfdl:lengthKind="delimited" name="arrayCombo">
                  <xs:complexType>
                    <xs:sequence dfdl:separator=",">
                      <xs:element maxOccurs="unbounded" minOccurs="3" dfdl:lengthKind="delimited" name="e">
                        <xs:simpleType>
                          <xs:restriction base="xs:int">
                            <xs:maxExclusive value="3"/>
                            <xs:totalDigits value="1"/>
                          </xs:restriction>
                        </xs:simpleType>
                      </xs:element>
                    </xs:sequence>
                  </xs:complexType>
                </xs:element>
              </surround>

  def sameScopeEverywhere(x: Node, scope: NamespaceBinding): Boolean = {
    x match {
      case e: Elem => e.scope == scope && e.child.forall {
        sameScopeEverywhere(_, scope)
      }
      case _ => true
    }
  }

  @Test def testDFDLTestSchema1() {
    val anns = test1 \\ "format"
    val elems = test1 \\ "element"
    val sch = SchemaUtils.dfdlTestSchema(anns, elems, schemaScope = test1.scope)
    val scope = sch.scope
    assertTrue(sameScopeEverywhere(sch, scope))
  }

}
