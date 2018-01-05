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

package org.apache.daffodil.general

import org.junit.Test

import org.apache.daffodil.Implicits.ImplicitsSuppressUnusedImportWarning
import org.apache.daffodil.compiler.Compiler; object INoWarnDSOM1 { ImplicitsSuppressUnusedImportWarning() }

import org.apache.daffodil.util.Fakes
import org.apache.daffodil.util.Logging
import org.apache.daffodil.util.SchemaUtils
import org.apache.daffodil.xml.XMLUtils
import junit.framework.Assert.assertEquals
import org.apache.daffodil.api.DaffodilTunables

class TestTunables extends Logging {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  // The below is lazy for a reason.
  // It defers evaluation until used. This is nice because suppose there is a bug
  // in the Fakes stuff. Then you want tests that use that to fail. But lots of
  // these tests don't use this. If you make this an eager val, then if there
  // is any problem in the Fakes, the whole class can't be constructed, and None
  // of the tests will run. Lazy lets this class be constructed no matter what.
  lazy val dummyGroupRef = Fakes.fakeGroupRef

  @Test def testTunableCopy() {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="list" type="tns:example1"/>
      <xs:complexType name="example1">
        <xs:sequence>
          <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:sequence>
      </xs:complexType>)

    val c = Compiler()

    c.setTunable("maxSkipLengthInBytes", "1026")
    val pf1 = c.compileNode(testSchema)

    c.setTunable("maxSkipLengthInBytes", "2048")
    val pf2 = c.compileNode(testSchema)

    val dp1 = pf1.onPath("/")
    val dp2 = pf2.onPath("/")

    val t1 = dp1.getTunables()
    val t2 = dp2.getTunables()

    /* Set tunable at run-time via data processor */
    dp2.setTunable("maxSkipLengthInBytes", "50")

    val t3 = dp2.getTunables() // modified tunables at 'run-time'
    val t4 = dp1.getTunables() // obtain first data processor to see if anything changed

    assertEquals(1026, t1.maxSkipLengthInBytes) // initial compiler-set value
    assertEquals(2048, t2.maxSkipLengthInBytes) // overwrite of compiler-set value
    assertEquals(50, t3.maxSkipLengthInBytes) // data-processor-set value
    //
    //  initial compiler-set value not changed
    //  for first data processor object.
    assertEquals(1026, t4.maxSkipLengthInBytes)
  }

  @Test def testTunableSuppressionListCopying() {
    val t1 = DaffodilTunables("suppressschemadefinitionwarnings", "escapeSchemeRefUndefined")
    val t2 = DaffodilTunables("suppressschemadefinitionwarnings", "all")

    val w1 = t1.suppressSchemaDefinitionWarnings.mkString(",")
    val w2 = t2.suppressSchemaDefinitionWarnings.mkString(",")
    assertEquals(true, w1.contains("escapeSchemeRefUndefined"))
    assertEquals(true, w2.contains("all"))

    val w3 = t1.suppressSchemaDefinitionWarnings.mkString(",")
    val w4 = t1.copy().suppressSchemaDefinitionWarnings.mkString(",")
    assertEquals(true, w3.contains("escapeSchemeRefUndefined"))
    assertEquals(true, w4.contains("escapeSchemeRefUndefined"))
  }

}
