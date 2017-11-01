/* Copyright (c) 2017 Tresys Technology, LLC. All rights reserved.
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

import org.junit.Assert.assertTrue
import org.junit.Test
import org.apache.daffodil.Implicits.intercept
import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.util.Misc
import org.apache.daffodil.util.SchemaUtils
import org.apache.daffodil.processors.unparsers.UnparseError

class TestInfosetInputter1 {

  def infosetInputter(testSchema: scala.xml.Node, infosetRdr: java.io.Reader) = {
    val compiler = Compiler()
    val pf = compiler.compileNode(testSchema)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    val u = pf.onPath("/").asInstanceOf[DataProcessor]
    if (u.isError) {
      val msgs = u.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    val rootERD = u.ssrd.elementRuntimeData
    val ic = new XMLTextInfosetInputter(infosetRdr)
    ic.initialize(rootERD, u.getTunables())
    ic
  }

  @Test def testInfosetInputterOnBadData() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>)

    val rdr = new java.io.StringReader("this is not XML");
    val ic = infosetInputter(sch, rdr)
    val exc = intercept[UnparseError] {
      ic.advance
    }
    val msg = Misc.getSomeMessage(exc).get
    assertTrue(msg.contains("prolog")) // content not allowed in prolog of an XML document.
  }

  @Test def testInfosetInputterOnBadData2() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>)

    val rdr = new java.io.StringReader("""<this pretends to be xml""");
    val ic = infosetInputter(sch, rdr)
    val exc = intercept[UnparseError] {
      ic.advance
    }
    val msg = Misc.getSomeMessage(exc).get

    assertTrue(msg.contains("Unexpected character")) // expects an equal sign for an attribute
  }

  @Test def testInfosetInputterOnBadData3() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>)

    val rdr = new java.io.StringReader("\u0000\u0000\uFFFF\uFFFF");
    val ic = infosetInputter(sch, rdr)
    val exc = intercept[UnparseError] {
      ic.advance
    }
    val msg = Misc.getSomeMessage(exc).get
    assertTrue(msg.contains("Illegal character")) // content not allowed in prolog.
  }
}
