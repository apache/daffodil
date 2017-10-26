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

package edu.illinois.ncsa.daffodil.xml.test.unit

import junit.framework.Assert._
import org.junit.Test

class TestXMLLoader {

  /**
   * Characterize behavior of scala's xml loader w.r.t. CDATA preservation.
   *
   * At the time of this testing. The basic scala xml loader uses Xerces (java)
   * under the hood. It seems hopelessly broken w.r.t CDATA region preservation.
   */
  @Test def test_scala_loader_cdata_bug() {

    val data = """<x><![CDATA[a
b&"<>]]></x>"""
    val node = scala.xml.XML.loadString(data)
    val <x>{ xbody @ _* }</x> = node
    assertEquals(1, xbody.length)
    val body = xbody(0)
    val txt = body.text
    assertTrue(txt.contains("a"))
    assertTrue(txt.contains("b"))
    //
    // Note to developer - whomewever sees this test failing....
    //
    // IF this test fails, it means that the scala xml loader have been FIXED (hooray!)
    // and our need for the ConstructingParser may have gone away.
    //
    assertFalse(txt.contains("<![CDATA[")) // wrong
    assertFalse(txt.contains("]]>")) // wrong
    assertTrue(txt.contains("a\nb")) // they preserve the contents
    assertFalse(body.isInstanceOf[scala.xml.PCData]) // wrong - they don't preserve the object properly.
    assertTrue(body.isInstanceOf[scala.xml.Text]) // wrong
    assertTrue(txt.contains("""&"<>""")) // They get the right characters in there.
    assertTrue(body.toString.contains("""&amp;&quot;&lt;&gt;""")) // wrong
    assertFalse(body.toString.contains("""<![CDATA[a
b&"<>]]>"""))

  }

}
