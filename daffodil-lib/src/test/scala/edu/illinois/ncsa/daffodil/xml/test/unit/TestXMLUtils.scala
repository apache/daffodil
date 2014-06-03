package edu.illinois.ncsa.daffodil.xml.test.unit

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.NS
import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.Implicits._

class TestXMLUtils {

  @Test def testDiff0() {
    val d1 = new Text("a")
    val d2 = new Text("b")
    val diffs = XMLUtils.computeTextDiff("", d1, d2)
    val Seq((p, a, b)) = diffs
    assertEquals(".charAt(1)", p)
    assertEquals("a", a)
    assertEquals("b", b)
  }

  @Test def testDiff1() {
    val d1 = <d>a</d>
    val d2 = <d>b</d>
    val diffs = XMLUtils.computeDiff(d1, d2)
    val Seq((p1, a, b)) = diffs
    assertEquals("d.charAt(1)", p1)
    assertEquals("a", a)
    assertEquals("b", b)
  }

  @Test def testDiff2() {
    val d1 = <a><d>a</d><d>x</d></a>
    val d2 = <a><d>a</d><d>y</d></a>
    val diffs = XMLUtils.computeDiff(d1, d2)
    val Seq((p1, a, b)) = diffs
    assertEquals("a/d[2].charAt(1)", p1)
    assertEquals("x", a)
    assertEquals("y", b)
  }

  @Test def testDiff3() {
    val d1 = <a><d>a</d><e>e</e><d>xxx</d><e>e</e></a>
    val d2 = <a><d>a</d><e>f</e><d>xxy</d><e>e</e></a>
    val diffs = XMLUtils.computeDiff(d1, d2)
    val Seq((p1, e, f), (p2, x, y)) = diffs
    assertEquals("a/e[1].charAt(1)", p1)
    assertEquals("e", e)
    assertEquals("f", f)
    assertEquals("a/d[2].charAt(3)", p2)
    assertEquals("x", x)
    assertEquals("y", y)

  }

  @Test def testNilDiff1() {
    val d1 = <a xmlns:xsi={ XMLUtils.XSI_NAMESPACE } xsi:nil="true" foo="bar"/>
    val d2 = <a baz="quuxly"/>
    val d1NoA = XMLUtils.removeAttributes(d1)
    // println(d1NoA)
    val d2NoA = XMLUtils.removeAttributes(d2)
    val diffs = XMLUtils.computeDiff(d1NoA, d2NoA)
    // println(diffs)
    val Seq((path, d1attribs, d2attribs)) = diffs
    assertEquals("", path)
    // for whatever reason, an attribute prints with a leading space or something.
    // so we make the comparison be a 'contains' style comparison.
    assertTrue(d1attribs.contains("xsi:nil=\"true\""))
    assertEquals("", d2attribs)
  }

  @Test def testIsNil() {
    val d1 = XMLUtils.elem2Element(<a xmlns:xsi={ XMLUtils.XSI_NAMESPACE } xsi:nil="true"/>)
    val d2 = XMLUtils.elem2Element(<a xmlns:xsi={ XMLUtils.XSI_NAMESPACE }>foo</a>)
    assertTrue(XMLUtils.isNil(d1))
    assertFalse(XMLUtils.isNil(d2))
  }

  @Test def testIsHidden() {
    val hid = <foo dafint:hidden="true" xmlns:dafint={ XMLUtils.INT_NS }/>
    val nid = <foo/>
    assertTrue(XMLUtils.isHidden(hid))
    assertFalse(XMLUtils.isHidden(nid))
  }

  @Test def testRemoveHidden() {
    val hid = <bar><baz/><foo dafint:hidden="true" xmlns:dafint={ XMLUtils.INT_NS }/></bar>
    // println("with hidden " + hid)
    val rid = XMLUtils.removeHiddenElements(hid)
    assertFalse(rid.toString.contains("hidden"))
  }

  @Test def testRemapXMLIllegalCharToPUA() {
    val ec = XMLUtils.remapXMLIllegalCharToPUA(0x0, false)
    assertEquals(0xE000, ec)
    val ed = XMLUtils.remapXMLIllegalCharToPUA(0xd880, false)
    assertEquals(0xE880, ed)
  }

  @Test def testWalkUnicodeString1() {
    val s = "abc"
    val Seq((ab, 'a', 'b'), ('a', 'b', 'c'), ('b', 'c', ca)) = XMLUtils.walkUnicodeString(s)((p, c, n) => (p, c, n))
    assertEquals(0.toChar, ab)
    assertEquals(0.toChar, ca)
  }

  @Test def testWalkUnicodeString2() {
    val s = ""
    val Seq() = XMLUtils.walkUnicodeString(s)((p, c, n) => (p, c, n))
  }

  @Test def testRemoveAttributes1() {
    val xml = <test:bar xmlns:test="http://test/" xmlns:test2="http://test2/" xmlns:dafint={ XMLUtils.INT_NS } xmlns:xsi={ XMLUtils.XSI_NAMESPACE }>
                <test2:foo dafint:qaz="qaz" test:raz="raz" xsi:nil="true"/>
              </test:bar>
    val res = XMLUtils.removeAttributes(xml)
    assertEquals(<bar><foo xsi:nil="true"/></bar>, Utility.trim(res))
  }

  @Test def testRemoveAttributes2() {
    val xml = <test:bar xmlns:test="http://test/" xmlns:test2="http://test2/" xmlns:dafint={ XMLUtils.INT_NS } xmlns:xsi={ XMLUtils.XSI_NAMESPACE }>
                <test2:foo dafint:qaz="qaz" test:raz="raz" xsi:nil="true"/>
              </test:bar>
    val res = XMLUtils.removeAttributes(xml, Seq(NS("http://test2/"), NS(XMLUtils.INT_NS)))
    println(res)
    assertEquals(<test:bar xmlns:test="http://test/" xmlns:xsi={ XMLUtils.XSI_NAMESPACE }><foo test:raz="raz" xsi:nil="true"/></test:bar>, Utility.trim(res))
  }

}
