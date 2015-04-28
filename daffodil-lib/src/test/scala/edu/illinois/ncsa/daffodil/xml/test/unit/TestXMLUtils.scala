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

import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.JDOMUtils
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
    assertEquals("a(%#x0061;)", a)
    assertEquals("b(%#x0062;)", b)
  }

  @Test def testDiff1() {
    val d1 = <d>a</d>
    val d2 = <d>b</d>
    val diffs = XMLUtils.computeDiff(d1, d2)
    val Seq((p1, a, b)) = diffs
    assertEquals("d.charAt(1)", p1)
    assertEquals("a(%#x0061;)", a)
    assertEquals("b(%#x0062;)", b)
  }

  @Test def testDiff2() {
    val d1 = <a><d>a</d><d>x</d></a>
    val d2 = <a><d>a</d><d>y</d></a>
    val diffs = XMLUtils.computeDiff(d1, d2)
    val Seq((p1, a, b)) = diffs
    assertEquals("a/d[2].charAt(1)", p1)
    assertEquals("x(%#x0078;)", a)
    assertEquals("y(%#x0079;)", b)
  }

  @Test def testDiff3() {
    val d1 = <a><d>a</d><e>e</e><d>xxx</d><e>e</e></a>
    val d2 = <a><d>a</d><e>f</e><d>xxy</d><e>e</e></a>
    val diffs = XMLUtils.computeDiff(d1, d2)
    val Seq((p1, e, f), (p2, x, y)) = diffs
    assertEquals("a/e[1].charAt(1)", p1)
    assertEquals("e(%#x0065;)", e)
    assertEquals("f(%#x0066;)", f)
    assertEquals("a/d[2].charAt(3)", p2)
    assertEquals("x(%#x0078;)", x)
    assertEquals("y(%#x0079;)", y)

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
    val d1 = JDOMUtils.elem2Element(<a xmlns:xsi={ XMLUtils.XSI_NAMESPACE } xsi:nil="true"/>)
    val d2 = JDOMUtils.elem2Element(<a xmlns:xsi={ XMLUtils.XSI_NAMESPACE }>foo</a>)
    assertTrue(JDOMUtils.isNil(d1))
    assertFalse(JDOMUtils.isNil(d2))
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
    val res = XMLUtils.removeAttributes(xml, Seq(NS("http://test2/"), XMLUtils.INT_NS))
    println(res)
    assertEquals(<test:bar xmlns:test="http://test/" xmlns:xsi={ XMLUtils.XSI_NAMESPACE }><foo test:raz="raz" xsi:nil="true"/></test:bar>, Utility.trim(res))
  }

  @Test def testRemoveAttributes3() {
    val xml = <foo xsi:nil="true"/>

    val res = XMLUtils.removeAttributes(xml)
    println(res)
    assertEquals(<foo xsi:nil="true"/>, Utility.trim(res))
  }

  /**
   * These next tests deal with XML and escaping.
   *
   * Turns out different ways of acquiring XML result in some
   * different behaviors.
   */
  @Test def testScalaLiteralXMLCoalesceText() {
    val xml = <foo>abc<![CDATA[&&&]]>def&#xE000;ghi</foo>
    assertEquals(5, xml.child.length)
    assertTrue(xml.child.forall { _.isInstanceOf[Text] })
    val res = XMLUtils.coalesceAdjacentTextNodes(xml.child)
    assertEquals(1, res.length)
    assertEquals("abc&amp;&amp;&amp;def" + 0xE000.toChar + "ghi", res(0).toString)
    assertEquals("abc&&&def" + 0xE000.toChar + "ghi", res(0).text)
  }

  @Test def testConstructingParserCoalesceText() {
    val xmlRaw = """<foo>abc<![CDATA[&&&]]>def&#xE000;ghi</foo>"""
    import scala.xml.parsing.ConstructingParser
    //
    // This is the way we load XML for the TDML runner
    // and it creates PCData nodes where Scala's basic loader
    // and literal XML in scala program text, converts
    // PCData to Text nodes (removing the bracket glop)
    //
    // We use this in the TDML runner because we can 
    // preserve whitespace robustly inside CDATA bracketing.
    // Other ways of loading XML all treat whitespace as
    // somewhat fungible. 
    //
    val parser = ConstructingParser.fromSource(
      scala.io.Source.fromString(xmlRaw), true)
    val xml = parser.document.docElem
    println(xml.text)
    println(xml.toString)
    assertEquals(5, xml.child.length)
    assertFalse(xml.child.forall { _.isInstanceOf[Text] })
    assertTrue(xml.child(1).isInstanceOf[PCData])
    val res = XMLUtils.coalesceAdjacentTextNodes(xml.child)
    println(res)
    assertEquals(3, res.length)
    assertEquals("abc<![CDATA[&&&]]>def" + 0xE000.toChar + "ghi", res.mkString)
    assertEquals("abc&&&def" + 0xE000.toChar + "ghi", res.text)
  }

  @Test def testStandardLoaderCoalesceText() {
    val xmlRaw = """<foo>abc<![CDATA[&&&]]>def&#xE000;ghi</foo>"""
    // This is the way we load XML for DFDL Schemas
    val xml = scala.xml.XML.loadString(xmlRaw)
    println(xml.text)
    println(xml.toString)
    //
    // Scala's scala.xml.XML.loaders do a good job 
    // coalescing adjacent Texts (of all Atom kinds)
    //
    assertEquals(1, xml.child.length)
    val res = XMLUtils.coalesceAdjacentTextNodes(xml.child)
    assertEquals(1, res.length)
    assertEquals("abc&amp;&amp;&amp;def" + 0xE000.toChar + "ghi", res(0).toString)
    assertEquals("abc&&&def" + 0xE000.toChar + "ghi", res(0).text)
  }

  @Test def testOnePCData() {
    val xmlRaw = """<foo><![CDATA[&&&]]></foo>"""
    import scala.xml.parsing.ConstructingParser
    //
    //see testConstructingParserCoalesceText
    //for a relevant comment about this use of the 
    //Constructing parser.
    //
    val parser = ConstructingParser.fromSource(
      scala.io.Source.fromString(xmlRaw), true)
    val xml = parser.document.docElem
    println(xml.text)
    println(xml.toString)
    assertEquals(1, xml.child.length)
    assertTrue(xml.child(0).isInstanceOf[PCData])
    val res = XMLUtils.coalesceAdjacentTextNodes(xml.child)
    println(res)
    assertEquals(1, res.length)
    assertTrue(res(0).isInstanceOf[PCData])
    assertEquals("<![CDATA[&&&]]>", res(0).toString)
    assertEquals("&&&", res(0).text)
  }

}
