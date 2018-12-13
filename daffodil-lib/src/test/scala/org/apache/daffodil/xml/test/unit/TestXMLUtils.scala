/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.xml.test.unit

import scala.xml._
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.xml.JDOMUtils
import org.apache.daffodil.xml.NS
import junit.framework.Assert._
import org.junit.Test
import org.apache.daffodil.Implicits._

class TestXMLUtils {

  @Test def testDiff0() {
    val d1 = new Text("a")
    val d2 = new Text("b")
    val diffs = XMLUtils.computeTextDiff("", d1, d2, None)
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
    val d2 = <a dafint:col="30" xmlns:dafint={ XMLUtils.INT_NS }/>
    val diffs = XMLUtils.computeDiff(d1, d2)
    val Seq((path, d1attribs, d2attribs)) = diffs
    assertEquals("/a@xsi:nil", path)
    // for whatever reason, an attribute prints with a leading space or something.
    // so we make the comparison be a 'contains' style comparison.
    assertTrue(d1attribs.contains("true"))
    assertEquals("", d2attribs)
  }

  @Test def testIsNil() {
    val d1 = JDOMUtils.elem2Element(<a xmlns:xsi={ XMLUtils.XSI_NAMESPACE } xsi:nil="true"/>)
    val d2 = JDOMUtils.elem2Element(<a xmlns:xsi={ XMLUtils.XSI_NAMESPACE }>foo</a>)
    assertTrue(JDOMUtils.isNil(d1))
    assertFalse(JDOMUtils.isNil(d2))
  }

  @Test def testRemapXMLIllegalCharToPUA() {
    val ec = XMLUtils.remapXMLIllegalCharToPUA(false)(0x0)
    assertEquals(0xE000, ec)
    val ed = XMLUtils.remapXMLIllegalCharToPUA(false)(0xd880)
    assertEquals(0xE880, ed)
  }

  @Test def testRemapPUAToXMLIllegalChar() {
    val ec = XMLUtils.remapPUAToXMLIllegalChar(false)(0xE000)
    assertEquals(0x0, ec)
    val ed = XMLUtils.remapPUAToXMLIllegalChar(false)(0xE880)
    assertEquals(0xD880, ed)
  }

  @Test def testWalkUnicodeString1() {
    val s = "abc"
    val Seq((ab, 'a', 'b'), ('a', 'b', 'c'), ('b', 'c', ca)) = XMLUtils.walkUnicodeString(s)((p, c, n) => (p, c, n))
    assertEquals(0.toChar, ab)
    assertEquals(0.toChar, ca)
  }

  @Test def testWalkUnicodeString2() {
    val s = ""
    XMLUtils.walkUnicodeString(s)((p, c, n) => (p, c, n)) match {
      case Seq() => // ok
    }
  }

  @Test def testRemoveAttributes1() {
    val xml = <test:bar xmlns:test="http://test/" xmlns:test2="http://test2/" xmlns:dafint={ XMLUtils.INT_NS } xmlns:xsi={ XMLUtils.XSI_NAMESPACE }>
                <test2:foo dafint:qaz="qaz" dafint:line="300" xsi:nil="true"/>
              </test:bar>
    val res = XMLUtils.removeAttributes(xml)
    assertEquals(<bar><foo xsi:nil="true"/></bar>, Utility.trim(res))
  }

  @Test def testRemoveAttributes2() {
    val xml = <test:bar xmlns:test="http://test/" xmlns:test2="http://test2/" xmlns:dafint={ XMLUtils.INT_NS } xmlns:xsi={ XMLUtils.XSI_NAMESPACE }>
                <test2:foo dafint:qaz="qaz" test:raz="raz" xsi:nil="true"/>
              </test:bar>
    val res = XMLUtils.removeAttributes(xml, Seq(NS("http://test2/"), XMLUtils.INT_NS))
    assertEquals(<test:bar xmlns:test="http://test/" xmlns:xsi={ XMLUtils.XSI_NAMESPACE }><foo test:raz="raz" xsi:nil="true"/></test:bar>, Utility.trim(res))
  }

  @Test def testRemoveAttributes3() {
    val xml = <foo xsi:nil="true"/>

    val res = XMLUtils.removeAttributes(xml)
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
    assertTrue(xml.child(0).isInstanceOf[Text])
    assertTrue(xml.child(1).isInstanceOf[PCData])
    assertTrue(xml.child(2).isInstanceOf[Text])
    assertTrue(xml.child(3).isInstanceOf[Text])
    assertTrue(xml.child(4).isInstanceOf[Text])

    val res = XMLUtils.coalesceAdjacentTextNodes(xml.child)
    assertEquals(3, res.length)

    assertEquals("abc", res(0).toString)
    assertEquals("<![CDATA[&&&]]>", res(1).toString)
    assertEquals("def" + 0xE000.toChar + "ghi", res(2).toString)

    assertEquals("abc", res(0).text)
    assertEquals("&&&", res(1).text)
    assertEquals("def" + 0xE000.toChar + "ghi", res(2).text)
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
    assertEquals(5, xml.child.length)
    assertFalse(xml.child.forall { _.isInstanceOf[Text] })
    assertTrue(xml.child(1).isInstanceOf[PCData])
    val res = XMLUtils.coalesceAdjacentTextNodes(xml.child)
    assertEquals(3, res.length)
    assertEquals("abc<![CDATA[&&&]]>def" + 0xE000.toChar + "ghi", res.mkString)
    assertEquals("abc&&&def" + 0xE000.toChar + "ghi", res.text)
  }

  @Test def testStandardLoaderCoalesceText() {
    val xmlRaw = """<foo>abc<![CDATA[&&&]]>def&#xE000;ghi</foo>"""
    // This is the way we load XML for DFDL Schemas
    val xml = scala.xml.XML.loadString(xmlRaw)
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
    assertEquals(1, xml.child.length)
    assertTrue(xml.child(0).isInstanceOf[PCData])
    val res = XMLUtils.coalesceAdjacentTextNodes(xml.child)
    assertEquals(1, res.length)
    assertTrue(res(0).isInstanceOf[PCData])
    assertEquals("<![CDATA[&&&]]>", res(0).toString)
    assertEquals("&&&", res(0).text)
  }

  @Test def testEscapeLineEndings() {
    val input = "abc\r\ndef\rghi\njkl\tmno\u0085pqr"
    val actual = XMLUtils.escape(input).toString()
    assertEquals("abc&#xE00D;&#xA;def&#xE00D;ghi&#xA;jkl&#x9;mno&#x85;pqr", actual)
  }

  @Test def testEscape0To127() {
    val input = (0 to 127).map { _.toChar }.mkString
    val actual = XMLUtils.escape(input).toString()
    val expected = "&#xE000;&#xE001;&#xE002;&#xE003;&#xE004;&#xE005;&#xE006;&#xE007;&#xE008;" + // first batch of C0 controls
      "&#x9;&#xA;" + // Tab and LF
      "&#xE00B;&#xE00C;" + // more C0 controls
      "&#xE00D;" + // CR
      // Even more of the C0 controls.
      "&#xE00E;&#xE00F;&#xE010;&#xE011;&#xE012;&#xE013;&#xE014;&#xE015;&#xE016;&#xE017;&#xE018;&#xE019;&#xE01A;&#xE01B;&#xE01C;&#xE01D;&#xE01E;&#xE01F;" +
      "&#x20;" + // space is whitespace comes through numeric.
      "!&quot;#$%&amp;" + // XML Entities for quot, amp
      "&#x27;" + // numeric entity for apos aka single quote (because &apos; is not universal, i.e., not in HTML
      "()*+,-./0123456789:;&lt;=&gt" + // XML entities for lt, gt
      ";?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[" + // all printing characters
      "\\" + // backslash char needs escape. This is ONE character
      "]^_`abcdefghijklmnopqrstuvwxyz{|}~" + // all printing characters
      "&#x7F;" // DEL is a control char, so numeric entity for that too.
    assertEquals(expected, actual)
  }

  @Test def testEscape128To255() {
    val input = (128 to 255).map { _.toChar }.mkString
    val actual = XMLUtils.escape(input).toString()
    val expected = "&#x80;&#x81;&#x82;&#x83;&#x84;&#x85;&#x86;&#x87;&#x88;&#x89;&#x8A;&#x8B;&#x8C;&#x8D;&#x8E;&#x8F;&#x90;&#x91;&#x92;&#x93;&#x94;&#x95;&#x96;&#x97;&#x98;&#x99;&#x9A;&#x9B;&#x9C;&#x9D;&#x9E;&#x9F;&#xA0;¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"
    assertEquals(expected, actual)
  }
}
