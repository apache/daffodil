package daffodil.xml.test.unit

import scala.xml._
import daffodil.xml.XMLUtils
import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import org.junit.Test

class TextXMLUtils extends JUnitSuite {

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
}