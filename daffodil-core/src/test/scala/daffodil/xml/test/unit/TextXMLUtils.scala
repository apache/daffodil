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
  
}