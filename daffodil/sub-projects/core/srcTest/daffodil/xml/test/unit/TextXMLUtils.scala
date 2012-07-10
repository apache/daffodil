package daffodil.xml.test.unit


import scala.xml._

import daffodil.xml.XMLUtils
import org.scalatest.junit.JUnit3Suite
import junit.framework.Assert._

class TextXMLUtils extends JUnit3Suite {

  def testDiff1() {
    val d1 = <d><e>abc</e><f>abc</f><g>abc</g></d>
    val d2 = <d><e>abx</e><f>abc</f><g>axc</g></d>
    val diffs = XMLUtils.computeDiff(d1, d2)
    assertEquals(2, diffs.length)
    val Seq((p1, a, b),(p2, c , d)) = diffs
    assertEquals("d/e[2]", p1)
    assertEquals("c", a)
    assertEquals("x", b)
    assertEquals("d/g[1]", p2)
    assertEquals("b", c)
    assertEquals("x", d)
    
  }
  
}