package edu.illinois.ncsa.daffodil.processors.unparsers

import org.junit.Test
import org.junit.Assert._
import scala.xml.pull._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.Implicits._
import scala.io.Source
import edu.illinois.ncsa.daffodil.xml.XMLPullParser
import org.xml.sax.SAXException

class TestXMLEventIterator {

  @Test def testPreprocess1() {
    val source = Source.fromString("""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
 <?instruction custom value="customvalue"?>
 <foo>Hello<!-- this is a comment --><bar>&bar;</bar>
    <bar>&gt;<!-- comment next to text --></bar>
    </foo>""")
    val reader = (new XMLPullParser(source))
    val iterator = new XMLEventIterator(null)
    val preProc = iterator.preprocess(reader.toStream)

    assertTrue(!preProc.find { _.isInstanceOf[EvComment] }.isDefined)
    assertTrue(!preProc.find { _.isInstanceOf[EvProcInstr] }.isDefined)
  }

  @Test def testfindMode1() {
    import XMLEventIterator._
    val source = Source.fromString("""<foo>Hello</foo>""")
    val reader = new XMLPullParser(source)
    val iterator = new XMLEventIterator(null)
    val m = iterator.findMode(reader.toStream)
    assertEquals(SimpleContent, m)
  }

  @Test def testfindMode2() {
    import XMLEventIterator._
    val source = Source.fromString("""<foo> </foo>""")
    val reader = new XMLPullParser(source)
    val iterator = new XMLEventIterator(null)
    val m = iterator.findMode(reader.toStream)
    assertEquals(SimpleContent, m)
  }

  @Test def testfindMode3() {
    import XMLEventIterator._
    val source = Source.fromString("""<foo><bar>Hello</bar></foo>""")
    val reader = new XMLPullParser(source)
    val iterator = new XMLEventIterator(null)
    val m = iterator.findMode(reader.toStream)
    assertEquals(ElementOnly, m)
  }

  @Test def testfindMode4() {
    import XMLEventIterator._
    val source = Source.fromString("""<foo>
        <bar>Hello</bar></foo>""")
    val reader = new XMLPullParser(source)
    val iterator = new XMLEventIterator(null)
    val tl = reader.toStream
    val m = iterator.findMode(tl)
    assertEquals(ElementOnly, m)
    val EvText(s) = tl.tail.head
    assertTrue(s.matches("""\s+"""))
  }

  @Test def testfindMode5() {
    import XMLEventIterator._
    val source = Source.fromString("""<foo>
        Hello
        </foo>""")
    val reader = new XMLPullParser(source)
    val iterator = new XMLEventIterator(null)
    val tl = reader.toStream
    val m = iterator.findMode(tl)
    assertEquals(SimpleContent, m)
    val EvText(s) = tl.tail.head
    assertTrue(s.matches("""\s+Hello\s+"""))
  }

  @Test def testIsNil1() {
    import scala.xml.Elem
    val e = <foo xsi:nil="true"/>
    val p = new XMLEventIterator(null).isNil(e.attributes, e.scope)
    assertTrue(p)
  }

  @Test def testIsNil2() {
    import scala.xml.Elem
    val e = <foo xmlns:foobar={ XMLUtils.XSI_NAMESPACE.toString } foobar:nil="true"/>
    val p = new XMLEventIterator(null).isNil(e.attributes, e.scope)
    assertTrue(p)
  }

  @Test def testIsNil3() {
    import scala.xml.Elem
    val e = <foo/>
    val p = new XMLEventIterator(null).isNil(e.attributes, e.scope)
    assertFalse(p)
  }

  @Test def testTransNil1() {
    val source = Source.fromString(<foo xsi:nil="true"/>.toString)
    val reader = new XMLPullParser(source)
    val iterator = new XMLEventIterator(reader)
    assertTrue(iterator.hasNext)
    val ev = iterator.next
    val NilElt(ns, local) = ev
    assertNull(ns)
    assertEquals("foo", local)
    assertFalse(iterator.hasNext)
  }

  @Test def testTransSimple1() {
    val source = Source.fromString(<foo>Hello</foo>.toString)
    val reader = new XMLPullParser(source)
    val iterator = new XMLEventIterator(reader)
    assertTrue(iterator.hasNext)
    val ev = iterator.next
    val Simple(ns, local, text) = ev
    assertNull(ns)
    assertEquals("foo", local)
    assertFalse(iterator.hasNext)
    assertEquals("Hello", text)
  }

  @Test def testTransSimpleCData1() {
    val source = Source.fromString("""<foo><![CDATA[<foo>&amp;</foo>]]></foo>""")
    val reader = new XMLPullParser(source)
    val iterator = new XMLEventIterator(reader)
    assertTrue(iterator.hasNext)
    val ev = iterator.next
    val Simple(ns, local, text) = ev
    assertNull(ns)
    assertEquals("foo", local)
    assertFalse(iterator.hasNext)
    assertEquals("<foo>&amp;</foo>", text)
  }

  @Test def testTransSimple2() {
    val source = Source.fromString(<foo>Hello&amp;World</foo>.toString)
    val reader = new XMLPullParser(source)
    val iterator = new XMLEventIterator(reader)
    assertTrue(iterator.hasNext)
    val Simple(null, "foo", "Hello&World") = iterator.next
    assertFalse(iterator.hasNext)
  }

  @Test def testTransComplex1() {
    val source = Source.fromString(<foo><bar/></foo>.toString)
    val reader = new XMLPullParser(source)
    val iterator = new XMLEventIterator(reader)
    assertTrue(iterator.hasNext)
    var ev = iterator.next
    val StartComplex(null, "foo") = ev
    assertTrue(iterator.hasNext)
    ev = iterator.next
    val Simple(null, "bar", "") = ev
    assertTrue(iterator.hasNext)
    ev = iterator.next
    val EndComplex(null, "foo") = ev
    assertFalse(iterator.hasNext)
  }

  @Test def testTransComplex2() {
    val source = Source.fromString("""<foo>
        <bar/>
        </foo>""")
    val reader = new XMLPullParser(source)
    val iterator = new XMLEventIterator(reader)
    assertTrue(iterator.hasNext)
    var ev = iterator.next
    val StartComplex(null, "foo") = ev
    assertTrue(iterator.hasNext)
    ev = iterator.next
    val Simple(null, "bar", "") = ev
    assertTrue(iterator.hasNext)
    ev = iterator.next
    val EndComplex(null, "foo") = ev
    assertFalse(iterator.hasNext)
  }

  @Test def testTransComplex3() {
    val source = Source.fromString("""<foo>
        <bar/>
        <baz>Hello&amp;World</baz>
        </foo>""")
    val reader = new XMLPullParser(source)
    val iterator = new XMLEventIterator(reader)
    assertTrue(iterator.hasNext)
    var ev = iterator.next
    val StartComplex(null, "foo") = ev
    assertTrue(iterator.hasNext)
    ev = iterator.next
    val Simple(null, "bar", "") = ev
    assertTrue(iterator.hasNext)
    ev = iterator.next
    val Simple(null, "baz", "Hello&World") = ev
    assertTrue(iterator.hasNext)
    ev = iterator.next
    val EndComplex(null, "foo") = ev
    assertFalse(iterator.hasNext)
  }

  /**
   * This test shows that exceptions thrown by the XMLPullParser
   * can be caught and handled locally here.
   *
   * (That is, they're not lost due to the actual parser
   * executing on a different thread/co-routine)
   */
  @Test def testTransSimpleMisMatch() {
    val source = Source.fromString("""<foo>Hello</bar>""")
    val reader = new XMLPullParser(source)
    val iterator = new XMLEventIterator(reader)
    val e = intercept[scala.xml.parsing.FatalError] {
      assertTrue(iterator.hasNext)
      val ev = iterator.next
    }
    assertFalse(iterator.hasNext)
    val msg = e.toString
    assertTrue(msg.contains("closing tag"))
    assertTrue(msg.contains("foo"))
  }

  @Test def testTransNoWellFormed1() {
    val source = Source.fromString("""<foo>Hello""")
    val reader = new XMLPullParser(source)
    val iterator = new XMLEventIterator(reader)
    val e = intercept[scala.xml.parsing.FatalError] {
      assertTrue(iterator.hasNext)
      val ev = iterator.next
    }
    assertFalse(iterator.hasNext)
    val msg = e.toString
    assertTrue(msg.contains("closing tag"))
    assertTrue(msg.contains("foo"))
  }

  @Test def testTransNotWellFormed2() {
    val source = Source.fromString("""<<foo>Hello""")
    val reader = new XMLPullParser(source)
    val iterator = new XMLEventIterator(reader)
    val e = intercept[SAXException] {
      while (iterator.hasNext) {
        iterator.next
      }
    }
    val msg = e.toString
    assertTrue(msg.contains("Syntax Error"))
  }
}