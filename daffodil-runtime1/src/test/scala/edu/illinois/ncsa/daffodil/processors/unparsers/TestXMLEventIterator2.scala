package edu.illinois.ncsa.daffodil.processors.unparsers

import org.junit.Test
import org.junit.Assert._
import scala.xml.pull._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.Implicits._
import scala.io.Source
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.xml.XMLPullParser

class FakeXMLEventReader(sz: Long) extends Iterator[scala.xml.pull.XMLEvent] {

  val source = Source.fromString(<bar xmlns={ XMLUtils.EXAMPLE_NAMESPACE }><foo>Hello</foo></bar>.toString)

  var dataSize: Long = 0

  val rdr = new XMLPullParser(source)
  val e1 = rdr.next()
  val e2 = rdr.next()
  val e3 = rdr.next()
  val e4 = rdr.next()
  val e5 = rdr.next()

  def repeating(n: Long): Stream[XMLEvent] = {
    if (n == 0) Stream()
    else e2 #:: e3 #:: e4 #:: repeating(n - 1)
  }

  var str: Stream[XMLEvent] = e1 #:: repeating(sz) ++ Stream(e5)

  // if you want to see it leak memory like crazy, just uncomment this line
  // val leak = str

  override def hasNext: Boolean = {
    !str.isEmpty
  }
  override def next = {
    val res = str.head
    dataSize += 1
    str = str.tail
    res
  }
}

class TestXMLEventIterator2 {

  def verify(iterator: Iterator[XMLInfosetEvent]) {
    assertTrue(iterator.hasNext)
    var ev = iterator.next
    ev match { case StartComplex("http://example.com", "bar") => /* ok */ ; case _ => fail() }
    assertTrue(iterator.hasNext)
    var stop: Boolean = false
    while (iterator.hasNext && !stop) {
      ev = iterator.next
      ev match {
        case Simple("http://example.com", "foo", "Hello") => //ok
        case EndComplex("http://example.com", "bar") => stop = true
        case _ => Assert.invariantFailed("not possible")
      }
    }
    assertFalse(iterator.hasNext)
  }

  @Test def testStreamingBehavior3() {
    val rdr = new FakeXMLEventReader(1000000)
    val iterator = new XMLEventIterator(new FakeXMLEventReader(1000000))
    println("dataSize = " + rdr.dataSize / (1024 * 1024 * 1024) + "G items")
    verify(iterator)
  }

  // @Test // uncomment to run a test that lasts around a minute so you can
  // watch the non-leaking behavior on jvisualvm.
  def testStreamingBehavior4() {
    val rdr = new FakeXMLEventReader(100000000)
    val iterator = new XMLEventIterator(rdr)
    println("dataSize = " + rdr.dataSize / (1024 * 1024 * 1024) + "G items")
    verify(iterator)
  }

}
