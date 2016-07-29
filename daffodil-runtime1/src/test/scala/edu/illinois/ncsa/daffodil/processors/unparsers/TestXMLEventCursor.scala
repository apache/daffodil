/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors.unparsers

import org.junit.Test
import org.junit.Assert._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.Implicits._
import scala.io.Source
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.xml._
import XMLEvent._
import edu.illinois.ncsa.daffodil.util.IteratorFromCursor
import edu.illinois.ncsa.daffodil.processors.DINode

object Start {
  def apply(node: DINode) = InfosetAccessor(StartKind, node)
  def unapply(ev: InfosetAccessor) = if (ev.kind eq StartKind) Some(ev.node) else None
}

object End {
  def apply(node: DINode) = InfosetAccessor(EndKind, node)
  def unapply(ev: InfosetAccessor) = if (ev.kind eq EndKind) Some(ev.node) else None
}

class FakeXMLEventCursor(nRepeats: Long,
  override val advanceAccessor: XMLAccessor) extends XMLEventCursor {

  private val sz = 2 + (3 * nRepeats)
  private val source = Source.fromString(<bar xmlns={ XMLUtils.EXAMPLE_NAMESPACE }><foo>Hello</foo></bar>.toString)

  var dataSize: Long = 0
  override val inspectAccessor = new XMLAccessor
  private val cursor = new XMLEventCursorFromInput(source, false, advanceAccessor, inspectAccessor)

  private def nextItem() = {
    cursor.advance;
    advanceAccessor.cpy()
  }
  private val e1 = nextItem()
  private val e2 = nextItem()
  private val e3 = nextItem()
  private val e4 = nextItem()
  private val e5 = nextItem()

  //  private def repeating(n: Long): Stream[XMLAccessor] = {
  //    if (n == 0) Stream()
  //    else e2 #:: e3 #:: e4 #:: repeating(n - 1)
  //  }
  //
  //  private var str = e1 #:: repeating(sz) ++ Stream(e5)

  override def advance = {
    if (dataSize == sz) false
    else {
      dataSize += 1
      val p = ((dataSize - 1) % 3)
      dataSize match {
        case 1 => advanceAccessor.event = e1.event
        //        case 2 => advanceAccessor.event = e2.event
        //        case 3 => advanceAccessor.event = e3.event
        //        case 4 => advanceAccessor.event = e4.event
        case `sz` => advanceAccessor.event = e5.event
        case _ if (p == 0) => advanceAccessor.event = e4.event
        case _ if (p == 1) => advanceAccessor.event = e2.event
        case _ if (p == 2) => advanceAccessor.event = e3.event
        case _ => Assert.impossibleCase()
      }
      true
    }
  }

  private val saveAccessor = new XMLAccessor

  override def inspect = {
    saveAccessor.assignFrom(advanceAccessor)
    val result = advance
    if (result) {
      dataSize -= 1
      inspectAccessor.assignFrom(advanceAccessor)
      advanceAccessor.assignFrom(saveAccessor)
    }
    result
  }

  override def getXMLErrorInfo(pos: Int, str: String): String = ???

  override def fini: Unit = {}
}

class TestXMLEventCursor {

  @Test def testFakeXMLEventCursor1 {
    val n = 100
    val expected = (n * 3) + 2
    var cnt = 0
    val cur = new FakeXMLEventCursor(n, new XMLAccessor)
    while (cur.advance) cnt += 1
    assertEquals(expected, cnt)
  }

  @Test def testFakeXMLEventCursor2 {
    val n = 3
    val cur = new FakeXMLEventCursor(n, new XMLAccessor)
    val list = (new IteratorFromCursor[XMLAccessor, XMLAccessor](cur, (a: XMLAccessor) => a)).toStream.toList
    val List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11) = list.map { _.event }
    e1 match { case EvStart(null, "bar", _, _) => /* ok */ ; case _ => fail() }
    e2 match { case EvStart(null, "foo", _, _) => /* ok */ ; case _ => fail() }
    e3 match { case EvText("Hello") => /* ok */ ; case _ => fail() }
    e4 match { case EvEnd(null, "foo", _) => /* ok */ ; case _ => fail() }
    e5 match { case EvStart(null, "foo", _, _) => /* ok */ ; case _ => fail() }
    e6 match { case EvText("Hello") => /* ok */ ; case _ => fail() }
    e7 match { case EvEnd(null, "foo", _) => /* ok */ ; case _ => fail() }
    e8 match { case EvStart(null, "foo", _, _) => /* ok */ ; case _ => fail() }
    e9 match { case EvText("Hello") => /* ok */ ; case _ => fail() }
    e10 match { case EvEnd(null, "foo", _) => /* ok */ ; case _ => fail() }
    e11 match { case EvEnd(null, "bar", _) => /* ok */ ; case _ => fail() }

  }

  //  def verify(iterator: Iterator[XMLInfosetEvent]) {
  //    assertTrue(iterator.hasNext)
  //    var ev = iterator.next
  //    ev match { case StartComplex("http://example.com", "bar") => /* ok */ ; case _ => fail() }
  //    assertTrue(iterator.hasNext)
  //    var stop: Boolean = false
  //    while (iterator.hasNext && !stop) {
  //      ev = iterator.next
  //      ev match {
  //        case Simple("http://example.com", "foo", "Hello") => //ok
  //        case EndComplex("http://example.com", "bar") => stop = true
  //        case _ => Assert.invariantFailed("not possible")
  //      }
  //    }
  //    assertFalse(iterator.hasNext)
  //  }
  //
  //  @Test def testStreamingBehavior3() {
  //    val rdr = new FakeXMLEventCursor(1000000, new XMLAccessor)
  //    val iterator = new XMLInfosetEventIterator(rdr)
  //    verify(iterator)
  //  }
  //
  //  // @Test // uncomment to run a test that lasts around a minute so you can
  //  // watch the non-leaking behavior on jvisualvm.
  //  def testStreamingBehavior4() {
  //    val rdr = new FakeXMLEventCursor(100000000, new XMLAccessor)
  //    val iterator = new XMLInfosetEventIterator(rdr)
  //    verify(iterator)
  //  }

}
