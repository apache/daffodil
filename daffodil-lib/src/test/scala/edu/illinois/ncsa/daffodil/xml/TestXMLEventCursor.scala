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

package edu.illinois.ncsa.daffodil.xml

import org.junit.Test
import org.junit.Assert._
import scala.io.Source
import XMLEvent._
import edu.illinois.ncsa.daffodil.util.IteratorFromCursor

class TestXMLEventCursor {

  val WS = """\s*""".r.pattern.matcher("")

  @Test def test1() {
    val source = Source.fromString("""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<?instruction custom value="customvalue"?>
<foo>Hello<!-- this is a comment --><bar>&gt;</bar>
 </foo>""")

    val cursor = new XMLEventCursorFromInput(source, true)

    assertTrue(cursor.advance)
    var ev = cursor.advanceAccessor.event
    assertEquals(EvProcInstr("instruction", "custom value=\"customvalue\""), ev)
    assertTrue(cursor.advance)
    ev = cursor.advanceAccessor.event
    val EvText(s) = ev
    assertTrue(WS.reset(s).matches)
    assertTrue(cursor.advance)
    ev = cursor.advanceAccessor.event
    ev match { case EvStart(null, "foo", _, _) => /* ok */ ; case _ => fail() }
    assertTrue(cursor.advance)
    ev = cursor.advanceAccessor.event
    ev match { case EvText("Hello") => /* ok */ ; case _ => fail() }
    assertTrue(cursor.advance)
    ev = cursor.advanceAccessor.event
    ev match { case EvComment(" this is a comment ") => /* ok */ ; case _ => fail() }
    assertTrue(cursor.advance)
    ev = cursor.advanceAccessor.event
    ev match { case EvStart(null, "bar", _, _) => /* ok */ ; case _ => fail() }
    assertTrue(cursor.advance)
    ev = cursor.advanceAccessor.event
    ev match { case EvEntityRef("gt") => /* ok */ ; case _ => fail() }
    assertTrue(cursor.advance)
    ev = cursor.advanceAccessor.event
    ev match { case EvEnd(null, "bar", _) => /* ok */ ; case _ => fail() }
    assertTrue(cursor.advance)
    ev = cursor.advanceAccessor.event
    val EvText(s2) = ev
    assertTrue(WS.reset(s2).matches)
    assertTrue(cursor.advance)
    ev = cursor.advanceAccessor.event
    ev match { case EvEnd(null, "foo", _) => /* ok */ ; case _ => fail() }
    assertFalse(cursor.advance)
  }

  @Test def test2() {
    val source = Source.fromString("""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
 <?instruction custom value="customvalue"?>
 <foo>Hello<!-- this is a comment --><bar>&gt;</bar>
    </foo>""")

    val cursor = new XMLEventCursorFromInput(source, true)
    //
    // The below is using toStream, which sort of defeats the whole purpose of
    // the XMLEventCursor API, which is to avoid allocating.
    // However, for debugging this is very much easier.
    //
    val list = (new IteratorFromCursor(cursor, (xa: XMLAccessor) => xa.event)).toStream.toList
    val reader = list.toIterator
    assertTrue(reader.hasNext)
    var ev = reader.next
    ev match { case EvProcInstr("instruction", "custom value=\"customvalue\"") => /* ok */ ; case _ => fail() }
    // println(ev)
    ev = reader.next
    val EvText(s) = ev
    assertTrue(WS.reset(s).matches)
    ev = reader.next
    ev match { case EvStart(null, "foo", _, _) => /* ok */ ; case _ => fail() }
    // println(ev)
    ev = reader.next
    ev match { case EvText("Hello") => /* ok */ ; case _ => fail() }
    ev = reader.next
    ev match { case EvComment(" this is a comment ") => /* ok */ ; case _ => fail() }
    ev = reader.next
    ev match { case EvStart(null, "bar", _, _) => /* ok */ ; case _ => fail() }
    ev = reader.next
    ev match { case EvEntityRef("gt") => /* ok */ ; case _ => fail() }
    ev = reader.next
    ev match { case EvEnd(null, "bar", _) => /* ok */ ; case _ => fail() }
    ev = reader.next
    val EvText(s2) = ev
    assertTrue(WS.reset(s2).matches)
    ev = reader.next
    ev match { case EvEnd(null, "foo", _) => /* ok */ ; case _ => fail() }
    assertFalse(reader.hasNext)
  }
}
