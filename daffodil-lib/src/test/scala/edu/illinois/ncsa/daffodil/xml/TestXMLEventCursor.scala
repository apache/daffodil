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
