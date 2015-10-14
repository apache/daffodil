package edu.illinois.ncsa.daffodil.xml

import org.junit.Test
import org.junit.Assert._
import scala.xml.pull._
import scala.io.Source
import scala.xml.pull._

class TestXMLPullParser {

  val WS = """\s*""".r.pattern.matcher("")

  @Test def test1() {
    val source = Source.fromString("""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
 <?instruction custom value="customvalue"?>
 <foo>Hello<!-- this is a comment --><bar>&gt;</bar>
    </foo>""")
    val reader = new XMLPullParser(source)
    assertTrue(reader.hasNext)
    var ev = reader.next
    ev match { case EvProcInstr("instruction", "custom value=\"customvalue\"") => /* ok */ }
    println(ev)
    ev = reader.next
    val EvText(s) = ev
    assertTrue(WS.reset(s).matches)
    ev = reader.next
    ev match { case EvElemStart(null, "foo", _, _) => /* ok */ }
    println(ev)
    ev = reader.next
    ev match { case EvText("Hello") => /* ok */ }
    ev = reader.next
    ev match { case EvComment(" this is a comment ") => /* ok */ }
    ev = reader.next
    ev match { case EvElemStart(null, "bar", _, _) => /* ok */ }
    ev = reader.next
    ev match { case EvEntityRef("gt") => /* ok */ }
    ev = reader.next
    ev match { case EvElemEnd(null, "bar") => /* ok */ }
    ev = reader.next
    val EvText(s2) = ev
    assertTrue(WS.reset(s2).matches)
    ev = reader.next
    ev match { case EvElemEnd(null, "foo") => /* ok */ }
    assertFalse(reader.hasNext)
  }
}
