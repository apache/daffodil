package edu.illinois.ncsa.daffodil.xml

import org.junit.Test
import org.junit.Assert._
import scala.xml.pull._
import edu.illinois.ncsa.daffodil.Implicits._
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
    val EvProcInstr("instruction", "custom value=\"customvalue\"") = ev
    println(ev)
    ev = reader.next
    val EvText(s) = ev
    assertTrue(WS.reset(s).matches)
    ev = reader.next
    val EvElemStart(null, "foo", _, _) = ev
    println(ev)
    ev = reader.next
    val EvText("Hello") = ev
    ev = reader.next
    val EvComment(" this is a comment ") = ev
    ev = reader.next
    val EvElemStart(null, "bar", _, _) = ev
    ev = reader.next
    val EvEntityRef("gt") = ev
    ev = reader.next
    val EvElemEnd(null, "bar") = ev
    ev = reader.next
    val EvText(s2) = ev
    assertTrue(WS.reset(s2).matches)
    ev = reader.next
    val EvElemEnd(null, "foo") = ev
    assertFalse(reader.hasNext)
  }
}
