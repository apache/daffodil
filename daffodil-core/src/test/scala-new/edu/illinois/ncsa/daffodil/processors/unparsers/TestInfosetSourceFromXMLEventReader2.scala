package edu.illinois.ncsa.daffodil.processors.unparsers

import org.junit.Test
import org.junit.Assert._
import scala.xml.pull._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.equality._
import scala.io.Source
import edu.illinois.ncsa.daffodil.util.TestUtils
import edu.illinois.ncsa.daffodil.util.SchemaUtils
import edu.illinois.ncsa.daffodil.processors.DataProcessor
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.processors.DISimple
import edu.illinois.ncsa.daffodil.processors.DIComplex
import edu.illinois.ncsa.daffodil.processors.DIArray
import edu.illinois.ncsa.daffodil.exceptions.Assert

class TestInfosetSourceFromXMLEventReader2 {

  def infosetUnlimitedSource(size: Long) = {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val compiler = Compiler()
    val pf = compiler.compileNode(sch)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    val u = pf.onPath("/").asInstanceOf[DataProcessor]
    if (u.isError) {
      val msgs = u.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }

    val xmlEventReader = new FakeXMLEventReader(size)
    val rootERD = u.ssrd.elementRuntimeData

    val infosetSource = InfosetSource.fromXMLSource(xmlEventReader, rootERD)
    infosetSource
  }

  @Test def testStreamingBehavior1() {
    val count = 100
    val is = infosetUnlimitedSource(count)
    val e1 @ Start(bar_s: DIComplex) = is.next
    val e2 @ Start(foo_arr_s: DIArray) = is.next
    1 to count foreach { i =>
      val e3 @ Start(foo_1_s: DISimple) = is.next
      val e4 @ End(foo_1_e: DISimple) = is.next
      assertTrue(foo_1_s eq foo_1_e)
      assertTrue(foo_1_s.dataValue.isInstanceOf[String])
      assertTrue(foo_1_s.dataValueAsString =:= "Hello")
    }
    val e5 @ End(foo_arr_e: DIArray) = is.next
    val e6 @ End(bar_e: DIComplex) = is.next
    assertFalse(is.hasNext)
    assertTrue(bar_s eq bar_e) // exact same object
    assertTrue(foo_arr_s eq foo_arr_e)
  }

  // @Test // uncomment to watch storage on jvisualvm to convince self of non-leaking. 
  def testStreamingBehavior2() {
    val count = 10000000
    val is = infosetUnlimitedSource(count)
    val e1 @ Start(bar_s: DIComplex) = is.next
    val e2 @ Start(foo_arr_s: DIArray) = is.next
    1 to count foreach { i =>
      val e3 @ Start(foo_1_s: DISimple) = is.next
      val e4 @ End(foo_1_e: DISimple) = is.next
      assertTrue(foo_1_s eq foo_1_e)
      assertTrue(foo_1_s.dataValue.isInstanceOf[String])
      assertTrue(foo_1_s.dataValueAsString =:= "Hello")
      val arr = bar_s.getChildArray(foo_1_s.runtimeData).get
      if (arr.length % 10000L =#= 0L) {
        println("array length is " + arr.length)
      }
      arr.asInstanceOf[DIArray].children
    }
    val e5 @ End(foo_arr_e: DIArray) = is.next
    val e6 @ End(bar_e: DIComplex) = is.next
    assertFalse(is.hasNext)
    assertTrue(bar_s eq bar_e) // exact same object
    assertTrue(foo_arr_s eq foo_arr_e)
  }
}