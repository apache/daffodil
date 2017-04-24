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

package edu.illinois.ncsa.daffodil.infoset

import org.junit.Test
import org.junit.Assert._
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.util.SchemaUtils
import edu.illinois.ncsa.daffodil.processors.DataProcessor
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import scala.collection.immutable.Stream.consWrapper

object INoWarnU1 { ImplicitsSuppressUnusedImportWarning() }

class TestInfosetInputterFromReader2 {

  def infosetUnlimitedSource(size: Int) = {
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
      val msgs = pf.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    val u = pf.onPath("/").asInstanceOf[DataProcessor]
    if (u.isError) {
      val msgs = u.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    val rootERD = u.ssrd.elementRuntimeData

    def foos: Stream[String] = "<foo>Hello</foo>" #:: foos
    val ex = XMLUtils.EXAMPLE_NAMESPACE.toString
    def strings =
      (("<bar xmlns='" + ex + "' >") #:: foos.take(size))

    val rdr = new java.io.InputStreamReader(
      new StreamInputStream(strings))

    val inputter = new XMLTextInfosetInputter(rdr)
    inputter.initialize(rootERD)
    val ic = Adapter(inputter)
    ic
  }

  class StreamInputStream(
    private var strings: Stream[String]) extends java.io.InputStream {

    private var bytes = {
      val ss = strings.flatMap { _.getBytes() } ++ "</bar>".getBytes().toStream
      strings = Nil.toStream
      ss
    }

    override def read(): Int = {
      if (bytes.isEmpty) -1
      else {
        val b = bytes.head
        bytes = bytes.tail
        b.toInt
      }
    }

    override def close() { bytes = Nil.toStream }
  }

  @Test def testStreamingBehavior1() {
    val count = 100
    val is = infosetUnlimitedSource(count)
    val Start(bar_s: DIComplex) = is.next
    val Start(foo_arr_s: DIArray) = is.next
    1 to count foreach { i =>
      val Start(foo_1_s: DISimple) = is.next
      val End(foo_1_e: DISimple) = is.next
      assertTrue(foo_1_s eq foo_1_e)
      assertTrue(foo_1_s.dataValue.isInstanceOf[String])
      assertEquals("Hello", foo_1_s.dataValueAsString)
    }
    val End(foo_arr_e: DIArray) = is.next
    val End(bar_e: DIComplex) = is.next
    assertFalse(is.hasNext)
    assertTrue(bar_s eq bar_e) // exact same object
    assertTrue(foo_arr_s eq foo_arr_e)
  }

  // @Test // uncomment to watch storage on jvisualvm to convince self of non-leaking.
  def testStreamingBehavior2() {
    val count = 100000000
    val is = infosetUnlimitedSource(count)
    val Start(bar_s: DIComplex) = is.next
    val Start(foo_arr_s: DIArray) = is.next
    1 to count foreach { i =>
      val Start(foo_1_s: DISimple) = is.next
      val End(foo_1_e: DISimple) = is.next
      assertTrue(foo_1_s eq foo_1_e)
      assertTrue(foo_1_s.dataValue.isInstanceOf[String])
      assertTrue(foo_1_s.dataValueAsString =:= "Hello")
      val arr = bar_s.getChildArray(foo_1_s.runtimeData)
      if (arr.length % 100L =#= 0L) {
        // println("array length is " + arr.length)
        bar_s.resetChildArray(0)
        foo_arr_s.reduceToSize(0)
      }
      arr.asInstanceOf[DIArray].children
    }
    val End(foo_arr_e: DIArray) = is.next
    val End(bar_e: DIComplex) = is.next
    assertFalse(is.hasNext)
    assertTrue(bar_s eq bar_e) // exact same object
    assertTrue(foo_arr_s eq foo_arr_e)
  }
}
