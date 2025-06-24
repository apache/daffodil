/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.core.infoset

import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.infoset._
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.SequenceRuntimeData

import org.junit.Assert._
import org.junit.Test

object INoWarnU1 { ImplicitsSuppressUnusedImportWarning() }

class TestInfosetInputterFromReader2 {

  def infosetUnlimitedSource(size: Int) = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
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

    def foos: LazyList[String] = "<foo>Hello</foo>" #:: foos
    val ex = XMLUtils.EXAMPLE_NAMESPACE.toString
    def strings =
      (("<bar xmlns='" + ex + "' >") #:: foos.take(size))

    val is = new StreamInputStream(strings)
    val inputter = new InfosetInputter(new XMLTextInfosetInputter(is))
    inputter.initialize(rootERD, u.tunables)
    val ic = Adapter(inputter)
    (ic, rootERD, inputter)
  }

  class StreamInputStream(private var strings: LazyList[String]) extends java.io.InputStream {

    private var bytes = {
      val ss = strings.flatMap { _.getBytes() } ++ "</bar>".getBytes().to(LazyList)
      strings = Nil.to(LazyList)
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

    override def close(): Unit = { bytes = Nil.to(LazyList) }
  }

  @Test def testStreamingBehavior1(): Unit = {
    val count = 100
    doTest(count)
  }

  def doTest(count: Int): Unit = {
    val (is, rootERD, inp) = infosetUnlimitedSource(count)
    val Some(barSeqTRD: SequenceRuntimeData) = rootERD.optComplexTypeModelGroupRuntimeData
    val Seq(fooERD: ElementRuntimeData) = barSeqTRD.groupMembers
    inp.pushTRD(rootERD)
    val Start(bar_s: DIComplex) = is.next()
    inp.pushTRD(barSeqTRD)
    inp.pushTRD(fooERD)
    val StartArray(foo_arr_s) = is.next()
    (1 to count).foreach { i =>
      val Start(foo_1_s: DISimple) = is.next()
      val End(foo_1_e: DISimple) = is.next()
      assertTrue(foo_1_s eq foo_1_e)
      assertTrue(foo_1_s.dataValue.getAnyRef.isInstanceOf[String])
      assertEquals("Hello", foo_1_s.dataValueAsString)
    }
    val EndArray(foo_arr_e) = is.next()
    inp.popTRD()
    inp.popTRD()
    val End(bar_e: DIComplex) = is.next()
    inp.popTRD()
    assertFalse(is.hasNext)
    assertTrue(bar_s eq bar_e) // exact same object
    assertTrue(foo_arr_s eq foo_arr_e)
  }

  // @Test // uncomment to watch storage on jvisualvm to convince self of non-leaking.
  def testStreamingBehavior2(): Unit = {
    val count = 100000000
    doTest(count)
  }
}
