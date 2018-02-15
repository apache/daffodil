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

package org.apache.daffodil.infoset

import org.junit.Test
import org.junit.Assert._
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.Implicits._
import org.apache.daffodil.equality._; object ENoWarnU1 { EqualitySuppressUnusedImportWarning() }
import org.apache.daffodil.util.SchemaUtils
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.xml._

/*
 * These are all tests of default-value insertion.
 * But I determined that the approach to defaulting,
 * which was a schema-aware separate pass, can't really work
 * and isn't worth a whole pass on its own.
 *
 * So these tests will have to be revisited someday when we
 * have defaulting working.
 *
 * The way these tests work is broken. They are assuming that
 * defaulting is being inserted by the XMLEventCursor. It will
 * not be there. Defaulting will be done as part of Unparsing.
 */
class TestInfosetDefaultingInUnparser {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testDefaultable = {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator=",">
            <xs:element name="foo" dfdl:lengthKind="delimited" type="xs:string" default="abcde"/>
            <xs:element name="afterFoo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
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

    val xml = <bar xmlns={ XMLUtils.EXAMPLE_NAMESPACE }><afterFoo>Hello</afterFoo></bar>

    val rootERD = u.ssrd.elementRuntimeData

    val inputter = new ScalaXMLInfosetInputter(xml)
    inputter.initialize(rootERD, u.getTunables())
    val is = Adapter(inputter)

    val Start(bar_s: DIComplex) = is.next
    assertNotNull(bar_s)
    val Start(foo_s: DISimple) = is.next
    val End(foo_e: DISimple) = is.next
    assertNotNull(foo_e)
    val Start(afterFoo_s: DISimple) = is.next
    val End(afterFoo_e: DISimple) = is.next
    assertNotNull(afterFoo_e)
    val End(bar_e: DIComplex) = is.next
    assertNotNull(bar_e)

    assertEquals("abcde", foo_s.dataValue)
    assertEquals("Hello", afterFoo_s.dataValue)
  }

}
