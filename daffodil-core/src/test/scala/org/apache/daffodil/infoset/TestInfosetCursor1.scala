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

import org.junit.Assert.assertTrue
import org.junit.Test
import org.apache.daffodil.Implicits.intercept
import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.util.Misc
import org.apache.daffodil.util.SchemaUtils
import org.apache.daffodil.processors.unparsers.UnparseError

class TestInfosetInputter1 {

  def infosetInputter(testSchema: scala.xml.Node, is: java.io.InputStream) = {
    val compiler = Compiler()
    val pf = compiler.compileNode(testSchema)
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
    val ic = new XMLTextInfosetInputter(is)
    ic.initialize(rootERD, u.getTunables())
    ic
  }

  @Test def testInfosetInputterOnBadData() {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>)

    val str = "this is not XML"
    val is = new java.io.ByteArrayInputStream(str.getBytes("UTF-8"))
    val ic = infosetInputter(sch, is)
    val exc = intercept[UnparseError] {
      ic.advance
    }
    val msg = Misc.getSomeMessage(exc).get
    assertTrue(msg.contains("prolog")) // content not allowed in prolog of an XML document.
  }

  @Test def testInfosetInputterOnBadData2() {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>)

    val str = """<this pretends to be xml"""
    val is = new java.io.ByteArrayInputStream(str.getBytes("UTF-8"))
    val ic = infosetInputter(sch, is)
    val exc = intercept[UnparseError] {
      ic.advance
    }
    val msg = Misc.getSomeMessage(exc).get

    assertTrue(msg.contains("Unexpected character")) // expects an equal sign for an attribute
  }

  @Test def testInfosetInputterOnBadData3() {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>)

    val str = "\u0000\u0000\uFFFF\uFFFF"
    val is = new java.io.ByteArrayInputStream(str.getBytes("UTF-8"))
    val ic = infosetInputter(sch, is)
    val exc = intercept[UnparseError] {
      ic.advance
    }
    val msg = Misc.getSomeMessage(exc).get
    assertTrue(msg.contains("Illegal content")) // content not allowed in prolog.
    assertTrue(msg.contains("Invalid UTF-8 character"))
  }
}
