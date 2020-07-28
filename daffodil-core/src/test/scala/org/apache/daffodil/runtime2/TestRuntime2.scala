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
package org.apache.daffodil.runtime2

import scala.xml.Node
import org.apache.daffodil.compiler.Compiler
import org.junit.Test
import java.io.ByteArrayInputStream

import org.apache.daffodil.util.{SchemaUtils, Misc}

class TestRuntime2 {

  @Test def test1(): Unit = {

    val sch = SchemaUtils.dfdlTestSchema(
        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
        <dfdl:format representation="binary" byteOrder="bigEndian" binaryNumberRep="binary" binaryFloatRep="ieee" ref="tns:GeneralFormat"/>,
        <xs:element name="r">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="e1" type="xs:int"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>,
      elementFormDefault = "unqualified")
    val b = Misc.hex2Bytes("000000FF")
    val inStream = new ByteArrayInputStream(b)
    val code = TestUtilsR2.generate(sch, inStream)
    System.out.println(code)
  }
    // @Test
    def test2(): Unit = {

      val sch = SchemaUtils.dfdlTestSchema(
        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
        <dfdl:format representation="binary" byteOrder="bigEndian" binaryNumberRep="binary" binaryFloatRep="ieee" ref="tns:GeneralFormat"/>,
        <xs:element name="r">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="e1" type="xs:int"/>
              <xs:element name="e2" type="tns:e2Type"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:complexType name="e2Type">
          <xs:sequence>
            <xs:element name="e3" type="xs:int"/>
          </xs:sequence>
        </xs:complexType>,
        elementFormDefault = "unqualified")
      val b = Misc.hex2Bytes("000000FF")
      val inStream = new ByteArrayInputStream(b)
      val code = TestUtilsR2.generate(sch, inStream)
      System.out.println(code)
    }
}

object TestUtilsR2 {

  def generate(testSchema: Node, inStream: java.io.InputStream): String = {
    val compiler = Compiler()
    val pf = compiler.compileNode(testSchema)

    def check() = {
      if (pf.isError) {
        val msgs = pf.getDiagnostics.map(_.getMessage()).mkString("\n")
        throw new Exception(msgs)
      }
    }

    check()

    //
    // More compile time errors can be detected by the code-generator
    // Those should be captured by the processor factory.
    //
    val finalState = pf.generateCode()

    check()

    // We need a way to output, separately compile, then load and run
    // the generated code, so that we can get back the expected object
    // and return it for inspection.

    //finalState.finalGenerate()
    finalState.viewCode
  }
}
