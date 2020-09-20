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

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.nio.channels.Channels

import org.apache.daffodil.Implicits.intercept
import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.util.Misc
import org.apache.daffodil.util.SchemaUtils
import org.apache.daffodil.util.TestUtils
import org.junit.Assert.assertArrayEquals
import org.junit.Test

/**
 * Checks that we can create a [[CodeGenerator]] and call its methods.
 * The value of this test is to debug the call path from [[Compiler]]
 * to [[CodeGenerator]] for one very simple DFDL schema.  Running TDML
 * tests with daffodil-runtime2 is a more effective way to test the
 * functionality of CodeGenerator's generated code for as many DFDL
 * schemas as you could want.
 */
class TestCodeGenerator {
  // Define a simple DFDL test schema for debugging our code path
  private val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format representation="binary" ref="GeneralFormat"/>,
    <xs:element name="e1">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="x" type="xs:int"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>)

  @Test def test_forLanguageC_success(): Unit = {
    // Create a ProcessorFactory from the test schema
    val pf = Compiler().compileNode(testSchema)
    assert(!pf.isError, pf.getDiagnostics.map(_.getMessage()).mkString("\n"))

    // Create a CodeGenerator from the ProcessorFactory for a supported language
    val cg = pf.forLanguage("c")
    assert(!cg.isError, cg.getDiagnostics.map(_.getMessage()).mkString("\n"))
  }

  @Test def test_forLanguage_error(): Unit = {
    // Create a ProcessorFactory from the test schema
    val pf = Compiler().compileNode(testSchema)
    assert(!pf.isError, pf.getDiagnostics.map(_.getMessage()).mkString("\n"))

    // Create a CodeGenerator from the ProcessorFactory for an unsupported language
    val e = intercept[Exception] {
      pf.forLanguage("vhld")
    }
    assert(e.getMessage.contains("source language vhld is not supported"))
  }

  @Test def test_generateCode_success(): Unit = {
    // Create a ProcessorFactory and CodeGenerator from the test schema
    val pf = Compiler().compileNode(testSchema)
    val cg = pf.forLanguage("c")

    // Generate code from the test schema successfully
    val outputDir = cg.generateCode(None, "./generateCode_tmp")
    assert(!cg.isError, cg.getDiagnostics.map(_.getMessage()).mkString("\n"))
    assert(os.exists(outputDir))
    assert(os.exists(outputDir/"c"/"generated_code.c"))

    // Remove the generated code
    os.remove.all(outputDir)
  }

  @Test def test_compileCode_success(): Unit = {
    // Create a CodeGenerator and generate code from the test schema
    val pf = Compiler().compileNode(testSchema)
    val cg = pf.forLanguage("c")
    val outputDir = cg.generateCode(None, "./compileCode_tmp")

    // Compile the generated code into an executable successfully
    val executable = cg.compileCode(outputDir)
    assert(!cg.isError, cg.getDiagnostics.map(_.getMessage()).mkString("\n"))
    assert(os.exists(executable))

    // Remove the generated code
    os.remove.all(outputDir)
  }

  @Test def test_parse_success(): Unit = {
    // Compile the test schema into a C executable
    val pf = Compiler().compileNode(testSchema)
    val cg = pf.forLanguage("c")
    val outputDir = cg.generateCode(None, "./parse_tmp")
    val executable = cg.compileCode(outputDir)

    // Create a Runtime2DataProcessor and parse a binary int32 number successfully
    val dp = new Runtime2DataProcessor(executable)
    val b = Misc.hex2Bytes("00000005")
    val input = new ByteArrayInputStream(b)
    val pr = dp.parse(input)
    assert(!pr.isError && pf.getDiagnostics.isEmpty, pr.getDiagnostics.map(_.getMessage()).mkString("\n"))
    val expected = <e1><x>5</x></e1>
    TestUtils.assertEqualsXMLElements(expected, pr.infosetAsXML)

    // Remove the generated code
    os.remove.all(outputDir)
  }

  @Test def test_unparse_success(): Unit = {
    // Compile the test schema into a C executable
    val pf = Compiler().compileNode(testSchema)
    val cg = pf.forLanguage("c")
    val outputDir = cg.generateCode(None, "./unparse_tmp")
    val executable = cg.compileCode(outputDir)

    // Create a Runtime2DataProcessor and unparse a binary int32 number successfully
    val dp = new Runtime2DataProcessor(executable)
    val input = Channels.newInputStream(Misc.stringToReadableByteChannel("<e1><x>5</x></e1>"))
    val output = new ByteArrayOutputStream()
    val pr = dp.unparse(input, output)
    assert(!pr.isError && pf.getDiagnostics.isEmpty, pr.getDiagnostics.map(_.getMessage()).mkString("\n"))
    val expected = Misc.hex2Bytes("00000005")
    assertArrayEquals(expected, output.toByteArray)

    // Remove the generated code
    os.remove.all(outputDir)
  }
}
