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
import org.junit.After
import org.junit.Assert.assertArrayEquals
import org.junit.Test

/**
 * Checks that we can create a [[CodeGenerator]] and call its methods.
 * The value of this test is to debug the call path from [[Compiler]]
 * to [[CodeGenerator]] for a single test DFDL schema.  Running TDML
 * tests with daffodil-runtime2 is a more effective way to check that
 * CodeGenerator can generate appropriate code for as many DFDL schemas
 * as you could want.
 */
class TestCodeGenerator {
  // Ensure all tests remove tempDir after creating it
  val tempDir: os.Path = os.temp.dir(dir = null, prefix = "daffodil-runtime2-")
  @After def after(): Unit = {
    os.remove.all(tempDir)
  }

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
    val codeDir = cg.generateCode(tempDir.toString)
    val daffodilMain = codeDir/"libcli"/"daffodil_main.c"
    val generatedCodeHeader = codeDir/"libruntime"/"generated_code.h"
    val generatedCodeFile = codeDir/"libruntime"/"generated_code.c"
    assert(!cg.isError, cg.getDiagnostics.map(_.getMessage()).mkString("\n"))
    assert(os.exists(codeDir))
    assert(os.exists(daffodilMain))
    assert(os.exists(generatedCodeHeader))
    assert(os.exists(generatedCodeFile))
  }

  @Test def test_compileCode_success(): Unit = {
    // Create a CodeGenerator and generate code from the test schema
    val pf = Compiler().compileNode(testSchema)
    val cg = pf.forLanguage("c")
    val codeDir = cg.generateCode(tempDir.toString)

    // Compile the generated code into an executable successfully
    val executable = cg.compileCode(codeDir)
    assert(!cg.isError, cg.getDiagnostics.map(_.getMessage()).mkString("\n"))
    assert(os.exists(executable))
  }

  @Test def test_parse_success(): Unit = {
    // Compile the test schema into a C executable
    val pf = Compiler().compileNode(testSchema)
    val cg = pf.forLanguage("c")
    val codeDir = cg.generateCode(tempDir.toString)
    val executable = cg.compileCode(codeDir)

    // Create a Runtime2DataProcessor and parse a binary int32 number successfully
    val dp = new Runtime2DataProcessor(executable)
    val b = Misc.hex2Bytes("00000005")
    val input = new ByteArrayInputStream(b)
    val pr = dp.parse(input)
    assert(!pr.isError && pf.getDiagnostics.isEmpty, pr.getDiagnostics.map(_.getMessage()).mkString("\n"))
    val expected = <e1><x>5</x></e1>
    TestUtils.assertEqualsXMLElements(expected, pr.infosetAsXML)
  }

  @Test def test_parse_error(): Unit = {
    // Compile the test schema into a C executable
    val pf = Compiler().compileNode(testSchema)
    val cg = pf.forLanguage("c")
    val codeDir = cg.generateCode(tempDir.toString)
    val executable = cg.compileCode(codeDir)

    // Create a Runtime2DataProcessor and parse an empty file unsuccessfully
    val dp = new Runtime2DataProcessor(executable)
    val b = Misc.hex2Bytes("")
    val input = new ByteArrayInputStream(b)
    val pr = dp.parse(input)
    assert(pr.isError, "expected pr.isError to be true")
    assert(pr.getDiagnostics.nonEmpty, "expected pr.getDiagnostics to be non-empty")
  }

  @Test def test_unparse_success(): Unit = {
    // Compile the test schema into a C executable
    val pf = Compiler().compileNode(testSchema)
    val cg = pf.forLanguage("c")
    val codeDir = cg.generateCode(tempDir.toString)
    val executable = cg.compileCode(codeDir)

    // Create a Runtime2DataProcessor and unparse a binary int32 number successfully
    val dp = new Runtime2DataProcessor(executable)
    val input = Channels.newInputStream(Misc.stringToReadableByteChannel("<e1><x>5</x></e1>"))
    val output = new ByteArrayOutputStream()
    val ur = dp.unparse(input, output)
    assert(!ur.isError && pf.getDiagnostics.isEmpty, ur.getDiagnostics.map(_.getMessage()).mkString("\n"))
    val expected = Misc.hex2Bytes("00000005")
    assertArrayEquals(expected, output.toByteArray)
  }

  @Test def test_unparse_error(): Unit = {
    // Compile the test schema into a C executable
    val pf = Compiler().compileNode(testSchema)
    val cg = pf.forLanguage("c")
    val codeDir = cg.generateCode(tempDir.toString)
    val executable = cg.compileCode(codeDir)

    // Create a Runtime2DataProcessor and unparse a binary int32 number unsuccessfully
    val dp = new Runtime2DataProcessor(executable)
    val input = Channels.newInputStream(Misc.stringToReadableByteChannel("<e1><x>FAIL</x></e1>"))
    val output = new ByteArrayOutputStream()
    val ur = dp.unparse(input, output)
    assert(ur.isError, "expected ur.isError to be true")
    assert(ur.getDiagnostics.nonEmpty, "expected ur.getDiagnostics to be non-empty")
  }

  // Test added for code coverage because "sbt coverage compile" doesn't include genExamples
  @Test def test_CodeGenerator_main(): Unit = {
    val rootDir = if (os.exists(os.pwd/"src")) os.pwd/os.up else os.pwd
    val examplesDir = rootDir/"daffodil-runtime2"/"target"/"test_CodeGenerator_main"
    val args = Array(examplesDir.toString)
    CodeGenerator.main(args)
  }
}
