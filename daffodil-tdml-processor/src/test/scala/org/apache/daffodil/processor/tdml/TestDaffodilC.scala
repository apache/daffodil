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

package org.apache.daffodil.processor.tdml

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream

import org.apache.daffodil.codegen.c.DaffodilCCodeGenerator
import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.lib.Implicits.intercept
import org.apache.daffodil.lib.iapi.TDMLImplementation
import org.apache.daffodil.lib.iapi.UnitTestSchemaSource
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.After
import org.junit.Assert.assertArrayEquals
import org.junit.Assert.fail
import org.junit.Test

/**
 * Checks that we can generate C code from a DFDL schema, build an
 * executable from the C code, and parse or unparse data or infosets
 * with the executable.
 * 
 * Those tests run DaffodilCCodeGenerator, DaffodilCTDMLDFDLProcessor,
 * and DaffodilCTDMLDFDLProcessorFactory on a very simple, single DFDL
 * schema to debug the complete call path.  You can test even more
 * DFDL schemas by writing TDML tests and running them with DaffodilC.
 */
class TestDaffodilC {

  // Defines temporary directory for all tests to use
  val tempDir: os.Path = os.temp.dir(dir = null, prefix = TDMLImplementation.DaffodilC.toString)

  // Ensures each test cleans up temporary directory afterwards
  @After def after(): Unit = {
    os.remove.all(tempDir)
  }

  // Defines a very simple DFDL test schema for all tests to use
  private val testSchema = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format representation="binary" ref="GeneralFormat"/>,
    <xs:element name="e1">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="x" type="xs:int"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>
  )

  // Checks that test schema compiles successfully without warnings
  @Test def test_compileNode(): Unit = {
    val pf = Compiler().compileNode(testSchema)
    assert(
      !pf.isError && pf.getDiagnostics.isEmpty,
      pf.getDiagnostics.map(_.getMessage()).mkString("\n")
    )
  }

  // Checks that processorFactory.forLanguage("c") succeeds
  @Test def test_forLanguage_success(): Unit = {
    // Create a ProcessorFactory from the test schema
    val pf = Compiler().compileNode(testSchema)
    assert(!pf.isError, pf.getDiagnostics.map(_.getMessage()).mkString("\n"))

    // Create a CodeGenerator from the ProcessorFactory for a supported language
    val cg = pf.forLanguage("c")
    assert(cg.isInstanceOf[DaffodilCCodeGenerator])
    assert(!cg.isError, cg.getDiagnostics.map(_.getMessage()).mkString("\n"))
  }

  // Checks that processorFactory.forLanguage("hls") fails
  @Test def test_forLanguage_error(): Unit = {
    // Create a ProcessorFactory from the test schema
    val pf = Compiler().compileNode(testSchema)
    assert(!pf.isError, pf.getDiagnostics.map(_.getMessage()).mkString("\n"))

    // Create a CodeGenerator from the ProcessorFactory for an unsupported language
    val e = intercept[Exception] {
      pf.forLanguage("hls")
    }
    assert(e.getMessage.contains("source language hls is not supported"))
  }

  // Checks that codeGenerator.generateCode(tempDir) succeeds
  @Test def test_generateCode_success(): Unit = {
    // Create a ProcessorFactory and CodeGenerator from the test schema
    val pf = Compiler().compileNode(testSchema)
    val cg = pf.forLanguage("c")

    // Generate code from the test schema successfully
    val codeDir = cg.generateCode(tempDir.toString)
    val daffodilMain = codeDir / "libcli" / "daffodil_main.c"
    val generatedCodeHeader = codeDir / "libruntime" / "generated_code.h"
    val generatedCodeFile = codeDir / "libruntime" / "generated_code.c"
    assert(!cg.isError, cg.getDiagnostics.map(_.getMessage()).mkString("\n"))
    assert(os.exists(codeDir))
    assert(os.exists(daffodilMain))
    assert(os.exists(generatedCodeHeader))
    assert(os.exists(generatedCodeFile))
  }

  // Checks that codeGenerator.compileCode(codeDir) succeeds
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

  // Checks that daffodilTDMLDFDLProcessor.parse(goodData) succeeds
  @Test def test_parse_success(): Unit = {
    // Compile the test schema into an executable
    val pf = Compiler().compileNode(testSchema)
    val cg = pf.forLanguage("c")
    val codeDir = cg.generateCode(tempDir.toString)
    val executable = cg.compileCode(codeDir)

    // Create a DaffodilCTDMLDFDLProcessor and parse a binary int32 number successfully
    val tdp = new DaffodilCTDMLDFDLProcessor(executable)
    val b = Misc.hex2Bytes("00000500")
    val input = new ByteArrayInputStream(b)
    val pr = tdp.parse(input, b.length * 8)
    assert(
      !pr.isProcessingError && pr.getDiagnostics.isEmpty,
      pr.getDiagnostics.map(_.getMessage()).mkString("\n")
    )
    val expected = <e1><x>1280</x></e1>
    XMLUtils.compareAndReport(expected, pr.getResult)
  }

  // Checks that daffodilTDMLDFDLProcessor.parse(badData) fails
  @Test def test_parse_error(): Unit = {
    // Compile the test schema into a C executable
    val pf = Compiler().compileNode(testSchema)
    val cg = pf.forLanguage("c")
    val codeDir = cg.generateCode(tempDir.toString)
    val executable = cg.compileCode(codeDir)

    // Create a DaffodilCTDMLDFDLProcessor and parse an empty file unsuccessfully
    val tdp = new DaffodilCTDMLDFDLProcessor(executable)
    val b = Misc.hex2Bytes("50")
    val input = new ByteArrayInputStream(b)
    val pr = tdp.parse(input, b.length * 8)
    assert(pr.isProcessingError, "expected pr.isError to be true")
    assert(pr.getDiagnostics.nonEmpty, "expected pr.getDiagnostics to be non-empty")
  }

  // Checks that daffodilTDMLDFDLProcessor.unparse(goodData) succeeds
  @Test def test_unparse_success(): Unit = {
    // Compile the test schema into an executable
    val pf = Compiler().compileNode(testSchema)
    val cg = pf.forLanguage("c")
    val codeDir = cg.generateCode(tempDir.toString)
    val executable = cg.compileCode(codeDir)

    // Create a DaffodilCTDMLDFDLProcessor and unparse a binary int32 number successfully
    val tdp = new DaffodilCTDMLDFDLProcessor(executable)
    val infosetXML = <e1 xmlns="http://example.com"><x>1280</x></e1>
    val output = new ByteArrayOutputStream()
    val ur = tdp.unparse(infosetXML, output)
    assert(
      !ur.isProcessingError && ur.getDiagnostics.isEmpty,
      ur.getDiagnostics.map(_.getMessage()).mkString("\n")
    )
    val expected = Misc.hex2Bytes("00000500")
    assertArrayEquals(expected, output.toByteArray)
  }

  // Checks that daffodilTDMLDFDLProcessor.unparse(badData) fails
  @Test def test_unparse_error(): Unit = {
    // Compile the test schema into an executable
    val pf = Compiler().compileNode(testSchema)
    val cg = pf.forLanguage("c")
    val codeDir = cg.generateCode(tempDir.toString)
    val executable = cg.compileCode(codeDir)

    // Create a DaffodilCTDMLDFDLProcessor and unparse a binary int32 number unsuccessfully
    val tdp = new DaffodilCTDMLDFDLProcessor(executable)
    val infosetXML = <e1 xmlns="http://example.com"><x>FAIL</x></e1>
    val output = new ByteArrayOutputStream()
    val ur = tdp.unparse(infosetXML, output)
    assert(ur.isProcessingError, "expected ur.isProcessingError to be true")
    assert(ur.getDiagnostics.nonEmpty, "expected ur.getDiagnostics to be non-empty")
  }

  // Checks that DaffodilCTDMLDFDLProcessorFactory.getProcessor succeeds
  @Test def test_getProcessor_success(): Unit = {
    val processorFactory = new DaffodilCTDMLDFDLProcessorFactory()
    val schemaSource =
      UnitTestSchemaSource(testSchema, nameHint = "getProcessor", Some(tempDir.toIO))
    val useSerializedProcessor = false
    val optRootName = None
    val optRootNamespace = None
    val tunables = Map.empty[String, String]
    val cr = processorFactory.getProcessor(
      schemaSource,
      useSerializedProcessor,
      optRootName,
      optRootNamespace,
      tunables
    )
    cr match {
      case Left(diagnostics) => fail(s"getProcessor failed: ${diagnostics.mkString}")
      case Right((diagnostics, tdmlDFDLProcessor)) =>
        assert(
          diagnostics.forall(!_.isError),
          diagnostics.filter(_.isError).map(_.getMessage()).mkString("\n")
        )
        assert(tdmlDFDLProcessor.isInstanceOf[DaffodilCTDMLDFDLProcessor])
    }
  }

}
