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

package org.apache.daffodil.codegen.c

import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.lib.iapi.TDMLImplementation
import org.apache.daffodil.lib.util.SchemaUtils

import org.junit.Test

/**
 * Runs DaffodilCExamplesGenerator in a test since "sbt coverage compile" doesn't
 * capture call of genCExamples
 */
class TestDaffodilCExamplesGenerator {

  // Calls DaffodilCExamplesGenerator for code coverage and debugging
  @Test def test_DaffodilCExamplesGenerator_main(): Unit = {
    // Generate the C examples in a safe place (target/examples)
    val rootDir = if (os.exists(os.pwd / "src")) os.pwd / os.up else os.pwd
    val examplesDir = rootDir / "daffodil-codegen-c" / "target" / "examples"
    val args = Array(examplesDir.toString)
    DaffodilCExamplesGenerator.main(args)

    // Verify the C examples were generated
    val generatedCode = examplesDir / "variablelen" / "generated_code.c"
    assert(os.exists(generatedCode))
  }

  // Checks C code can be generated from a schema with an empty grammar object
  @Test def test_generateCode(): Unit = {
    // Define a schema containing an empty grammar object
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format representation="binary" ref="GeneralFormat"/>,
      <xs:element name="foo">
        <xs:complexType>
          <xs:choice>
            <xs:element name="bar" type="xs:int"/>
            <xs:sequence/>
          </xs:choice>
        </xs:complexType>
      </xs:element>
    )

    // Compile the schema into a ProcessorFactory
    val pf = Compiler().compileNode(testSchema)
    assert(!pf.isError, pf.getDiagnostics.map(_.getMessage()).mkString("\n"))

    // Get a CodeGenerator from the ProcessorFactory
    val cg = pf.forLanguage("c")
    assert(!cg.isError, cg.getDiagnostics.map(_.getMessage()).mkString("\n"))

    // Generate C code into a temporary directory
    val tempDir: os.Path =
      os.temp.dir(dir = null, prefix = TDMLImplementation.DaffodilC.toString)
    cg.generateCode(tempDir.toString)
    os.remove.all(tempDir)

    // Check the C code was generated successfully
    assert(!cg.isError, cg.getDiagnostics.map(_.getMessage()).mkString("\n"))
  }

}
