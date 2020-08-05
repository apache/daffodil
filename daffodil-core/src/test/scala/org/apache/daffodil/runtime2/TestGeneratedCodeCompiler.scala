package org.apache.daffodil.runtime2

import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.runtime2.generators.CodeGeneratorState
import org.apache.daffodil.util.SchemaUtils
import org.junit.Test


class TestGeneratedCodeCompiler {

  @Test
  def compileTinyProgram(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
        <dfdl:format ref="tns:GeneralFormat"/>,
        <xs:element dfdl:representation="binary" name="number" type="xs:int"/>)
    val schemaCompiler = Compiler()
    val pf = schemaCompiler.compileNode(testSchema)
    val codeGeneratorState = new CodeGeneratorState(
      """
        |#include <stdio.h>
        |
        |int main() {
        |  printf("Hello World\n");
        |  return 0;
        |}
        |""".stripMargin)
    val generatedCodeCompiler = new GeneratedCodeCompiler(pf)
    generatedCodeCompiler.compile(codeGeneratorState)
    assert(!pf.isError)
  }

  @Test
  def checkCompilerDiagnosticMessages(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
        <dfdl:format ref="tns:GeneralFormat"/>,
        <xs:element dfdl:representation="binary" name="number" type="xs:int"/>)
    val schemaCompiler = Compiler()
    val pf = schemaCompiler.compileNode(testSchema)
    val codeGeneratorState = new CodeGeneratorState(
      """
        |#include <stdio.h>
        |
        |int main() {
        |  printff("Hello World\n");
        |  return 0;
        |}
        |""".stripMargin)
    val generatedCodeCompiler = new GeneratedCodeCompiler(pf)
    generatedCodeCompiler.compile(codeGeneratorState)
    assert(pf.isError)
    pf.getDiagnostics.find(_.isError).toString.contains("printff")
  }

  @Test
  def compileFirstRealProgram(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
        <dfdl:format representation="binary" ref="tns:GeneralFormat"/>,
      <xs:element name="C">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="e1" type="xs:int"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified")
    val schemaCompiler = Compiler()
    val pf = schemaCompiler.compileNode(testSchema)
    val codeGeneratorState = pf.generateCode()
    val generatedCodeCompiler = new GeneratedCodeCompiler(pf)
    val rootElementName = "C"
    generatedCodeCompiler.compile(rootElementName, codeGeneratorState)
    assert(!pf.isError)
    // Our next step will be to run the compiled C code and get a parse result
    //val b = Misc.hex2Bytes("000000FF")
    //dataProcessor: Runtime2DataProcessor = generatedCodeCompiler.dataProcessor
    //input: InputSourceDataInputStream = <something that will read from our b val>
    //output: InfosetOutputter = <something we can pass to our Runtime2DataProcessor>
    //parseResult: DFDL.ParseResult = dataProcessor.parse(input, output)
    //assert(!parseResult.isError)
  }

  @Test
  def testCompileTwoElementSchema(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
        <dfdl:format representation="binary" ref="tns:GeneralFormat"/>,
      <xs:element name="C">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="e1" type="xs:int"/>
            <xs:element name="e2" type="xs:int"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified")
    val schemaCompiler = Compiler()
    val pf = schemaCompiler.compileNode(testSchema)
    val codeGeneratorState = pf.generateCode()
    val generatedCodeCompiler = new GeneratedCodeCompiler(pf)
    val rootElementName = "C"
    generatedCodeCompiler.compile(rootElementName, codeGeneratorState)
    assert(!pf.isError)
  }

}