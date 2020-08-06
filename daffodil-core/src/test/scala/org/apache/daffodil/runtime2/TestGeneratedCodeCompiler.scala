package org.apache.daffodil.runtime2

import java.io.ByteArrayInputStream

import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.runtime2.generators.CodeGeneratorState
import org.apache.daffodil.util.Misc
import org.apache.daffodil.util.SchemaUtils
import org.junit.Test


class TestGeneratedCodeCompiler {

  @Test
  def compileHelloWorld(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
        <dfdl:format representation="binary" ref="tns:GeneralFormat"/>,
        <xs:element name="number" type="xs:int"/>)
    val schemaCompiler = Compiler()
    val pf = schemaCompiler.compileNode(testSchema)
    assert(!pf.isError, pf.getDiagnostics.map(_.getMessage()).mkString("\n"))
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
    assert(!pf.isError, pf.getDiagnostics.map(_.getMessage()).mkString("\n"))
  }

  @Test
  def checkCompilerDiagnosticMessages(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
        <dfdl:format representation="binary" ref="tns:GeneralFormat"/>,
        <xs:element name="number" type="xs:int"/>)
    val schemaCompiler = Compiler()
    val pf = schemaCompiler.compileNode(testSchema)
    assert(!pf.isError, pf.getDiagnostics.map(_.getMessage()).mkString("\n"))
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
    assert(pf.isError, pf.getDiagnostics.map(_.getMessage()).mkString("\n"))
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
            <xs:element name="field" type="xs:int"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified")
    val schemaCompiler = Compiler()
    val pf = schemaCompiler.compileNode(testSchema)
    assert(!pf.isError, pf.getDiagnostics.map(_.getMessage()).mkString("\n"))
    val codeGeneratorState = pf.generateCode()
    val generatedCodeCompiler = new GeneratedCodeCompiler(pf)
    val rootElementName = "C"
    generatedCodeCompiler.compile(rootElementName, codeGeneratorState)
    assert(!pf.isError, pf.getDiagnostics.map(_.getMessage()).mkString("\n"))
    // Our next step will be to run the compiled C code and check if it works
    val dp = generatedCodeCompiler.dataProcessor
    val b = Misc.hex2Bytes("000000FF")
    val input = new ByteArrayInputStream(b)
    //dp.parse(input)
    //assert(!dp.isError, dp.getDiagnostics.map(_.getMessage()).mkString("\n"))
  }

  @Test
  def testCompileTwoElementSchema(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
        <dfdl:format representation="binary" ref="tns:GeneralFormat"/>,
      <xs:element name="C">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="first" type="xs:int"/>
            <xs:element name="second" type="xs:int"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified")
    val schemaCompiler = Compiler()
    val pf = schemaCompiler.compileNode(testSchema)
    assert(!pf.isError, pf.getDiagnostics.map(_.getMessage()).mkString("\n"))
    val codeGeneratorState = pf.generateCode()
    val generatedCodeCompiler = new GeneratedCodeCompiler(pf)
    val rootElementName = "C"
    generatedCodeCompiler.compile(rootElementName, codeGeneratorState)
    assert(!pf.isError, pf.getDiagnostics.map(_.getMessage()).mkString("\n"))
  }

}