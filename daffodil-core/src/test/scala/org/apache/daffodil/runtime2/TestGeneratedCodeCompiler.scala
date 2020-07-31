package org.apache.daffodil.runtime2

import org.apache.daffodil.compiler.{ Compiler, ProcessorFactory }
import org.apache.daffodil.runtime2.generators.CodeGeneratorState
import org.apache.daffodil.util.Misc
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
    val pf = schemaCompiler.compileNode(testSchema).asInstanceOf[ProcessorFactory]
    val codeGeneratorState = new CodeGeneratorState(
      """
        |#include <stdio.h>
        |
        |int main() {
        |  printf("Hello World\n");
        |  return 0;
        |}
        |""".stripMargin);
    val generatedCodeCompiler = new GeneratedCodeCompiler(pf)
    val rootElementName = "r"
    generatedCodeCompiler.compile(rootElementName, codeGeneratorState)
    assert(!pf.isError)
  }

  @Test
  def checkCompilerDiagnosticMessages(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
        <dfdl:format ref="tns:GeneralFormat"/>,
        <xs:element dfdl:representation="binary" name="number" type="xs:int"/>)
    val schemaCompiler = Compiler()
    val pf = schemaCompiler.compileNode(testSchema).asInstanceOf[ProcessorFactory]
    val codeGeneratorState = new CodeGeneratorState(
      """
        |#include <stdio.h>
        |
        |int main() {
        |  printff("Hello World\n");
        |  return 0;
        |}
        |""".stripMargin);
    val generatedCodeCompiler = new GeneratedCodeCompiler(pf)
    val rootElementName = "r"
    generatedCodeCompiler.compile(rootElementName, codeGeneratorState)
    assert(pf.isError)
    pf.getDiagnostics.find(_.isError).toString.contains("printff")
  }

  @Test
  def compileFirstRealProgram(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
        <dfdl:format representation="binary" ref="tns:GeneralFormat"/>,
      <xs:element name="r">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="e1" type="xs:int"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified")
    val b = Misc.hex2Bytes("000000FF")
    val schemaCompiler = Compiler()
    val pf = schemaCompiler.compileNode(testSchema).asInstanceOf[ProcessorFactory]
    val codeGeneratorState = pf.generateCode()
    val generatedCodeCompiler = new GeneratedCodeCompiler(pf)
    val rootElementName = "r"
    generatedCodeCompiler.compile(rootElementName, codeGeneratorState)
    assert(!pf.isError)
  }

}