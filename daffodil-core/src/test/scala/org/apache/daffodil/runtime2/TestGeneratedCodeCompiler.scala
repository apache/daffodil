package org.apache.daffodil.runtime2

import org.apache.daffodil.compiler.{ Compiler, ProcessorFactory }
import org.apache.daffodil.runtime2.generators.CodeGeneratorState
import org.apache.daffodil.util.SchemaUtils
import org.junit.Test


class TestGeneratedCodeCompiler {

    @Test
    def compile(): Unit = {
        val testSchema = SchemaUtils.dfdlTestSchema(
                <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
                <dfdl:format ref="tns:GeneralFormat"/>,
                <xs:element name="number" type="xs:int"/>)
        val schemaCompiler = Compiler()
        val pf = schemaCompiler.compileNode(testSchema).asInstanceOf[ProcessorFactory]
        val codeGeneratorState = new CodeGeneratorState("""
            |#include <stdio.h>
            |
            |int main() {
            |  printf("Hello World\n");
            |  return 0;
            |}
            |""".stripMargin);
        val generatedCodeCompiler = new GeneratedCodeCompiler(pf)
        generatedCodeCompiler.compile(codeGeneratorState)
        assert(!pf.isError)
    }
}