package org.apache.daffodil.runtime2

import org.apache.daffodil.compiler.ProcessorFactory
import org.apache.daffodil.dsom.SchemaDefinitionError
import org.apache.daffodil.runtime2.generators.CodeGeneratorState
import org.apache.daffodil.util.Misc
import os.{ Path, Pipe }

class GeneratedCodeCompiler(pf: ProcessorFactory) {
  private var executableFile: Path = _

  // Original method now used only for hello world compilation unit testing
  def compile(codeGeneratorState: CodeGeneratorState): Unit = {
    val code = codeGeneratorState.viewCode
    val tempDir = os.temp.dir()
    val tempCodeFile = tempDir / "code.c"
    val tempExe = tempDir / "exe"
    try {
      executableFile = null
      os.write(tempCodeFile, code)
      val result = os.proc("/usr/bin/gcc", tempCodeFile, "-o", tempExe).call(cwd = tempDir, stderr = Pipe)
      if (!result.out.text.isEmpty || !result.err.text.isEmpty) {
        pf.sset.SDW(null, "Unexpected generated code compiler output:\n" + result.out.text + result.err.text)
      }
      executableFile = tempExe
    } catch {
      case e: os.SubprocessException =>
        val sde = new SchemaDefinitionError(None, None, Misc.getSomeMessage(e).get)
        pf.sset.error(sde)
    }
  }

  // New method, generates the C code needed to parse or unparse an input stream
  def compile(rootElementName: String, codeGeneratorState: CodeGeneratorState): Unit = {
    val compiler = "/usr/bin/gcc"
    val includeDir = "/home/interran/apache/incubator-daffodil-codegen/daffodil-runtime2/src/main/c"
    val libDir = "/home/interran/apache/incubator-daffodil-codegen/daffodil-runtime2/target/streams/compile/ccTargetMap/_global/streams/compile/sbtcc.Library"
    val generatedCodeHeader = codeGeneratorState.viewCodeHeader
    val generatedCodeFile = codeGeneratorState.viewCodeFile(rootElementName)
    val tempDir = os.temp.dir()
    val tempCodeHeader = tempDir / "generated_code.h"
    val tempCodeFile = tempDir / "generated_code.c"
    val tempExe = tempDir / "daffodil"
    try {
      executableFile = null
      os.write(tempCodeHeader, generatedCodeHeader)
      os.write(tempCodeFile, generatedCodeFile)
      val result = os.proc(compiler, "-I", includeDir, tempCodeFile, "-L", libDir, "-lruntime2", "-lmxml", "-lpthread", "-o", tempExe).call(cwd = tempDir, stderr = Pipe)
      if (!result.out.text.isEmpty || !result.err.text.isEmpty) {
        pf.sset.SDW(null, "Unexpected generated code compiler output:\n" + result.out.text + result.err.text)
      }
      executableFile = tempExe
    } catch {
      case e: os.SubprocessException =>
        val sde = new SchemaDefinitionError(None, None, Misc.getSomeMessage(e).get)
        pf.sset.error(sde)
    }
  }

  def dataProcessor: Runtime2DataProcessor = new Runtime2DataProcessor(executableFile)
}
