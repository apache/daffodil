package org.apache.daffodil.runtime2

import org.apache.daffodil.compiler.ProcessorFactory
import org.apache.daffodil.runtime2.generators.CodeGeneratorState
import os.Path

class GeneratedCodeCompiler(pf: ProcessorFactory) {
  private var executableFile: Path = _

  def compile(codeGeneratorState: CodeGeneratorState) = {
    val generatedCode = codeGeneratorState.viewCode
    val tempDir = os.temp.dir()
    val tempFile = tempDir / "hello.c"
    val tempExe = tempDir / "hello"
    try {
      os.write(tempFile, generatedCode)
      os.proc("gcc", tempFile, "-o", tempExe).call()
    } catch {
      case e: os.SubprocessException => pf.sset.SDE(e)
    }
    executableFile = tempExe
  }

  def dataProcessor: Runtime2DataProcessor = ???
}
