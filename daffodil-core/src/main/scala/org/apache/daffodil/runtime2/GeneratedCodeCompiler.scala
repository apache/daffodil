package org.apache.daffodil.runtime2

import org.apache.daffodil.compiler.ProcessorFactory
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.runtime2.generators.CodeGeneratorState
import os.Path

class GeneratedCodeCompiler(pf: ProcessorFactory) {
  private var executableFile: Path = _

  def compile(codeGeneratorState: CodeGeneratorState) = {
    val generatedCode = codeGeneratorState.viewCode
    val tempDir = os.temp.dir()
    val tempFile = tempDir / "better-name-later.c"
    os.write(tempFile, generatedCode)
    val result = os.proc("cc", tempFile).call() // Hopefully we will capture both stdout/stderr
    // Check result.exitCode != 0 first, set a diag otherwise?
    if (result.exitCode != 0) {
      // Collect result.out.string and result.err.string for analysis too
      val diagnostic = new Exception(s"Compiler returned $result.exitCode, $result.out.string, $result.err.string")
      pf.sset.SDE(diagnostic)
    }
    executableFile = tempDir / "a.out"
  }

  def dataProcessor: Runtime2DataProcessor = ???
}
