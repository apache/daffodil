package org.apache.daffodil.runtime2

import org.apache.daffodil.compiler.ProcessorFactory
import org.apache.daffodil.dsom.SchemaDefinitionError
import org.apache.daffodil.runtime2.generators.CodeGeneratorState
import org.apache.daffodil.util.Misc
import os.{ Path, Pipe }

class GeneratedCodeCompiler(pf: ProcessorFactory) {
  private var executableFile: Path = _

  def compile(rootElementName: String, codeGeneratorState: CodeGeneratorState) = {
    val generatedCodeHeader = codeGeneratorState.viewCodeHeader
    val generatedCodeFile = codeGeneratorState.viewCodeFile(rootElementName)
    val tempDir = os.temp.dir()
    val tempCodeHeader = tempDir / "generated_data.h"
    val tempCodeFile = tempDir / "generated_data.c"
    val tempExe = tempDir / "generated_data"
    executableFile = null
    try {
      os.write(tempCodeHeader, generatedCodeHeader)
      os.write(tempCodeFile, generatedCodeFile)
      val result = os.proc("gcc", tempCodeFile, "-o", tempExe).call(stderr = Pipe)
      if (!result.out.text.isEmpty || !result.err.text.isEmpty) {
        pf.sset.SDW(null, "Captured warning messages\n" + result.out.text + result.err.text)
      }
      executableFile = tempExe
    } catch {
      case e: os.SubprocessException => {
        val sde = new SchemaDefinitionError(None, None, Misc.getSomeMessage(e).get)
        pf.sset.error(sde)
      }
    }
  }

  def dataProcessor: Runtime2DataProcessor = ???
}
