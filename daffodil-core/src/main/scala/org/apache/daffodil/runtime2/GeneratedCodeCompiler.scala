package org.apache.daffodil.runtime2

import org.apache.daffodil.compiler.ProcessorFactory
import org.apache.daffodil.dsom.SchemaDefinitionError
import org.apache.daffodil.runtime2.generators.CodeGeneratorState
import org.apache.daffodil.util.Misc
import os.{ Path, Pipe }

class GeneratedCodeCompiler(pf: ProcessorFactory) {
  private var executableFile: Path = _

  // Original method now used only for tiny program compilation unit testing
  def compile(codeGeneratorState: CodeGeneratorState): Unit = {
    val code = codeGeneratorState.viewCode
    val tempDir = os.temp.dir()
    val tempCodeFile = tempDir / "code.c"
    val tempExe = tempDir / "exe"
    executableFile = null
    try {
      os.write(tempCodeFile, code)
      val result = os.proc("gcc", tempCodeFile, "-o", tempExe).call(stderr = Pipe)
      if (!result.out.text.isEmpty || !result.err.text.isEmpty) {
        pf.sset.SDW(null, "Captured warning messages\n" + result.out.text + result.err.text)
      }
      executableFile = tempExe
    } catch {
      case e: os.SubprocessException =>
        val sde = new SchemaDefinitionError(None, None, Misc.getSomeMessage(e).get)
        pf.sset.error(sde)
    }
  }

  // New method, now generates all of the C code needed to parse an input stream
  def compile(rootElementName: String, codeGeneratorState: CodeGeneratorState): Unit = {
    val commonRuntimeHeader = codeGeneratorState.viewRuntimeHeader
    val commonRuntimeFile = codeGeneratorState.viewRuntimeFile
    val generatedCodeHeader = codeGeneratorState.viewCodeHeader
    val generatedCodeFile = codeGeneratorState.viewCodeFile(rootElementName)
    val xmlWriterHeader = codeGeneratorState.viewWriterHeader
    val xmlWriterFile = codeGeneratorState.viewWriterFile
    val tempDir = os.temp.dir()
    val tempRuntimeHeader = tempDir / "common_runtime.h"
    val tempRuntimeFile = tempDir / "common_runtime.c"
    val tempCodeHeader = tempDir / "generated_data.h"
    val tempCodeFile = tempDir / "generated_data.c"
    val tempWriterHeader = tempDir / "xml_writer.h"
    val tempWriterFile = tempDir / "xml_writer.c"
    val tempExe = tempDir / "generated_data"
    executableFile = null
    try {
      os.write(tempRuntimeHeader, commonRuntimeHeader)
      os.write(tempRuntimeFile, commonRuntimeFile)
      os.write(tempCodeHeader, generatedCodeHeader)
      os.write(tempCodeFile, generatedCodeFile)
      os.write(tempWriterHeader, xmlWriterHeader)
      os.write(tempWriterFile, xmlWriterFile)
      val result = os.proc("gcc", "-I", ".", tempRuntimeFile, tempCodeFile, tempWriterFile, "-o", tempExe).call(stderr = Pipe)
      if (!result.out.text.isEmpty || !result.err.text.isEmpty) {
        pf.sset.SDW(null, "Captured warning messages\n" + result.out.text + result.err.text)
      }
      executableFile = tempExe
    } catch {
      case e: os.SubprocessException =>
        val sde = new SchemaDefinitionError(None, None, Misc.getSomeMessage(e).get)
        pf.sset.error(sde)
    }
  }

  def dataProcessor: Runtime2DataProcessor = ???
}
