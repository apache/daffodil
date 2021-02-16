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

package org.apache.daffodil.runtime2

import java.io.File
import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.file.Paths
import java.util.Collections

import org.apache.daffodil.api.DFDL
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.dsom.Root
import org.apache.daffodil.dsom.SchemaDefinitionError
import org.apache.daffodil.runtime2.generators.CodeGeneratorState
import org.apache.daffodil.util.Misc
import org.apache.daffodil.xml.RefQName

/**
 * We need a mutux object for exclusive access to a code block
  */
private object mutex {}

/**
 * Generates and compiles C source files from a DFDL schema encapsulated in a [[Root]].
 * Implements the [[DFDL.CodeGenerator]] trait to allow it to be called by Daffodil code.
 * @param root Provides the DFDL schema for code generation
 */
class CodeGenerator(root: Root) extends DFDL.CodeGenerator {
  // Used by compileCode and pickCompiler methods
  private lazy val isWindows = System.getProperty("os.name").toLowerCase().startsWith("windows")
  // Used by WithDiagnostics methods
  private var diagnostics: Seq[Diagnostic] = Nil
  private var errorStatus: Boolean = false

  /**
   * Writes C source files into a "c" subdirectory of the given output directory.
   * Removes the "c" subdirectory if it existed before.  Returns the "c" subdirectory.
  */
  override def generateCode(rootNS: Option[RefQName], outputDirArg: String): os.Path = {
    // Get the paths of the output directory and its code subdirectory
    val outputDir = os.Path(Paths.get(outputDirArg).toAbsolutePath)
    val codeDir = outputDir/"c"

    // Ensure our output directory exists while our code subdirectory does not
    os.makeDir.all(outputDir)
    os.remove.all(codeDir)

    // Copy our resource directory and all its C source files to our code subdirectory
    // (using synchronized to avoid calling FileSystems.newFileSystem concurrently)
    val resourceUri = Misc.getRequiredResource("/c")
    mutex.synchronized {
      val fileSystem = if (resourceUri.getScheme == "jar") {
        val env: java.util.Map[String, String] = Collections.emptyMap()
        FileSystems.newFileSystem(resourceUri, env)
      } else {
        null
      }
      try {
        val resourceDir = os.Path(if (fileSystem != null) fileSystem.getPath("/c") else Paths.get(resourceUri))
        os.copy(resourceDir, codeDir)
      }
      finally
        if (fileSystem != null) fileSystem.close()
    }

    // Generate C code from the DFDL schema, appending any warnings to our diagnostics
    val rootElementName = rootNS.getOrElse(root.refQName).local
    val codeGeneratorState = new CodeGeneratorState()
    Runtime2CodeGenerator.generateCode(root.document, codeGeneratorState)
    diagnostics = diagnostics ++ root.warnings
    val codeHeaderText = codeGeneratorState.generateCodeHeader
    val codeFileText = codeGeneratorState.generateCodeFile(rootElementName)

    // Write the generated C code into our code subdirectory
    val generatedCodeHeader = codeDir/"libruntime"/"generated_code.h"
    val generatedCodeFile = codeDir/"libruntime"/"generated_code.c"
    os.write(generatedCodeHeader, codeHeaderText)
    os.write(generatedCodeFile, codeFileText)

    // Return our code directory in case caller wants to call compileCode next
    codeDir
  }

  /**
   * Compiles any C source files inside the given code directory.  Returns the path
   * of the newly created executable to use in TDML tests or somewhere else.
   */
  override def compileCode(codeDir: os.Path): os.Path = {
    // Get the path of the executable we will build
    val exe = if (isWindows) codeDir/"daffodil.exe" else codeDir/"daffodil"

    try {
      // Assemble the compiler's command line arguments
      val compiler = pickCompiler
      val files = os.walk(codeDir).filter(_.ext == "c")
      val libs = if (isWindows) Seq("-largp", "-lmxml") else Seq("-lmxml")

      // Run the compiler in the code directory (if we found "zig cc"
      // as a compiler, it will cache previously built files in zig's
      // global cache directory, not a local zig_cache directory)
      if (compiler.nonEmpty) {
        val result = os.proc(compiler, "-I", codeDir/"libcli", "-I", codeDir/"libruntime",
          files, libs, "-o", exe).call(cwd = codeDir, stderr = os.Pipe)

        // Report any compiler output as a warning
        if (result.out.text.nonEmpty || result.err.text.nonEmpty) {
          warning("Unexpected compiler output on stdout: %s on stderr: %s", result.out.text, result.err.text)
        }
      }
    } catch {
      // Report any subprocess termination error as an error
      case e: os.SubprocessException =>
        error("Error compiling generated code: %s wd: %s", Misc.getSomeMessage(e).get, codeDir.toString)
    }

    // Report any failure to build the executable as an error
    if (!os.exists(exe)) error("No executable was built: %s", exe.toString)
    exe
  }

  /**
   * Searches for any available C compiler on the system.  Tries to find the
   * compiler given by `CC` if `CC` exists in the environment, then tries to
   * find any compiler from the following list:
   *
   *   - zig cc
   *   - gcc
   *   - clang
   *   - cc
   *
   * Returns the first compiler found as a sequence of strings in case the
   * compiler is a program with a subcommand argument.  Returns the empty
   * sequence if no compiler could be found in the user's PATH.
   */
  lazy val pickCompiler: Seq[String] = {
    val ccEnv = System.getenv("CC")
    val compilers = Seq(ccEnv, "zig cc", "gcc", "clang", "cc")
    val path = System.getenv("PATH").split(File.pathSeparatorChar)
    def inPath(compiler: String): Boolean = {
      (compiler != null) && {
        val exec = compiler.takeWhile(_ != ' ')
        val exec2 = exec + ".exe"
        path.exists(dir => Files.isExecutable(Paths.get(dir, exec))
          || (isWindows && Files.isExecutable(Paths.get(dir, exec2))))
      }
    }
    val compiler = compilers.find(inPath)
    if (compiler.isDefined)
      compiler.get.split(' ').toSeq
    else
      Seq.empty[String]
  }

  /**
   * Adds a warning message to the diagnostics
   */
  def warning(formatString: String, args: Any*): Unit = {
    val sde = new SchemaDefinitionError(None, None, formatString, args: _*)
    diagnostics :+= sde
  }

  /**
   * Adds an error message to the diagnostics and sets isError true
   */
  def error(formatString: String, args: Any*): Unit = {
    val sde = new SchemaDefinitionError(None, None, formatString, args: _*)
    diagnostics :+= sde
    errorStatus = true
  }

  // Implements the WithDiagnostics methods
  override def getDiagnostics: Seq[Diagnostic] = diagnostics
  override def isError: Boolean = errorStatus
}
