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
import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.dsom.Root
import org.apache.daffodil.dsom.SchemaDefinitionError
import org.apache.daffodil.runtime2.generators.CodeGeneratorState
import org.apache.daffodil.util.Misc

import scala.util.Properties.isWin

/**
 * We need a mutux object for exclusive access to a code block
 */
private object mutex {}

/**
 * Generates and compiles C source files from a DFDL schema encapsulated in the parameter.
 * Implements the DFDL.CodeGenerator trait to allow it to be called by Daffodil code.
 * Note: Also implements WithDiagnostics trait with mutable state which means you need
 * to create a mew CodeGenerator each time you generate code.
 * @param root Provides the DFDL schema for code generation
 */
class CodeGenerator(root: Root) extends DFDL.CodeGenerator {
  // CodeGenerator is not thread-safe due to these variables
  // which are needed to implement our WithDiagnostics trait
  private var diagnostics: Seq[Diagnostic] = Nil
  private var errorStatus: Boolean = false

  /**
   * Writes C source files into a "c" subdirectory of the given output directory.
   * Removes the "c" subdirectory if it existed before.  Returns the newly created
   * "c" subdirectory.
   */
  override def generateCode(outputDirArg: String): os.Path = {
    // Get the paths of the C resources, the output directory, and its code subdirectory
    val resources = "/org/apache/daffodil/runtime2/c"
    val outputDir = os.Path(Paths.get(outputDirArg).toAbsolutePath)
    val codeDir = outputDir/"c"

    // Ensure our output directory exists while our code subdirectory does not
    os.makeDir.all(outputDir)
    os.remove.all(codeDir)

    // Copy all the C source files from our resources to our code subdirectory
    // (using synchronized to avoid calling FileSystems.newFileSystem concurrently)
    val resourceUri = Misc.getRequiredResource(resources)
    mutex.synchronized {
      val fileSystem = if (resourceUri.getScheme == "jar") {
        val env: java.util.Map[String, String] = Collections.emptyMap()
        FileSystems.newFileSystem(resourceUri, env)
      } else {
        null
      }
      try {
        val resourceDir = os.Path(if (fileSystem != null) fileSystem.getPath(resources) else Paths.get(resourceUri))
        os.copy(resourceDir, codeDir)
      }
      finally
        if (fileSystem != null) fileSystem.close()
    }

    // Generate C code from the DFDL schema, appending any warnings to our diagnostics
    val codeGeneratorState = new CodeGeneratorState(root)
    Runtime2CodeGenerator.generateCode(root.document, codeGeneratorState)
    diagnostics = diagnostics ++ root.warnings
    val versionHeaderText = codeGeneratorState.generateVersionHeader
    val codeHeaderText = codeGeneratorState.generateCodeHeader
    val codeFileText = codeGeneratorState.generateCodeFile

    // Write the generated C code into our code subdirectory
    val generatedVersionHeader = codeDir/"libcli"/"daffodil_version.h"
    val generatedCodeHeader = codeDir/"libruntime"/"generated_code.h"
    val generatedCodeFile = codeDir/"libruntime"/"generated_code.c"
    os.write.over(generatedVersionHeader, versionHeaderText)
    os.write(generatedCodeHeader, codeHeaderText)
    os.write(generatedCodeFile, codeFileText)

    // Return our code directory in case caller wants to call compileCode next
    codeDir
  }

  /**
   * Compiles any C source files inside the given code directory.  Returns the path
   * of the newly built executable in order to run it in a TDML test.
   */
  override def compileCode(codeDir: os.Path): os.Path = {
    // Get the path of the executable we will build
    val exe = if (isWin) codeDir/"daffodil.exe" else codeDir/"daffodil"

    try {
      // Assemble the compiler's command line arguments
      val compiler = pickCompiler
      val cFlags = Seq("-std=gnu11")
      val includes = Seq("-Ilibcli", "-Ilibruntime")
      val absFiles = os.walk(codeDir, skip = _.last == "tests").filter(_.ext == "c")
      val relFiles = Seq("libcli/*.c", "libruntime/*.c")
      val libs = Seq("-lmxml")

      // Run the compiler in the code directory (if we found "zig cc"
      // as a compiler, it will cache previously built files in zig's
      // global cache directory, not a local zig_cache directory)
      if (compiler.nonEmpty) {
        val result = os
          .proc(compiler, cFlags, includes, if (isWin) relFiles else absFiles, libs, "-o", exe)
          .call(cwd = codeDir, stderr = os.Pipe)

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

    // Return our executable in case caller wants to run it next
    exe
  }

  /**
   * Searches for any available C compiler on the system.  Tries to find the
   * compiler given by `CC` if `CC` exists in the environment, then tries to
   * find any compiler from the following list:
   *
   *   - zig cc
   *   - cc
   *   - clang
   *   - gcc
   *
   * Returns the first compiler found as a sequence of strings in case the
   * compiler is a program with a subcommand argument.  Returns the empty
   * sequence if no compiler could be found in the user's PATH.
   */
  lazy val pickCompiler: Seq[String] = {
    val ccEnv = sys.env.getOrElse("CC", "zig cc")
    val compilers = Seq(ccEnv, "zig cc", "cc", "clang", "gcc")
    val path = sys.env.getOrElse("PATH", ".").split(File.pathSeparatorChar)
    def inPath(compiler: String): Boolean = {
      (compiler != null) && {
        val exec = compiler.takeWhile(_ != ' ')
        val exec2 = exec + ".exe"
        path.exists(dir => Files.isExecutable(Paths.get(dir, exec))
          || (isWin && Files.isExecutable(Paths.get(dir, exec2))))
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

/** Runs from "sbt compile" to keep all example generated code files up to date */
object CodeGenerator {
  // Update one set of example generated code files from an example schema
  private def updateGeneratedCodeExample(schemaFile: os.Path, rootName: Option[String],
                                         exampleCodeHeader: os.Path, exampleCodeFile: os.Path): Unit = {
    // Generate code from the example schema file
    val pf = Compiler().compileFile(schemaFile.toIO, rootName)
    assert(!pf.isError, pf.getDiagnostics.map(_.getMessage()).mkString("\n"))
    val cg = pf.forLanguage("c")
    val tempDir = os.temp.dir(dir = null, prefix = "daffodil-runtime2-")
    val codeDir = cg.generateCode(tempDir.toString)
    assert(!cg.isError, cg.getDiagnostics.map(_.getMessage()).mkString("\n"))

    // Replace the example generated files with the newly generated files
    val generatedCodeHeader = codeDir/"libruntime"/"generated_code.h"
    val generatedCodeFile = codeDir/"libruntime"/"generated_code.c"
    os.copy(generatedCodeHeader, exampleCodeHeader, replaceExisting = true, createFolders = true)
    os.copy(generatedCodeFile, exampleCodeFile, replaceExisting = true, createFolders = true)

    // Print the example generated files' names so "sbt 'show genExamples'" can list them
    System.out.println(exampleCodeHeader)
    System.out.println(exampleCodeFile)

    // tempDir should be removed automatically after main exits; this is just in case
    os.remove.all(tempDir)
  }

  // Make sure "sbt compile" calls this main method
  def main(args: Array[String]): Unit = {
    // We expect one mandatory parameter, the examples directory's absolute location.
    if (args.length != 1) {
      System.err.println(s"Usage: $CodeGenerator <examples directory location>")
      System.exit(1)
    }

    // Get paths to our example schemas and example generated code files
    val rootDir = if (os.exists(os.pwd/"src")) os.pwd/os.up else os.pwd

    val schemaDir = rootDir/"daffodil-runtime2"/"src"/"test"/"resources"/"org"/"apache"/"daffodil"/"runtime2"
    val exNumsSchema = schemaDir/"ex_nums.dfdl.xsd"
    val exNumsRootName = None
    val nestedSchema = schemaDir/"nested.dfdl.xsd"
    val nestedRootName = Some("NestedUnion")

    val examplesDir = os.Path(args(0))
    val exNumsCodeHeader = examplesDir/"ex_nums"/"generated_code.h"
    val exNumsCodeFile = examplesDir/"ex_nums"/"generated_code.c"
    val nestedCodeHeader = examplesDir/"NestedUnion"/"generated_code.h"
    val nestedCodeFile = examplesDir/"NestedUnion"/"generated_code.c"

    // Update each set of example generated code files
    try {
      updateGeneratedCodeExample(exNumsSchema, exNumsRootName, exNumsCodeHeader, exNumsCodeFile)
      updateGeneratedCodeExample(nestedSchema, nestedRootName, nestedCodeHeader, nestedCodeFile)
    } catch {
      case e: Throwable =>
        System.err.println(s"Error generating example code files: $e")
        System.exit(1);
    }
  }
}
