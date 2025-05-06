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

package org.apache.daffodil.codegen.c

import java.io.File
import java.net.JarURLConnection
import java.nio.file.Files
import java.nio.file.Paths
import scala.jdk.CollectionConverters._
import scala.util.Properties.isWin

import org.apache.daffodil.api
import org.apache.daffodil.codegen.c.generators.AlignmentFillCodeGenerator
import org.apache.daffodil.codegen.c.generators.AssertStatementGenerateCode
import org.apache.daffodil.codegen.c.generators.BinaryBooleanCodeGenerator
import org.apache.daffodil.codegen.c.generators.BinaryFloatCodeGenerator
import org.apache.daffodil.codegen.c.generators.BinaryIntegerKnownLengthCodeGenerator
import org.apache.daffodil.codegen.c.generators.CodeGeneratorState
import org.apache.daffodil.codegen.c.generators.HexBinaryCodeGenerator
import org.apache.daffodil.core.dsom.Root
import org.apache.daffodil.core.grammar.Gram
import org.apache.daffodil.core.grammar.Prod
import org.apache.daffodil.core.grammar.SeqComp
import org.apache.daffodil.core.grammar.primitives.AlignmentFill
import org.apache.daffodil.core.grammar.primitives.AssertBooleanPrim
import org.apache.daffodil.core.grammar.primitives.BinaryBoolean
import org.apache.daffodil.core.grammar.primitives.BinaryDouble
import org.apache.daffodil.core.grammar.primitives.BinaryFloat
import org.apache.daffodil.core.grammar.primitives.BinaryIntegerKnownLength
import org.apache.daffodil.core.grammar.primitives.CaptureContentLengthEnd
import org.apache.daffodil.core.grammar.primitives.CaptureContentLengthStart
import org.apache.daffodil.core.grammar.primitives.CaptureValueLengthEnd
import org.apache.daffodil.core.grammar.primitives.CaptureValueLengthStart
import org.apache.daffodil.core.grammar.primitives.ChoiceCombinator
import org.apache.daffodil.core.grammar.primitives.ElementCombinator
import org.apache.daffodil.core.grammar.primitives.ElementParseAndUnspecifiedLength
import org.apache.daffodil.core.grammar.primitives.ElementUnused
import org.apache.daffodil.core.grammar.primitives.HexBinaryLengthPrefixed
import org.apache.daffodil.core.grammar.primitives.HexBinarySpecifiedLength
import org.apache.daffodil.core.grammar.primitives.OrderedSequence
import org.apache.daffodil.core.grammar.primitives.RepOrderedExactlyNSequenceChild
import org.apache.daffodil.core.grammar.primitives.RepOrderedExpressionOccursCountSequenceChild
import org.apache.daffodil.core.grammar.primitives.RepOrderedWithMinMaxSequenceChild
import org.apache.daffodil.core.grammar.primitives.RightFill
import org.apache.daffodil.core.grammar.primitives.ScalarOrderedSequenceChild
import org.apache.daffodil.core.grammar.primitives.SpecifiedLengthExplicit
import org.apache.daffodil.core.grammar.primitives.SpecifiedLengthImplicit
import org.apache.daffodil.core.grammar.primitives.SpecifiedLengthPrefixed
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.iapi.Diagnostic
import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.schema.annotation.props.gen.FailureType
import org.apache.daffodil.lib.schema.annotation.props.gen.TestKind
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.runtime1.dsom.SchemaDefinitionError
import org.apache.daffodil.runtime1.dsom.SchemaDefinitionWarning

/**
 * Generates C source files from a DFDL schema.  Implements
 * DFDL.CodeGenerator trait in order to be called by Daffodil's
 * command line interface.  Contains mutable state for implementing
 * WithDiagnostics trait, so you need to instantiate a new
 * DaffodilCCodeGenerator each time you generate code for any schema.
 *
 * @param root Passes DFDL schema from which to generate code
 */
class DaffodilCCodeGenerator(root: Root) extends api.CodeGenerator {
  // Note this class is not thread-safe due to mutable state needed to
  // implement WithDiagnostics trait
  private var diagnostics: Seq[Diagnostic] = Nil
  private var errorStatus: Boolean = false

  /**
   * Writes C source files generated from the schema into a "c"
   * subdirectory of the given output directory.  Removes the "c"
   * subdirectory if it existed before.  Returns the newly created "c"
   * subdirectory.
   */
  override def generateCode(outputDirArg: String): os.Path = {
    // Get the paths of the output directory and its code subdirectory and
    // recreate the code subdirectory to ensure it has no old files in it
    val outputDir = os.Path(Paths.get(outputDirArg).toAbsolutePath)
    val codeDir = outputDir / "c"
    os.makeDir.all(outputDir)
    os.remove.all(codeDir)

    // Copy all the C resources to the code subdirectory
    val resources = "/org/apache/daffodil/codegen/c/files"
    val resourceUri = Misc.getRequiredResource(resources)
    if (resourceUri.getScheme == "jar") {
      val jarConnection = resourceUri.toURL.openConnection().asInstanceOf[JarURLConnection]
      val jarFile = jarConnection.getJarFile
      jarFile.entries.asScala
        .filter { entry => ("/" + entry.getName).startsWith(resources) }
        .filterNot { entry => entry.isDirectory }
        .foreach { entry =>
          val entryPath = "/" + entry.getName
          val subPath = os.SubPath(entryPath.stripPrefix(resources + "/"))
          val dstPath = codeDir / subPath
          val stream = jarFile.getInputStream(entry)
          os.write(dstPath, stream, createFolders = true)
        }
    } else {
      val resourceDir = os.Path(Paths.get(resourceUri))
      os.copy(resourceDir, codeDir)
    }

    // Generate C code from the given root element of the DFDL schema,
    // while appending any warnings to our diagnostics
    val cgState = new CodeGeneratorState(root)
    DaffodilCCodeGenerator.generateCode(root.document, cgState)
    diagnostics = diagnostics ++ root.warnings
    val versionHeaderText = cgState.generateVersionHeader
    val codeHeaderText = cgState.generateCodeHeader
    val codeFileText = cgState.generateCodeFile

    // Write the generated C code into our code subdirectory
    val generatedVersionHeader = codeDir / "libcli" / "daffodil_version.h"
    val generatedCodeHeader = codeDir / "libruntime" / "generated_code.h"
    val generatedCodeFile = codeDir / "libruntime" / "generated_code.c"
    os.write.over(generatedVersionHeader, versionHeaderText)
    os.write(generatedCodeHeader, codeHeaderText)
    os.write(generatedCodeFile, codeFileText)

    // Return our code directory in case caller wants to call compileCode next
    codeDir
  }

  /**
   * Compiles any C files inside the given code directory.  Returns the path
   * of the newly built executable in order to run it in a TDML test.
   */
  override def compileCode(codeDir: os.Path): os.Path = {
    // Get the path of the executable we will build
    val exe = if (isWin) codeDir / "daffodil.exe" else codeDir / "daffodil"

    try {
      // Assemble the compilation command line arguments
      val command = pickCommand
      val cFlags = Seq("-std=gnu11")
      val includes = Seq("-Ilibcli", "-Ilibruntime")
      val absFiles = os.walk(codeDir, skip = _.last == "tests").filter(_.ext == "c")
      val relFiles = Seq("libcli/*.c", "libruntime/*.c")
      val libs = Seq("-lmxml")

      // Run the compilation command within the code directory
      if (command.nonEmpty) {
        val result = os
          .proc(command, cFlags, includes, if (isWin) relFiles else absFiles, libs, "-o", exe)
          .call(cwd = codeDir, stderr = os.Pipe)
        if (result.chunks.nonEmpty) {
          // Report any compilation output as a warning
          warning(result.toString())
        }
      }
    } catch {
      // Report any subprocess termination error as an error
      case e: os.SubprocessException =>
        error(
          "Error compiling C files: %s wd: %s",
          Misc.getSomeMessage(e).get,
          codeDir.toString
        )
    }

    // Report any failure to build the executable as an error
    if (!os.exists(exe)) error("No executable was built: %s", exe.toString)

    // Return our executable in case caller wants to run it next
    exe
  }

  /**
   * Searches for any available C compilation command on the system.
   * Tries to find the command given by `CC` if `CC` exists in the
   * environment, then tries to find any command from the following
   * list:
   *
   *   - zig cc
   *   - cc
   *   - clang
   *   - gcc
   *
   * Returns the first command found as a sequence of strings in case
   * the command has a subcommand argument.  Returns the empty
   * sequence if no command could be found in the user's PATH.
   */
  private lazy val pickCommand: Seq[String] = {
    val ccEnv = sys.env.getOrElse("CC", "zig cc")
    val commands = Seq(ccEnv, "zig cc", "cc", "clang", "gcc")
    val path = sys.env.getOrElse("PATH", ".").split(File.pathSeparatorChar)
    def inPath(command: String): Boolean = {
      (command != null) && {
        val exec = command.takeWhile(_ != ' ')
        val exec2 = exec + ".exe"
        path.exists(dir =>
          Files.isExecutable(Paths.get(dir, exec))
            || (isWin && Files.isExecutable(Paths.get(dir, exec2)))
        )
      }
    }
    val command = commands.find(inPath)
    if (command.isDefined)
      command.get.split(' ').toSeq
    else
      Seq.empty[String]
  }

  /**
   * Adds a warning message to the diagnostics
   */
  private def warning(formatString: String, args: Any*): Unit = {
    val sde =
      new SchemaDefinitionWarning(WarnID.CodeGenerator, None, None, formatString, args: _*)
    diagnostics :+= sde
  }

  /**
   * Adds an error message to the diagnostics and sets isError true
   */
  private def error(formatString: String, args: Any*): Unit = {
    val sde = new SchemaDefinitionError(None, None, formatString, args: _*)
    diagnostics :+= sde
    errorStatus = true
  }

  // Implements the WithDiagnostics trait
  override def getDiagnostics: java.util.List[api.Diagnostic] = diagnostics
  override def isError: Boolean = errorStatus
}

/**
 * Performs recursive pattern matching starting from a root document
 * to generate code for selected components recognized by the pattern
 * matcher.  Handles more complicated code generation situations by
 * delegating them to trait methods defined in other classes.
 */
object DaffodilCCodeGenerator
  extends AlignmentFillCodeGenerator
  with AssertStatementGenerateCode
  with BinaryBooleanCodeGenerator
  with BinaryIntegerKnownLengthCodeGenerator
  with BinaryFloatCodeGenerator
  with HexBinaryCodeGenerator {

  /**
   * Starting from a root document, performs recursive pattern
   * matching to generate code for components recognized by the
   * pattern matching code.
   */
  def generateCode(gram: Gram, cgState: CodeGeneratorState): Unit = {
    gram match {
      // Skip empty grams
      case g: Gram if g.isEmpty => noop(g)
      // Handle non-empty grams
      case g: AlignmentFill => alignmentFillGenerateCode(g, cgState)
      case g: AssertBooleanPrim =>
        assertStatementGenerateCode(
          g.name,
          g.exprText,
          g.stmt.failureType == FailureType.RecoverableError,
          cgState
        )
      case g: BinaryBoolean => binaryBooleanGenerateCode(g.e, cgState)
      case g: BinaryDouble => binaryFloatGenerateCode(g.e, lengthInBits = 64, cgState)
      case g: BinaryFloat => binaryFloatGenerateCode(g.e, lengthInBits = 32, cgState)
      case g: BinaryIntegerKnownLength =>
        binaryIntegerKnownLengthGenerateCode(g.e, g.lengthInBits, cgState)
      case g: CaptureContentLengthEnd => noop(g)
      case g: CaptureContentLengthStart => noop(g)
      case g: CaptureValueLengthEnd => noop(g)
      case g: CaptureValueLengthStart => noop(g)
      case g: ChoiceCombinator => choiceCombinator(g, cgState)
      case g: ElementCombinator => elementCombinator(g, cgState)
      case g: ElementParseAndUnspecifiedLength =>
        elementParseAndUnspecifiedLengthGenerateCode(g, cgState)
      case g: ElementUnused => noop(g)
      case g: HexBinaryLengthPrefixed => hexBinaryLengthPrefixedGenerateCode(g.e, cgState)
      case g: HexBinarySpecifiedLength => hexBinarySpecifiedLengthGenerateCode(g.e, cgState)
      case g: OrderedSequence => orderedSequenceGenerateCode(g, cgState)
      case g: Prod => prod(g, cgState)
      case g: RepOrderedExactlyNSequenceChild => repOrderedExactlyNSequenceChild(g, cgState)
      case g: RepOrderedExpressionOccursCountSequenceChild =>
        repOrderedExpressionOccursCountSequenceChild(g, cgState)
      case g: RepOrderedWithMinMaxSequenceChild => repOrderedWithMinMaxSequenceChild(g, cgState)
      case g: RightFill => noop(g)
      case g: ScalarOrderedSequenceChild => scalarOrderedSequenceChild(g, cgState)
      case g: SeqComp => seqCompGenerateCode(g, cgState)
      case g: SpecifiedLengthExplicit => specifiedLengthExplicit(g, cgState)
      case g: SpecifiedLengthImplicit => specifiedLengthImplicit(g, cgState)
      case g: SpecifiedLengthPrefixed => specifiedLengthPrefixed(g, cgState)
      case _ => gram.SDE("Code generation not supported for: %s", Misc.getNameFromClass(gram))
    }
  }

  private def choiceCombinator(g: ChoiceCombinator, cgState: CodeGeneratorState): Unit = {
    cgState.addBeforeSwitchStatements() // switch statements for choices
    for (gram <- g.alternatives) {
      DaffodilCCodeGenerator.generateCode(gram, cgState)
    }
    cgState.addAfterSwitchStatements() // switch statements for choices
  }

  private def elementCombinator(g: ElementCombinator, cgState: CodeGeneratorState): Unit = {
    cgState.pushElement(g.context)
    DaffodilCCodeGenerator.generateCode(g.subComb, cgState)
    // Also generate code for the element's assert statements
    g.context.assertStatements.foreach { assert =>
      assert.testKind match {
        case TestKind.Pattern =>
          g.SDW(
            WarnID.IgnoreDFDLProperty,
            "Code generation not supported for dfdl:assert pattern stmts"
          )
        case TestKind.Expression =>
          val name = g.name
          val exprText = assert.testTxt
          val recoverable = assert.failureType == FailureType.RecoverableError
          assertStatementGenerateCode(name, exprText, recoverable, cgState)
      }
    }
    cgState.popElement(g.context)
  }

  private def elementParseAndUnspecifiedLengthGenerateCode(
    g: ElementParseAndUnspecifiedLength,
    cgState: CodeGeneratorState
  ): Unit = {
    DaffodilCCodeGenerator.generateCode(g.eGram, cgState)
  }

  private def noop(g: Gram): Unit = {
    g.name // Not generating code, but can use as a breakpoint
  }

  private def orderedSequenceGenerateCode(
    g: OrderedSequence,
    cgState: CodeGeneratorState
  ): Unit = {
    for (gram <- g.sequenceChildren) {
      DaffodilCCodeGenerator.generateCode(gram, cgState)
    }
  }

  private def prod(g: Prod, cgState: CodeGeneratorState): Unit = {
    if (g.guard) DaffodilCCodeGenerator.generateCode(g.gram, cgState)
  }

  private def repOrderedExactlyNSequenceChild(
    g: RepOrderedExactlyNSequenceChild,
    cgState: CodeGeneratorState
  ): Unit = {
    cgState.pushArray(g.context)
    DaffodilCCodeGenerator.generateCode(g.term.termContentBody, cgState)
    cgState.popArray(g.context)
  }

  private def repOrderedExpressionOccursCountSequenceChild(
    g: RepOrderedExpressionOccursCountSequenceChild,
    cgState: CodeGeneratorState
  ): Unit = {
    cgState.pushArray(g.context)
    DaffodilCCodeGenerator.generateCode(g.term.termContentBody, cgState)
    cgState.popArray(g.context)
  }

  private def repOrderedWithMinMaxSequenceChild(
    g: RepOrderedWithMinMaxSequenceChild,
    cgState: CodeGeneratorState
  ): Unit = {
    DaffodilCCodeGenerator.generateCode(g.term.termContentBody, cgState)
  }

  private def scalarOrderedSequenceChild(
    g: ScalarOrderedSequenceChild,
    cgState: CodeGeneratorState
  ): Unit = {
    DaffodilCCodeGenerator.generateCode(g.term.termContentBody, cgState)
  }

  private def seqCompGenerateCode(g: SeqComp, cgState: CodeGeneratorState): Unit = {
    for (gram <- g.children) {
      DaffodilCCodeGenerator.generateCode(gram, cgState)
    }
  }

  private def specifiedLengthExplicit(
    g: SpecifiedLengthExplicit,
    cgState: CodeGeneratorState
  ): Unit = {
    DaffodilCCodeGenerator.generateCode(g.eGram, cgState)
  }

  private def specifiedLengthImplicit(
    g: SpecifiedLengthImplicit,
    cgState: CodeGeneratorState
  ): Unit = {
    DaffodilCCodeGenerator.generateCode(g.eGram, cgState)
  }

  private def specifiedLengthPrefixed(
    g: SpecifiedLengthPrefixed,
    cgState: CodeGeneratorState
  ): Unit = {
    DaffodilCCodeGenerator.generateCode(g.eGram, cgState)
  }
}
