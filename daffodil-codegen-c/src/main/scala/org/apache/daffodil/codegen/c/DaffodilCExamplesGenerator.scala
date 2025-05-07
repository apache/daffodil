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

import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.iapi.TDMLImplementation
import org.apache.daffodil.runtime1.iapi.DFDL.ProcessorFactory

/** 
 * Runs from "sbt compile" to keep all examples of generated C code up
 * to date whenever C code generator is changed
 */
object DaffodilCExamplesGenerator {

  // Update one example of generated C code from a sample schema
  private def updateCExample(
    schemaFile: os.Path,
    optRootName: Option[String],
    exampleDir: os.Path
  ): Unit = {
    // Generate example code from the sample schema
    val pf = Compiler().compileFile(schemaFile.toIO, optRootName).asInstanceOf[ProcessorFactory]
    assert(!pf.isError, pf.getDiagnostics.map(_.getMessage()).mkString("\n"))
    val cg = pf.forLanguage("c")
    val tempDir = os.temp.dir(dir = null, prefix = TDMLImplementation.DaffodilC.toString)
    val codeDir = cg.generateCode(tempDir.toString)
    assert(!cg.isError, cg.getDiagnostics.map(_.getMessage()).mkString("\n"))

    // Replace the example generated files with the newly generated files
    val generatedCodeHeader = codeDir / "libruntime" / "generated_code.h"
    val generatedCodeFile = codeDir / "libruntime" / "generated_code.c"
    val exampleCodeHeader = exampleDir / "generated_code.h"
    val exampleCodeFile = exampleDir / "generated_code.c"
    os.copy(
      generatedCodeHeader,
      exampleCodeHeader,
      replaceExisting = true,
      createFolders = true
    )
    os.copy(generatedCodeFile, exampleCodeFile, replaceExisting = true, createFolders = true)

    // Print the example directory so "sbt 'show genCExamples'" can list it
    println(exampleDir)

    // JVM will remove tempDir automatically when it exits; this is just in case
    os.remove.all(tempDir)
  }

  /**
   * Make sure "sbt compile" calls this main method
   */
  def main(args: Array[String]): Unit = {
    // We expect one mandatory parameter, the absolute location of the examples directory
    assert(
      args.length == 1,
      s"Usage: $DaffodilCExamplesGenerator <examples directory location>"
    )

    // Get paths to our sample schemas and their corresponding example directories
    val rootDir = if (os.exists(os.pwd / "src")) os.pwd / os.up else os.pwd
    val schemaDir =
      rootDir / "daffodil-codegen-c" / "src" / "test" / "resources" / "org" / "apache" / "daffodil" / "codegen" / "c"
    val examplesDir = os.Path(args(0))

    // Update each example of generated C code
    val examples = Array(
      (schemaDir / "ex_nums.dfdl.xsd", None, examplesDir / "ex_nums"),
      (schemaDir / "nested.dfdl.xsd", Some("NestedUnion"), examplesDir / "NestedUnion"),
      (schemaDir / "padtest.dfdl.xsd", None, examplesDir / "padtest"),
      (schemaDir / "simple.dfdl.xsd", Some("simple"), examplesDir / "simple"),
      (
        schemaDir / "variablelen.dfdl.xsd",
        Some("expressionElement"),
        examplesDir / "variablelen"
      )
    )

    // Update each example of generated C code
    examples.foreach { case (schemaFile, optRootName, exampleDir) =>
      updateCExample(schemaFile, optRootName, exampleDir)
    }
  }
}
