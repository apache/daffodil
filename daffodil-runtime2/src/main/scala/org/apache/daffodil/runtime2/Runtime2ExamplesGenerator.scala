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

import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.lib.api.TDMLImplementation

/** 
 * Runs from "sbt compile" to keep all examples of generated C code up
 * to date whenever C code generator is changed
 */
object Runtime2ExamplesGenerator {

  // Update one example of generated C code from a sample schema
  private def updateRuntime2Example(
    schemaFile: os.Path,
    rootName: Option[String],
    exampleDir: os.Path,
  ): Unit = {
    // Generate example code from the sample schema
    val pf = Compiler().compileFile(schemaFile.toIO, rootName)
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
      createFolders = true,
    )
    os.copy(generatedCodeFile, exampleCodeFile, replaceExisting = true, createFolders = true)

    // Print the example directory so "sbt 'show genRuntime2Examples'" can list it
    println(exampleDir)

    // JVM will remove tempDir automatically when it exits; this is just in case
    os.remove.all(tempDir)
  }

  /**
   * Make sure "sbt compile" calls this main method
   */
  def main(args: Array[String]): Unit = {
    // We expect one mandatory parameter, the absolute location of the examples directory
    assert(args.length == 1, s"Usage: $Runtime2ExamplesGenerator <examples directory location>")

    // Get paths to our sample schemas and their corresponding example directories
    val rootDir = if (os.exists(os.pwd / "src")) os.pwd / os.up else os.pwd

    val schemaDir =
      rootDir / "daffodil-runtime2" / "src" / "test" / "resources" / "org" / "apache" / "daffodil" / "runtime2"
    val exNumsSchema = schemaDir / "ex_nums.dfdl.xsd"
    val exNumsRootName = None
    val nestedSchema = schemaDir / "nested.dfdl.xsd"
    val nestedRootName = Some("NestedUnion")
    val padTestSchema = schemaDir / "padtest.dfdl.xsd"
    val padTestRootName = None
    val variableLenSchema = schemaDir / "variablelen.dfdl.xsd"
    val variableLenRootName = Some("expressionElement")

    val examplesDir = os.Path(args(0))
    val exNumsExampleDir = examplesDir / "ex_nums"
    val nestedExampleDir = examplesDir / "NestedUnion"
    val padTestExampleDir = examplesDir / "padtest"
    val variableLenExampleDir = examplesDir / "variablelen"

    // Update each example of generated C code
    updateRuntime2Example(exNumsSchema, exNumsRootName, exNumsExampleDir)
    updateRuntime2Example(nestedSchema, nestedRootName, nestedExampleDir)
    updateRuntime2Example(padTestSchema, padTestRootName, padTestExampleDir)
    updateRuntime2Example(variableLenSchema, variableLenRootName, variableLenExampleDir)
  }
}
