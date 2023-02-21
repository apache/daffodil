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

import org.junit.Test

/**
 * Runs DaffodilCExamplesGenerator in a test since "sbt coverage compile" doesn't
 * capture call of genCExamples
 */
class TestDaffodilCExamplesGenerator {
  // Test added for code coverage and debugging
  @Test def test_DaffodilCExamplesGenerator_main(): Unit = {
    // Generate the C examples in a safe place (target/examples)
    val rootDir = if (os.exists(os.pwd / "src")) os.pwd / os.up else os.pwd
    val examplesDir = rootDir / "daffodil-codegen-c" / "target" / "examples"
    val args = Array(examplesDir.toString)
    DaffodilCExamplesGenerator.main(args)

    // Verify the C examples were generated
    val generatedCode = examplesDir / "variablelen" / "generated_code.c"
    assert(os.exists(generatedCode))
  }
}
