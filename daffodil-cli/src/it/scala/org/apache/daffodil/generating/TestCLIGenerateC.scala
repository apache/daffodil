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

package org.apache.daffodil.generating

import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.eof
import org.apache.daffodil.CLI.Util
import org.junit.After
import org.junit.Test

/**
 * Checks that we can run the "daffodil generate c" subcommand with
 * various options and get expected outputs.
 */
class TestCLIGenerateC {

  val daffodil: String = Util.binPath
  lazy val schemaFile: String = if (Util.isWindows) Util.cmdConvert(sf) else sf
  val sf: String = Util.daffodilPath("daffodil-runtime2/src/test/resources/org/apache/daffodil/runtime2/ex_nums.dfdl.xsd")
  // Ensure all tests remove tempDir after creating it
  val tempDir: os.Path = os.temp.dir()

  @After def after(): Unit = {
    os.remove.all(tempDir)
  }

  @Test def test_CLI_Generate_schema(): Unit = {
    val generateCmd = s"$daffodil generate c -s $schemaFile $tempDir"
    val exitCmd = "exit"

    val shell = Util.start("")
    try {
      shell.sendLine(generateCmd)
      shell.sendLine(exitCmd)
      shell.expect(eof())
    } finally {
      shell.close()
    }

    assert(os.exists(tempDir/"c"/"libruntime"/"generated_code.c"))
  }

  @Test def test_CLI_Generate_noC_error(): Unit = {
    val generateCmd = s"$daffodil generate -s $schemaFile $tempDir"
    val exitCmd = "exit"

    val shell = Util.start("", expectErr = true)
    try {
      shell.sendLine(generateCmd)
      shell.expect(contains("Unknown option 's'"))
      shell.sendLine(exitCmd)
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Generate_otherThanC_error(): Unit = {
    val generateCmd = s"$daffodil generate vhld -s $schemaFile $tempDir"
    val exitCmd = "exit"

    val shell = Util.start("", expectErr = true)
    try {
      shell.sendLine(generateCmd)
      shell.expect(contains("Unknown option 's'"))
      shell.sendLine(exitCmd)
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Generate_noSchema_error(): Unit = {
    val generateCmd = s"$daffodil generate c $tempDir"
    val exitCmd = "exit"

    val shell = Util.start("", expectErr = true)
    try {
      shell.sendLine(generateCmd)
      shell.expect(contains("Required option 'schema' not found"))
      shell.sendLine(exitCmd)
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Generate_twoSchema_error(): Unit = {
    val generateCmd = s"$daffodil generate c -s $schemaFile -s $schemaFile $tempDir"
    val exitCmd = "exit"

    val shell = Util.start("", expectErr = true)
    try {
      shell.sendLine(generateCmd)
      shell.expect(contains("you should provide exactly one argument"))
      shell.sendLine(exitCmd)
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Generate_verbose(): Unit = {
    val generateCmd = s"$daffodil -v generate c -s $schemaFile $tempDir"
    val exitCmd = "exit"

    val shell = Util.start("", expectErr = true)
    try {
      shell.sendLine(generateCmd)
      shell.expect(contains("[info] Time (compiling)"))
      shell.expect(contains("[info] Time (generating)"))
      shell.sendLine(exitCmd)
      shell.expect(eof())
    } finally {
      shell.close()
    }

    assert(os.exists(tempDir/"c"/"libruntime"/"generated_code.c"))
  }

  @Test def test_CLI_Generate_root(): Unit = {
    val generateCmd = s"$daffodil generate c -s $schemaFile -r {http://example.com}ex_nums $tempDir"
    val exitCmd = "exit"

    val shell = Util.start("")
    try {
      shell.sendLine(generateCmd)
      shell.sendLine(exitCmd)
      shell.expect(eof())
    } finally {
      shell.close()
    }

    assert(os.exists(tempDir/"c"/"libruntime"/"generated_code.c"))
  }

  @Test def test_CLI_Generate_root_error(): Unit = {
    val generateCmd = s"$daffodil generate c -s $schemaFile -r {ex}ex_nums $tempDir"
    val exitCmd = "exit"

    val shell = Util.start("", expectErr = true)
    try {
      shell.sendLine(generateCmd)
      shell.expect(contains("Schema Definition Error"))
      shell.expect(contains("No global element found for {ex}ex_nums"))
      shell.sendLine(exitCmd)
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Generate_namespaceNoRoot_error(): Unit = {
    val generateCmd = s"$daffodil generate c -s $schemaFile -r {http://example.com} $tempDir"
    val exitCmd = "exit"

    val shell = Util.start("", expectErr = true)
    try {
      shell.sendLine(generateCmd)
      shell.expect(contains("Invalid syntax for extended QName"))
      shell.sendLine(exitCmd)
      shell.expect(eof())
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Generate_tunable(): Unit = {
    val generateCmd = s"$daffodil generate c -s $schemaFile -T parseUnparsePolicy=parseOnly $tempDir"
    val exitCmd = "exit"

    val shell = Util.start("")
    try {
      shell.sendLine(generateCmd)
      shell.sendLine(exitCmd)
      shell.expect(eof())
    } finally {
      shell.close()
    }

    assert(os.exists(tempDir/"c"/"libruntime"/"generated_code.c"))
  }
}
