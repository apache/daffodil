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

package org.apache.daffodil.tunables

import org.junit.Test
import org.apache.daffodil.CLI.Util
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.eof

class TestCLITunables {

  val unqualifiedPathStep01 = Util.getExpectedString("unqualified_path_step_01.txt")
  val unqualifiedPathStep02 = Util.getExpectedString("unqualified_path_step_02.txt")
  val unqualifiedPathStep03 = Util.getExpectedString("unqualified_path_step_03.txt")
  val unqualifiedPathStep04 = Util.getExpectedString("unqualified_path_step_04.txt")

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_noNamespace_test_01(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format(Util.echoN("12") + "| %s parse -s %s -r test_01 -TunqualifiedPathStepPolicy=noNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains(unqualifiedPathStep01))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_noNamespace_test_02(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("", true)

    try {
      val cmd = String.format(Util.echoN("12") + "| %s parse -s %s -r test_02 -TunqualifiedPathStepPolicy=noNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      //shell.expect(contains(unqualifiedPathStep02))
      shell.expect(contains("Schema Definition Error"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_noNamespace_test_03(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("", true)

    try {
      val cmd = String.format(Util.echoN("12") + "| %s parse -s %s -r test_03 -TunqualifiedPathStepPolicy=noNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      //shell.expect(contains(unqualifiedPathStep03))
      shell.expect(contains("Schema Definition Error"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_noNamespace_test_04(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format(Util.echoN("12") + "| %s parse -s %s -r test_04 -TunqualifiedPathStepPolicy=noNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains(unqualifiedPathStep04))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }



  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_defaultNamespace_test_01(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format(Util.echoN("12") + "| %s parse -s %s -r test_01 -TunqualifiedPathStepPolicy=defaultNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains(unqualifiedPathStep01))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_defaultNamespace_test_02(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format(Util.echoN("12") + "| %s parse -s %s -r test_02 -TunqualifiedPathStepPolicy=defaultNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains(unqualifiedPathStep02))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_defaultNamespace_test_03(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("", true)

    try {
      val cmd = String.format(Util.echoN("12") + "| %s parse -s %s -r test_03 -TunqualifiedPathStepPolicy=defaultNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      //shell.expect(contains(unqualifiedPathStep03))
      shell.expect(contains("Schema Definition Error"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_defaultNamespace_test_04(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("", true)

    try {
      val cmd = String.format(Util.echoN("12") + "| %s parse -s %s -r test_04 -TunqualifiedPathStepPolicy=defaultNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("Schema Definition Error"))
      //shell.expect(contains(unqualifiedPathStep04))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }



  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_preferDefaultNamespace_test_01(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format(Util.echoN("12") + "| %s parse -s %s -r test_01 -TunqualifiedPathStepPolicy=preferDefaultNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains(unqualifiedPathStep01))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_preferDefaultNamespace_test_02(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format(Util.echoN("12") + "| %s parse -s %s -r test_02 -TunqualifiedPathStepPolicy=preferDefaultNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains(unqualifiedPathStep02))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_preferDefaultNamespace_test_03(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("", true)

    try {
      val cmd = String.format(Util.echoN("12") + "| %s parse -s %s -r test_03 -TunqualifiedPathStepPolicy=preferDefaultNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      //shell.expect(contains(unqualifiedPathStep03))
      shell.expect(contains("Schema Definition Error"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_preferDefaultNamespace_test_04(): Unit = {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format(Util.echoN("12") + "| %s parse -s %s -r test_04 -TunqualifiedPathStepPolicy=preferDefaultNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains(unqualifiedPathStep04))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

}
