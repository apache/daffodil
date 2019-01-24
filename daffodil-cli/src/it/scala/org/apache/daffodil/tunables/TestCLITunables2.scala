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

class TestCLITunables2 {

  /**
   * Suppresses SDW messages.
   */
  @Test def test_CLI_Parsing_SuppressSDEWarnings1() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/suppressWarnTest.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      // note: 2>&1 is shell-speak for "connect stderr into stdout"
      val cmd = String.format("""echo a,b| %s parse -s %s -TsuppressSchemaDefinitionWarnings=all 2>&1""", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("""<ex:e1 xmlns:ex="http://example.com">
  <s1>a</s1>
  <s2>b
</s2>
</ex:e1>""".replace("\r\n", "\n")))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  /**
   * Will display SDW warnings. Does not set the tunable that suppresses them.
   */
  @Test def test_CLI_Parsing_SuppressSDEWarnings2() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/suppressWarnTest.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      // note: 2>&1 is shell-speak for "connect stderr into stdout"
      val cmd = String.format("""echo a,b| %s parse -s %s 2>&1""", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("""Schema Definition Warning"""))
      shell.expect(contains("""dfdl:lengthKind"""))
      shell.expect(contains("""delimited"""))
      shell.expect(contains("""dfdl:length"""))
      shell.expect(contains("""<ex:e1 xmlns:ex="http://example.com">
  <s1>a</s1>
  <s2>b
</s2>
</ex:e1>""".replace("\r\n", "\n")))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }
}
