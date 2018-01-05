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

package org.apache.daffodil.listing

import org.junit.Test
import org.apache.daffodil.CLI.Util
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.eof

class TestCLIlisting {

  @Test def test_992_CLI_Executing_Listing_singleTestList() {
    val tdmlFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/Entities.tdml")
    val testTdmlFile = if (Util.isWindows) Util.cmdConvert(tdmlFile) else tdmlFile
    val shell = Util.start("")

    try {
      val cmd = String.format("%s test -l %s byte_entities_6_08", Util.binPath, testTdmlFile)
      shell.sendLine(cmd)
      shell.expect(contains("byte_entities_6_08"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }
  /*
  @Test def test_993_CLI_Executing_Listing_listAll() {
    val shell = ex.spawn("/bin/bash")
    shell.send("/bin/grep -c 'parserTestCase>' daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/Entities.tdml\n")
    val num = shell.getCurrentStandardOutContents()
    shell.send(Util.binPath + " test -l daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/Entities.tdml | tee /dev/tty | wc -l\n")
    shell.expect(num)
    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_999_CLI_Executing_Listing_listRegex01() {
    val cmd = ""Util.binPath + " test -l --regex daffodil-test/src/test/resources/org/apache/daffodil/section31/escape_characters/Escapes.tdml "escape_entry4-\\d+"\n"""
    val shell = Util.start(cmd)
    shell.expect("escape_entry4-20")
    shell.expect("escape_entry4-21")
    shell.send("exit\n")
    shell.expectClose()
  }

  @Test def test_1000_CLI_Executing_Listing_listRegex02() {
    val cmd = Util.binPath + " test -l --regex daffodil-test/src/test/resources/org/apache/daffodil/section31/escape_characters/Escapes.tdml 'escape_entryb-\\d+'\n"
    val shell = Util.start(cmd)
    val output = shell.getCurrentStandardOutContents()
    if (output != ""){
      throw new Exception("Output does not match expected.")
    }
    shell.send("exit\n")
    shell.expectClose()
  }
*/

  @Test def test_1016_CLI_Executing_Listing_listVerbose() {
    val tdmlFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section07/assertions/assert.tdml")
    val testTdmlFile = if (Util.isWindows) Util.cmdConvert(tdmlFile) else tdmlFile
    val shell = Util.start("")

    try {
      shell.sendLine(String.format("%s test -l --regex %s assertPattern.*", Util.binPath, testTdmlFile))
      shell.expect(contains("assertPatternAndExp"))

      shell.sendLine(String.format("%s test -l -i --regex %s assertPattern.*", Util.binPath, testTdmlFile))
      shell.expect(contains("assertPatternAndExp              s2                e3         Section 7 - Assert Schema Error for Expression/Pattern - DFDL-7-047R"))

      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }
}
