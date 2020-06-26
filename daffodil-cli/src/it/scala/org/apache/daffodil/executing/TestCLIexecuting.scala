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

package org.apache.daffodil.executing

import org.junit.Assert._
import org.junit.Test
import org.apache.daffodil.CLI.Util
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.matches
import net.sf.expectit.ExpectIOException

class TestCLIexecuting {

  val output3 = Util.getExpectedString("output3.txt")
  val output13 = Util.getExpectedString("output13.txt", true)
  val output14 = Util.getExpectedString("output14.txt", true)
  val output15 = Util.getExpectedString("output15.txt", true)
  val output16 = Util.getExpectedString("output16.txt", true)

  @Test def test_995_CLI_Executing_Listing_negativeTest01(): Unit = {
    val tdmlFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section31/escape_characters/Escapes.tdml")
    val testTdmlFile = if (Util.isWindows) Util.cmdConvert(tdmlFile) else tdmlFile

    val shell = Util.start("")

    try {
      val cmd = String.format("%s test %s escape_entry1 escape_entry2-11 escape_entry1-5 escape_entry4_3", Util.binPath, testTdmlFile)
      shell.sendLine(cmd)
      shell.expect(contains(output3))
      shell.sendLine("exit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1001_CLI_Executing_Listing_execRegex01(): Unit = {
    val tdmlFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section31/escape_characters/Escapes.tdml")
    val testTdmlFile = if (Util.isWindows) Util.cmdConvert(tdmlFile) else tdmlFile

    val shell = Util.start("")

    try {
      val cmd = String.format("%s test --regex %s \"escape_entry4_\\d\"", Util.binPath, testTdmlFile)
      shell.sendLine(cmd)
      shell.expect(contains(output13))
      shell.sendLine("exit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1000_CLI_Executing_Listing_listRegex02(): Unit = {
    val tdmlFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section31/escape_characters/Escapes.tdml")
    val testTdmlFile = if (Util.isWindows) Util.cmdConvert(tdmlFile) else tdmlFile

    val shell = Util.start("", timeout = 5)
    try {
      val cmd = String.format("%s test -l --regex %s \"escape_entryb-\\d+\"", Util.binPath, testTdmlFile)
      shell.sendLine(cmd)
      shell.expect(matches(""))
    } catch {
      case ex: ExpectIOException => {
        fail("Output was found when none was expected.")
      }
    } finally {
      shell.sendLine("exit")
      shell.close()
    }
  }

  @Test def test_999_CLI_Executing_Listing_listRegex01(): Unit = {
    val tdmlFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section31/escape_characters/Escapes.tdml")
    val testTdmlFile = if (Util.isWindows) Util.cmdConvert(tdmlFile) else tdmlFile

    val shell = Util.start("")

    try {
      val cmd = String.format("%s test -l --regex %s \"escape_entry4_\\d+\"", Util.binPath, testTdmlFile)
      shell.sendLine(cmd)
      shell.expect(contains(output14))
      shell.sendLine("exit")
    } finally {
      shell.close()
    }
  }

  //
  // Test removed to scala-debug as order of execution of the individual tests is not deterministic.
  // You'd like them to go in the order they appear in the file, but that's not the case
  // necessarily: http://stackoverflow.com/questions/3693626/how-to-run-test-methods-in-specific-order-in-junit4
  // JIRA DFDL-1240
  //
  //  @Test def test_994_CLI_Executing_Listing_execAll() {
  //    val tdmlFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section31/escape_characters/Escapes.tdml")
  //    val testTdmlFile = if (Util.isWindows) Util.cmdConvert(tdmlFile) else tdmlFile
  //
  //    val shell = Util.start("")
  //
  //    try {
  //      val cmd = String.format("%s test %s", Util.binPath, testTdmlFile)
  //      shell.sendLine(cmd)
  //      shell.expect(contains(output15))
  //      shell.sendLine("exit")
  //    } finally {
  //      shell.close()
  //    }
  //  }

  @Test def test_993_CLI_Executing_Listing_listAll(): Unit = {
    val tdmlFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/Entities.tdml")
    val testTdmlFile = if (Util.isWindows) Util.cmdConvert(tdmlFile) else tdmlFile

    val shell = Util.start("")

    try {
      shell.sendLine(String.format("%s test -l %s", Util.binPath, testTdmlFile))
      shell.expect(contains(output16))
      shell.sendLine("exit")
    } finally {
      shell.close()
    }
  }

  @Test def test_992_CLI_Executing_Listing_singleTestList(): Unit = {
    val tdmlFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/Entities.tdml")
    val testTdmlFile = if (Util.isWindows) Util.cmdConvert(tdmlFile) else tdmlFile

    val shell = Util.start("")

    try {
      val cmd = String.format("%s test -l %s byte_entities_6_08", Util.binPath, testTdmlFile)
      shell.sendLine(cmd)
      shell.expect(contains("byte_entities_6_08"))
      shell.sendLine("exit")
    } finally {
      shell.close()
    }
  }

  @Test def test_990_CLI_Executing_Listing_singleTest(): Unit = {
    val tdmlFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/Entities.tdml")
    val testTdmlFile = if (Util.isWindows) Util.cmdConvert(tdmlFile) else tdmlFile

    val shell = Util.start("")

    try {
      val cmd = String.format("%s test %s byte_entities_6_08", Util.binPath, testTdmlFile)
      shell.sendLine(cmd)
      shell.expect(contains("[Pass] byte_entities_6_08"))
      shell.sendLine("exit")
    } finally {
      shell.close()
    }
  }
}
