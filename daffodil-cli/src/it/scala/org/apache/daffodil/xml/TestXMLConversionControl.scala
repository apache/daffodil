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

package org.apache.daffodil.xml

import org.apache.commons.io.FileUtils
import org.junit.Test
import org.apache.daffodil.CLI.Util._
import org.apache.daffodil.Main.ExitCode
import org.junit.Assert.assertTrue

import java.nio.charset.StandardCharsets

class TestXMLConversionControl {

  //
  // To run tests conveniently under IntelliJ IDEA,
  // rename the src/test dir to src/test1. Rename the src/it dir to src/test.
  // Then modify this val to be "test".
  // Then you can run these as ordinary junit-style tests under the IDE.
  val test = "it"

  @Test def test_CLI_XMLConversionControlConvertCR(): Unit = {
    withTempFile { output =>
      val schema = path(s"daffodil-cli/src/$test/resources/org/apache/daffodil/CLI/aString.dfdl.xsd")
      val config = path(s"daffodil-cli/src/$test/resources/org/apache/daffodil/CLI/config-convertCR.cfg.xml")
      val input = path(s"daffodil-cli/src/$test/resources/org/apache/daffodil/CLI/input/inputWithCRLFs.bin")

      runCLI(args"parse -s $schema -c $config --root a -o $output $input") {
        cli => //ok
      }(ExitCode.Success)

      val res = FileUtils.readFileToString(output.toFile, StandardCharsets.UTF_8)
      assertTrue(res.contains("<ex:a xmlns:ex=\"urn:ex\">abc\ndef\nghi</ex:a>"))
    }
  }

  @Test def test_CLI_XMLConversionControlPreserveCRParse(): Unit = {
    withTempFile { output =>
      val schema = path(s"daffodil-cli/src/$test/resources/org/apache/daffodil/CLI/aString.dfdl.xsd")
      val config = path(s"daffodil-cli/src/$test/resources/org/apache/daffodil/CLI/config-preserveCR.cfg.xml")
      val input = path(s"daffodil-cli/src/$test/resources/org/apache/daffodil/CLI/input/inputWithCRLFs.bin")

      runCLI(args"parse -s $schema -c $config --root a -o $output $input") { cli =>
        //ok
      }(ExitCode.Success)

      val res = FileUtils.readFileToString(output.toFile, StandardCharsets.UTF_8)
      assertTrue(res.contains("<ex:a xmlns:ex=\"urn:ex\">abc\uE00D\ndef\uE00D\nghi</ex:a>"))
    }
  }

  @Test def test_CLI_XMLConversionControlPreserveCRRoundTrip(): Unit = {
    withTempFile { output =>
      withTempFile { xmlOut =>
        val schema = path(s"daffodil-cli/src/$test/resources/org/apache/daffodil/CLI/aString.dfdl.xsd")
        val config = path(s"daffodil-cli/src/$test/resources/org/apache/daffodil/CLI/config-preserveCR.cfg.xml")
        val input = path(s"daffodil-cli/src/$test/resources/org/apache/daffodil/CLI/input/inputWithCRLFs.bin")

        var cmd = args"parse -s $schema -c $config --root a -o $xmlOut $input "
        runCLI(cmd) { cli =>
          // ok
        }(ExitCode.Success)

        cmd = args"unparse -s $schema -c $config --root a -o $output $xmlOut"
        runCLI(cmd) { cli =>
          // ok
        }(ExitCode.Success)


        val xml = FileUtils.readFileToString(xmlOut.toFile, StandardCharsets.UTF_8)
        assertTrue(xml.toString.contains("abc\uE00D\ndef\uE00D\nghi"))
      }

      val xml = FileUtils.readFileToString(output.toFile, StandardCharsets.UTF_8)
      assertTrue(xml.toString.contains("abc\r\ndef\r\nghi"))
    }
  }

  @Test def test_CLI_XMLConversionControlPreserveCRUnparseToFile(): Unit = {
    withTempFile { output =>
      val schema = path(s"daffodil-cli/src/$test/resources/org/apache/daffodil/CLI/aString.dfdl.xsd")
      val config = path(s"daffodil-cli/src/$test/resources/org/apache/daffodil/CLI/config-preserveCR.cfg.xml")

      runCLI(args"unparse -s $schema -c $config --root a -o $output ") { cli =>
        cli.send("<ex:a xmlns:ex='urn:ex'>abc\uE00D\ndef\uE00D\nghi</ex:a>", inputDone = true)
      }(ExitCode.Success)

      val res = FileUtils.readFileToString(output.toFile, StandardCharsets.UTF_8)
      assertTrue(res.contains("abc\r\ndef\r\nghi"))
    }
  }
}
