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

package org.apache.daffodil.saving

import org.junit.Test
import scala.xml.XML
import java.nio.file.Paths

import org.apache.daffodil.CLI.Util._
import org.apache.daffodil.Main.ExitCode
import org.apache.daffodil.xml.XMLUtils

class TestCLIEncodeDecodeEXI {

  @Test def test_CLI_Encode_Decode_EXI_SA(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/usertests/Book2.dfdl.xsd")
    val infosetPath = path("daffodil-test/src/test/resources/org/apache/daffodil/usertests/test_Book2.expected.xml")
    val infosetXML = XML.loadFile(infosetPath.toFile)

    withTempDir { tempDir =>
      val tempEXI = Paths.get(tempDir.toString, "temp.exi")
      val tempXML = Paths.get(tempDir.toString, "temp.xml")

      // Encode infoset to schema aware EXI
      runCLI(args"exi -s $schema -o $tempEXI $infosetPath") { cli =>
      } (ExitCode.Success)

      // Decode EXI to XML and compare against original XML infoset
      runCLI(args"exi -d -s $schema -o $tempXML $tempEXI") { cli =>
      } (ExitCode.Success)

      val resultNode = XML.loadFile(tempXML.toFile)
      XMLUtils.compareAndReport(infosetXML, resultNode)
    }
  }

  @Test def test_CLI_Encode_Decode_EXI(): Unit = {
    val inputXML = <person><name>Edward</name><age>42</age></person>

    withTempDir { tempDir =>
      val tempEXI = Paths.get(tempDir.toString, "temp.exi")
      val tempXML = Paths.get(tempDir.toString, "temp.xml")

      runCLI(args"exi -o $tempEXI") { cli =>
        cli.sendLine(inputXML.toString, inputDone = true)
      } (ExitCode.Success)

      runCLI(args"exi -d -o $tempXML $tempEXI") { cli =>
      } (ExitCode.Success)

      val resultNode = XML.loadFile(tempXML.toFile)
      XMLUtils.compareAndReport(inputXML, resultNode)
    }
  }

  @Test def test_CLI_EncodeBadFile_EXI(): Unit = {
    val badXML = path("daffodil-test/src/test/resources/org/apache/daffodil/usertests/Book2.csv")

    runCLI(args"exi $badXML") { cli =>
      cli.expectErr("Error parsing input XML")
    } (ExitCode.Failure)
  }

  @Test def test_CLI_DecodeBadFile_EXI(): Unit = {
    val badEXI = path("daffodil-test/src/test/resources/org/apache/daffodil/usertests/Book2.csv")

    runCLI(args"exi -d $badEXI") { cli =>
      cli.expectErr("No valid EXI document")
    } (ExitCode.Failure)
  }

  @Test def test_CLI_LoadBadSchema_EXI(): Unit = {
    val badSchema = path("daffodil-test/src/test/resources/org/apache/daffodil/usertests/Book2.csv")

    runCLI(args"exi -s $badSchema") { cli =>
      cli.expectErr("Error creating EXI grammar for the supplied schema")
    } (ExitCode.Failure)
  }

  @Test def test_CLI_AttemptDecodeOfSA_EXI(): Unit = {
    val schema = path("daffodil-test/src/test/resources/org/apache/daffodil/usertests/Book2.dfdl.xsd")
    val infosetPath = path("daffodil-test/src/test/resources/org/apache/daffodil/usertests/test_Book2.expected.xml")

    withTempFile { tempEXI =>
      // Encode infoset to schema aware EXI
      runCLI(args"exi -s $schema -o $tempEXI $infosetPath") { cli =>
      } (ExitCode.Success)

      // Attempt to decode EXI using schema unaware decoding
      runCLI(args"exi -d $tempEXI") { cli =>
        cli.expectErr("Error decoding EXI input")
      } (ExitCode.Failure)
    }
  }
}
