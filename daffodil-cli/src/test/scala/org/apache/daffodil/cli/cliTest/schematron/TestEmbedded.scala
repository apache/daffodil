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

package org.apache.daffodil.cli.cliTest.schematron

import java.util.UUID

import org.apache.daffodil.cli.Main.ExitCode
import org.apache.daffodil.cli.cliTest.Util._

import org.junit.Test

class TestEmbedded {
  @Test def alwaysFails(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/always-fails-1.dfdl.xsd")

    runCLI(args"""parse --validate schematron=${jsonEscape(schema.toString)} -s $schema""") {
      cli =>
        cli.sendLine(UUID.randomUUID.toString, inputDone = true)
        cli.expect("</always-fails>")
    }(ExitCode.ParseError)
  }

  @Test def alwaysFails_2(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/always-fails-1.dfdl.xsd")

    runCLI(args"""parse --validate schematron -s $schema""") { cli =>
      cli.sendLine(UUID.randomUUID.toString, inputDone = true)
      cli.expect("</always-fails>")
    }(ExitCode.ParseError)
  }

  @Test def unitPriceWithoutValidation(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/unit_price.dfdl.xsd")

    runCLI(args"""parse -r list -s $schema""") { cli =>
      cli.send("widget,monday,1,$5.00,$5.00", inputDone = true)
      cli.expect("</ex:list>")
    }(ExitCode.Success)
  }

  @Test def unitPriceWithValidation(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/unit_price.dfdl.xsd")

    runCLI(
      args"""parse -r list --validate schematron=${jsonEscape(schema.toString)} -s $schema"""
    ) { cli =>
      cli.send("widget,monday,1,$5.00,$6.00", inputDone = true)
      cli.expect("</ex:list>")
      cli.expectErr("Validation Error: wrong unit price for widget, monday")
    }(ExitCode.ParseError)
  }

  @Test def unitPriceWithValidationCheckMessage(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/unit_price.dfdl.xsd")

    runCLI(
      args"""parse -r list --validate schematron=${jsonEscape(schema.toString)} -s $schema"""
    ) { cli =>
      cli.send("widget,monday,5,$5.00,$25.00||gadget,tuesday,1,$10.00,$11.00", inputDone = true)
      cli.expect("</ex:list>")
      cli.expectErr("Validation Error: wrong unit price for gadget, tuesday")
    }(ExitCode.ParseError)
  }

  @Test def extends1(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/extends-1.dfdl.xsd")

    runCLI(args"""parse --validate schematron=${jsonEscape(schema.toString)} -s $schema""") {
      cli =>
        cli.send("bob;l;smith", inputDone = true)
        cli.expect("</name>")
    }(ExitCode.Success)
  }

  @Test def extends2(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/extends-1.dfdl.xsd")

    runCLI(args"""parse --validate schematron=${jsonEscape(schema.toString)} -s $schema""") {
      cli =>
        cli.send("ob;;smith", inputDone = true)
        cli.expect("</name>")
    }(ExitCode.Success)
  }

  @Test def extends3(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/extends-1.dfdl.xsd")

    runCLI(args"""parse --validate schematron=${jsonEscape(schema.toString)} -s $schema""") {
      cli =>
        cli.send(";;smith", inputDone = true)
        cli.expectErr("Validation Error: first is blank")
    }(ExitCode.ParseError)
  }

  @Test def extends4(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/extends-1.dfdl.xsd")

    runCLI(args"""parse --validate schematron=${jsonEscape(schema.toString)} -s $schema""") {
      cli =>
        cli.send("bob;l;", inputDone = true)
        cli.expectErr("Validation Error: last is blank")
    }(ExitCode.ParseError)
  }

  @Test def extends5(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/extends-1.dfdl.xsd")

    runCLI(args"""parse --validate schematron=${jsonEscape(schema.toString)} -s $schema""") {
      cli =>
        cli.send(";l;", inputDone = true)
        cli.expectErr("Validation Error: last is blank")
        cli.expectErr("Validation Error: first is blank")
    }(ExitCode.ParseError)
  }

  @Test def testWithNs1(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/with-ns-1.dfdl.xsd")

    runCLI(args"""parse --validate schematron=${jsonEscape(schema.toString)} -s $schema""") {
      cli =>
        cli.send("0;1", inputDone = true)
        cli.expect("</myns:interval>")
    }(ExitCode.Success)
  }

  @Test def testWithNs2(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/with-ns-1.dfdl.xsd")

    runCLI(args"""parse --validate schematron=${jsonEscape(schema.toString)} -s $schema""") {
      cli =>
        cli.send("2;1", inputDone = true)
        cli.expectErr("Validation Error")
    }(ExitCode.ParseError)
  }

  @Test def testWithNs3(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/with-ns-1.dfdl.xsd")

    runCLI(args"""parse --validate schematron=${jsonEscape(schema.toString)} -s $schema""") {
      cli =>
        cli.send("0;0", inputDone = true)
        cli.expectErr("Validation Error")
    }(ExitCode.ParseError)
  }

  @Test def testRelativeImport(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/relative-import.dfdl.xsd")

    runCLI(args"""parse --validate schematron=${jsonEscape(schema.toString)} -s $schema""") {
      cli =>
        cli.sendLine("not-valid-uuid-field-lengths", inputDone = true)
        cli.expect("</uuid>")
        cli.expectErr("Validation Error: uuid wrong length")
    }(ExitCode.ParseError)
  }
}
