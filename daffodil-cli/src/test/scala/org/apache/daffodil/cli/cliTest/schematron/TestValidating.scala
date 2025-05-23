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

import org.apache.daffodil.cli.Main.ExitCode
import org.apache.daffodil.cli.cliTest.Util._

import org.junit.Test

class TestValidating {

  // always fails sch, but no validate flag so it should pass
  @Test def nonShouldPass(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/string.dfdl.xsd")
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/uuid.txt")

    runCLI(args"parse -s $schema $input") { cli =>
      cli.expect("<never-fails>2f6481e6-542c-11eb-ae93-0242ac130002</never-fails>")
    }(ExitCode.Success)
  }

  // always fails sch, with validate flag should fail
  @Test def failShouldFail(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/string.dfdl.xsd")
    val schematron = path("daffodil-schematron/src/test/resources/sch/always-fails.sch")
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/uuid.txt")

    runCLI(args"""parse --validate schematron=${jsonEscape(
        schematron.toString
      )} -s $schema $input""") { cli =>
      cli.expect("<never-fails>2f6481e6-542c-11eb-ae93-0242ac130002</never-fails>")
      cli.expectErr("[error] Validation Error: never fails")
    }(ExitCode.ParseError)
  }

  // never fails sch, with validate flag should pass
  @Test def passShouldPass(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/string.dfdl.xsd")
    val schematron = path("daffodil-schematron/src/test/resources/sch/never-fails.sch")
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/uuid.txt")

    runCLI(args"""parse --validate schematron=${jsonEscape(
        schematron.toString
      )} -s $schema $input""") { cli =>
      cli.expect("<never-fails>2f6481e6-542c-11eb-ae93-0242ac130002</never-fails>")
    }(ExitCode.Success)
  }

  // fails to resolve included schema
  @Test def failToResolve(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/string.dfdl.xsd")
    val schematron = path("daffodil-schematron/src/test/resources/sch/missing-include.sch")
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/uuid.txt")

    runCLI(args"""parse --validate schematron=${jsonEscape(
        schematron.toString
      )} -s $schema $input""") { cli =>
      cli.expectErr("Bad arguments")
      cli.expectErr("does-no-exist/title-rules.sch")
    }(ExitCode.Usage)
  }
}
