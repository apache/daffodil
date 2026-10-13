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

package org.apache.daffodil.cliTest

import org.apache.daffodil.cli.Main.ExitCode
import org.apache.daffodil.cli.cliTest.Util.*

import org.junit.Test

class TestCLIEnvTunables {

  // parses a 4 byte hexBinary and sets no tunables of its own
  private val simpleTypes = path(
    "daffodil-test/src/test/resources/org/apache/daffodil/section05/simple_types/SimpleTypes.tdml"
  )

  // sets maxHexBinaryLengthInBytes to 10 in its own config and expects an 11 byte hexBinary to fail
  private val tunables = path(
    "daffodil-test/src/test/resources/org/apache/daffodil/section00/general/tunables.tdml"
  )

  @Test def test_CLI_EnvTunables_smallHexLimitFails(): Unit = {
    val envs = Map("DAFFODIL_TDML_TUNABLES" -> "maxHexBinaryLengthInBytes=1")

    runCLI(args"test $simpleTypes hexBinary_01", envs = envs) { cli =>
      cli.expect("[Fail] hexBinary_01")
    }(ExitCode.TestError)
  }

  @Test def test_CLI_EnvTunables_largeHexLimitPasses(): Unit = {
    val envs = Map("DAFFODIL_TDML_TUNABLES" -> "maxHexBinaryLengthInBytes=100")

    runCLI(args"test $simpleTypes hexBinary_01", envs = envs) { cli =>
      cli.expect("[Pass] hexBinary_01")
    }(ExitCode.Success)
  }

  @Test def test_CLI_EnvTunables_multipleTunables(): Unit = {
    val envs = Map(
      "DAFFODIL_TDML_TUNABLES" -> "maxSkipLengthInBytes=4, maxHexBinaryLengthInBytes=1"
    )

    runCLI(args"test $simpleTypes hexBinary_01", envs = envs) { cli =>
      cli.expect("[Fail] hexBinary_01")
    }(ExitCode.TestError)
  }

  @Test def test_CLI_EnvTunables_testConfigOverridesEnv(): Unit = {
    val envs = Map("DAFFODIL_TDML_TUNABLES" -> "maxHexBinaryLengthInBytes=100000")

    runCLI(args"test $tunables maxHexBinaryError", envs = envs) { cli =>
      cli.expect("[Pass] maxHexBinaryError")
    }(ExitCode.Success)
  }
}
