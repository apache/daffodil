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
import org.apache.daffodil.cli.cliTest.Util._

import org.junit.Test

class TestCLITdml {

  @Test def test_CLI_Tdml_Trace_singleTest1(): Unit = {
    val tdml = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/Entities.tdml"
    )

    val envs = Map("DAFFODIL_TDML_API_INFOSETS" -> "all")

    runCLI(args"test -i -t $tdml byte_entities_6_08", envs = envs) { cli =>
      // legacy parse
      cli.expect("parser: <Element name='e3'>")
      // sax parse
      cli.expect("parser: <Element name='e3'>")
      cli.expect("[Pass] byte_entities_6_08")
    }(ExitCode.Success)
  }

  @Test def test_CLI_Tdml_Trace_singleTest2(): Unit = {
    val tdml = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/Entities.tdml"
    )

    val envs = Map("DAFFODIL_TDML_API_INFOSETS" -> "scala")

    runCLI(args"test -i -t $tdml byte_entities_6_08", envs = envs) { cli =>
      // parse
      cli.expect("parser: <Element name='e3'>")
      // unparse
      cli.expect("parser: not available")
      cli.expect("[Pass] byte_entities_6_08")
    }(ExitCode.Success)
  }
}
