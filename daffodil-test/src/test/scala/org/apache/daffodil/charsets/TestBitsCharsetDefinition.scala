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

package org.apache.daffodil.charsets

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestCharsets {

  val testDir = "/org/apache/daffodil/charsets/"
  val runner = Runner(testDir, "TestBitsCharsetDefinition.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}

class TestCharsets{
    import TestCharsets._

  @Test def parse_loaded_charsets(): Unit = { runner.runOneTest("parse_charsets") }
  @Test def unparse_loaded_charsets(): Unit = { runner.runOneTest("unparse_charsets") }
  @Test def parse_loaded_charsets2(): Unit = { runner.runOneTest("parse_charsets2") }
  @Test def unparse_loaded_charsets2(): Unit = { runner.runOneTest("unparse_charsets2") }
  @Test def parse_loaded_charsets3(): Unit = { runner.runOneTest("parse_charsets3") }
  @Test def unparse_loaded_charsets3(): Unit = { runner.runOneTest("unparse_charsets3") }
  @Test def unparse_loaded_charsets_DNE(): Unit = { runner.runOneTest("verify_error_message") }

}
