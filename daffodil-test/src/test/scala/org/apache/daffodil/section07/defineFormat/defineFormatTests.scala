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

package org.apache.daffodil.section07.defineFormat

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object defineFormatTests {
  val testDir = "/org/apache/daffodil/section07/defineFormat/"
  val runner = Runner(testDir, "defineFormat.tdml")

  @AfterClass def tearDown(): Unit = {
    runner.reset
  }

}

class defineFormatTests {

  import defineFormatTests._

  @Test def test_format_with_comment(): Unit = { runner.runOneTest("format_with_comment") }

  //DFDL-478
  //@Test def test_nameCollision() { runner.runOneTest("nameCollision") }

  @Test def test_defineFormat_01(): Unit = { runner.runOneTest("defineFormat_01") }
  @Test def test_Lesson3_defineFormat(): Unit = { runner.runOneTest("Lesson3_defineFormat") }
  @Test def test_Lesson3_inherit_defineFormat(): Unit = { runner.runOneTest("Lesson3_inherit_defineFormat") }
  @Test def test_formatOnlyDefine(): Unit = { runner.runOneTest("formatOnlyDefine") }
  @Test def test_circularRef(): Unit = { runner.runOneTest("circularRef") }
  @Test def test_noNameFormat(): Unit = { runner.runOneTest("noNameFormat") }

}
