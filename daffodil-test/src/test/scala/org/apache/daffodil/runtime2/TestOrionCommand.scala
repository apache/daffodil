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

package org.apache.daffodil.runtime2

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestOrionCommand {
  val testDir = "/org/apache/daffodil/runtime2/"
  val runner = Runner(testDir, "orion-command.tdml")

  @AfterClass def shutDown(): Unit = { runner.reset }
}

class TestOrionCommand {
  import TestOrionCommand._

  @Test def test_command_parse(): Unit = { runner.runOneTest("command_parse") }
  @Test def test_command_unparse(): Unit = { runner.runOneTest("command_unparse") }
  @Test def test_camera_state_parse(): Unit = { runner.runOneTest("camera_state_parse") }
  @Test def test_camera_state_unparse(): Unit = { runner.runOneTest("camera_state_unparse") }
  @Test def test_video_settings_parse(): Unit = { runner.runOneTest("video_settings_parse") }
  @Test def test_video_settings_unparse(): Unit = { runner.runOneTest("video_settings_unparse") }
  @Test def test_aptina_settings_parse(): Unit = { runner.runOneTest("aptina_settings_parse") }
  @Test def test_aptina_settings_unparse(): Unit = { runner.runOneTest("aptina_settings_unparse") }
  @Test def test_limits_parse(): Unit = { runner.runOneTest("limits_parse") }
  @Test def test_limits_unparse(): Unit = { runner.runOneTest("limits_unparse") }
}
