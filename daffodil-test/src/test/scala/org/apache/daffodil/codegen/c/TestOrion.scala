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

package org.apache.daffodil.codegen.c

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.lib.iapi.TDMLImplementation
import org.apache.daffodil.tdml.Runner

import org.junit.Test

object TestOrion extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/codegen/c/orion.tdml"

  override def createRunner() = Runner(tdmlDir, tdmlFile, TDMLImplementation.DaffodilC)
}

class TestOrion extends TdmlTests {
  val tdmlSuite = TestOrion

  @Test def orion_aptina = test
  @Test def orion_camera = test
  @Test def orion_command = test
  @Test def orion_limits = test
  @Test def orion_video = test
}
