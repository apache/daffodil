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

package org.apache.daffodil.unparser

import org.junit.Test
import org.junit.AfterClass
import org.apache.daffodil.tdml.Runner

object TestEnvelopePayload {
  val runner = Runner("/org/apache/daffodil/unparser/", "envelopePayload.tdml")

  @AfterClass def tearDown(): Unit = {
    runner.reset
  }
}

class TestEnvelopePayload {
  import TestEnvelopePayload._

  @Test def test_ep1(): Unit = { runner.runOneTest("ep1") }
  @Test def test_ep2(): Unit = { runner.runOneTest("ep2") }
  @Test def test_ep3(): Unit = { runner.runOneTest("ep3") }

}
