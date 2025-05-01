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

package org.apache.daffodil.sexample

import scala.util.Using

import org.apache.daffodil.api.Daffodil
import org.apache.daffodil.api.debugger.Debugger
import org.apache.daffodil.api.infoset.Infoset
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.runtime1.processors.parsers.PState
import org.apache.daffodil.runtime1.processors.parsers.Parser

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class TestCustomDebuggerAPI {
  class CustomDebugger extends Debugger {
    var nodes = 0
    var inited = false
    var finished = false

    override def init(state: PState, processor: Parser): Unit = inited = true
    override def before(state: PState, processor: Parser): Unit = nodes += 1
    override def fini(processor: Parser): Unit = finished = true
  }

  @Test
  def testCustomDebugger(): Unit = {
    val c = Daffodil.compiler()
    val dbg = new CustomDebugger()

    val schemaFile = Misc.getRequiredResource("/test/api/mySchema1.dfdl.xsd")
    val pf = c.compileSource(schemaFile)
    val dp = pf
      .onPath("/")
      .withDebugger(dbg)
      .withDebugging(true)

    val file = Misc.getRequiredResource("/test/api/myData.dat")
    val fis = new java.io.FileInputStream(file.toURL.getFile)
    Using.resource(Infoset.getInputSourceDataInputStream(fis)) { input =>
      dp.parse(input, Infoset.getNullInfosetOutputter)

      assertEquals(6, dbg.nodes)
      assertTrue(dbg.inited)
      assertTrue(dbg.finished)
    }
  }
}
