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

package org.apache.daffodil.runtime1.debugger

import java.io.PrintStream

import org.apache.daffodil.api

class TraceDebuggerRunner(out: PrintStream = System.out)
  extends api.debugger.TraceDebuggerRunner {
  val traceIter = Seq(
    "set infosetParents 1",
    "display info parser",
    "display info bitPosition",
    "display info data",
    "display info infoset",
    "display info diff",
    "trace"
  ).iterator

  override def init(id: InteractiveDebugger): Unit = {
    // do nothing
  }

  override def getCommand: String = {
    if (traceIter.hasNext) {
      traceIter.next()
    } else {
      // If the traceItr commands are good this should never happen. The only
      // time this would ever get hit is if something caused the debugger to
      // break. So if this does happen, just keep running trace. We should
      // eventually finish parsing.
      "trace"
    }
  }

  override def lineOutput(line: String): Unit = {
    out.println(line)
  }

  override def fini(): Unit = {
    // do nothing
  }

}
