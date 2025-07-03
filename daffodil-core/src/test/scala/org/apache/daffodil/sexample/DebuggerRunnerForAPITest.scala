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

import scala.collection.mutable.ListBuffer

import org.apache.daffodil.api.debugger.DebuggerRunner

class DebuggerRunnerForAPITest extends DebuggerRunner {
  val lines = ListBuffer[String]()

  val commandsIter = Seq(
    "display info parser",
    "display info bitPosition",
    "display info data",
    "display eval ..",
    "display info diff",
    "trace"
  ).iterator

  def init(): Unit = {}

  def fini(): Unit = {}

  def getCommand(): String = {
    val cmd = if (commandsIter.hasNext) {
      commandsIter.next()
    } else {
      // If the commandsIter commands are good this should never happen. The
      // only time this would ever get hit is if something caused the
      // debugger to break. But if this does happen, just keep running trace.
      // We should eventually finish parsing.
      "trace"
    }
    cmd
  }

  def lineOutput(line: String): Unit = {
    lines.append(line + "\n")
  }
}
