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

import org.apache.daffodil.api.debugger.InteractiveDebuggerRunner

/*
 * The below class is empty and is not ever actually used. It is just a place
 * holder. Whenever the API uses it, it is translated to the appropriate
 * Scala debugger runner. It is marked final so that it cannot be extended,
 * since the Scala pattern matcher would still match and use the equivalent
 * Scala trace runner and lose any added functionality. One must extend the
 * DebuggerRunner to create their own debugger runner.
 */ /*
 * The below class is empty and is not ever actually used. It is just a place
 * holder. Whenever the API uses it, it is translated to the appropriate
 * Scala debugger runner. It is marked final so that it cannot be extended,
 * since the Scala pattern matcher would still match and use the equivalent
 * Scala trace runner and lose any added functionality. One must extend the
 * DebuggerRunner to create their own debugger runner.
 */

/**
 * `DebuggerRunner` that writes verbose information about parsing to stdout
 */
final class InteractiveTraceDebuggerRunner extends InteractiveDebuggerRunner {
  override def init(): Unit = {
    // do nothing
  }

  override def init(id: InteractiveDebugger): Unit = {
    // do nothing
  }

  override def getCommand = ""

  override def lineOutput(line: String): Unit = {
    // do nothing
  }

  override def fini(): Unit = {
    // do nothing
  }
}
