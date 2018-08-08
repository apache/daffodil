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

package org.apache.daffodil.debugger

import java.io.File
import scala.io.Source
import jline.console.ConsoleReader

class CLIDebuggerRunner(cmdsIter: Iterator[String]) extends InteractiveDebuggerRunner {
  def this() {
    this(Iterator.empty)
  }

  def this(file: File) {
    this(Source.fromFile(file).getLines)
  }

  def this(seq: Seq[String]) {
    this(seq.iterator)
  }

  var reader: Option[ConsoleReader] = None

  def init(id: InteractiveDebugger): Unit = {
    val r = new ConsoleReader()
    r.setPrompt("(debug) ")
    r.addCompleter(id.DebugCommandBase.completer)
    r.setExpandEvents(false)
    reader = Some(r)
  }

  def fini(): Unit = {
    reader.map { _.close }
    reader = None
  }

  def getCommand: String = {
    val cmd = if (cmdsIter.hasNext) {
      val line = cmdsIter.next
      if (line.length > 0) {
        reader.get.getHistory.add(line)
      }
      println("%s%s".format(reader.get.getPrompt, line))
      line
    } else {
      val line = reader.get.readLine
      if (line == null) "quit" else line
    }
    cmd.trim
  }

  def lineOutput(line: String): Unit = {
    println("  " + line)
  }
}
