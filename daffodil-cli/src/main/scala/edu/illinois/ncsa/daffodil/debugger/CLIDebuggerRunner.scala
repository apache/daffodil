package edu.illinois.ncsa.daffodil.debugger

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

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
    reader.get.shutdown
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
