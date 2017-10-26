/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.example

import edu.illinois.ncsa.daffodil.sapi.debugger._

import scala.collection.mutable.ListBuffer

class DebuggerRunnerForSAPITest extends DebuggerRunner {
  val lines = ListBuffer[String]()

  val commandsIter = Seq(
    "display info parser",
    "display info bitPosition",
    "display info data",
    "display eval ..",
    "display info diff",
    "trace"
  ).iterator

  def init() {
  }

  def fini() {
  }

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

  def lineOutput(line: String) {
    lines.append(line + "\n")
  }
}
