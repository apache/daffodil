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

package edu.illinois.ncsa.daffodil.util

import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.exceptions._
import org.junit.Test

class MyClass extends Logging {

  lazy val msg = {
    // System.err.println("computing the message string.")
    "Message %s"
  }

  lazy val argString = {
    // System.err.println("computing the argument string.")
    "about nothing at all."
  }

  lazy val bombArg: String = Assert.abort("bombArg should not be evaluated")
  lazy val bombMsg: String = Assert.abort("bombMsg should not be evaluated")

  def logSomething() {

    ForUnitTestLogWriter.loggedMsg = null

    setLogWriter(ForUnitTestLogWriter)

    // won't log because below threshhold. Won't even evaluate args.
    log(LogLevel.Debug, msg, bombArg) // Won't show up in log. Won't bomb.

    // alas, no by-name passing of var-args.
    // so instead, we pass by name, a by-name/lazy constructed tuple
    // instead.

    // Illustrates that our Glob object, because its parts are all passed by name,
    // does NOT force evaluation of the pieces that go into it.
    // So it really makes the whole system behave like it was entirely lazy.

    // If we're logging below the threshhold of Debug, then this log line
    // doesn't evaluate bombMsg or bombArg. So it is ok if those are expensive
    // to compute.

    log(LogLevel.Debug, bombMsg, bombArg) // bomb is not evaluated at all.
    log(LogLevel.Error, msg, argString) // Will show up in log.

    setLoggingLevel(LogLevel.Info)
    setLogWriter(ConsoleWriter)
  }
}

class TestLogger {

  @Test def test1() {
    val c = new MyClass
    c.setLoggingLevel(LogLevel.Error)
    c.logSomething()
    Console.out.flush()
    val fromLog = ForUnitTestLogWriter.loggedMsg
    // println(fromLog)
    val hasExpected = fromLog.contains("Message about nothing at all.")
    val doesntHaveUnexpected = !fromLog.contains("number 1")
    assertTrue(hasExpected)
    assertTrue(doesntHaveUnexpected)
  }

}
