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

package org.apache.daffodil.util

import org.junit.Assert._
import org.apache.daffodil.exceptions._
import org.junit.Test

class ForUnitTestLogWriter extends LogWriter {
  var loggedMsg: String = null
  def write(msg: String): Unit = {
    loggedMsg = msg
  }
}

class TestLogger {
  @Test def test1(): Unit = {

    class A extends Logging {

      lazy val msg = "Message %s"
      lazy val argString = "about nothing at all."

      lazy val bombArg: String = Assert.abort("bombArg should not be evaluated")
      lazy val bombMsg: String = Assert.abort("bombMsg should not be evaluated")

      def logSomething(): Unit = {
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
      }
    }


    val lw = new ForUnitTestLogWriter
    val c = new A
    c.setLoggingLevel(LogLevel.Error)
    c.setLogWriter(lw)
    c.logSomething()
    val fromLog = lw.loggedMsg
    val hasExpected = fromLog.contains("Message about nothing at all.")
    val doesntHaveUnexpected = !fromLog.contains("number 1")
    assertTrue(hasExpected)
    assertTrue(doesntHaveUnexpected)
  }

  @Test def test_var_args_different_primitives(): Unit = {

    class A extends Logging {
      def logSomething(): Unit = {
        log(LogLevel.Error, "Message: int=%d float=%f", 1, 3.0)
      }
    }

    val lw = new ForUnitTestLogWriter
    val a = new A
    a.setLoggingLevel(LogLevel.Error)
    a.setLogWriter(lw)
    a.logSomething()
    assertTrue(lw.loggedMsg matches ".*Message: int=1 float=3[.,]0.*")
  }

}
