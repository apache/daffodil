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

package org.apache.daffodil.util

object Timer extends Logging {

  def printTime(message: String, nanos: Long, units: String) {
    val msg = message match {
      case null | "" => ""
      case s => " (%s)".format(s)
    }
    val time = units match {
      case "ms" => nanos / 1000000
      case "ns" => nanos
    }
    log(LogLevel.Info, "Time%s: %d%s", msg, time, units)
  }

  def getResult[A](message: String, f: => A): A = {
    val (nanos, result) = getTimeResult(f)
    printTime(message, nanos, "ms")
    result
  }

  def getResult[A](f: => A): A = {
    getResult(null, f)
  }

  def getResultNS[A](message: String, f: => A): A = {
    val (nanos, result) = getTimeResult(f)
    printTime(message, nanos, "ns")
    result
  }

  def getResultNS[A](f: => A): A = {
    getResultNS(null, f)
  }

  def getTime[A](message: String, f: => A): Long = {
    val (nanos, _) = getTimeResult(f)
    printTime(message, nanos, "ms")
    nanos
  }

  def getTime[A](f: => A): Long = {
    getTime(null, f)
  }

  def getTimeNS[A](message: String, f: => A): Long = {
    val (nanos, _) = getTimeResult(f)
    printTime(message, nanos, "ns")
    nanos
  }

  def getTimeNS[A](f: => A): Long = {
    getTimeNS(null, f)
  }

  def getTimeResult[A](f: => A): (Long, A) = {
    val t0 = System.nanoTime
    val result = f
    val t1 = System.nanoTime
    val nanos = t1 - t0
    (nanos, result)
  }
}
