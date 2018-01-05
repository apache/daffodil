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
