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

package org.apache.daffodil

import org.apache.daffodil.util._

/**
 * Tak is about establishing a sort-of platform independent self-calibrating
 * unit of comparison, which is how fast the JVM implements a 3 argument
 * function call.
 *
 * A unit, called a takeon, or scala takeon because these are language
 * specific, is the number of nanoseconds per 3-argument java function call.
 *
 * The function tak(a, b, c), does not build a deep stack,
 * uses no heap allocation, and never creates a large number.
 * It can be setup to run for billions of calls using only 3 small integer values.
 * All the calls are 3-arguments, passing Scala Int type.
 */

object Tak {

  def calibrate() = {
    if (takeons == 0.0) {
      testTak
    }
  }

  var takeons = 0.0 // value on Mike Beckerle's laptop

  var callCount: Long = 0

  // Original Tak function
  def tak(x: Int, y: Int, z: Int): Int = {
    callCount += 1
    if (y < x)
      tak(
        tak(x - 1, y, z),
        tak(y - 1, z, x),
        tak(z - 1, x, y))
    else
      z
  }

  def testTak(): Unit = {
    println("Calibrating takeon units")
    callCount = 0
    val nanos = Timer.getTimeNS { tak(21, 3, 21) }
    println("tak call count = " + callCount + " in " + nanos + "ns")
    takeons = (1.0 * nanos) / callCount
    println("Under current load, 1 CPU of this system executes " + takeons + " nanoseconds per tak call.")
    println("So on this system, currently, 1 takeon = " + takeons + "ns")
    println("Done calibrating")
  }

  // $COVERAGE-OFF$ // not part of any unit testing
  def main(args: Array[String]): Unit = {
    testTak()
  }
  // $COVERAGE-ON$
}
