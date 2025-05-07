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

package org.apache.daffodil.lib.util

import scala.collection.mutable
import scala.jdk.CollectionConverters._

object Timer {

  def printTime(message: String, nanos: Long, units: String): Unit = {
    val msg = message match {
      case null | "" => ""
      case s => " (%s)".format(s)
    }
    val time = units match {
      case "ms" => nanos / 1000000
      case "ns" => nanos
    }
    Logger.log.info(s"Time${msg}: ${time}${units}")
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

/**
 * Utility for computing timings in "Takeon" units.
 * A takeon is the time it takes to do a 3-argument Java/Scala
 * method/function call, passing 3 Long values, returning 1 Long value.
 *
 * Takeon comes from the Tak function or Takeuchi's function. This
 * calls itself recursively billions of times given only small integers
 * as arguments. It never computes a very large integer, never recurses very
 * deeply. It is a useful function for measuring raw
 * best-possible function-call speed.
 *
 * The point of this is to have way to measure performance that is
 * not in units of time, but units that are self-relative
 * to the speed of the machine.
 *
 * In theory, widely different CPUs at different speeds *could*
 * produce similar timings when timed in units of "takeons".
 */
object TakTimer {

  /**
   * Tak or Takeuchi's function
   */
  def tak(x: Long, y: Long, z: Long): Long = {
    callCount += 1
    if (y < x)
      tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y))
    else
      z
  }

  /**
   * Useful if you want to also compare variations on the Tak
   * function to this basic tak function used to compute the takeon unit.
   *
   * That's handy if you want to compare overheads of things that can
   * be expressed in terms of tak argument passing.
   *
   * Valid after TakTimer.takeon has been evaluated.
   */
  var callCount: Long = 0

  /**
   * These x,y,z values produce about 1.4 billion tak calls.
   * Taking about 2 seconds to calibrate. Increasing these
   * (or decreasing y), increases the call count, but doesn't
   * improve performance (i.e., make a smaller takeon unit,
   * perhaps due to further hotspot optimizing)
   * on openjdk 1.8.0_222 running on Ubuntu Linux 18 with lscpu
   * output of:
   * <pre>
   * Model name:          Intel(R) Core(TM) i7-6820HQ CPU @ 2.70GHz
   * CPU MHz:             2017.538
   * CPU max MHz:         3600.0000
   * CPU min MHz:         800.0000
   * BogoMIPS:            5424.00
   * </pre>
   */
  val x = 20L
  val y = 2L
  val z = 20L

  /**
   * The number of nanoseconds for 1 tak function call.
   *
   * Divide nanosecond timings by this to get timings in Takeon units.
   */
  lazy val takeon: Double = {
    callCount = 0
    val basenanos = Timer.getTimeNS("Calibrating Takeon Units", tak(x, y, z))
    val takUnit = (1.0e0 * basenanos) / callCount
    takUnit
  }

  def timeInTakeons(call: => Any, message: String = null) = {
    takeon // force initialization
    val nanos = Timer.getTimeNS(message, call)
    nanos / takeon
  }

}

object TimeTracker extends TimeTrackerUsingMacrosMixin {

  case class SectionTime(var time: Long, var count: Int)

  /**
   * A mapping of each section of code tracked
   */
  val sectionTimes = new java.util.HashMap[String, SectionTime]()

  /**
   * Used to track the time of child tracked sections so they can be excluded
   * from the parents tracked time. When a tracked section begins, we will push
   * a zero onto the top of the stack. As each child section finishes and
   * calculates their time, they will add their time to the value at the top of
   * the stack. This effectively keeps track of how long all child tracked
   * sections take. Then when the parent tracked section ends it can pop and
   * subtract the value on the top of the stack to determine how much time just
   * that section took, excluding the nested child track sections. This is
   * necessary since we often want to track the time each parser takes to
   * complete, but our parsers are nested making that difficult with standard
   * profilers. This makes that much easier.
   *
   * TODO: not covered by tests
   */
  val childrenTimeStack = new mutable.Stack[Long]()

  /**
   * Output the results of the tracked sections in sorted columnar format.
   */
  def logTimes(): Unit = {
    val stats = sectionTimes.asScala.toSeq
      .map {
        case (name, SectionTime(timeNS, count)) => {
          val average = timeNS / count
          (name, timeNS / 1000000000.0, average, count)
        }
      }
      .sortBy(_._2)
      .reverse

    val totalTime = stats.map { _._2 }.sum

    val stringStats = stats.map { case (name, time, average, count) =>
      (
        name,
        "%.3f".format(time),
        "%.2f%%".format(time * 100 / totalTime),
        average.toString,
        count.toString
      )
    }

    val nameLen = stringStats.map(_._1.length).max
    val timeLen = stringStats.map(_._2.length).max
    val percentLen = stringStats.map(_._3.length).max
    val averageLen = stringStats.map(_._4.length).max
    val countLen = stringStats.map(_._5.length).max

    val formatString =
      "%-" + nameLen + "s  " +
        "%" + timeLen + "s  " +
        "%" + percentLen + "s  " +
        "%" + averageLen + "s  " +
        "%" + countLen + "s"

    Logger.log.info(formatString.format("Name", "Time", "Pct", "Average", "Count"))
    stringStats.foreach { stats =>
      Logger.log.info(formatString.format(stats.productIterator.toList: _*))
    }
    Logger.log.info(f"Total Time: $totalTime%.3f")
  }

  def clear(): Unit = {
    sectionTimes.clear()
    childrenTimeStack.clear()
  }
}
