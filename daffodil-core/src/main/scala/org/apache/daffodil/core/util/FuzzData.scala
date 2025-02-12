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
package org.apache.daffodil.core.util

import java.io.ByteArrayInputStream
import scala.collection.mutable
import scala.util.Random
import scala.xml.Elem
import scala.xml.Node

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Misc.getAMessage
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.lib.xml.XMLUtils.XMLDifferenceException
import org.apache.daffodil.runtime1.processors.parsers.ParseError

/**
 * Utility base class to support fuzz testing. Given a piece of data,
 * it fuzzes it (without modifying the original data) each time next() is
 * called.
 *
 * The kind of fuzzing, what region within the data, how much data (run length),
 * and what it is overwritten with, is determined by the parameters to
 * the constructor, and the implementations of the runLength and fuzzOneByte
 * methods.
 * @param data a byte array of data to created fuzzed instances of.
 * @param offset where in the data to start the fuzzed region
 * @param endOffset where, measured back from the end, we want to protect from fuzzing
 * @param seed random number generator seed. Choosing different values for this
 *             will produce different fuzzed data, but it should be perfectly
 *             repeatable. Defaults to a specific value so that tests will be
 *             repeatable.
 */
abstract class FuzzData(
  val data: Array[Byte],
  offset: Int, // allows you not to clobber a prefix of the data
  endOffset: Int, // number of bytes at the end to not clobber
  seed: Int = 1 // specify to get varied, but repeatable results
) extends Iterator[Array[Byte]] {

  protected val r = new Random(seed)
  protected val dataLen = data.length
  private val originalData = data.clone()

  /**
   * Override this to generate a random run length, or to specify
   * a specific run length. Must be > 0.
   * @return an integer greater than 0, and less than than
   *         the size of the region to fuzz.
   */
  def runLength: Int

  override def hasNext = true // we can keep fuzzing forever

  override def next(): Array[Byte] = {
    val data = originalData.clone()
    val limit = dataLen - endOffset
    val regionLen = limit - offset
    val runLen = runLength
    val regionRunLocations = regionLen - runLen
    val startOfRun = r.nextInt(regionRunLocations)
    (startOfRun to (startOfRun + runLen)).foreach { i =>
      fuzzOneByte(data, i)
    }
    data
  }

  /**
   * Modifies one byte of the data array at the given position
   * @param data the byte array to modify
   * @param i zero-based position of the byte to modify
   */
  def fuzzOneByte(data: Array[Byte], i: Int): Unit
}

class FuzzZeroBits(data: Array[Byte], offset: Int, endOffset: Int)
  extends FuzzData(data, offset, endOffset) {

  override def runLength: Int = r.nextInt(dataLen - (offset + endOffset) - 1)

  override def fuzzOneByte(data: Array[Byte], i: Int): Unit = data(i) = 0
}

class FuzzOneBits(data: Array[Byte], offset: Int, endOffset: Int)
  extends FuzzData(data, offset, endOffset) {

  override def runLength: Int = r.nextInt(dataLen - (offset + endOffset) - 1)

  override def fuzzOneByte(data: Array[Byte], i: Int): Unit = data(i) = 0xff.toByte
}

class FuzzRandomSingleByte(data: Array[Byte], offset: Int, endOffset: Int)
  extends FuzzData(data, offset, endOffset) {

  override def runLength: Int = 1

  override def fuzzOneByte(data: Array[Byte], i: Int): Unit = {
    var current: Byte = data(i)
    var newValue: Byte = current
    // make sure they are different so we're definitely changing a byte value
    while (current == newValue) {
      newValue = ((current ^ r.nextInt(256)) & 0xff).toByte
    }
    data(i) = newValue
  }
}

class FuzzRandomByteRuns(data: Array[Byte], offset: Int, endOffset: Int)
  extends FuzzRandomSingleByte(data, offset, endOffset) {
  override def runLength = r.nextInt(dataLen - (offset + endOffset) - 1)
}

/**
 * Parses fuzzed data with a DFDL schema. Gathers unexpected throw classes
 * (things thrown other than parse errors), and also gathers unexpected
 * successes where the fuzzed data did not cause the parse to fail.
 * @param sch - scala.xml.Node which is the schema to test
 * @param data - data which will be fuzzed to create many variations
 * @param expected - expected XML if the data parses successfully
 * @param fuzzer - a FuzzData object
 */
class FuzzParseTester(
  sch: Elem,
  data: Array[Byte],
  expected: Elem,
  fuzzer: FuzzData
) {

  private val p = TestUtils.compileSchema(sch)

  val unexpectedThrowClasses = new mutable.HashSet[Class[_ <: Throwable]]()

  val okParses = new mutable.HashSet[(Node, Array[Byte], XMLDifferenceException)]

  protected def handleParseError(p: ParseError, testData: Array[Byte], i: Int) = {
    // do nothing by default
  }

  protected def handleThrow(t: Throwable): Unit = {
    unexpectedThrowClasses += t.getClass
  }

  protected def handleUnexpectedSuccess(
    actual: Node,
    testData: Array[Byte],
    diff: XMLDifferenceException
  ): Unit = {
    // we didn't get a throw from this fuzzed data
    // but the XML output was not what was expected either
    // save up the first N such.
    if (okParses.size <= 10)
      okParses += ((actual, testData, diff)) // extra parens because of varargs stuff ??
  }

  def run(nTrials: Int): Unit = {
    Assert.usage(nTrials >= 1)
    (1 to nTrials).foreach { i =>
      val testData = fuzzer.next()
      val bais = new ByteArrayInputStream(testData)
      val (pr, node) =
        try {
          TestUtils.runDataProcessorOnInputStream(p, bais, areTracing = false)
        } catch {
          case p: ParseError => {
            handleParseError(p, testData, i)
            (null, null)
          }
          case t: Throwable => {
            handleThrow(t)
            (null, null)
          }
        }
      if (pr ne null) {
        try {
          XMLUtils.compareAndReport(expected, node)
        } catch {
          case e: XMLDifferenceException =>
            handleUnexpectedSuccess(node, testData, e)
        }
      }
    }
  }

}

class LayerParseTester(
  sch: Elem,
  data: Array[Byte],
  expected: Elem,
  fuzzer: FuzzData,
  excludes: Seq[String]
) extends FuzzParseTester(sch, data, expected, fuzzer) {

  var shouldFail = false

  override def handleParseError(p: ParseError, data: Array[Byte], nth: Int): Unit = {
    val msg = getAMessage(p)
    if (excludes.exists { msg.contains(_) }) {
      // we know about this one
    } else {
      // new parse error type
      println(s"trial $nth produced $p")
      shouldFail = true
    }
  }

  override def run(nTrials: Int) = {
    super.run(nTrials)
    if (unexpectedThrowClasses.size > 0) {
      unexpectedThrowClasses.foreach { c =>
        println(s"Class ${c.getSimpleName} was thrown")
        shouldFail = true
      }
    }
    if (okParses.size > 0)
      println(s"Parse succeeded on fuzzed data ${okParses.size} times.")
    if (shouldFail)
      throw new AssertionError("errors were found")
  }
}
