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

package org.apache.daffodil.layers.runtime1

import java.io.InputStream
import java.io.OutputStream
import java.util.ArrayDeque
import java.util.Deque

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.layers.api.Layer

final class TwoByteSwapLayer extends ByteSwap("twobyteswap", 2)

final class FourByteSwapLayer extends ByteSwap("fourbyteswap", 4)

/**
 * This class represents a byte swap layer that can be used to swap the byte order of data.
 *
 * DFDL Variable `requireLengthInWholeWords` can be used to request that the layer enforce
 * the length being a multiple of the word size.
 *
 * @constructor Creates a new ByteSwap instance with the specified name and word size.
 * @param name     The name of the byte swap layer.
 * @param wordsize The word size in bytes.
 */
abstract class ByteSwap(name: String, wordsize: Int)
  extends Layer(name, "urn:org.apache.daffodil.layers.byteSwap") {

  private var wholeWords: Boolean = false

  lazy val notWordSize = new IllegalStateException(
    "Data length is not a multiple of " + wordsize
  )

  /**
   * Initialize from DFDL variables that are parameters.
   * @param requireLengthInWholeWords a string that is the value of the DFDL variable of the same name in this layer's
   *                                  namespace. Must be "yes" or "no" or it is a SDE.
   */
  def setLayerVariableParameters(requireLengthInWholeWords: String): Unit = {
    requireLengthInWholeWords match {
      case "yes" => this.wholeWords = true
      case "no" => this.wholeWords = false // this is the default
      case _ =>
        runtimeSchemaDefinitionError(
          "requireLengthInWholeWords variable must be either 'yes' or 'no', but was: " + requireLengthInWholeWords
        )
    }
  }

  override def wrapLayerOutput(jos: OutputStream): OutputStream =
    new ByteSwapOutputStream(this, wordsize, jos, wholeWords)

  override def wrapLayerInput(jis: InputStream): InputStream =
    new ByteSwapInputStream(this, wordsize, jis, wholeWords)
}

/**
 * This class represents an input stream that performs byte swapping on
 * the data read from another input stream.
 *
 * This is streaming - does not require buffering up the data. So can be used on
 * very large data objects.
 *
 * Bytes within the wrapped input stream are re-ordered wordsize bytes at a time.
 * For example, if the wrapped input stream contains 10 bytes and wordsize is
 * 4, then the bytes from the wrapped input stream are returned in the
 * order 4 3 2 1 8 7 6 5 10 9.  If wordsize were 2 then the bytes from the
 * wrapped input stream are returned in the order 2 1 4 3 6 5 8 7 10 9.
 *
 * @param layer      The layer object used for error reporting.
 * @param wordsize   The number of bytes to swap at a time.
 * @param jis        The underlying input stream.
 * @param wholeWords It is a parse error if this is true and the length is not a multiple of the wordsize.
 */
class ByteSwapInputStream(layer: ByteSwap, wordsize: Int, jis: InputStream, wholeWords: Boolean)
  extends InputStream {

  object State extends org.apache.daffodil.lib.util.Enum {
    sealed trait Type extends EnumValueType

    /**
     * Buffering bytes in a word.
     */
    case object Filling extends Type

    /**
     * Return buffered bytes in a word in LIFO order.
     */
    case object Emptying extends Type

    /**
     * No more data.  Drain buffered bytes.
     */
    case object Draining extends Type

    /**
     *  No more data and no buffered bytes.
     */
    case object Done extends Type
  }

  private var c: Int = -2
  private val stack: Deque[Int] = new ArrayDeque[Int](wordsize)
  private var state: State.Type = State.Filling

  /**
   * Swap wordsize bytes at a time
   *
   */
  override def read(): Int = {
    import State._
    if (state eq Done) return -1
    while (state != Done) {
      state match {
        case Filling => {
          c = jis.read()
          if (c == -1) {
            state = Draining
            if (wholeWords && (stack.size() % wordsize != 0)) {
              // end of data but we have only a partial word on stack.
              layer.processingError(layer.notWordSize)
            }
          } else {
            stack.push(c)
            if (stack.size() == wordsize) {
              state = Emptying
            }
          }
        }
        case Emptying => {
          if (stack.isEmpty) {
            state = Filling
          } else {
            c = stack.pop()
            return c
          }
        }
        case Draining => {
          if (stack.isEmpty) {
            state = Done
            return -1
          } else {
            c = stack.pop()
            return c
          }
        }
        case Done =>
          Assert.invariantFailed("Done state not allowed.")
      }
    }
    Assert.invariantFailed("No fall through to here.")
  }
}

/**
 * An output stream wrapper that re-orders bytes according to wordsize.
 *
 * This is streaming, so is suitable for use on very large data.
 *
 * Bytes sent to the wrapped output stream are re-ordered wordsize bytes at a
 * time.  For example, if 10 bytes are written and wordsize is 4, then the
 * bytes are written to the wrapped output stream in the
 * order 4 3 2 1 8 7 6 5 10 9.  If wordsize were 2 then the bytes are written
 * to the wrapped output stream in the order 2 1 4 3 6 5 8 7 10 9.
 *
 * @param layer      The layer object used for error reporting.
 * @param wordsize   The number of bytes to swap at a time.
 * @param jos        OutputStream where the data is written after byte swapping
 * @param wholeWords It is a unparse error if this is true and the length at close time is not a multiple of the wordsize.
 */
class ByteSwapOutputStream(
  layer: ByteSwap,
  wordsize: Int,
  jos: OutputStream,
  wholeWords: Boolean
) extends OutputStream {

  private val stack: Deque[Byte] = new ArrayDeque[Byte](wordsize)
  private var closed = false

  override def close(): Unit = {
    if (!closed) {
      closed = true
      if (wholeWords && (stack.size() % wordsize != 0)) {
        // end of data but we have only a partial word on stack.
        layer.processingError(layer.notWordSize)
      }
      while (!stack.isEmpty) {
        jos.write(stack.pop())
      }
      jos.close()
    }
  }

  override def write(bInt: Int): Unit = {
    Assert.usage(!closed)
    stack.push(bInt.toByte)
    if (stack.size() == wordsize) {
      while (!stack.isEmpty) {
        jos.write(stack.pop())
      }
    }
  }
}
