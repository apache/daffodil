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

abstract class ByteSwap(name: String, count: Int)
  extends Layer(name, "urn:org.apache.daffodil.layers.byteSwap") {

  override def wrapLayerOutput(jos: OutputStream): OutputStream =
    new ByteSwapOutputStream(count, jos)

  override def wrapLayerInput(jis: InputStream): InputStream =
    new ByteSwapInputStream(count, jis)
}

/**
 * An input stream wrapper that re-orders bytes according to wordsize.
 *
 * This is streaming - does not require buffering up the data. So can be used on
 * very large data objects.
 *
 * Bytes within the wrapped input stream are re-ordered wordsize bytes at a time.
 * For example, if the wrapped input stream contains 10 bytes and wordsize is
 * 4, then the bytes from the wrapped input stream are returned in the
 * order 4 3 2 1 8 7 6 5 10 9.  If wordsize were 2 then the bytes from the
 * wrapped input stream are returned in the order 2 1 4 3 6 5 8 7 10 9.
 */
class ByteSwapInputStream(wordsize: Int, jis: InputStream) extends InputStream {

  object State extends org.apache.daffodil.lib.util.Enum {
    abstract sealed trait Type extends EnumValueType

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
          } else {
            stack.push(c)
            if (stack.size() == wordsize) {
              state = Emptying
            }
          }
        }
        case Emptying => {
          if (stack.isEmpty()) {
            state = Filling
          } else {
            c = stack.pop()
            return c
          }
        }
        case Draining => {
          if (stack.isEmpty()) {
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
 */
class ByteSwapOutputStream(wordsize: Int, jos: OutputStream) extends OutputStream {

  private val stack: Deque[Byte] = new ArrayDeque[Byte](wordsize)
  private var closed = false

  override def close(): Unit = {
    if (!closed) {
      while (!stack.isEmpty()) {
        jos.write(stack.pop())
      }
      jos.close()
      closed = true
    }
  }

  override def write(bInt: Int): Unit = {
    Assert.usage(!closed)
    stack.push(bInt.toByte)
    if (stack.size() == wordsize) {
      while (!stack.isEmpty()) {
        jos.write(stack.pop())
      }
    }
  }
}
