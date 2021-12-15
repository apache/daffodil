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

package org.apache.daffodil.layers

import java.io.OutputStream
import java.io.InputStream
import java.util.ArrayDeque
import java.util.Deque
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthKind
import org.apache.daffodil.io.ExplicitLengthLimitingStream
import org.apache.daffodil.processors.ParseOrUnparseState

final class FourByteSwapLayerCompiler
  extends LayerCompiler("fourbyteswap") {

  override def compileLayer(layerCompileInfo: LayerCompileInfo): LayerTransformerFactory = {

    layerCompileInfo.optLayerLengthKind match {
      case Some(LayerLengthKind.Explicit) => // ok
      case None => layerCompileInfo.SDE("The property dfdlx:layerLengthKind must be defined and have value 'explicit'.")
      case Some(other) => layerCompileInfo.SDE("The property dfdlx:layerLengthKind must be 'explicit' but was '$other'.")
    }

    layerCompileInfo.SDEUnless(
    layerCompileInfo.optLayerLengthOptConstantValue.isDefined,
      "The property dfdlx:layerLength must be defined.")

    val xformer = new ByteSwapTransformerFactory(4, name)
    xformer
  }
}


/**
 * LayerTransformerFactory for ByteSwapTransformer instances.
 *
 * Requires that layerLengthKind="explicit".
 */
final class ByteSwapTransformerFactory(wordsize: Int, name: String)
  extends LayerTransformerFactory(name) {

  override def newInstance(layerRuntimeInfo: LayerRuntimeInfo)= new ByteSwapTransformer(wordsize, name, layerRuntimeInfo)
}

/**
 * An input stream wrapper that re-orders bytes according to wordsize.
 *
 * Bytes within the wrapped input stream are re-ordered wordsize bytes at a time.
 * For example, if the wrapped input stream contains 10 bytes and wordsize is
 * 4, then the bytes from the wrapped input stream are returned in the
 * order 4 3 2 1 8 7 6 5 10 9.  If wordsize were 2 then the bytes from the
 * wrapped input stream are returned in the order 2 1 4 3 6 5 8 7 10 9.
 */
class ByteSwapInputStream(wordsize: Int, jis: InputStream)
  extends InputStream {

  object State extends org.apache.daffodil.util.Enum {
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
 * Bytes sent to the wrapped output stream are re-ordered wordsize bytes at a
 * time.  For example, if 10 bytes are written and wordsize is 4, then the
 * bytes are written to the wrapped output stream in the
 * order 4 3 2 1 8 7 6 5 10 9.  If wordsize were 2 then the bytes are written
 * to the wrapped output stream in the order 2 1 4 3 6 5 8 7 10 9.
 */
class ByteSwapOutputStream(wordsize: Int, jos: OutputStream)
  extends OutputStream {

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

/**
 * A LayerTransformer that re-orders bytes for the over/underlying layer.
 */
class ByteSwapTransformer(wordsize: Int, name: String, layerRuntimeInfo: LayerRuntimeInfo)
  extends LayerTransformer(name, layerRuntimeInfo) {

  override def wrapLayerDecoder(jis: java.io.InputStream) = {
    val s = new ByteSwapInputStream(wordsize, jis)
    s
  }

  override def wrapLimitingStream(state: ParseOrUnparseState, jis: java.io.InputStream) = {
    val layerLengthInBytes = layerRuntimeInfo.optLayerLength(state).get

    val s = new ExplicitLengthLimitingStream(jis, layerLengthInBytes)
    s
  }

  override protected def wrapLayerEncoder(jos: java.io.OutputStream): java.io.OutputStream = {
    val s = new ByteSwapOutputStream(wordsize, jos)
    s
  }

  override protected def wrapLimitingStream(state: ParseOrUnparseState, jos: java.io.OutputStream) = {
    jos // just return jos. The way the length will be used/stored is by way of
    // taking the content length of the enclosing element. That will measure the
    // length relative to the "ultimate" data output stream.
  }
}




