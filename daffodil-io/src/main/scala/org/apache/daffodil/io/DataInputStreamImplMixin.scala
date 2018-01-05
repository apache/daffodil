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

package org.apache.daffodil.io

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.util.Maybe.Nope
import java.nio.CharBuffer

trait DataInputStreamImplMixin extends DataInputStream
  with DataStreamCommonImplMixin
  with LocalBufferMixin {

  /**
   * Returns true if it fills all remaining space in the char buffer.
   *
   * Convenience method since this idiom is so common due to the
   * way fillCharBuffer works to return early when decode errors are
   * encountered.
   */
  protected final def fillCharBufferLoop(cb: CharBuffer, finfo: FormatInfo): Boolean = {
    var maybeN: MaybeULong = MaybeULong(0)
    var total: Long = 0
    val nChars = cb.remaining
    while (maybeN.isDefined && total < nChars) {
      maybeN = fillCharBuffer(cb, finfo)
      if (maybeN.isDefined) total += maybeN.get.toLong
    }
    total == nChars
  }

  final def getString(nChars: Long, finfo: FormatInfo): Maybe[String] = {
    withLocalCharBuffer { lcb =>
      val cb = lcb.getBuf(nChars)
      val gotAll = fillCharBufferLoop(cb, finfo)
      val res = if (!gotAll) Nope
      else Maybe(cb.flip.toString)
      res
    }
  }

  final def getSomeString(nChars: Long, finfo: FormatInfo): Maybe[String] = {
    withLocalCharBuffer { lcb =>
      val cb = lcb.getBuf(nChars)
      fillCharBufferLoop(cb, finfo)
      if (cb.position() == 0) Nope
      else Maybe(cb.flip.toString)
      // TODO: Performance - we need to copy here. Consider return type of Maybe[StringBuilder]
      // as that will allow for non-copying trim and other manipulations of the string
      // without further copyies.
    }
  }

  override def setDebugging(setting: Boolean) {
    if (bitPos0b > 0) throw new IllegalStateException("Must call before any access to data")
    cst.debugging = setting
  }

  final override def isAligned(bitAlignment1b: Int): Boolean = {
    Assert.usage(bitAlignment1b >= 1)
    val alignment = bitPos0b % bitAlignment1b
    val res = alignment == 0
    res
  }

  final override def align(bitAlignment1b: Int, finfo: FormatInfo): Boolean = {
    if (isAligned(bitAlignment1b)) return true
    val deltaBits = bitAlignment1b - (bitPos0b % bitAlignment1b)
    skip(deltaBits, finfo)
  }

  final override def remainingBits = {
    if (this.bitLimit0b.isEmpty) MaybeULong.Nope
    else {
      val lim = bitLimit0b.get
      Assert.invariant(lim >= 0)
      val nBits = lim - bitPos0b
      MaybeULong(nBits)
    }
  }

}
