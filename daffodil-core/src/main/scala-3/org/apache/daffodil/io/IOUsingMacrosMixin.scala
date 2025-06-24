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

trait IOUsingMacrosMixin {
  this: org.apache.daffodil.io.DataInputStream =>

  /**
   * Convenience methods that temporarily set and (reliably) restore the bitLimit.
   * The argument gives the limit length. Note this is a length, not a bit position.
   *
   * This is added to the current bit position to get the limiting bit position
   * which is then set as the bitLimit when
   * the body is evaluated. On return the bit limit is restored to its
   * prior value.
   * <p>
   * The return value is false if the new bit limit is beyond the existing bit limit range.
   * Otherwise the return value is true.
   * <p>
   * The prior value is restored even if an Error/Exception is thrown. (ie., via a try-finally)
   * <p>
   * These are intended for use implementing specified-length types (simple or complex).
   * <p>
   * Note that length limits in lengthUnits Characters are not implemented
   * this way. See fillCharBuffer(cb) method.
   */
  final inline def withBitLengthLimit(
    lengthLimitInBits: Long
  )(inline body: => Unit): Boolean = {
    import org.apache.daffodil.lib.util.MaybeULong

    val dStream = this
    val newLengthLimit = lengthLimitInBits
    val savedLengthLimit = dStream.bitLimit0b

    if (!dStream.setBitLimit0b(MaybeULong(dStream.bitPos0b + newLengthLimit))) false
    else {
      try {
        body
      } finally {
        dStream.resetBitLimit0b(savedLengthLimit)
      }
      true
    }
  }
}
