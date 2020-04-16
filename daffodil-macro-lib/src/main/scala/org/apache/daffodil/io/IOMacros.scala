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

import scala.reflect.macros.blackbox.Context

object IOMacros {

  /**
   * For Data Input Streams
   *
   * Used to temporarily vary the bit length limit.
   *
   * Implementing as a macro eliminates the creation of a downward function object every time this
   * is called.
   *
   */
  def withBitLengthLimitMacroForInput(c: Context)(lengthLimitInBits: c.Tree)(body: c.Tree) = {

    import c.universe._

    val dStream = TermName(c.freshName)
    val newLengthLimit = TermName(c.freshName)
    val savedLengthLimit = TermName(c.freshName)
    // c.prefix is the expression this macro was expanded on. Not quite same thing as 'this' because we have to be
    // careful not to use it more than once or it will evaluate more than once.
    val selfExp = c.prefix

    q"""{
    import org.apache.daffodil.util.MaybeULong

    val $dStream = $selfExp
    val $newLengthLimit = $lengthLimitInBits
    val $savedLengthLimit = $dStream.bitLimit0b

    if (!$dStream.setBitLimit0b(MaybeULong($dStream.bitPos0b + $newLengthLimit))) false
    else {
      try {
        $body
      } finally {
        $dStream.resetBitLimit0b($savedLengthLimit)
      }
      true
    }
  }"""
  }
}
