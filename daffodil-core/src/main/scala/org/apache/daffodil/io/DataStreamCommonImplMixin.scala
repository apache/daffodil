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

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.lib.util.MaybeChar

trait DataStreamCommonState {

  /**
   * Keeps track of the bitOrder of the last operation on the
   * data stream.
   */
  private var maybePriorBitOrder_ : Maybe[BitOrder] = Maybe.Nope

  def setPriorBitOrder(pbo: BitOrder): Unit = {
    maybePriorBitOrder_ = One(pbo)
  }

  def priorBitOrder: BitOrder = {
    Assert.usage(maybePriorBitOrder_.isDefined)
    maybePriorBitOrder_.value
  }

  var debugging: Boolean = false
  //
  // These are for dealing with 4-byte UTF-8 codepoints
  // that require 2 16-bit characters.
  //
  // This only comes up in an incredibly obscure case
  // when fillCharBuffer is called with a char buffer having
  // room for only a single 16-bit codepoint, and the
  // data's first byte is 0xF0, which indicates 4-bytes
  // need to be consumed, to create two 16 bit code units
  // aka a surrogate-pair.
  //
  var maybeTrailingSurrogateForUTF8: MaybeChar = MaybeChar.Nope
  var priorBitPos: Long = 0L

  def resetUTF8SurrogatePairCapture(): Unit = {
    this.priorBitPos = -1
  }

  def assignFrom(other: DataStreamCommonState): Unit = {
    this.debugging = other.debugging
    this.priorBitPos = other.priorBitPos
    this.maybePriorBitOrder_ = other.maybePriorBitOrder_
  }

}

/**
 * Shared by both DataInputStream and DataOutputStream implementations
 */
trait DataStreamCommonImplMixin extends DataStreamCommon {

  protected def cst: DataStreamCommonState

  /*
   * Debugger support
   */

  final override def areDebugging = cst.debugging

}
