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

package org.apache.daffodil.unparsers.runtime1

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.runtime1.infoset.RetryableException
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.UnparseTargetLengthInBitsEv
import org.apache.daffodil.runtime1.processors.unparsers._

abstract class HexBinaryUnparserBase(override val context: ElementRuntimeData)
  extends PrimUnparser {

  override def runtimeDependencies = Vector()

  protected def getLengthInBits(state: UState): Long

  override def unparse(state: UState): Unit = {

    val node = state.currentInfosetNode.asSimple
    val value = node.dataValue.getByteArray

    val valueLengthInBytes = value.length.toLong
    if (valueLengthInBytes > state.tunable.maxHexBinaryLengthInBytes) {
      UnparseError(
        One(context.schemaFileLocation),
        One(state.currentLocation),
        "Length of xs:hexBinary exceeds maximum of %s bytes: %s",
        state.tunable.maxHexBinaryLengthInBytes,
        valueLengthInBytes
      )
    }

    val lengthInBits = getLengthInBits(state)
    val lengthInBytes = (lengthInBits + 7) / 8

    if (valueLengthInBytes > lengthInBytes) {
      UnparseError(
        One(context.schemaFileLocation),
        One(state.currentLocation),
        "Length of xs:hexBinary exceeds calculated length of %s bits: %s",
        valueLengthInBytes * 8,
        lengthInBits
      )
    }

    val bitsFromValueToPut =
      if (lengthInBytes > valueLengthInBytes) {
        // the length to put is larger than the number of available bytes in the
        // array. So put the whole array and we'll add fill bytes later
        valueLengthInBytes * 8
      } else {
        // the length to put is either everything or some fragment of the last
        // byte, so put the length in bits. putByte will deal with the fragment byte
        Assert.invariant(lengthInBytes == valueLengthInBytes)
        lengthInBits
      }

    val dos = state.getDataOutputStream

    if (bitsFromValueToPut > 0) {
      val ret = dos.putByteArray(value, bitsFromValueToPut, state)
      if (!ret) {
        UnparseError(
          One(context.schemaFileLocation),
          One(state.currentLocation),
          "Failed to write %d hexBinary bits",
          bitsFromValueToPut
        )
      }
    }

    // calculate the skip bits
    val nFillBits = lengthInBits - bitsFromValueToPut
    if (nFillBits > 0) {
      val ret = dos.skip(nFillBits, state)
      if (!ret) {
        UnparseError(Nope, One(state.currentLocation), "Failed to skip %d bits.", nFillBits)
      }
    }
  }
}

class HexBinaryMinLengthInBytesUnparser(minLengthInBytes: Long, erd: ElementRuntimeData)
  extends HexBinaryUnparserBase(erd) {

  override def getLengthInBits(state: UState): Long = {
    val len = state.currentNode.get.asSimple.dataValue.getByteArray.length.toLong * 8
    val min = minLengthInBytes * 8
    scala.math.max(len, min)
  }
}

final class HexBinarySpecifiedLengthUnparser(
  erd: ElementRuntimeData,
  val lengthEv: UnparseTargetLengthInBitsEv
) extends HexBinaryUnparserBase(erd) {

  override def getLengthInBits(state: UState): Long = {
    val l: Long =
      try {
        lengthEv.evaluate(state).getULong.toLong
      } catch {
        case e: RetryableException => {
          val bytes = state.currentInfosetNode.asSimple.dataValue.getByteArray
          val len = bytes.length.toLong * 8
          len
        }
      }
    l
  }
}
