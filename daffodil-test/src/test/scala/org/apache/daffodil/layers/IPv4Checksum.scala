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

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.processors.VariableRuntimeData
import passera.unsigned.UShort

import java.nio.ByteBuffer

/**
 *  The layer transform computes the checksum of the header data.
 *  per IETF RFC 791.
 *
 *  The data has a well-known fixed length, so layerLengthKind is always 'implicit'.
 */
final class IPv4Checksum(
  name: String,
  layerRuntimeInfo: LayerRuntimeInfo,
  outputVar: VariableRuntimeData)
extends ByteBufferExplicitLengthLayerTransform[Int](
  layerRuntimeInfo,
  name,
  inputVars = Seq(),
  outputVar: VariableRuntimeData) {

  /**
   * This layer is always exactly 20 bytes long.
   */
  override protected def layerBuiltInConstantLength = Some(20L)

  private def chksumShortIndex = 5

  override protected def compute(state: ParseOrUnparseState, isUnparse: Boolean, inputs: Seq[Any], byteBuffer: ByteBuffer) = {
    val shortBuf = byteBuffer.asShortBuffer()

    var i = 0
    var chksum: Int = 0
    val nShorts = layerBuiltInConstantLength.get / 2
    while (i < nShorts) {
      if (i == chksumShortIndex) {
        // for the checksum calculation treat the incoming checksum field of the data as 0
        // so we just don't do an addition here.
      } else {
        chksum += UShort(shortBuf.get(i)).toInt
      }
      Assert.invariant(chksum >= 0)
      i += 1
    }
    //
    // now combine the carry bits in the most significant 16 bits into the lower 16 bits.
    //
    val checksumLow = chksum & 0xFFFF
    val checksumHigh = chksum >>> 16
    val checksumTotal: Int = checksumLow + checksumHigh
    Assert.invariant(checksumTotal <= 0xFFFF && checksumTotal >= 0)
    val checksumTotalShort = UShort(checksumTotal.toShort)
    val checksum = checksumTotalShort.toInt
    //
    // take ones complement to get the final checksum
    //
    val finalChecksum: Int = (~checksum) & 0xFFFF

    if (isUnparse) {

      //
      // clobber the byte buffer bytes corresponding to the checksum with
      // the recomputed value
      //
      shortBuf.put(chksumShortIndex, finalChecksum.toShort)
    }

    finalChecksum
  }
}
