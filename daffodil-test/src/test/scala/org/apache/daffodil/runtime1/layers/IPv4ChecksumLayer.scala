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

package org.apache.daffodil.runtime1.layers

import java.lang.{ Integer => JInt }
import java.nio.ByteBuffer

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.layers.api.ChecksumLayer
import org.apache.daffodil.runtime1.layers.api.LayerRuntime

import passera.unsigned.UShort

/**
 *  The layer transform computes the checksum of the header data.
 *  per IETF RFC 791.
 *
 *  It has a single DFDL result variable named IPv4Checksum.
 */
final class IPv4ChecksumLayer()
  extends ChecksumLayer(
    "IPv4Checksum",
    "urn:org.apache.daffodil.layers.IPv4Checksum",
  ) {

  /**
   * This layer is always exactly 20 bytes long.
   */
  private def lenInBytes = 20
  private def chksumShortIndex = 5

  override protected def init(lr: LayerRuntime): Unit = {
    setLength(lenInBytes)
    super.init(lr)
  }

  /**
   * Result DFDL variable value getter. This getter is called to obtain the value for,
   * and populate the DFDL variable named checkDigit, in the layer's target namespace.
   * @return the check digit value
   */
  def getDFDLResultVariable_IPv4Checksum: JInt = getChecksum

  /**
   * Computes the checksum value.
   * When unparsing this also modifies the output bytes to have the checksum at the
   * middle location as per the IPv4 spec.
   * The LayerChecksumMixin assigns the computed checksum value to the first variable.
   * @param layerRuntime structure providing access to state such as variables, when needed.
   * @param isUnparse true if the checksum is for unparsing.
   * @param byteBuffer contains the 20 bytes to be used to compute this checksum.
   * @return the computed checksum value as a 32-bit signed Int.
   */
  override def compute(
    layerRuntime: LayerRuntime,
    isUnparse: Boolean,
    byteBuffer: ByteBuffer,
  ): Int = {
    init(layerRuntime)
    val shortBuf = byteBuffer.asShortBuffer()
    var i = 0
    var chksum: Int = 0
    val nShorts = lenInBytes / 2
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
    val checksumLow = chksum & 0xffff
    val checksumHigh = chksum >>> 16
    val checksumTotal: Int = checksumLow + checksumHigh
    Assert.invariant(checksumTotal <= 0xffff && checksumTotal >= 0)
    val checksumTotalShort = UShort(checksumTotal.toShort)
    val checksum = checksumTotalShort.toInt
    //
    // take ones complement to get the final checksum
    //
    val finalChecksum: Int = (~checksum) & 0xffff

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
