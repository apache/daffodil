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

import java.nio.ByteBuffer
import java.util.Optional
import scala.collection.JavaConverters._

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.api.DFDLPrimType
import org.apache.daffodil.runtime1.layers.api.JLayerLengthKind
import org.apache.daffodil.runtime1.layers.api.Layer
import org.apache.daffodil.runtime1.layers.api.LayerChecksumMixin
import org.apache.daffodil.runtime1.layers.api.LayerRuntime
import org.apache.daffodil.runtime1.layers.api.LayerVariables

import passera.unsigned.UShort

object IPv4ChecksumLayer {
  val name: String = "IPv4Checksum"
  val vars: LayerVariables =
    LayerVariables(
      prefix = "chksum",
      namespace = "urn:org.apache.daffodil.layers.IPv4Checksum",
      variables = Seq(
        (name, DFDLPrimType.UnsignedShort),
      ).asJava,
    )
}

/**
 *  The layer transform computes the checksum of the header data.
 *  per IETF RFC 791.
 *
 *  The data has a well-known fixed length, so layerLengthKind is 'implicit' or not specified.
 */
final class IPv4ChecksumLayer()
  extends Layer(
    IPv4ChecksumLayer.name,
    supportedLayerLengthKinds =
      Seq(JLayerLengthKind.Implicit).asJava, // Implicit implies you cannot specify layerLength.
    supportedLayerLengthUnits = Seq().asJava,
    isRequiredLayerEncoding = false,
    optLayerVariables = Optional.of(IPv4ChecksumLayer.vars),
  )
  with LayerChecksumMixin // Checksums are easily expressed as a compute method on a byte buffer.
  {

  /**
   * This layer is always exactly 20 bytes long.
   */
  private def lenInBytes = 20 // bytes

  override def optLayerBuiltInConstantLength: Optional[Int] = Optional.of(lenInBytes)

  private def chksumShortIndex = 5

  /**
   * Computes the checksum value.
   * When unparsing this also modifies the output bytes to have the checksum at the
   * middle location as per the IPv4 spec.
   * The LayerChecksumMixin assigns the computed checksum value to the first variable.
   * @param layerRuntimeInfo structure providing access to state such as variables, when needed.
   * @param isUnparse true if the checksum is for unparsing.
   * @param byteBuffer contains the 20 bytes to be used to compute this checksum.
   * @return the computed checksum value as a 32-bit signed Int.
   */
  override def compute(
    layerRuntimeInfo: LayerRuntime,
    isUnparse: Boolean,
    byteBuffer: ByteBuffer,
  ): Int = {
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
