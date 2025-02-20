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

package org.apache.daffodil.runtime1.processors.parsers

import java.nio.ByteBuffer

import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.LengthInBitsEv

sealed abstract class HexBinaryLengthParser(override val context: ElementRuntimeData)
  extends PrimParser
  with ByteChunkWriter {

  protected def getLengthInBits(pstate: PState): Long

  private val zeroLengthArray = new Array[Byte](0)

  override final def parse(start: PState): Unit = {
    val currentElement = start.simpleElement
    val nBits = getLengthInBits(start)
    val nBytes = (nBits + 7) / 8
    if (nBytes == 0) {
      currentElement.setDataValue(zeroLengthArray)
    } else if (nBytes > start.tunable.maxHexBinaryLengthInBytes) {
      PE(
        start,
        "Length for xs:hexBinary exceeds maximum of %s bytes: %s",
        start.tunable.maxHexBinaryLengthInBytes,
        nBytes
      )
    } else if (nBytes <= start.tunable.blobChunkSizeInBytes) {
      // For small hex binary that can fit in a single chunk, don't bother
      // chunking so we avoid the overhead of reading a chunk to one array only
      // to copy it to the target array. Instead, just have the InputSource
      // create and fill the byte array
      val dis = start.dataInputStream
      if (!dis.isDefinedForLength(nBits)) {
        PENotEnoughBits(start, nBits, dis)
      } else {
        val array = start.dataInputStream.getByteArray(nBits.toInt, start)
        currentElement.setDataValue(array)
      }
    } else {
      // For larger hex binary, read the data in chunks. There's additional
      // overhead with this since there's more copying of data between arrays,
      // but it avoids issues with the input stream caches getting emptied and
      // limiting the size of hex binarie data.
      val array = new Array[Byte](nBytes.toInt)
      val buffer = ByteBuffer.wrap(array)
      writeBitsInChunks(start, nBits) { case (bytes, nBytes) =>
        buffer.put(bytes, 0, nBytes)
      }
      if (start.isSuccess) {
        currentElement.setDataValue(array)
      }
    }
  }
}

final class HexBinarySpecifiedLengthParser(erd: ElementRuntimeData, lengthEv: LengthInBitsEv)
  extends HexBinaryLengthParser(erd) {

  override def runtimeDependencies = Vector(lengthEv)

  override def getLengthInBits(pstate: PState): Long = {
    lengthEv.evaluate(pstate).get
  }

}

final class HexBinaryEndOfBitLimitParser(erd: ElementRuntimeData)
  extends HexBinaryLengthParser(erd) {

  override def runtimeDependencies = Vector()

  override def getLengthInBits(pstate: PState): Long = {
    pstate.bitLimit0b.get - pstate.bitPos0b
  }
}
