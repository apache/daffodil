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

import java.nio.file.Files
import java.nio.file.StandardOpenOption

import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.LengthInBitsEv

trait ByteChunkWriter { self: PrimParser =>

  /**
   * Read nBits of data from the input source associated with pstate in chunks.
   * For each chunk, the writeChunk function passed as a parameter is called,
   * which takes as parameters an array of bytes and the number of bytes in
   * that array that should be written. The chunk size is defined by the
   * blobChunkSizeInBytes tunable.
   */
  def writeBitsInChunks(start: PState, nBits: Long)(
    writeChunk: (Array[Byte], Int) => Unit
  ): Unit = {
    val dis = start.dataInputStream
    val startLoc = start.currentLocation
    val startSfl = start.schemaFileLocation
    var remainingBitsToGet = nBits

    val array = new Array[Byte](start.tunable.blobChunkSizeInBytes)
    // Tunable restrictions ensure blobChunkSizeInBits still fits in an int.
    // All the places where toInt is called below can be no larger than this
    // value, and so have no issues with potential overflow.
    val blobChunkSizeInBits = start.tunable.blobChunkSizeInBytes * 8

    while (remainingBitsToGet > 0) {
      val bitsToGet = Math.min(remainingBitsToGet, blobChunkSizeInBits)
      if (dis.isDefinedForLength(bitsToGet)) {
        start.dataInputStream.getByteArray(bitsToGet.toInt, start, array)
        val bytesToPut = (bitsToGet + 7) / 8
        writeChunk(array, bytesToPut.toInt)
        remainingBitsToGet -= bitsToGet
      } else {
        PENotEnoughBits(start, startSfl, startLoc, nBits, start.dataInputStream)
        remainingBitsToGet = 0 // break out of the loop
      }
    }
  }

}

sealed abstract class BlobLengthParser(override val context: ElementRuntimeData)
  extends PrimParser
  with ByteChunkWriter {

  protected def getLengthInBits(pstate: PState): Long

  override final def parse(start: PState): Unit = {
    val blobPath =
      try {
        val blobDir = start.output.getBlobDirectory()
        Files.createDirectories(blobDir)
        Files.createTempFile(
          blobDir,
          start.output.getBlobPrefix(),
          start.output.getBlobSuffix()
        )
      } catch {
        case e: Exception => start.SDE("Unable to create blob file: ", e.getMessage)
      }

    val blobStream = Files.newOutputStream(blobPath, StandardOpenOption.WRITE)

    val nBits = getLengthInBits(start)
    writeBitsInChunks(start, nBits) { case (bytes, nBytes) =>
      blobStream.write(bytes, 0, nBytes)
    }

    blobStream.close()

    if (start.isSuccess) {
      val currentElement = start.simpleElement
      currentElement.setDataValue(blobPath.toUri)
      start.addBlobPath(blobPath)
    } else {
      Files.delete(blobPath)
    }

  }
}

final class BlobSpecifiedLengthParser(erd: ElementRuntimeData, lengthEv: LengthInBitsEv)
  extends BlobLengthParser(erd) {

  override def runtimeDependencies = Vector(lengthEv)

  override def getLengthInBits(pstate: PState): Long = {
    lengthEv.evaluate(pstate).get
  }

}
