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

package org.apache.daffodil.processors.parsers

import java.nio.file.Files
import java.nio.file.StandardOpenOption

import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.LengthInBitsEv
import org.apache.daffodil.util.MaybeULong

sealed abstract class BlobLengthParser(override val context: ElementRuntimeData)
  extends PrimParser {

  protected def getLengthInBits(pstate: PState): Long

  override final def parse(start: PState): Unit = {
    val dis = start.dataInputStream
    val currentElement = start.simpleElement
    val nBits = getLengthInBits(start)

    val blobPath = try {
      val blobDir = start.output.getBlobDirectory
      Files.createDirectories(blobDir)
      Files.createTempFile(blobDir, start.output.getBlobPrefix, start.output.getBlobSuffix)
    } catch {
      case e: Exception => start.SDE("Unable to create blob file: ", e.getMessage)
    }
    val blobStream = Files.newOutputStream(blobPath, StandardOpenOption.WRITE)

    var remainingBitsToGet = nBits

    val array = new Array[Byte](start.tunable.blobChunkSizeInBytes)
    val blobChunkSizeInBits = start.tunable.blobChunkSizeInBytes * 8

    while (remainingBitsToGet > 0) {
      val bitsToGet = Math.min(remainingBitsToGet, blobChunkSizeInBits).toInt
      if (dis.isDefinedForLength(bitsToGet)) {
        start.dataInputStream.getByteArray(bitsToGet, start, array)
        val bytesToPut = (bitsToGet + 7) / 8
        blobStream.write(array, 0, bytesToPut)
        remainingBitsToGet -= bitsToGet
      } else {
        val remainingBits =
          if (dis.remainingBits.isDefined) {
            val totalBitsRead = nBits - remainingBitsToGet
            MaybeULong(dis.remainingBits.get + totalBitsRead)
          } else {
            MaybeULong.Nope
          }
        PENotEnoughBits(start, nBits, remainingBits)
        remainingBitsToGet = 0 // break out of the loop
      }
    }

    blobStream.close()

    if (start.isSuccess) {
      currentElement.setDataValue(blobPath.toUri)
      start.addBlobPath(blobPath)
    } else {
      Files.delete(blobPath)
    }

  }
}

final class BlobSpecifiedLengthParser(erd: ElementRuntimeData, lengthEv: LengthInBitsEv)
  extends BlobLengthParser(erd) {

  override val runtimeDependencies = Vector(lengthEv)

  override def getLengthInBits(pstate: PState): Long = {
    lengthEv.evaluate(pstate).get
  }

}
