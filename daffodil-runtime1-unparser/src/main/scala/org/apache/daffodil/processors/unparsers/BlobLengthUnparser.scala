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

package org.apache.daffodil.processors.unparsers

import passera.unsigned.ULong

import java.net.URI
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.StandardOpenOption

import org.apache.daffodil.infoset.RetryableException
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.UnparseTargetLengthInBitsEv
import org.apache.daffodil.util.Maybe._

abstract class BlobUnparserBase(override val context: ElementRuntimeData)
  extends PrimUnparser {

  override lazy val runtimeDependencies = Vector()

  protected def getLengthInBits(state: UState): Long

  override def unparse(state: UState): Unit = {

    val node = state.currentInfosetNode.asSimple
    val value = node.dataValue.asInstanceOf[URI]
    val lengthInBits = getLengthInBits(state)

    val lengthInBytes = (lengthInBits + 7) / 8

    val uriScheme = value.getScheme
    val uriPath = value.getPath

    if (uriScheme != "file") {
      UnparseError(
        One(context.schemaFileLocation),
        One(state.currentLocation),
        "Blob URI must be a file: %s", value.toString)
    }

    val path = Paths.get(value)

    val fileSizeInBytes = try {
      Files.size(path)
    } catch {
      case e: Exception => 
        UnparseError(
          One(context.schemaFileLocation),
          One(state.currentLocation),
          "Unable to open blob for reading: %s",
          value.toString)
    }

    if (fileSizeInBytes > lengthInBytes) {
      UnparseError(
        One(context.schemaFileLocation),
        One(state.currentLocation),
        "Blob length (%d bits) exceeds explicit length value: %d bits",
        fileSizeInBytes * 8,
        lengthInBits)
    }

    val blobStream =
      try {
        Files.newInputStream(path, StandardOpenOption.READ)
      } catch {
        case e: Exception =>
          UnparseError(
            One(context.schemaFileLocation),
            One(state.currentLocation),
            "Unable to open blob for reading: %s",
            value.toString)
      }

    val dos = state.dataOutputStream

    val array = new Array[Byte](state.tunable.blobChunkSizeInBytes)

    var remainingBitsToPut = lengthInBits
    var fileHasData = true
    while (remainingBitsToPut > 0 && fileHasData) {
      val bitsToRead = Math.min(remainingBitsToPut, state.tunable.blobChunkSizeInBytes * 8)
      val bytesToRead = (bitsToRead + 7) / 8
      val bytesRead = blobStream.read(array, 0, bytesToRead.toInt)
      if (bytesRead == -1) {
        fileHasData = false
      } else {
        val bitsToPut = Math.min(bytesRead * 8, bitsToRead)
        val ret = dos.putByteArray(array, bitsToPut.toInt, state)
        if (!ret) {
          blobStream.close()
          UnparseError(
            One(context.schemaFileLocation),
            One(state.currentLocation),
            "Failed to write %d blob bits",
            lengthInBits)
        }
        remainingBitsToPut -= bitsToPut
      }
    }

    blobStream.close()

    // calculate the skip bits
    val nFillBits = remainingBitsToPut
    if (nFillBits > 0) {
      val ret = dos.skip(nFillBits, state)
      if (!ret) {
        UnparseError(Nope, One(state.currentLocation), "Failed to skip %d bits.", nFillBits)
      }
    }
  }
}

final class BlobSpecifiedLengthUnparser(erd: ElementRuntimeData, val lengthEv: UnparseTargetLengthInBitsEv)
  extends BlobUnparserBase(erd) {

  override def getLengthInBits(state: UState): Long = {
    val l: Long = try {
      lengthEv.evaluate(state).getULong.toLong
    } catch {
      case e: RetryableException => {
        val uri = state.currentInfosetNode.asSimple.dataValue.asInstanceOf[URI]
        val path = Paths.get(uri)
        val len = Files.size(path) * 8
        len
      }
    }
    l
  }
}
