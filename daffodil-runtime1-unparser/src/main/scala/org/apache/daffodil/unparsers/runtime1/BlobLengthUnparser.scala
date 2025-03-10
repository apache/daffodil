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

import java.nio.file.Files
import java.nio.file.Paths

import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.runtime1.infoset.RetryableException
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.UnparseTargetLengthInBitsEv
import org.apache.daffodil.runtime1.processors.unparsers._

abstract class BlobUnparserBase(override val context: ElementRuntimeData) extends PrimUnparser {

  override def runtimeDependencies = Vector()

  protected def getLengthInBits(state: UState): Long

  override def unparse(state: UState): Unit = {

    val node = state.currentInfosetNode.asSimple
    val value = node.dataValue.getURI
    val lengthInBits = getLengthInBits(state)

    val lengthInBytes = (lengthInBits + 7) / 8

    val uriScheme = value.getScheme
    val uriPath = value.getPath

    if (uriScheme != "file") {
      UnparseError(
        One(context.schemaFileLocation),
        One(state.currentLocation),
        "Blob URI must be a file: %s",
        value.toString
      )
    }

    val path = Paths.get(value)

    val fileSizeInBytes =
      try {
        Files.size(path)
      } catch {
        case e: Exception =>
          UnparseError(
            One(context.schemaFileLocation),
            One(state.currentLocation),
            "Unable to open blob for reading: %s",
            value.toString
          )
      }

    if (fileSizeInBytes > lengthInBytes) {
      UnparseError(
        One(context.schemaFileLocation),
        One(state.currentLocation),
        "Blob length (%d bits) exceeds explicit length value: %d bits",
        fileSizeInBytes * 8,
        lengthInBits
      )
    }

    // This adds two new buffered DataOutputStreams. The first is specific to a
    // Blob, and contains all the logic for delivering the blob content to the
    // current data output stream once it becomes direct without loading all
    // the blob data into memory. The second is a buffered data output stream
    // that Unparsers following this blob one will write data to. Note that if
    // the current data stream is direct, the call to setFinished will cause
    // Daffodil to immediately deliver the blob content to the direct output
    // stream and make the blob data output stream the new direct. Since the
    // blob data output stream is marked as finished in addBufferedBlob, the
    // second data output stream will then be delivered, finally making it the
    // direct stream.
    val dos = state.getDataOutputStream
    val newStream =
      dos.addBufferedBlob(path, lengthInBits, state.tunable.blobChunkSizeInBytes, state)
    state.setDataOutputStream(newStream)
    dos.setFinished(state)
  }
}

final class BlobSpecifiedLengthUnparser(
  erd: ElementRuntimeData,
  val lengthEv: UnparseTargetLengthInBitsEv
) extends BlobUnparserBase(erd) {

  override def getLengthInBits(state: UState): Long = {
    val l: Long =
      try {
        lengthEv.evaluate(state).getULong.toLong
      } catch {
        case e: RetryableException => {
          val uri = state.currentInfosetNode.asSimple.dataValue.getURI
          val path = Paths.get(uri)
          val len = Files.size(path) * 8
          len
        }
      }
    l
  }
}
