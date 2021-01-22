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

import org.apache.commons.compress.archivers.zip.ZipArchiveEntry
import org.apache.commons.compress.archivers.zip.ZipArchiveOutputStream
import org.apache.commons.compress.archivers.zip.ZipFile
import org.apache.commons.compress.utils.SeekableInMemoryByteChannel
import org.apache.commons.io.IOUtils
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthKind
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthUnits
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.processors.LayerLengthInBytesEv
import org.apache.daffodil.processors.LayerBoundaryMarkEv
import org.apache.daffodil.processors.LayerCharsetEv
import org.apache.daffodil.processors.parsers.PState
import org.apache.daffodil.processors.unparsers.UState
import org.apache.daffodil.dsom.DPathCompileInfo
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.io.ExplicitLengthLimitingStream

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.io.OutputStream
import java.io.SequenceInputStream
import java.nio.charset.StandardCharsets
import scala.Array.emptyByteArray
import scala.collection.JavaConverters._

/*
 * Zip files require seeking to the end of the file. Hence, we need to have an explicit
 * length for a zip layer to work from.
 *
 * When writing out zip file contents, to get the length, one must take the
 * dfdl:contentLength of an enclosing element that surrounds the entire layer.
 *
 * All of this forces buffering. So there's no point in worrying about
 * somehow avoiding buffering inside the implementation.
 * That can simplify many things. There's no point, for example, in trying
 * to not read and buffer all the entries.
 *
 */

/**
 * Zip files are hard to deal with. The entries can be present in the file, but marked deleted
 * based on information in the master information at the end of the file.
 *
 * This requires seeking or buffering. The entries are also compressed (usually, not always)
 *
 * Rather than try to cope with zip by describing the zip format in DFDL, which is not really possible,
 * we instead have a zip layer transformer.
 *
 * This uses java zip libraries to cope with the zip file seeking, decryption, decompress, etc.
 * But enables each entry of the zip file to be presented for Daffodil parsing as something
 * easier to parse, consisting of the metadata of the zip entry in a straightforward layout,
 * followed by the contents of the entry, fully decompressed.
 *
 * This allows a Daffodil element of complex type that knows this layout to decompose the series of
 * zip entries into DFDL elements corresponding to those entries.
 *
 * If you read, and then write a zip using this layer transformer, the data is canonicalized in that
 * deleted entries will not be represented in the zip contents any longer, skipped over sections will be
 * omitted, etc.
 *
 * For unparsing the opposite occurs. A standard layout of entries is filled in from Daffodil unparsing,
 * and this layer then consumes those and creates zip entries from them which are placed into
 * the zip output stream.
 */
final class ZIPTransformer(layerLengthInBytesEv: LayerLengthInBytesEv)
  extends LayerTransformer() {

  override def wrapLayerDecoder(jis: java.io.InputStream) = {
    val s = new ZipAllEntriesInputStream(jis)
    s
  }

  override def wrapLimitingStream(jis: java.io.InputStream, state: PState) = {
    val layerLengthInBytes: Int = layerLengthInBytesEv.evaluate(state).toInt
    val s = new ExplicitLengthLimitingStream(jis, layerLengthInBytes)
    s
  }

  override protected def wrapLayerEncoder(jos: java.io.OutputStream): java.io.OutputStream = {
    val s = new ZipAllEntriesOutputStream(jos)
    s
  }

  override protected def wrapLimitingStream(jos: java.io.OutputStream, state: UState): java.io.OutputStream = {
    jos // just return jis. The way the length will be used/stored is by way of
    // taking the content length of the enclosing element. That will measure the
    // length relative to the "ultimate" data output stream.
  }
}

final class ZipAllEntriesInputStream(is: InputStream)
  extends java.io.InputStream {

  private lazy val sbc = new SeekableInMemoryByteChannel(IOUtils.toByteArray(is))
  private lazy val zf = new ZipFile(sbc)
  private val emptyByteArray = new Array[Byte](0)
  private val emptyInputStream = new ByteArrayInputStream(emptyByteArray)

  private lazy val zeis: InputStream = {
    val entries = zf.getEntries.asScala
    var priorInputStream: InputStream = emptyInputStream
    while (entries.hasNext) {
      //
      // now we pull all the entry fields off the entry
      //
      // we don't bother with getSize() nor getCompressedSize() because
      // the API says they can return -1, so we can't depend on them being there.
      //
      val e: ZipArchiveEntry = entries.next()
      val comment: String = "" // Option(e.getComment()).getOrElse("")
      val extra: Array[Byte] = emptyByteArray // Option(e.getExtra()).getOrElse(emptyByteArray)
      val method: Int = e.getMethod()
      val name: String = e.getName()
      val time: Long = 0L // e.getTime() // modification time, or -1 if not specified
      val isDirectory: Boolean = e.isDirectory()
      // val crc: Long = e.getCrc()

      val baos = new ByteArrayOutputStream()
      val jdos = new java.io.DataOutputStream(baos)
      jdos.writeInt(name.length * 2) // 4 byte length in bytes not characters
      jdos.writeInt(comment.length * 2) // 4 byte length in bytes not characters
      jdos.writeInt(extra.length) // 4 byte length in bytes
      jdos.writeInt(method) // 4 byte value
      jdos.writeLong(time) // 8 byte value - milliseconds since the epoch.
      jdos.writeBoolean(isDirectory) // single byte. 1 is true, 0 is false

      // Now we want to write out a 8-byte data length
      // But because zip entries don't necessarily have the uncompressed data length
      // ( the getSize() method of ZipEntry is allowed to return -1 for unknown length )
      // we actually have to compute the length while we consume the data.

      val dataBaos = new ByteArrayOutputStream()
      val dataInputStream = zf.getInputStream(e)
      val dataLength = org.apache.commons.io.IOUtils.copyLarge(dataInputStream, dataBaos)
      dataBaos.close()
      dataInputStream.close()
      jdos.writeLong(dataLength) // 8 byte length of uncompressed data (in bytes)

      // That's the end of all the fixed-position parts of the header

      Assert.invariant(jdos.size() == 33) // fixed length part.

      // Now we have all the variable-length parts

      jdos.writeChars(name) // string in utf16 encoding
      jdos.writeChars(comment) // string in utf16 encoding
      jdos.write(extra) // byte array

      jdos.close()
      val entryHeaderInputStream = new ByteArrayInputStream(baos.toByteArray)
      val dataBais = new ByteArrayInputStream(dataBaos.toByteArray)
      priorInputStream =
        new SequenceInputStream(priorInputStream,
          new SequenceInputStream(entryHeaderInputStream, dataBais))
    }
    priorInputStream
  }

  override def read(): Int = {
    val res = zeis.read()
    res
  }
}

final class ZipAllEntriesOutputStream(os: OutputStream)
  extends OutputStream {

  private lazy val sbc = new SeekableInMemoryByteChannel()
  private lazy val zos = new ZipArchiveOutputStream(sbc)

  private object State extends Enumeration {
    type State = Value
    val Header, Name, Comment, Extra, Data = Value
  }

  import State._

  private val headerLength = 33

  private var state: State = Header
  private val headerBaos = new ByteArrayOutputStream(headerLength)
  private var nameLen: Int = 0 // length in bytes
  private var name: String = null
  private var commentLen: Int = 0 // length in bytes
  private var comment: String = null
  private var extraLen: Int = 0 // length in bytes.
  private var extra: Array[Byte] = null
  private var method: Int = 0
  private var time: Long = 0L
  private var isDirectory: Boolean = false
  private var dataLength: Long = 0L
  private var dataCount: Long = 0L // counts from 0 to dataLength so we know when we've got all the data.

  private val nameBaos = new ByteArrayOutputStream()
  private val commentBaos = new ByteArrayOutputStream()
  private val extraBaos = new ByteArrayOutputStream()

  override def write(b: Int) = {
    var tryAgain = true
    while (tryAgain) {
      tryAgain = false // by default we don't go around the loop again
      state match {
        case Header => {
          // accumulating the header
          if (headerBaos.size() < headerLength) headerBaos.write(b)
          if (headerBaos.size() == headerLength) {
            // We have accumulated the whole header
            val jdis = new java.io.DataInputStream(new ByteArrayInputStream(headerBaos.toByteArray))

            nameLen = jdis.readInt() // 4 byte length
            commentLen = jdis.readInt() // 4 byte length
            extraLen = jdis.readInt() // 4 byte length
            method = jdis.readInt() // 4 byte value
            time = jdis.readLong() // 8 byte value
            isDirectory = jdis.readBoolean() // single byte. 1 is true, 0 is false
            dataLength = jdis.readLong()

            headerBaos.reset()
            state = Name // transition. Next we accumulate the name.
          }
        }
        case Name => {
          Assert.invariant(nameLen > 0)
          if (nameBaos.size < nameLen) nameBaos.write(b)
          if (nameBaos.size() == nameLen) {
            name = new String(nameBaos.toByteArray, StandardCharsets.UTF_16BE)
            if (isDirectory)
              Assert.invariant(name.endsWith("/")) // TBD: should we even check this?
            nameBaos.reset()
            state = Comment
          }
        }
        case Comment if (commentLen == 0) => {
          state = Extra
          tryAgain = true
        } // not isDone
        case Comment => {
          if (commentBaos.size() < commentLen) commentBaos.write(b)
          if (commentBaos.size() == commentLen) {
            comment = new String(commentBaos.toByteArray, StandardCharsets.UTF_16BE)
            commentBaos.reset()
            state = Extra
          }
        }
        case Extra if (extraLen == 0) => {
          extra = emptyByteArray
          finishEntry()
          state = Data
          tryAgain = true
        }
        case Extra => {
          if (extraBaos.size() < extraLen) extraBaos.write(b)
          if (extraBaos.size() == extraLen) {
            extra = extraBaos.toByteArray
            extraBaos.reset()
            // Now we can create the entry and write all the fields to it.
            finishEntry()
            state = Data
          }
        }
        case Data if (dataLength == 0) => {
          zos.closeArchiveEntry()
          state = Header
          tryAgain = true
        }
        case Data => {
          if (dataCount < dataLength) {
            zos.write(b)
            dataCount += 1
          }
          if (dataCount == dataLength) {
            zos.closeArchiveEntry()
            dataCount = 0
            state = Header
          }
        }
      }
    }
  }

  private def finishEntry() : Unit = {
    val e = new ZipArchiveEntry(name)
    // e.setComment("") // comment)
    // e.setExtra(emptyByteArray) // extra)
    e.setTime(0L)// time)
    e.setMethod(method)
    zos.putArchiveEntry(e)
  }

  override def close(): Unit = {
    Assert.usageErrorUnless(state == Header && headerBaos.size() == 0, "Closed in middle of an entry.")
    zos.close()
    val rawBytes: Array[Byte] = sbc.array()
    val sz = sbc.size()
    os.write(rawBytes, 0, sz.toInt)
    os.flush()
    os.close()
  }
}

object ZIPTransformerFactory
  extends LayerTransformerFactory("zip") {

  override def newInstance(
    maybeLayerCharsetEv: Maybe[LayerCharsetEv],
    maybeLayerLengthKind: Maybe[LayerLengthKind],
    maybeLayerLengthInBytesEv: Maybe[LayerLengthInBytesEv],
    maybeLayerLengthUnits: Maybe[LayerLengthUnits],
    maybeLayerBoundaryMarkEv: Maybe[LayerBoundaryMarkEv],
    tci: DPathCompileInfo): LayerTransformer = {

    val xformer = new ZIPTransformer(maybeLayerLengthInBytesEv.get)
    xformer
  }
}
