/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.Implicits._; object INoWarn { ImplicitsSuppressUnusedImportWarning() }
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.util.Bits
import java.io.ByteArrayInputStream
import java.nio.charset.Charset
import java.nio.CharBuffer
import java.io.InputStreamReader
import edu.illinois.ncsa.daffodil.processors.charset.CharsetUtils
import java.io.InputStream
import java.io.File
import org.apache.commons.io.IOUtils
import java.nio.file.Files
import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import java.nio.charset.CharsetDecoder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import edu.illinois.ncsa.daffodil.io.Utils
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.Representation
import edu.illinois.ncsa.daffodil.io.ByteBufferDataInputStream
import edu.illinois.ncsa.daffodil.io.DataInputStream
import edu.illinois.ncsa.daffodil.io.DataOutputStream
import java.nio.channels.Channels
import java.io.ByteArrayOutputStream
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.io.DataDumper
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.api.DataLocation

class DataLoc(val bitPos1b: Long, bitLimit1b: MaybeULong, eitherStream: Either[DataOutputStream, DataInputStream],
    val maybeERD: Maybe[ElementRuntimeData]) extends DataLocation {

  // override def toString = "DataLoc(bitPos1b='%s', bitLimit1b='%s')".format(bitPos1b, bitLimit1b)
  override def toString() = {
    "byte " + bitPos1b / 8 + (if (bitLimit1b.isDefined) " limit(bytes) " + bitLimit1b.get / 8 else "")
  }

  private val Dump = new DataDumper

  lazy val optERD = maybeERD.toScalaOption

  Assert.usage(bitLimit1b.isEmpty || bitLimit1b.get >= 0)
  Assert.usage(bitPos1b >= 1)

  val bitPos0b = math.max(bitPos1b - 1, 0).toInt
  val bitLimit0b = if (bitLimit1b.isDefined) MaybeULong(bitLimit1b.get - 1) else MaybeULong.Nope
  val lengthInBits = if (bitLimit0b.isDefined) math.max(bitLimit0b.get - bitPos0b, 0) else 256L

  // The dump region is the identified data for this data loc
  lazy val regionStartBitPos0b = bitPos0b
  lazy val regionLengthInBits = lengthInBits
  lazy val regionEndPos0b = lengthInBits + bitPos0b

  // The dump rounds down and up to boundaries of 16 bytes (128 bits)
  lazy val dumpStartBitPos0b = (regionStartBitPos0b >> 7) << 7
  lazy val dumpEndBitPos0b = (math.ceil(regionEndPos0b / 128.0) * 128).toInt
  lazy val dumpLengthInBits = dumpEndBitPos0b - dumpStartBitPos0b

  lazy val (bytePos0b, lengthInBytes, endBytePos0b) = Dump.convertBitsToBytesUnits(bitPos0b, lengthInBits)
  lazy val bytePos1b = bytePos0b + 1
  lazy val (dumpStartBytePos0b, dumpLengthInBytes, dumpEndBytePos0b) = Dump.convertBitsToBytesUnits(dumpStartBitPos0b, dumpLengthInBits)
  lazy val (regionStartBytePos0b, regionLengthInBytes, regionEndBytePos0b) = Dump.convertBitsToBytesUnits(regionStartBitPos0b, regionLengthInBits)

  def dump(rep: Option[Representation], prestate: DataLocation, state: ParseOrUnparseState): String = {

    val maybeEncodingName = optERD.flatMap { erd =>
      if (erd.encodingInfo.isKnownEncoding) {
        if (erd.encodingInfo.knownEncodingAlignmentInBits != 8) None // non byte aligned encoding
        else Some(erd.encodingInfo.knownEncodingName) // byte-aligned encoding
      } else None
    }
    val optEncodingName = maybeEncodingName.toScalaOption

    def binary: Dump.Kind = optERD.map { erd =>
      val bitOrder: BitOrder = erd.defaultBitOrder
      bitOrder match {
        case BitOrder.MostSignificantBitFirst => Dump.MixedHexLTR(optEncodingName)
        case BitOrder.LeastSignificantBitFirst => Dump.MixedHexRTL(None)
      }
    }.getOrElse(Dump.MixedHexLTR(optEncodingName))

    //    def text: Dump.Kind = optERD.map { erd =>
    //      val rootERD = erd.parent
    //      if (erd.rootERD.encodingInfo.isScannable) Dump.TextOnly(optEncodingName)
    //      else binary
    //    }.getOrElse(binary)

    //    val dumpKind: Dump.Kind = (rep, optERD.toScalaOption) match {
    //      case (None, None) => binary
    //      case (Some(Representation.Binary), _) => binary
    //      case (Some(Representation.Text), _) => text
    //      case (None, Some(erd)) => erd.impliedRepresentation match {
    //        case Representation.Text => text
    //        case Representation.Binary => binary
    //      }
    //    }
    // dumpStream(dumpKind, prestate, state)
    dumpStream(binary, prestate, state) // for now. Let's require the hex+text dumps always.
  }

  private def dumpStream(dumpKind: Dump.Kind, prestate: DataLocation, state: ParseOrUnparseState): String = {

    val startOfInterestRegionBits0b = prestate.bitPos1b - 1
    val endOfInterestRegionBits0b = state.bitPos0b
    val lengthOfInterestRegionInBits = math.min(math.max(endOfInterestRegionBits0b - startOfInterestRegionBits0b, 0),
      DaffodilTunableParameters.maxDataDumpSizeInBytes)
    val regionSpecifier = Some((startOfInterestRegionBits0b, lengthOfInterestRegionInBits.toInt))

    val s = (eitherStream, prestate, state) match {
      //
      // Unparsing
      //
      case (Left(os: DataOutputStream), prestate: DataLocation, state: UState) => {
        val howFarIntoPastData = 16 // bytePos0b - dumpStartBytePos0b
        val pastBBuf = os.pastData(howFarIntoPastData.toInt)
        val howMuchPast = pastBBuf.remaining()
        if (pastBBuf.remaining == 0) return "No data yet"
        val pastDump = Dump.dump(dumpKind,
          dumpStartBitPos0b, howMuchPast.toInt * 8, pastBBuf,
          includeHeadingLine = true,
          indicatorInfo = regionSpecifier)
        pastDump.mkString("\n")
      }
      //
      // Parsing
      //
      case (Right(vis: DataInputStream), prestate: DataLocation, state: PState) => {
        // Parser
        val howFarIntoPastData = math.min(bytePos0b - dumpStartBytePos0b, DaffodilTunableParameters.maxDataDumpSizeInBytes)
        val pastBBuf = vis.pastData(howFarIntoPastData.toInt)
        val howFarIntoFutureData = math.min((dumpEndBytePos0b + 1) - bytePos0b, DaffodilTunableParameters.maxDataDumpSizeInBytes)
        Assert.invariant(howFarIntoFutureData >= 0)
        val futureBBuf = vis.futureData(howFarIntoFutureData.toInt)
        val allDataBBuf = Utils.concatByteBuffers(pastBBuf, futureBBuf)
        val dataLength = allDataBBuf.remaining
        val dump = Dump.dump(dumpKind, dumpStartBitPos0b, dataLength * 8, allDataBBuf,
          includeHeadingLine = true,
          indicatorInfo = regionSpecifier).mkString("\n")
        dump
      }
      case _ => Assert.invariantFailed("No other case possible.")
    }
    s
  }

  def aligned128BitsPos = (bitPos1b >> 7) << 7

  /*
   * We're at the end if the position is at the limit.
   */
  def isAtEnd: Boolean = {
    Assert.invariant(bitLimit1b.isDefined)
    bitPos1b >= bitLimit1b.get
  }
}
