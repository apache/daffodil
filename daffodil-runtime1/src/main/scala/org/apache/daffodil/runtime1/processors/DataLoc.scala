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

package org.apache.daffodil.runtime1.processors

import org.apache.daffodil.io.DataDumper
import org.apache.daffodil.io.DataInputStream
import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.io.Utils
import org.apache.daffodil.lib.iapi.DataLocation
import org.apache.daffodil.lib.exceptions._
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.schema.annotation.props.gen.Representation
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.runtime1.processors.parsers.PState
import org.apache.daffodil.runtime1.processors.unparsers.UState

class DataLoc(
  val bitPos1b: Long,
  bitLimit1b: MaybeULong,
  eitherStream: Either[DataOutputStream, DataInputStream],
  val maybeERD: Maybe[ElementRuntimeData]
) extends DataLocation {

  // override def toString = "DataLoc(bitPos1b='%s', bitLimit1b='%s')".format(bitPos1b, bitLimit1b)
  override def toString() = {
    "byte " + bitPos1b / 8 + (if (bitLimit1b.isDefined) " limit(bytes) " + bitLimit1b.get / 8
                              else "")
  }

  private val Dump = new DataDumper

  lazy val optERD = maybeERD.toOption

  Assert.usage(bitLimit1b.isEmpty || bitLimit1b.get >= 0)
  Assert.usage(bitPos1b >= 1)

  val bitPos0b: Long = bitPos1b - 1
  val bitLimit0b: MaybeULong =
    if (bitLimit1b.isDefined) MaybeULong(bitLimit1b.get - 1) else MaybeULong.Nope
  val lengthInBits: Long = if (bitLimit0b.isDefined) (bitLimit0b.get - bitPos0b) else 256L

  // The dump region is the identified data for this data loc
  private lazy val regionStartBitPos0b = bitPos0b
  private lazy val regionLengthInBits = lengthInBits
  private lazy val regionEndPos0b = lengthInBits + bitPos0b

  // The dump rounds down and up to boundaries of 16 bytes (128 bits)
  private lazy val dumpStartBitPos0b = (regionStartBitPos0b >> 7) << 7
  private lazy val dumpEndBitPos0b = (math.ceil(regionEndPos0b / 128.0) * 128).toLong
  private lazy val dumpLengthInBits = dumpEndBitPos0b - dumpStartBitPos0b

  lazy val bytePos1b: Long = bytePos0b + 1

  lazy val (bytePos0b: Long, lengthInBytes: Int, endBytePos0b: Long) =
    Dump.convertBitsToBytesUnits(bitPos0b, lengthInBits)

  private lazy val (dumpStartBytePos0b, dumpLengthInBytes, dumpEndBytePos0b) =
    Dump.convertBitsToBytesUnits(dumpStartBitPos0b, dumpLengthInBits)

  private lazy val (regionStartBytePos0b, regionLengthInBytes, regionEndBytePos0b) =
    Dump.convertBitsToBytesUnits(regionStartBitPos0b, regionLengthInBits)

  def dump(
    rep: Option[Representation],
    prestate: DataLocation,
    state: ParseOrUnparseState
  ): String = {

    val maybeEncodingName = optERD.flatMap { erd =>
      if (erd.encodingInfo.isKnownEncoding) {
        if (erd.encodingInfo.knownEncodingAlignmentInBits != 8)
          None // non byte aligned encoding
        else Some(erd.encodingInfo.knownEncodingName) // byte-aligned encoding
      } else None
    }
    val optEncodingName = maybeEncodingName.toOption

    def binary: Dump.Kind = optERD
      .map { erd =>
        val bitOrder: BitOrder = erd.defaultBitOrder
        bitOrder match {
          case BitOrder.MostSignificantBitFirst => Dump.MixedHexLTR(optEncodingName)
          case BitOrder.LeastSignificantBitFirst => Dump.MixedHexRTL(None)
        }
      }
      .getOrElse(Dump.MixedHexLTR(optEncodingName))

    dumpStream(binary, prestate, state) // for now. Let's require the hex+text dumps always.
  }

  private def dumpStream(
    dumpKind: Dump.Kind,
    prestate: DataLocation,
    state: ParseOrUnparseState
  ): String = {

    val startOfInterestRegionBits0b = prestate.bitPos1b - 1
    val endOfInterestRegionBits0b = state.bitPos0b
    val lengthOfInterestRegionInBits = math.min(
      math.max(endOfInterestRegionBits0b - startOfInterestRegionBits0b, 0),
      state.tunable.maxDataDumpSizeInBytes
    )
    val regionSpecifier = Some(
      (startOfInterestRegionBits0b, lengthOfInterestRegionInBits.toInt)
    )

    val s = (eitherStream, prestate, state) match {
      //
      // Unparsing
      //
      case (Left(os: DataOutputStream), prestate: DataLocation, state: UState) => {
        val howFarIntoPastData = 16 // bytePos0b - dumpStartBytePos0b
        val pastBBuf = os.pastData(howFarIntoPastData.toInt)
        val howMuchPast = pastBBuf.remaining()
        if (pastBBuf.remaining == 0) return "No data yet"
        val pastDump = Dump.dump(
          dumpKind,
          dumpStartBitPos0b,
          howMuchPast.toInt * 8,
          pastBBuf,
          includeHeadingLine = true,
          indicatorInfo = regionSpecifier
        )
        pastDump.mkString("\n")
      }
      //
      // Parsing
      //
      case (Right(vis: DataInputStream), prestate: DataLocation, state: PState) => {
        // Parser
        val howFarIntoPastData =
          math.min(bytePos0b - dumpStartBytePos0b, state.tunable.maxDataDumpSizeInBytes)
        val pastBBuf = vis.pastData(howFarIntoPastData.toInt)
        val howFarIntoFutureData =
          math.min((dumpEndBytePos0b + 1) - bytePos0b, state.tunable.maxDataDumpSizeInBytes)
        Assert.invariant(howFarIntoFutureData >= 0)
        val futureBBuf = vis.futureData(howFarIntoFutureData.toInt)
        val allDataBBuf = Utils.concatByteBuffers(pastBBuf, futureBBuf)
        val dataLength = allDataBBuf.remaining
        val dump = Dump
          .dump(
            dumpKind,
            dumpStartBitPos0b,
            dataLength * 8,
            allDataBBuf,
            includeHeadingLine = true,
            indicatorInfo = regionSpecifier
          )
          .mkString("\n")
        dump
      }
      case _ => Assert.invariantFailed("No other case possible.")
    }
    s
  }

  def aligned128BitsPos = (bitPos1b >> 7) << 7
}
