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

package org.apache.daffodil.io

import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset.CoderResult
import java.nio.charset.{ Charset => JavaCharset }
import java.nio.charset.{ CharsetDecoder => JavaCharsetDecoder }

import org.apache.daffodil.lib.equality._
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Misc

import com.ibm.icu.lang.UCharacter
import com.ibm.icu.lang.UCharacterEnums
import com.ibm.icu.lang.UProperty

/**
 * Hex/Bits and text dump formats for debug/trace purposes.
 *
 * By definition this is a dump, so doesn't know much about where the
 * fields in the data are. (To do that you'd need a format description
 * language, like DFDL, but this is here to help debug DFDL descriptions,
 * so it really cannot exploit any information about the data format)
 */
class DataDumper {

  val defaultMaxLineLength = 70

  /**
   * What kind of dump do you want?
   * Text only - no hex will be displayed.
   * Binary only - do you want hex? or binary bits?
   * Mixed - show both hex/bits and text side by side
   */
  sealed trait Kind
  protected sealed class TextKind(val optCharset: Option[String]) extends Kind
  protected sealed trait BinaryKind extends Kind
  protected sealed trait HexKind extends BinaryKind // hexadecimal
  protected sealed trait Direction
  protected sealed trait RTL
    extends Direction // used with least-signif-bit first data like mil-std-2045
  protected sealed trait LTR extends Direction
  case class TextOnly(override val optCharset: Option[String] = None)
    extends TextKind(optCharset)
  case class MixedHexLTR(override val optCharset: Option[String] = None)
    extends TextKind(optCharset)
    with HexKind
    with LTR
  case class MixedHexRTL(override val optCharset: Option[String] = None)
    extends TextKind(optCharset)
    with HexKind
    with RTL

  def convertBitsToBytesUnits(
    startBitAddress0b: Long,
    lengthInBits: Long
  ): (Long, Int, Long) = {
    Assert.usage(startBitAddress0b >= 0)
    Assert.usage(lengthInBits >= 0)
    val startByteAddress0b = startBitAddress0b >> 3
    val lengthInBytes = {
      val endBit0b = startBitAddress0b + lengthInBits
      val extraStartByte =
        if (startBitAddress0b % 8 == 0) 0 else 1
      val extraEndByte =
        if (endBit0b % 8 == 0) 0 else 1
      val res = (lengthInBits >> 3) + extraStartByte + extraEndByte
      res
    }
    val endByteAddress0b = math.max(startByteAddress0b + lengthInBytes - 1, 0)
    (startByteAddress0b, lengthInBytes.toInt, endByteAddress0b)
  }

  /**
   * A dump is a sequence of dump lines.
   *
   * An optional first line is a header that numbers the bits/bytes
   * An optional indicator line uses draw-characters to point at the
   * significant part of the data - e.g., where in the dump the current
   * element is.
   * The data lines follow. They contain up to 3 sections (each of which is
   * optional) an address, a hex/binary dump, a text dump.
   *
   * If the kind includes charset that charset is used to display text. Decode
   * errors will display as the Unicode replacement character. If no charset
   * then text will be displayed as iso-8859-1, augmented by using glyph characters
   * for the control C0 and C1 and any other non-glyph codepoint.
   *
   * indicator info is a start position and length for the "region of interest". The units
   * are in bits.
   *
   * The shamStartBitAddress0b is the location where the data in the byteBuffer starts.
   * E.g., the byte at byteBuffer.get(0) is from the data stream at the shamStartBitAddress0b.
   *
   * The byte source is a window into the data stream.
   */
  def dump(
    kind: Kind,
    shamStartBitAddress0b: Long,
    lengthInBits: Int,
    byteBuffer: ByteBuffer,
    maxLineLength: Int = defaultMaxLineLength,
    includeHeadingLine: Boolean = true,
    indicatorInfo: Option[(Long, Int)] = None
  ): Seq[String] = {
    val (shamStartByteAddress0b, lengthInBytes, _) =
      convertBitsToBytesUnits(shamStartBitAddress0b, lengthInBits)
    val indicatorInfoInBytes = indicatorInfo.map { case (indStartBits0b, indLenBits) =>
      val (indStartByteAddress0b, indLengthInBytes, _) =
        convertBitsToBytesUnits(indStartBits0b, indLenBits)
      (indStartByteAddress0b, indLengthInBytes)
    }
    val optEncName = Option(kind).collect { case t: TextKind => t.optCharset }.flatten
    kind match {
      case TextOnly(enc) => {
        dumpTextLine(
          maxLineLength,
          shamStartByteAddress0b,
          lengthInBytes,
          byteBuffer,
          enc,
          indicatorInfoInBytes
        )
      }
      case MixedHexLTR(optionCS) =>
        dumpHexAndTextBytes(
          shamStartByteAddress0b,
          lengthInBytes,
          byteBuffer,
          includeHeadingLine,
          optEncName,
          indicatorInfoInBytes
        )
      case MixedHexRTL(None) =>
        dumpHexAndTextBytesLSBFirst(
          shamStartByteAddress0b,
          lengthInBytes,
          byteBuffer,
          includeHeadingLine,
          optEncName
        )
      case _ => Assert.usageError("unsupported dump kind")
    }
  }

  //
  // These vars are used by the txt dump when the multiple bytes of a
  // character wrap from one line to the next.
  //
  var paddingFromPriorLine = ""
  var nPadBytesFromPriorLine = 0

  private def textDump(
    addr: Long,
    rowStart0b: Int,
    txtsb: StringBuilder,
    limit0b: Int,
    endByteAddress0b: Long,
    byteBuffer: ByteBuffer,
    decoder: Option[JavaCharsetDecoder],
    textByteWidth: Int
  ): Unit = {
    var i = rowStart0b + nPadBytesFromPriorLine
    txtsb ++= paddingFromPriorLine
    while (i <= limit0b) {
      val bytePos0b = addr + i
      val (charRep, nBytesConsumed, width) =
        convertToCharRepr(bytePos0b, endByteAddress0b, byteBuffer, decoder)
      Assert.invariant(nBytesConsumed > 0)
      // some characters will print double width. It is assumed all such
      // characters occupy at least one byte.
      Assert.invariant(nBytesConsumed >= width)
      //
      // Will padding wrap to next line?
      //
      val padByteRep = "~" * (textByteWidth - 1)
      val nBytesPastEnd =
        if (nBytesConsumed == 1) 0
        else {
          (limit0b - i + 1, nBytesConsumed) match {
            case (1, 2) => 1
            case (1, 3) => 2
            case (1, 4) => 3
            case (2, 2) => 0
            case (2, 3) => 1
            case (2, 4) => 2
            case (3, 2) => 0
            case (3, 3) => 0
            case (3, 4) => 1
            case (4, _) => 0
            case _ => 0
          }
        }
      paddingFromPriorLine = padByteRep * 2 * nBytesPastEnd
      nPadBytesFromPriorLine = nBytesPastEnd
      //
      // Adjust padding downward if the character is double wide.
      //
      val padding = padByteRep * ((nBytesConsumed, width) match {
        case (1, 1) => 1
        case (1, 2) => 0
        case (2, x) => 4 - x
        case (3, x) => 6 - x
        case (4, x) => 8 - x
        case (n, x) => Assert.impossible()
      })
      val trimmedPadding = padding.take(padding.length - paddingFromPriorLine.length)
      txtsb ++= charRep + trimmedPadding
      i += nBytesConsumed
    }
  }

  /**
   * Creates a dump that looks like Emacs Hexl mode.
   *
   * <p>
   * Note that the character glphs on the right depend on the
   * font being used. These all are printing characters but whether they
   * line up perfectly under the heading columns depends on the font
   * being used. In particular, it makes use of the C0 control picture
   * unicode characters to give glyphs to those otherwise non-printing
   * characters, but these are not all monospaced widths.
   * <p>
   * For examples see the TestDump class.
   */
  private[io] def dumpHexAndTextBytes(
    startByteAddress0b: Long,
    lengthInBytes: Int,
    byteBuffer: ByteBuffer,
    includeHeadingLine: Boolean,
    optEncodingName: Option[String],
    indicatorInfoInBytes: Option[(Long, Int)]
  ): Seq[String] = {

    Assert.usage(startByteAddress0b >= 0)
    Assert.usage(lengthInBytes >= 0)

    val (textDataHeader, textByteWidth, optEncName) = getTextParameters(optEncodingName)
    val decoder = getReportingDecoder(optEncName)

    val endByteAddress0b = math.max(startByteAddress0b + lengthInBytes - 1, 0)
    val addressHeader = """87654321  """
    val hexHeader = """0011 2233 4455 6677 8899 aabb ccdd eeff""" // space on the end is needed
    val headingHex = addressHeader + hexHeader
    val firstGutter = ": "
    val offset0b = (startByteAddress0b & 0xf).toInt
    val hexRegionInitialWhitespace = {
      val offset2 = offset0b / 2
      val res = "     " * offset2 +
        ("  " * (offset0b & 0x1)) // blank first half of pair
      res
    }
    val textRegionInitialWhitespace = (" " * textByteWidth) * offset0b

    val indicatorLine =
      makeHexAndTextIndicatorLine(
        indicatorInfoInBytes,
        startByteAddress0b,
        lengthInBytes,
        hexHeader.length,
        addressHeader.length,
        textByteWidth
      )

    var isFirstRow = true
    var isLastRow = false
    val firstLeftAddress = startByteAddress0b & 0x7fffffffffffff0L
    val lastLeftAddress =
      math.max(0, (startByteAddress0b + lengthInBytes - 1)) & 0x7ffffffffffffff0L

    val headingLine = headingHex + "  " + textDataHeader

    val ab = scala.collection.mutable.ArrayBuffer[String]()
    indicatorLine.foreach { line => ab += line }
    if (includeHeadingLine) ab += headingLine

    val hexsb = new StringBuilder
    val txtsb = new StringBuilder
    var rowStart0b = offset0b
    var limit0b = 15 // except for last row it will be shortened. Inclusive limit.

    //
    // These vars are used by the txt dump when the multiple bytes of a
    // character wrap from one line to the next.
    //
    paddingFromPriorLine = ""
    nPadBytesFromPriorLine = 0

    (firstLeftAddress to lastLeftAddress by 16).foreach {
      //
      // for each line/row, we assemble the address part, the hex part, and the text part
      //
      addr =>
        if (addr == lastLeftAddress) {
          isLastRow = true
          limit0b = (endByteAddress0b & 0xf).toInt // might be fewer than all 16 for last row
        }
        val addrString = "%08x".format(addr)
        hexsb ++= addrString + firstGutter
        if (isFirstRow) {
          isFirstRow = false
          hexsb ++= hexRegionInitialWhitespace
          txtsb ++= textRegionInitialWhitespace
        }

        //
        // Hex dump
        //
        (rowStart0b to limit0b).foreach { i =>
          val bytePos0b = addr + i - startByteAddress0b
          val byteValue =
            try {
              byteBuffer.get(bytePos0b.toInt)
            } catch {
              case e: IndexOutOfBoundsException => 0.toByte
            }
          val hex = "%02x".format(byteValue)
          val gutter = if ((i & 0x1) == 0) "" else " "
          hexsb ++= hex + gutter
        }
        //
        // Text dump
        //
        textDump(
          addr - startByteAddress0b,
          rowStart0b,
          txtsb,
          limit0b,
          endByteAddress0b,
          byteBuffer,
          decoder,
          textByteWidth
        )

        if (isLastRow) {
          //
          // Trailing spaces on the hex dump
          //
          ((limit0b + 1) to 15).foreach { i =>
            val gutter = if ((i & 0x1) == 1) " " else ""
            hexsb ++= "  " + gutter
          }
          //
          // Trailing spaces on the text dump
          //
          ((limit0b + 1) to 15).foreach { i =>
            txtsb ++= (" " * textByteWidth)
          }
        }
        ab += hexsb.mkString + " " + txtsb.mkString
        hexsb.clear()
        txtsb.clear()
        //
        // we're done with first row, so subsequent rows will have
        // zero as the row start.
        //
        rowStart0b = 0
    }
    ab.toSeq
  }

  // indicators over these dumps are of maximum length 16 bytes.
  //
  // like this:
  //    ├─────────────────────────────────────═  ├──────────────═
  //    0011 2233 4455 6677 8899 aabb ccdd eeff  0123456789abcdef
  // or
  //    ├─────────────────────────────────────┤  ├──────────────┤
  //    0011 2233 4455 6677 8899 aabb ccdd eeff  0123456789abcdef
  //
  // but they can also be shorter than 16 bytes if the region starts further
  // in from the left.
  //
  private def makeHexAndTextIndicatorLine(
    indicatorInfoInBytes: Option[(Long, Int)],
    startByteAddress0b: Long,
    lengthInBytes: Int,
    hexHeaderLength: Int,
    addressHeaderLength: Int,
    textByteWidth: Int
  ) = {
    indicatorInfoInBytes.map { case (goalIndByteAddress0b: Long, indLengthInBytes: Int) =>
      val indByteAddress0b = math.max(goalIndByteAddress0b, startByteAddress0b)
      val delta = indByteAddress0b - startByteAddress0b
      val realLengthInBytes = math.min(indLengthInBytes, lengthInBytes)
      //
      // if the delta is more than this, the indicator will be ambiguous because what it
      // points at isn't directly below, but possibly a further row down.
      //
      Assert.usage(delta < 16)
      Assert.usage(indLengthInBytes >= 0) // if too big we'll clamp it.
      val indicatorOffset0b = indByteAddress0b.toInt % 16
      val indOffset2 = indicatorOffset0b / 2
      val initialHexSpaces = "     " * indOffset2 +
        ("  " * (indicatorOffset0b & 0x1)) // blank first half of pair

      val pictureLengthInBytes = math.min(16 - indicatorOffset0b.toInt, realLengthInBytes)
      val hexIndicator = {
        val picture =
          (pictureLengthInBytes, indByteAddress0b % 2) match {
            case (0, _) => "│"
            case (1, _) if realLengthInBytes =#= 1 => "├┤"
            case (1, _) => "├═"
            case (2, 0) if realLengthInBytes =#= 2 => "├──┤"
            case (2, 0) => "├──═"
            case (2, 1) => "├───┤" // middle dash spans the gutter
            case (n, s) => {
              Assert.invariant(n >= 3)
              val startCap = "├─"
              val endCap =
                if (realLengthInBytes > n) "─═"
                else "─┤"
              val startBytePic = if (s =#= 0) startCap + "──" else startCap
              val endBytePic =
                if (((indicatorOffset0b.toInt + n) % 2) =#= 0) "──" + endCap else endCap

              val startRoundUp2 =
                (indicatorOffset0b.toInt + 2) - (indicatorOffset0b.toInt + 2) % 2
              val endRoundDown2 =
                (indicatorOffset0b.toInt + n - 1) - (indicatorOffset0b.toInt + n - 1) % 2

              val middleBytes = (endRoundDown2 - startRoundUp2) / 2
              val middleBytePics = Seq.fill(middleBytes.toInt)("────")

              val bytePix = startBytePic +: middleBytePics :+ endBytePic
              val pic = bytePix.mkString("─") // for the single space gutters between
              pic
            }
          }
        val pictureOnly = initialHexSpaces + picture
        val endPadLength = hexHeaderLength - pictureOnly.length
        val endPad = " " * endPadLength
        val finalPicture = pictureOnly + endPad
        finalPicture
      }
      val textIndicator = {
        val initialTextSpaces = " " * textByteWidth * indicatorOffset0b
        val picture =
          (pictureLengthInBytes, textByteWidth) match {
            case (0, _) => "│"
            case (1, 1) => "║"
            case (1, 2) => "├┤"
            case (n, w) => {
              val pad = if (w =#= 1) "" else "─"
              val startCap = "├" + pad
              val endCap =
                if (realLengthInBytes > n) pad + "═"
                else pad + "┤"
              val middleBytePics = (1 to (pictureLengthInBytes - 2)).map { _ => "─" + pad }
              val allPix = startCap +: middleBytePics :+ endCap
              val pic = allPix.mkString
              pic
            }
          }
        val finalPicture = initialTextSpaces + picture
        finalPicture
      }
      val initialSpaces = " " * addressHeaderLength
      val line = initialSpaces + hexIndicator + "  " + textIndicator
      line
    }
  }

  /**
   * Some characters act as combining marks and modify characters surrounding them.
   * In order for us to display these characters as they are, we need to combine them with
   * the appropriate number of spaces so they don't disturb other characters around them
   */
  private def homogenizeChars(codepoint: Int): (String, Int) = {
    val charType = UCharacter.getType(codepoint)
    val nCols = charNColumns(codepoint)
    // supposed to always modify preceding character,
    // but sometimes appears to modify succeeding character
    // e.g \u093f or \u064d. Dual spacing is here to protect from this
    charType match {
      case UCharacterEnums.ECharacterCategory.COMBINING_SPACING_MARK => {
        // can occupy spacing position by themselves
        (" " + Character.toChars(codepoint).mkString + " ", nCols + 2)
      }
      case UCharacterEnums.ECharacterCategory.NON_SPACING_MARK => {
        // do not occupy spacing position by themselves
        // (so 1 space should be consumed, leaving an extra 1 for padding)
        (" " + Character.toChars(codepoint).mkString + " ", nCols + 1)
      }
      case _ => {
        ("" + Character.toChars(codepoint).mkString, nCols)
      }
    }
  }

  /**
   * The width of the character in terms of how many "places" it uses up
   * relative to a regular monospaced font character. This is for trying to get
   * east asian and other double-wide characters to line up properly in columns.
   */
  private def charNColumns(codepoint: Int): Int = {
    val charWidth = UCharacter.getIntPropertyValue(codepoint, UProperty.EAST_ASIAN_WIDTH)
    charWidth match {
      //
      // see http://unicode.org/reports/tr11/tr11-8.html
      //
      case UCharacter.EastAsianWidth.AMBIGUOUS => 1
      case UCharacter.EastAsianWidth.FULLWIDTH => 2
      case UCharacter.EastAsianWidth.HALFWIDTH => 1
      case UCharacter.EastAsianWidth.NARROW => 1
      case UCharacter.EastAsianWidth.WIDE => 2
      case UCharacter.EastAsianWidth.NEUTRAL => 1
    }
  }

  private def getReportingDecoder(
    optEncodingName: Option[String]
  ): Option[JavaCharsetDecoder] = {
    val cs = optEncodingName.map { JavaCharset.forName(_) }
    lazy val decoder = cs.map { _.newDecoder() }
    decoder
  }

  /**
   * Decoder must be setup for REPORT (default) on decode error.
   * We will manually handle the replacing
   */
  private def convertToCharRepr(
    startingBytePos0b: Long,
    endingBytePos0b: Long,
    byteBuffer: ByteBuffer,
    decoder: Option[JavaCharsetDecoder]
  ): (String, Int, Int) = {

    Assert.invariant(decoder.map { d => Misc.isAsciiBased(d.charset()) }.getOrElse(true))
    decoder match {
      case Some(dec) => {
        val bb = ByteBuffer.allocate(6)
        var cb = CharBuffer.allocate(1)
        var cr = CoderResult.OVERFLOW
        var nConsumedBytes = 0
        var remapped = ""
        var nCols = 0
        val INVALID_CODEPOINT = -1
        val lastAvailableBytePos0b = scala.math.min(
          endingBytePos0b,
          startingBytePos0b + 5
        ) // widest possible char representation is 6 bytes.
        val nBytes = (lastAvailableBytePos0b - startingBytePos0b).toInt + 1
        Assert.invariant(nBytes > 0) // have to have at least 1 byte left
        (0 until nBytes).foreach { i =>
          val thePos = (startingBytePos0b + i).toInt
          Assert.invariant(thePos >= 0)
          val theByte =
            try {
              byteBuffer.get(thePos)
            } catch {
              case e: IndexOutOfBoundsException => 0.toByte
            }
          bb.put(theByte)
        }
        bb.flip()

        Assert.invariant(bb.remaining > 0)
        while (cr.isOverflow && nConsumedBytes == 0 && cb.capacity <= bb.capacity) {
          // An overflow means we were able to start to decode at least 1 sequence of characters, but there was either insufficient
          // space in the output buffer to store said decoded char or there were left over bytes after parsing. If it is
          // the former, we can proceed and we'll get the left over bytes on the next run, if it was the latter
          // (as can be the case with decoding a 4 byte character sequence), we will call decode with a larger buffer
          // until we consume something or the output buffer is at same capacity as input buffer
          cr = dec.decode(bb, cb, true)
          nConsumedBytes = bb.position()
          if (cr.isOverflow && nConsumedBytes == 0) {
            cb = CharBuffer.allocate(cb.capacity + 1)
          }
        }

        // Once we leave the loop, we will either have consumed bytes to process (with a variety of left over bytes that we
        // don't care about) or malformed/unmappable results with no consumed bytes that we do care about so we will do a
        // manual replace and set consumed bytes ourselves. We should not do an automatic replace as it creates ambiguity
        // with the malformed/unmapped/consumed bytes with our current implementation of handling a decoded character at a time.

        // We should never have an underflow condition with no bytes consumed. As that would indicate it needs more input than
        // we've provided. Even if we only provide 1 byte of a 4 byte sequence, it will return a malformed[1]
        Assert.invariant(!(cr.isUnderflow && nConsumedBytes == 0))

        if ((cr.isMalformed || cr.isUnmappable) && nConsumedBytes == 0) {
          // do manual replacement
          remapped = dec.replacement()
          // grab malformed/unmappable byte so we can keep decoding
          nConsumedBytes = cr.length
          nCols = charNColumns(remapped(0))
        } else {
          // An overflow, at this point, means that we got our one character, but there were more bytes available that could
          // be decoded. We're not interested in those right now.
          //
          // An underflow means that we got our one character, but the bytes were exactly used up
          // by constructing that one character.
          //
          // Either way, we got our one character
          Assert.invariant(nConsumedBytes > 0)
          Assert.invariant(cb.hasArray)
          val allChars = cb.array

          val uCodePoint: Int =
            if (allChars.length > 1) {
              if (UCharacter.isSurrogatePair(allChars(0), allChars(1))) {
                UCharacter.getCodePoint(allChars(0), allChars(1))
              } else {
                INVALID_CODEPOINT
              }
            } else allChars(0).toInt

          val (r: String, n: Int) =
            if (allChars.length > 1) {
              if (uCodePoint == INVALID_CODEPOINT) {
                allChars.map(c => homogenizeChars(c)).foldLeft(("", 0)) {
                  (accForRemappedAndNcols, tupResultRemappedAndNcols) =>
                    (
                      accForRemappedAndNcols._1 + tupResultRemappedAndNcols._1, // concat remapped value for each char
                      accForRemappedAndNcols._2 + tupResultRemappedAndNcols._2
                    ) // add width value for each char
                }
              } else {
                homogenizeChars(uCodePoint)
              }
            } else {
              homogenizeChars(Misc.remapControlOrLineEndingToVisibleGlyphs(allChars(0)))
            }
          remapped = r
          nCols = n
        }
        (remapped, nConsumedBytes, nCols)
      }
      case None => {
        // no encoding, so use the general one based on windows-1252 where
        // every byte corresponds to a character with a glyph.
        val byteValue =
          try {
            byteBuffer.get(startingBytePos0b.toInt)
          } catch {
            case e: IndexOutOfBoundsException => 0.toByte
          }
        // decoding using a decoder might produce C0 or C1 control characters or
        // other whitespace characters. But we want visible glyphs no matter what for those.
        //
        // FIXME: This will be really broken for EBCDIC-based encodings. Pass the encoding
        // so that the glyph routine can be ascii/ebcdic sensitive.
        val remapped = Misc.remapOneByteToVisibleGlyph(byteValue)
        (remapped.toString, 1, 1)
      }
    }
  }

  /**
   * If displaying ONLY text, then we just display one long line
   * and replace any whitespace or non-glyph characters with glyph characters.
   */
  def dumpTextLine(
    maxLineLen: Int,
    startByteAddress0b: Long,
    lengthInBytesRequested: Int,
    byteBuffer: ByteBuffer,
    optEncodingName: Option[String] = None,
    indicatorInfoInBytes: Option[(Long, Int)] = None
  ): Seq[String] = {
    Assert.usage(startByteAddress0b >= 0)
    Assert.usage(lengthInBytesRequested >= 0)
    val lengthInBytes = math.min(lengthInBytesRequested, maxLineLen)

    val indicatorLine = indicatorInfoInBytes.map {
      case (indicatorStartByteAddress0b, indicatorLengthInBytes) => {
        Assert.usage(indicatorStartByteAddress0b >= 0)
        Assert.usage(indicatorLengthInBytes >= 0)
        val numLeadingSpaces = (indicatorStartByteAddress0b - startByteAddress0b).toInt
        Assert.invariant(numLeadingSpaces >= 0)
        val leadingSpaces = " " * numLeadingSpaces
        val maxIndicatorLength = math.min(maxLineLen - numLeadingSpaces, lengthInBytes)
        val realIndicatorLength = math.min(indicatorLengthInBytes, maxIndicatorLength)
        val maxLineLength = math.min(maxLineLen, lengthInBytes)
        val indicatorEndLength = realIndicatorLength + numLeadingSpaces
        val indicator = realIndicatorLength match {
          case 0 => "│"
          case 1 => "║"
          case n => {
            Assert.invariant(n >= 2)
            val nDashes = (n - 2).toInt
            val closeOrOpenEnd =
              if (lengthInBytesRequested <= maxLineLength) {
                // the number of characters displayed will be shorter than
                // the max width
                if (indicatorEndLength <= lengthInBytesRequested)
                  "┤" // indicator ends at or before the data
                else "═" // indicator indicates past the end. This shouldn't really happen.
              } else {
                // the number of characters displayed will meet the maximum
                if (indicatorEndLength < maxLineLength) "┤"
                else "═"
              }
            val picture = "├" + ("─" * nDashes) + closeOrOpenEnd
            picture
          }
        }
        leadingSpaces + indicator
      }
    }

    val endByteAddress0b = math.max(startByteAddress0b + lengthInBytes - 1, 0)
    val decoder = getReportingDecoder(optEncodingName)
    var i = startByteAddress0b
    val sb = new StringBuilder
    while (i <= endByteAddress0b) {
      val (cR, nBytesConsumed, _) =
        convertToCharRepr(i - startByteAddress0b, endByteAddress0b, byteBuffer, decoder)
      sb ++= cR
      i += nBytesConsumed
    }
    val s = sb.mkString
    val lines: Seq[String] = indicatorLine.toSeq :+ s
    lines
  }

  /**
   * gets header line, width of a character, and encoding name to actually use
   */
  private def getTextParameters(
    optEncodingName: Option[String]
  ): (String, Int, Option[String]) = {
    //
    // this def and subsequent match-case are done this way to silence
    // a scala compiler warning
    //
    def unicode = ("0~1~2~3~4~5~6~7~8~9~a~b~c~d~e~f~", 2, optEncodingName)
    optEncodingName.map { _.toLowerCase } match {
      case Some("utf-8") => unicode
      case Some("utf-16be") | Some("utf-16le") => unicode
      case Some("utf-32be") | Some("utf-32le") => unicode
      case None | Some("ascii") | Some("us-ascii") | Some("iso-8859-1") =>
        ("0123456789abcdef", 1, optEncodingName)
      case Some("utf-32") | Some("utf-16") => unicode
      case Some(x) => {
        // Don't know how to dump this text specific to this encoding
        // so proceed but without encoding information
        ("0123456789abcdef", 1, None)
      }
    }
  }

  /**
   * Create a right-to-left presentation of the kind used for LSB-first
   * little-endian data
   */
  private[io] def dumpHexAndTextBytesLSBFirst(
    startByteAddress0b: Long,
    lengthInBytes: Int,
    byteBuffer: ByteBuffer,
    includeHeadingLine: Boolean = true,
    optEncodingName: Option[String] = None
  ): Seq[String] = {
    val ltrDump = dumpHexAndTextBytes(
      startByteAddress0b,
      lengthInBytes,
      byteBuffer,
      includeHeadingLine,
      optEncodingName,
      None
    )
    val ltrLines =
      ltrDump.filterNot { _.length() == 0 }
    val wholeLineRegex = """([0-9a-fA-F]{8})(:?\s+)([0-9a-fA-F ]+[0-9a-fA-F])(\s+)(.*)""".r
    val rtlLines = ltrLines.map { ltrLine =>
      ltrLine match {
        case wholeLineRegex(addr, sep1, hexlBytes, sep2, asciiText) => {
          val hexlNibblesSwitch = hexlBytes
            .split(" ")
            .map { hexlGroup =>
              hexlGroup
                .sliding(2, 2) // grab each incorrectly reversed (nibbles are switched) byte
                .map(_.reverse) // reverse the byte
                .mkString // and convert back to string
            }
            .mkString(" ") // convert back to string
          asciiText.reverse + sep2 + hexlNibblesSwitch.reverse + sep1.reverse + addr
        }
        case x => x
      }
    }
    val rtlDump =
      rtlLines
    rtlDump
  }

}
