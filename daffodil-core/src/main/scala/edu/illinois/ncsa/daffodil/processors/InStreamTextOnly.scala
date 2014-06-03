package edu.illinois.ncsa.daffodil.processors

import java.nio.ByteOrder
import java.nio.charset.Charset
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.api.DFDL
import java.io.File
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.Path
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.input.Reader
import java.io.FileReader
import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.PagedSeqReader
import java.nio.charset.CodingErrorAction
import java.nio.channels.ReadableByteChannel
import java.io.InputStream
import java.io.FileInputStream
import java.io.InputStreamReader
import scala.util.parsing.input.OffsetPosition
import scala.util.parsing.input.Position
import java.nio.CharBuffer
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.dsom.SchemaComponent
import edu.illinois.ncsa.daffodil.io.FastAsciiToUnicodeConverter
import scala.util.parsing.input.Position
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._

/**
 * Shares constants so that the pure-functional scala I/O Reader[Char]
 * idiom is creating the smallest possible structures as it is advanced
 * through the characters.
 * <p>
 * bitLimit is the maximum length. It is equal to the first
 * position past the end. ( i.e., zero based indexing )
 */
case class FixedWidthTextInfoCBuf(context: SchemaComponent, bitsPerChar: Int, mandatoryAlignment: Int, charset: Charset,
  bitLimit: Long, cBuf: CharBuffer, bBuf: ByteBuffer) {

  Assert.usage(bitLimit == -1 || bitLimit % bitsPerChar == 0)
  // Only true the first time we create one of these. During a parse, the bit limit
  // can get tightened down, and in that case the cBuf.limit will be larger than 
  // what is allowed.
  // Assert.invariant(bitLimit == -1 || (bitLimit / bitsPerChar) == cBuf.limit)
}

case class DummyPosition(cPos0b: Long) extends Position {
  def line = 0
  def column = cPos0b.toInt
  def lineContents = "This is just a dummy position object."
}

/**
 * Blends together InStream and DFDLCharReader functionality into the same object.
 * Depends on the charset encoding being fixed width to be able to compute the
 * number of bytes of input as a function of the number of characters. Depends on
 * encodingErrorPolicy='replace' because data was bulk converted into characters
 * in a CharBuffer.
 * <p>
 * TODO: These two types InStream and DFDLCharReader, their distinction should be
 * removed. They should be one trait/class.
 */
case class InStreamFixedWidthTextOnly(
  var cPos0b: Int,
  var info: FixedWidthTextInfoCBuf)
  extends DFDLCharReader with InStream
  with Logging
  with WithParseErrorThrowing {

  override def assignFrom(other: InStream) {
    val oth = other.asInstanceOf[InStreamFixedWidthTextOnly]
    cPos0b = oth.cPos0b
    info = oth.info
  }
  override def duplicate() = copy()
  override def source: java.lang.CharSequence = info.cBuf

  override def offset: Int = cPos0b

  def context = info.context
  override def charLimit0b = info.bitLimit / info.bitsPerChar
  override def bitPos0b = cPos0b * info.bitsPerChar

  lazy val first = {
    Assert.invariant(cPos0b <= info.cBuf.limit)
    if (atEnd) -1.toChar
    else info.cBuf.get(cPos0b) // cBuf are zero-based indexing.
  }

  lazy val rest = {
    val res =
      if (atEnd) this
      else this.copy(cPos0b = cPos0b + 1)
    res
  }
  def atEnd = cPos0b == (info.bitLimit / info.bitsPerChar)
  override def pos: Position = DummyPosition(cPos0b)
  def atCharPos(cp0b: Int) = {
    // We have to call next all these times so that
    // if we backtrack, we will still encounter these same
    // characters, and the cBuf position is only being advanced 
    // the right number of times.
    Assert.usage(cp0b >= cPos0b)
    val delta = cp0b - cPos0b
    var next = this
    1 to delta.toInt foreach { i =>
      next = next.rest
    }
    next
  }
  def atBitPos(bp0b: Long) = {
    // let's make sure the alignment isn't being
    // thrown off.
    Assert.usage(bp0b % info.bitsPerChar == 0)
    val nChars = bp0b / info.bitsPerChar
    atCharPos(nChars.toInt)
  }

  def getCharsetName = info.charset.name
  lazy val characterPos = cPos0b // zero based

  def charset = Assert.usageError("not to be used in test reader")
  override def bitLimit0b = info.bitLimit

  override def charPos0b: Long = cPos0b
  override val reader: Maybe[DFDLCharReader] = One(this)

  def withPos(newBitPos0b: Long, newCharPos0b: Long, reader: Maybe[DFDLCharReader]): InStream = {
    Assert.usage(newBitPos0b >= 0)
    Assert.invariant(newCharPos0b == -1 || (newBitPos0b / info.bitsPerChar) == newCharPos0b)
    // We don't use the reader being provided because this object IS the reader.
    // However, the existing code does think it has computed a new reader and passes
    // it to us. So we might as well verify that it is doing that consistently.
    if (reader.isDefined) {
      if (reader.get.isInstanceOf[InStreamFixedWidthTextOnly]) {
        Assert.invariant(reader.get.asInstanceOf[InStreamFixedWidthTextOnly].info == info)
      } else Assert.invariantFailed("not same inStream/reader")
    }

    atBitPos(newBitPos0b)
  }

  def withPos(newBitPos: Long, newCharPos: Long): InStream = {
    withPos(newBitPos, newCharPos, Nope)
  }

  def withEndBitLimit(newBitLimit: Long): InStream = {
    Assert.usage(newBitLimit % 8 == 0)
    val newCharLimit = math.min((newBitLimit / info.bitsPerChar), info.cBuf.capacity())
    val newByteLimit = math.min((newBitLimit / 8), info.bBuf.capacity())
    // Here we are setting the limit smaller on the cBuf.
    // This side effect has to be undone symmetrically when
    // the parser leaves the context which has the smaller limit.
    info.cBuf.limit(newCharLimit.toInt)
    info.bBuf.limit(newByteLimit.toInt)
    val newInfo = info.copy(bitLimit = newBitLimit)
    this.copy(info = newInfo)
  }

  def getByte(bitPos0b: Long, ignore: ByteOrder, bitOrder: BitOrder): Byte = {
    Assert.usage(bitPos0b % 8 == 0)
    val bytePos = (bitPos0b >> 3).toInt
    info.bBuf.get(bytePos)
  }
  def getRawByte(bitPos: Long, order: ByteOrder, bitOrder: BitOrder): Byte = getByte(bitPos, order, bitOrder)

  def getBytes(bitPos: Long, numBytes: Long): Array[Byte] = {
    val nbytes = numBytes.toInt
    val bytes = new Array[Byte](nbytes)
    val bytePos = (bitPos >> 3).toInt
    0 to (nbytes - 1) foreach { i =>
      val iBitPos = bitPos + (i << 3)
      val dontCareByteOrder = ByteOrder.BIG_ENDIAN
      val dontCareBitOrder = BitOrder.MostSignificantBitFirst
      // getting one aligned byte, so we don't care what the byteOrder is.
      bytes.update(i, getByte(iBitPos, dontCareByteOrder, dontCareBitOrder))
    }
    bytes
  }

  def getLong(bitPos: Long, bitCount: Long, order: ByteOrder, bitOrder: BitOrder): Long =
    Assert.usageError("should not get binary long values from all text input source")

  def getBigInt(bitPos: Long, bitCount: Long, order: ByteOrder, bitOrder: BitOrder): BigInt =
    Assert.usageError("should not get binary long values from all text input source")

  def getCharReader(charset: Charset, bitPos: Long): DFDLCharReader = {
    Assert.usage(charset == info.charset)
    this.atBitPos(bitPos)
  }

  def lengthInBytes(): Long = {
    if (info.bitLimit != -1) info.bitLimit / 8
    else -1
  }

  def withBitOrder(bitOrder: BitOrder) = this
}

/**
 * Contructs a Scala Reader[Char] for a file or
 * input stream. Optimizes the ascii case.
 */
object TextOnlyReplaceOnErrorReaderFactory {

  private val ascii = Charset.forName("ascii")

  // make new ones of these because decoders are stateful.
  private def getDecoder(charset: Charset) = {
    val decoder = charset.newDecoder()
    decoder.onMalformedInput(CodingErrorAction.REPLACE)
    decoder.onUnmappableCharacter(CodingErrorAction.REPLACE)
    decoder
  }

  /**
   * More desirable to make from a file because we can do
   * different things depending on the size.
   */
  //  def getReader(file: File, charset: Charset): Reader[Char] = {
  //    val filesz: Long = file.length()
  //    if (filesz > DaffodilTunableParameters.inputFileMemoryMapHighThreshold) {
  //      // need to stream it. Can't pull it in all at once
  //      val fis = new FileInputStream(file)
  //      val rdr = getReader(fis, charset)
  //      rdr
  //    } else {
  //      val cb = getCharBuffer(file, charset)
  //      val rdr = new CharSequenceReader(cb)
  //      rdr
  //    }
  //  }

  def getCharBuffer(bb: ByteBuffer, charset: Charset): CharBuffer = {
    val cb =
      if (charset == ascii) {
        FastAsciiToUnicodeConverter.convert(bb)
      } else {
        getDecoder(charset).decode(bb)
      }
    cb
  }

  def getCharBuffer(file: File, charset: Charset): (CharBuffer, ByteBuffer) = {
    val path = file.toPath
    val filesz: Long = file.length()
    val fc = FileChannel.open(path)
    val bb: ByteBuffer =
      if (filesz > DaffodilTunableParameters.inputFileMemoryMapLowThreshold) {
        // mmap it. Big enough to be worth it.
        fc.map(FileChannel.MapMode.READ_ONLY, 0, filesz)
      } else {
        // read it. Too small for mmap to pay off.
        val bb = ByteBuffer.allocate(filesz.toInt)
        val len = fc.read(bb)
        bb.flip()
        bb
      }
    val cb = getCharBuffer(bb, charset)
    fc.close()
    (cb, bb)
  }

}

