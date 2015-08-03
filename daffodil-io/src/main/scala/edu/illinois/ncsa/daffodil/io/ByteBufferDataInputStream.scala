package edu.illinois.ncsa.daffodil.io

import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BinaryFloatRep
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import java.nio.charset.CharsetDecoder
import java.nio.charset.StandardCharsets
import java.nio.charset.CodingErrorAction
import org.apache.commons.io.IOUtils
import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.nio.charset.CharacterCodingException
import java.nio.CharBuffer
import java.nio.channels.FileChannel
import java.nio.channels.Channels
import java.util.ArrayList
import java.nio.LongBuffer
import edu.illinois.ncsa.daffodil.util.Bits
import passera.unsigned.ULong
import java.io.ByteArrayInputStream
import java.io.InputStream
import java.nio.charset.CoderResult
import edu.illinois.ncsa.daffodil.Implicits
import scala.collection.mutable
import edu.illinois.ncsa.daffodil.equality._

/**
 * Factory for creating this type of DataInputStream
 *
 * Examines channels and input streams to see if they are associated
 * to files. If so it memory maps the file.
 */
object ByteBufferDataInputStream {

  /**
   * This is the only method that actually constructs the object.
   * The other methods just arrange to call this one.
   *
   * The initialBitPos0b is mostly there for testing purposes - to create small unit tests that
   * start with non-byte alignment and such.
   */
  def apply(byteBuffer: ByteBuffer, initialBitPos0b: Long): DataInputStream =
    new ByteBufferDataInputStream(byteBuffer, initialBitPos0b)

  /**
   * Mostly this is for unit testing
   */
  def apply(byteArray: Array[Byte], initialBitPos0b: Long = 0): DataInputStream = apply(ByteBuffer.wrap(byteArray), initialBitPos0b)

  /**
   * File becomes a direct mapped byte buffer.
   */
  def apply(file: java.io.File, initialBitPos0b: Long): DataInputStream = {
    Assert.usage(file.length() < Int.MaxValue)
    val path = file.toPath()
    val channel = FileChannel.open(path)
    apply(channel, initialBitPos0b)
  }

  /**
   * If it is a FileInputStream, get its channel so we can mmap it.
   * Otherwise copy into a byte array.
   */
  def apply(in: InputStream, initialBitPos0b: Long): DataInputStream = {
    in match {
      case fis: java.io.FileInputStream => {
        val fc = fis.getChannel()
        apply(fc, initialBitPos0b)
      }
      case _ => {
        // copy the contents of the stream into an array of bytes
        val bos = new ByteArrayOutputStream
        IOUtils.copy(in, bos)
        bos.flush()
        bos.close()
        in.close()
        val ba = bos.toByteArray
        apply(ba, initialBitPos0b)
      }
    }
  }

  /**
   * If it's a file channel, mmap it. Otherwise treat like an InputStream
   */
  def apply(channel: java.nio.channels.ReadableByteChannel, initialBitPos0b: Long): DataInputStream = {
    channel match {
      case fc: FileChannel => {
        val bbuf = fc.map(FileChannel.MapMode.READ_ONLY, 0L, fc.size())
        apply(bbuf, initialBitPos0b)
      }
      case _ => apply(Channels.newInputStream(channel), initialBitPos0b)
    }
  }

  def fromByteChannel(
    in: java.nio.channels.ReadableByteChannel,
    bitStartPos0b: Long,
    numBitsLimit: Long, // a count, not a position
    bitOrder: BitOrder) = {
    val dis = ByteBufferDataInputStream(in, bitStartPos0b)
    if (numBitsLimit > 0) dis.setBitLimit0b(One(bitStartPos0b + numBitsLimit))
    dis.setBitOrder(bitOrder)
    dis
  }

}

/**
 * The state that should be merged into PState eventually.
 *
 * As of this writing, there is a bunch of copying of DataInputStream objects
 * to provide compatibility with the older functional-style input layer which
 * copied the PState objects and InStream objects endlessly.
 *
 * The compatibility code still does this copying.
 *
 * So, to make the copying cheaper, we recognize that all the copies of
 * a specific DataInputStream object are really all part of the backtracking
 * business for the same thread.
 *
 * Some state is only needed per thread, so we don't need to copy it
 * over and over for the compatibility layer. We can just share it via
 * thread-local data.
 *
 * This is very unlike the state that must be saved/restored by the mark/reset discipline
 */
private class TLState {
  val markStack = mutable.Stack[MarkState]()
  val markPool = mutable.Stack[MarkState]()
  var numOutstandingMarks = 0
  val skipCharBuf = CharBuffer.allocate(BBSLimits.maximumSimpleElementSizeInCharacters.toInt)
  val regexMatchBuffer = CharBuffer.allocate(BBSLimits.maximumRegexMatchLengthInCharacters.toInt)
  val lengthDeterminationBuffer = CharBuffer.allocate(BBSLimits.maximumRegexMatchLengthInCharacters.toInt)
}

protected trait TLStateAccessMixin {
  private def tlState = TLState.get() // def here, just in case this gets mixed into a singleton someplace.

  protected final def markStack = tlState.markStack

  protected final def numOutstandingMarks =
    tlState.numOutstandingMarks

  protected final def incrNumOutstandingMarks {
    tlState.numOutstandingMarks += 1
  }

  protected final def decrNumOutstandingMarks {
    tlState.numOutstandingMarks -= 1
  }

  protected final def markPool = tlState.markPool
  protected final def skipCharBuf = tlState.skipCharBuf
  protected final def regexMatchBuffer = tlState.regexMatchBuffer
  protected final def lengthDeterminationBuffer = tlState.lengthDeterminationBuffer
}

private object TLState extends ThreadLocal[TLState] {
  final private def alloc = new TLState
  override protected def initialValue() = alloc
}

/**
 * The state that must be saved and restored by mark/reset calls
 */
final class MarkState(initialBitPos0b: Long,
  val defaultCodingErrorAction: CodingErrorAction,
  var dis: ByteBufferDataInputStream)
  extends DataInputStream.Mark with DataStreamCommonState {

  override def equals(other: Any) = other match {
    case ar: AnyRef => this eq ar // only if the same object
    case _ => false
  }

  var savedBytePosition0b: Int = 0
  var savedByteLimit0b: Int = 0
  /**
   * We store the bit position in two parts. The whole-byte part is the byte buffer's position().
   * The fraction-of-a-byte part is this bit offset. If one needs the bitPos0b, that's a method that
   * recombines the position() with this offset.
   */
  var bitOffset0b: Int = (initialBitPos0b % 8).toInt
  var savedByteOrder: java.nio.ByteOrder = dis.data.order()

  var maybeBitLimitOffset0b: Maybe[Long] = One(0)

  var decoder: CharsetDecoder = {
    val dec = StandardCharsets.UTF_8.newDecoder()
    dec.onMalformedInput(defaultCodingErrorAction)
    dec.onUnmappableCharacter(defaultCodingErrorAction)
    dec
  }
  var codingErrorAction: CodingErrorAction = defaultCodingErrorAction

  var adaptedRegexMatchBufferLimit: Int = 0

  val charIterator = new BBDISCharIterator(this, dis)
  // any members added here must be added to assignFrom below.

  def assignFrom(other: MarkState): Unit = {
    super.assignFrom(other)
    this.dis = other.dis
    this.savedBytePosition0b = other.savedBytePosition0b
    this.savedByteLimit0b = other.savedByteLimit0b
    this.bitOffset0b = other.bitOffset0b
    this.savedByteOrder = other.savedByteOrder
    this.maybeBitLimitOffset0b = other.maybeBitLimitOffset0b
    this.decoder = other.decoder
    this.codingErrorAction = other.codingErrorAction
    this.adaptedRegexMatchBufferLimit = other.adaptedRegexMatchBufferLimit
    this.charIterator.assignFrom(other.charIterator)
  }
}

/**
 * These magic numbers aren't used really except for unit test situations.
 * The DataProcessor calls setLimits(...) before parsing, which replaces this.
 * However, as this daffodil-io module is more primitive than the
 * daffodil runtime, this allows us to unit test things in isolation.
 */
private object BBSLimits extends DataStreamCommon.Limits {
  def maximumSimpleElementSizeInBytes: Long = 1024 * 1024
  def maximumSimpleElementSizeInCharacters: Long = 1024 * 1024
  def maximumForwardSpeculationLengthInBytes: Long = 1024 * 1024
  def maximumRegexMatchLengthInCharacters: Long = 1024 * 1024
  def defaultInitialRegexMatchLimitInChars: Long = maximumRegexMatchLengthInCharacters //32
}
/**
 * Simple realization of the DataInputStream API
 *
 * Underlying representation is just a ByteBuffer containing all input data.
 *
 * In many cases this can be a direct byte buffer, and so doesn't occupy storage
 * in the java heap.
 *
 * At top level (when creating one for all the data), the supplied byte buffer must have
 * position 0 and its limit should be equal to its capacity.
 *
 * The backward compatibility layers, however, until removed, make copies of
 * these objects, and those may have non-zero positions. E.g., see the def makeACopy.
 */
final class ByteBufferDataInputStream private (var data: ByteBuffer, initialBitPos0b: Long)
  extends DataInputStream with TLStateAccessMixin with DataStreamCommonImplMixin {

  Assert.usage(initialBitPos0b >= 0)
  val initialBytePos0b = initialBitPos0b / 8

  Assert.usage(initialBytePos0b < data.capacity() ||
    (initialBytePos0b == data.capacity() && (initialBitPos0b % 8 == 0))) // when equal to capacity, bits of fragment partial byte can't spill over past the capacity.

  data.position((initialBitPos0b / 8).toInt) // set data position based on the initialBitPos0b

  private lazy val myFirstThread = Thread.currentThread()

  private lazy val converter_BE_MSBFirst = new Converter_BE_MSBFirst
  private lazy val converter_LE_MSBFirst = new Converter_LE_MSBFirst
  private lazy val converter_LE_LSBFirst = new Converter_LE_LSBFirst

  protected final def threadCheck() {
    Assert.invariant(Thread.currentThread eq myFirstThread)
  }

  override def toString = {
    val bp0b = bitPos0b
    val bl0b = bitLimit0b
    val bl0b1 = bl0b.map { _.toString }.getOrElse("none")
    val str = "DataInputStream(bitPos=" + bp0b +
      ", bitLimit=" + bl0b1 + ")"
    str
  }

  @deprecated("2015-06-22", "Remove when InStream is replaced fully by DataInputStream - which uses mark/reset to backtrack, not copying")
  def makeACopy() = {
    threadCheck()
    val newData = data.duplicate()
    newData.order(data.order) // must copy the byte order explicitly. It is not inherited by copies.
    val cl = new ByteBufferDataInputStream(newData, bitPos0b)
    cl.st.assignFrom(st)
    cl
  }

  @deprecated("2015-06-22", "Remove when InStream is replaced fully by DataInputStream - which uses mark/reset to backtrack, not copying")
  def assignFrom(other: DataInputStream) {
    threadCheck()
    Assert.usage(other != null)
    other match {
      case other: ByteBufferDataInputStream => {
        this.data = other.data // assign this so we get the position/limit/etc from the other.
        this.st.assignFrom(other.st)
      }
      case x => Assert.invariantFailed("can only be a BBDIS. Was " + x)
    }
  }

  val st = new MarkState(initialBitPos0b, defaultCodingErrorAction, this)

  override final def cst = st

  private def bytePos0b_ = data.position

  def bitPos0b: Long = (bytePos0b_ << 3) + st.bitOffset0b

  def setBitPos0b(newBitPos0b: Long) {
    // threadCheck()
    Assert.invariant(newBitPos0b < (Int.MaxValue * 8L))
    Assert.invariant(newBitPos0b >= 0)
    val newBitOffset0b = (newBitPos0b & 0x7).toInt
    val newBytePos0b = (newBitPos0b >> 3).toInt
    val bitLim0b = bitLimit0b.get
    if (newBitPos0b <= bitLim0b) {
      data.position(newBytePos0b)
      st.bitOffset0b = newBitOffset0b
    } else {
      // in this case, we're asking to set the bit pos past the limit
      // clamp it to the limit
      setBitPos0b(bitLim0b)
    }
  }

  def bitLimit0b: Maybe[Long] = { // we always have a bitLimit in this implementation
    // threadCheck()
    Assert.invariant(st.maybeBitLimitOffset0b.isDefined)
    st.maybeBitLimitOffset0b.map { offset =>
      val lim = (data.limit << 3) + offset
      lim
    }
  }

  def setBitLimit0b(newBitLimit0b: Maybe[Long]): Boolean = {
    // threadCheck()
    Assert.invariant(newBitLimit0b.isDefined)
    val newBitLimit = newBitLimit0b.get
    if (newBitLimit > Int.MaxValue) return false
    Assert.usage(newBitLimit >= bitPos0b)
    if (st.maybeBitLimitOffset0b.isDefined) {
      if (newBitLimit > this.bitLimit0b.get) return false // new limit is beyond our available length.
    }
    resetBitLimit0b(newBitLimit0b)
    true
  }

  /*
   * Package private for unit test purposes. Otherwise would be protected.
   */
  final private[io] override def resetBitLimit0b(savedBitLimit0b: Maybe[Long]): Unit = {
    // threadCheck()
    Assert.invariant(savedBitLimit0b.isDefined)
    val newBitLimit = savedBitLimit0b.get
    val bitLim = newBitLimit.toInt
    val newBitLimitOffset0b = bitLim & 0x7
    val newByteLimit = bitLim >> 3
    data.limit(newByteLimit)
    st.maybeBitLimitOffset0b = One(newBitLimitOffset0b)
  }

  private def javaByteOrder = {
    // threadCheck()
    data.order()
  }

  def setByteOrder(byteOrder: ByteOrder): Unit = {
    // threadCheck()
    byteOrder match {
      case ByteOrder.BigEndian => data.order(java.nio.ByteOrder.BIG_ENDIAN)
      case ByteOrder.LittleEndian => data.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    }
    st.savedByteOrder = data.order()
  }

  def byteOrder: ByteOrder = {
    data.order match {
      case java.nio.ByteOrder.BIG_ENDIAN => ByteOrder.BigEndian
      case java.nio.ByteOrder.LITTLE_ENDIAN => ByteOrder.LittleEndian
    }
  }

  def getDecoder = st.decoder

  def setDecoder(decoder: CharsetDecoder): Unit = {
    // threadCheck()
    if (st.decoder == decoder) return
    st.decoder = decoder
    st.decoder.onMalformedInput(st.codingErrorAction)
    st.decoder.onUnmappableCharacter(st.codingErrorAction)
    val cs = decoder.charset()
    val (mCharWidthInBits, mandatoryAlignInBits) = {
      if (cs == StandardCharsets.UTF_16 || cs == StandardCharsets.UTF_16BE || cs == StandardCharsets.UTF_16LE)
        if (st.maybeUTF16Width.isDefined && st.maybeUTF16Width.get == UTF16Width.Fixed) (One(16), 8)
        else (Nope, 8)
      else {
        cs match {
          case decoderWithBits: NonByteSizeCharsetEncoderDecoder =>
            (One(decoderWithBits.bitWidthOfACodeUnit), 1)
          case _ => {
            val encoder = cs.newEncoder()
            val maxBytes = encoder.maxBytesPerChar()
            if (maxBytes == encoder.averageBytesPerChar())
              (One((maxBytes * 8).toInt), 8)
            else (Nope, 8)
          }
        }
      }
    }
    st.maybeCharWidthInBits = mCharWidthInBits
    st.encodingMandatoryAlignmentInBits = mandatoryAlignInBits
  }

  private def defaultCodingErrorAction = CodingErrorAction.REPLACE

  def setEncodingErrorPolicy(eep: EncodingErrorPolicy): Unit = {
    // threadCheck()
    st.codingErrorAction = eep match {
      case EncodingErrorPolicy.Replace => CodingErrorAction.REPLACE
      case EncodingErrorPolicy.Error => CodingErrorAction.REPORT
    }
    st.decoder.onMalformedInput(st.codingErrorAction)
    st.decoder.onUnmappableCharacter(st.codingErrorAction)
    ()
  }

  def fillByteBuffer(bb: java.nio.ByteBuffer): Maybe[Int] = {
    // threadCheck()
    val res =
      if (isAligned(8))
        fillByteBufferByteAligned(bb) // bit order doesn't matter.
      else
        fillByteBufferUnaligned(bb) // bit order sensitive
    Assert.invariant(bitPos0b <= bitLimit0b.get)
    res
  }

  private def fillByteBufferByteAligned(bb: java.nio.ByteBuffer): Maybe[Int] = {
    // threadCheck()
    Assert.usage(isAligned(8))
    var i = 0
    val dataRemainingBefore = data.remaining()
    val delta = math.min(bb.remaining(), dataRemainingBefore)
    while (i < delta) {
      i += 1
      bb.put(data.get()) // TODO: use a bulk operation here, not byte at a time. Worst case do it in Long units so 1/8 as many iterations.
    }
    val dataRemainingAfter = data.remaining()
    if (st.maybeBitLimitOffset0b.isDefined) {
      val bitLimOffset = st.maybeBitLimitOffset0b.get
      if (bitLimOffset > 0) {
        // the data is restricted to end in middle of a byte. So we may need to transfer
        // one more byte if we are up against this limit.
        if (bb.remaining > 0 && dataRemainingAfter == 0) {
          // there is room for one more byte at least, but we stopped because
          // we ran out of whole bytes of input data. So we need to also transfer
          // a final byte containing the bit fragment.
          Assert.invariant(data.capacity() > data.limit()) // the final fragment byte must live somewhere
          val savedDataLimit = data.limit()
          data.limit(savedDataLimit + 1)
          val finalByte = data.get(savedDataLimit) // absolute get.
          data.limit(savedDataLimit)
          bb.put(finalByte)
          i += 1
          // however, we consume only the partial byte's available bits, 
          // not the entire final byte.
          setBitPos0b(bitPos0b + bitLimOffset)
        }
      }
    }
    if (i == 0) Nope else One(i)
  }

  private def fillByteBufferUnaligned(bb: java.nio.ByteBuffer): Maybe[Int] = {
    // threadCheck()
    Assert.usage(!isAligned(8))
    val initialBitPos0b = bitPos0b
    val numFragmentBits = 8 - st.bitOffset0b
    Assert.invariant(numFragmentBits > 0)
    val src = data
    val tgt = bb
    if (tgt.remaining() == 0) return Nope // nothing to do
    //
    // Bit Order sensitive since the first byte will be unaligned to a byte boundary
    // which bits of that byte we take are specific to bitOrder. Ditto for last byte. 
    // Also which bits are shifted from one byte to another in the final result byte buffer
    // depends on bit order also.
    //
    // At some complexity this could be made faster by some low-level coding trickery, 
    // Such as grabbing the data in chunks of 64 bits instead of one byte at a time.
    //
    val nBytesTransferred = {
      var priorSrcByte: Int = if (src.remaining > 0) Bits.asUnsignedByte(src.get()) else return Nope
      var countBytesTransferred: Int = 0
      val mask = ((1 << numFragmentBits) - 1) & 0xFF
      var done = false
      var aaa = 0
      var bbbbb = 0
      while (tgt.remaining > 0 && !done) {
        done = src.remaining == 0
        val currentSrcByte =
          if (done) 0
          else Bits.asUnsignedByte(src.get())
        st.bitOrder match {
          case BitOrder.MostSignificantBitFirst => {
            //
            // example with bitOffset0b = 5
            //
            // src = UUUUUAAA BBBBBCCC DDDDDEEE FFFFFGGG
            // tgt = AAABBBBB CCCDDDDD EEEFFFFF ....
            //
            aaa = (priorSrcByte << st.bitOffset0b) & 0xFF
            bbbbb = currentSrcByte >>> numFragmentBits
          }
          case BitOrder.LeastSignificantBitFirst => {
            //
            // example with bitOffset0b = 5
            //
            // src = AAAUUUUU CCCBBBBB EEEDDDDD GGGFFFFF
            // tgt = BBBBBAAA DDDDDCCC FFFFFEEE ....
            //            //
            // often better to visualize this with bytes and bits numbered Right to Left
            //
            // same example with bitOffset0b = 5
            //
            // GGGFFFFF EEEDDDDD CCCBBBBB AAAUUUUU = src
            //     .... FFFFFEEE DDDDDCCC BBBBBAAA = tgt
            //
            aaa = priorSrcByte >>> (8 - numFragmentBits) // logical shift. No sign extension
            bbbbb = (currentSrcByte << numFragmentBits) & 0xFF
          }
        }
        val ab = (aaa | bbbbb) & 0xFF
        tgt.put(Bits.asSignedByte(ab))
        priorSrcByte = currentSrcByte
        countBytesTransferred += 1
      }
      countBytesTransferred
    }
    Assert.invariant(bitLimit0b.isDefined)
    //
    // Important detail: the bitPos after this method may be shorter than 8 * number of bytes 
    // transferred, because the data may end in the middle of a byte. If so the final partial
    // byte may be delivered, including bits which are technically past the end
    // of the available data. 
    //
    // However, the bitPos after this method does not reflect that these additional 
    // bits beyond the final partial byte are consumed. 
    // 
    setBitPos0b(math.min(initialBitPos0b + (8 * nBytesTransferred), bitLimit0b.get))
    One(nBytesTransferred)
  }

  def getBinaryDouble(): Maybe[Double] = {
    Assert.usage(isAligned(8)) // aligned, so bitOrder doesn't matter
    val db = data.asDoubleBuffer() // note: byte order is inherited from data
    if (db.remaining() < 1) Nope
    else {
      val d = db.get()
      data.position(data.position + 8)
      setBitPos0b(bitPos0b + 64)
      One(d)
    }
  }

  def getBinaryFloat(): Maybe[Float] = {
    Assert.usage(isAligned(8)) // aligned, so bitOrder doesn't matter
    val db = data.asFloatBuffer() // note: byte order is inherited from data's current
    if (db.remaining() < 1) Nope
    else {
      val d = db.get()
      data.position(data.position + 4)
      setBitPos0b(bitPos0b + 32)
      One(d)
    }
  }

  def computeNBytesNeeded(bitLength: Int, bitOffset0b: Int) = {
    // threadCheck()
    val nBitsRemainingInFirstByte = (8 - bitOffset0b)
    val nAdditionalBitsNeeded = bitLength - nBitsRemainingInFirstByte
    val nAdditionalBytesNeeded = (nAdditionalBitsNeeded >> 3) +
      (if ((nAdditionalBitsNeeded & 0x7) > 0) 1 else 0)
    val needed = 1 + nAdditionalBytesNeeded
    needed
  }

  def signExtend(l: Long, bitLength: Int): Long = {
    Assert.usage(bitLength > 0 && bitLength <= 64)
    if (bitLength == 1) return l // a single bit has no sign to extend
    val shift = 64 - bitLength
    val res = ((l << shift) >> shift) // arithmetic shift right extends sign.
    res
  }

  def unSignExtend(l: Long, bitLength: Int): Long = {
    Assert.usage(bitLength > 0 && bitLength <= 64)
    val mask = if (bitLength == 64) -1L else (1L << bitLength) - 1
    l & mask
  }

  /**
   * This is the thing we can do fast with one tap on the data
   *
   * No allocation of objects occurs when calling this.
   */
  def getSignedLong(bitLengthFrom1To64: Int): Maybe[Long] = {
    // threadCheck()
    Assert.usage(bitLengthFrom1To64 >= 1)
    Assert.usage(bitLengthFrom1To64 <= 64)
    val sl =
      if (data.order() eq java.nio.ByteOrder.BIG_ENDIAN) {
        Assert.invariant(st.bitOrder eq BitOrder.MostSignificantBitFirst)
        converter_BE_MSBFirst.getSignedLong(bitLengthFrom1To64, this)
      } else {
        Assert.invariant(data.order() eq java.nio.ByteOrder.LITTLE_ENDIAN)
        if (st.bitOrder eq BitOrder.MostSignificantBitFirst) {
          converter_LE_MSBFirst.getSignedLong(bitLengthFrom1To64, this)
        } else {
          Assert.invariant(st.bitOrder eq BitOrder.LeastSignificantBitFirst)
          converter_LE_LSBFirst.getSignedLong(bitLengthFrom1To64, this)
        }
      }
    Assert.invariant(bitPos0b <= bitLimit0b.get)
    sl
  }

  def getUnsignedLong(bitLengthFrom1To64: Int): Maybe[ULong] = {
    val signed = getSignedLong(bitLengthFrom1To64)
    if (signed.isEmpty) return Nope
    // 
    // we need to ignore sign (and sign extension) bits
    //
    val s = signed.get
    val u = unSignExtend(s, bitLengthFrom1To64)
    One(ULong(u))
  }

  def getSignedBigInt(bitLengthFrom1: Int): Maybe[BigInt] = {
    // threadCheck()
    Assert.usage(bitLengthFrom1 >= 1)
    if (bitLengthFrom1 <= 64) {
      getSignedLong(bitLengthFrom1).map { long => BigInt(long) }
    } else {
      val nBytesNeeded = computeNBytesNeeded(bitLengthFrom1, 0)
      if (data.remaining() < nBytesNeeded) return Nope
      val savedDataLimit = data.limit()
      // data.limit(data.position() + nBytesNeeded) // Not necessary since we're allocating the rawBytes array below.
      val rawBytes = ByteBuffer.allocate(nBytesNeeded) // FIXME: Performance, no need to allocate this. Can re-use a max-sized one.
      val maybeN = fillByteBuffer(rawBytes)
      Assert.invariant(maybeN.isDefined)
      Assert.invariant(maybeN.get == nBytesNeeded)
      // at this point we have the bytes we need. The last byte may contain a fragment of a byte
      val fragmentByteWidth = bitLengthFrom1 % 8
      val fragmentByteShift = if (fragmentByteWidth == 0) 0 else 8 - fragmentByteWidth
      val allBytes = rawBytes.array()
      // data.limit(savedDataLimit) // restore data limit - not necessary per above
      val result =
        if (javaByteOrder =:= java.nio.ByteOrder.BIG_ENDIAN) {
          Assert.invariant(st.bitOrder == BitOrder.MostSignificantBitFirst)
          val res = BigInt(allBytes) >> fragmentByteShift
          res
        } else {
          Assert.invariant(javaByteOrder =:= java.nio.ByteOrder.LITTLE_ENDIAN)
          Bits.reverseBytes(allBytes)
          if (st.bitOrder eq BitOrder.MostSignificantBitFirst) {
            // if the last byte was a fragment, now the first byte is the fragment. 
            // But the first byte's content are in the most significant bits of the byte. The
            // unused part of the first byte is in the least significant bits of the byte. So 
            // we have to shift the first byte so that the bits in use are in the least significant
            // bits of the byte.
            val msb = allBytes(0)
            val newMSB = msb >> fragmentByteShift // arithmetic shift (extends sign bit)
            allBytes(0) = newMSB.toByte
          }
          //
          // In the case of BitOrder LSB First, reverse the bytes is sufficient. The bits of the last fragment byte
          // now occupy the least significant bits of the first byte, and that is correct.
          // This seems simple but that is because the fillByteBuffer call above already dealt with the 
          // shifting of the data if there is an initial bit offset.
          //
          BigInt(allBytes)
        }
      One(result)
    }
  }

  private def signedBigIntToUnsigned(bitLength: Int, signed: BigInt): BigInt = {
    if (signed >= 0) signed
    else {
      val highBit = BigInt(1) << bitLength
      val abs = signed.abs
      val unsigned = highBit - abs
      unsigned
    }
  }

  def getUnsignedBigInt(bitLengthFrom1: Int): Maybe[BigInt] = {
    Assert.usage(bitLengthFrom1 >= 1)
    getSignedBigInt(bitLengthFrom1).map {
      signed => signedBigIntToUnsigned(bitLengthFrom1, signed)
    }
  }

  /**
   * Determines whether the input stream has this much more data.
   *
   * Does not advance the position
   */
  final def isDefinedForLength(nBits: Long): Boolean = {
    val m = mark
    val res = skip(nBits)
    reset(m) // TODO: Performance - this can be done more cheaply than a full mark/reset. We only need to reset the bitPos0b.
    res
  }

  def skip(nBits: Long): Boolean = {
    // threadCheck()
    Assert.usage(nBits <= Int.MaxValue)
    val n = nBits.toInt
    val result = {
      if (n <= 64) {
        getSignedLong(n).isDefined
      } else {
        val nLongs = n / 64
        val rest = n % 64
        var i = 0
        var res: Boolean = true
        while (i < nLongs && res) {
          res = getSignedLong(64).isDefined
        }
        if (res && rest > 0) {
          getSignedLong(rest).isDefined
        } else res
      }
    }
    result
  }

  private def getStateFromPool: MarkState = {
    // threadCheck()
    val m = if (markPool.isEmpty) {
      new MarkState(0, defaultCodingErrorAction, this)
    } else markPool.pop
    Assert.invariant(numOutstandingMarks >= 0)
    incrNumOutstandingMarks
    m
  }

  private def releaseStateToPool(st: MarkState) {
    // threadCheck()
    Assert.invariant(!markPool.contains(st))
    markPool.push(st)
    Assert.invariant(numOutstandingMarks > 0)
    decrNumOutstandingMarks
  }

  def mark: DataInputStream.Mark = {
    val m = getStateFromPool
    Assert.invariant(!markStack.contains(m))
    m.assignFrom(st)
    m.savedBytePosition0b = data.position()
    m.savedByteLimit0b = data.limit()
    m.savedByteOrder = data.order()
    markStack.push(m)
    m
  }

  private def releaseUntilMark(mark: DataInputStream.Mark) = {
    // threadCheck()
    Assert.usage(!markStack.isEmpty)
    Assert.usage(mark != null)
    Assert.invariant(!markPool.contains(mark))
    Assert.invariant(markStack.contains(mark))
    var current = markStack.pop
    while (!(markStack.isEmpty) && (current ne mark)) {
      releaseStateToPool(current)
      current = markStack.pop
    }
    Assert.invariant(current eq mark) // holds unless markStack was empty
    current
  }

  def reset(mark: DataInputStream.Mark): Unit = {
    val current = releaseUntilMark(mark)
    Assert.invariant(current eq mark)
    st.assignFrom(current)
    data.position(st.savedBytePosition0b)
    data.limit(st.savedByteLimit0b)
    data.order(st.savedByteOrder)
    releaseStateToPool(current)
    st.charIterator.reset() // this also resets the decoder.
  }

  def discard(mark: DataInputStream.Mark): Unit = {
    val current = releaseUntilMark(mark)
    Assert.invariant(current eq mark)
    Assert.invariant(!markStack.contains(mark))
    releaseStateToPool(current)
  }

  def validateFinalStreamState {
    // threadCheck()
    if (numOutstandingMarks != 0) {
      System.err.println("Outstanding dataInputStream marks: " + numOutstandingMarks)
      System.err.println("Still on markStack: " + markStack.toList)
      System.err.println("In markPool: " + markPool.toList)
      markStack.foreach { ms =>
        if (markPool.contains(ms)) System.err.println("In BOTH: " + ms)
      }
    }
    Assert.invariant(numOutstandingMarks == 0)
  }

  private def stringLengthInbits(nBytesConsumed: Long, nCharsTransferred: Long): Long = {
    // threadCheck()
    //
    // There are two ways length is determined.
    //
    if (st.maybeCharWidthInBits.isEmpty) {
      // we don't have a character width in bits
      // that would be because the encoding is variable width 
      // like utf-8 or utf-16BE/LE with dfdl:utf16Width='variable', or shift-JIS
      // 
      // In all these cases, we assume that the mandatory alignment is 8,
      // and the character codes are decoded/encoded to one or more 8-bit bytes.
      // 
      // Hence, we can determine the number of bits consumed by way of the
      // number of bytes consumed.
      8 * nBytesConsumed
    } else {
      val charWidthInBits = st.maybeCharWidthInBits.get
      // we have a character width in bits, and it is fixed
      // 
      // Hence, we can determine the number of bits consumed by way of
      // the number of characters
      charWidthInBits * nCharsTransferred
    }
  }

  def fillCharBuffer(cb: java.nio.CharBuffer): Maybe[Long] = {
    // threadCheck()
    if (!align(st.encodingMandatoryAlignmentInBits)) return Nope
    val cbRemainingBefore = cb.remaining()
    val bbRemainingBefore = data.remaining()
    val bitPos0bBefore = bitPos0b
    //
    // irrespective of whether the user wants REPLACE semantics
    // we need REPORT semantics here.
    //
    st.decoder.onMalformedInput(CodingErrorAction.REPORT)
    st.decoder.onUnmappableCharacter(CodingErrorAction.REPORT)
    var cr: CoderResult = null
    var nCharsTransferred: Int = 0
    var nBytesConsumed: Int = 0
    val decoder = st.decoder match {
      case decoderWithBits: NonByteSizeCharsetEncoderDecoder => {
        decoderWithBits.reset()
        decoderWithBits.setInitialBitOffset(st.bitOffset0b)
        decoderWithBits.setFinalByteBitLimitOffset0b(st.maybeBitLimitOffset0b)
        decoderWithBits
      }
      case _ => st.decoder
    }
    def decodeIt = {
      cr = decoder.decode(data, cb, true)
      nCharsTransferred = cbRemainingBefore - cb.remaining()
      nBytesConsumed = bbRemainingBefore - data.remaining()
    }
    decodeIt
    //
    // Note that while we have been advancing through the bytes in 
    // the data byte buffer, that doesn't tell us how much 
    // data has been consumed because we could have stopped 
    // in the middle of a byte.
    //
    if (cr.isError) {
      //
      // The way we deal with decode errors here is very important
      // Suppose we are decoding along and start reaching some
      // binary data. That is likely to cause a decode error.
      // 
      // Perhaps we are just pre-filling a buffer of characters
      // and thereafter we will test if it matches a regular expression.
      // Well, what if the match could have been successful 
      // on just the data prior to the decode error, and can't be
      // lost based on yet more characters. In that case it would
      // be a shame to decode characters past the decode error, because
      // it would just be wasting time. 
      //
      // So we have a soft-stop at a decode error. This method
      // will return as if it had not encountered the decode error,
      // unless there are no characters transferred at all. In that
      // case the decode error is happening on the very first character
      // of the decode call. In that case we generate a replacement character
      // and continue decoding. The caller has in essence asked for more
      // characters, presumably because they are needed for parsing.
      // If encodingErrorPolicy is 'replace' then we can provide more characters
      // but if encodingErrorPolicy is 'error' then we throw an exception.
      //
      if (nCharsTransferred == 0) {
        // we got an error on the very first character
        if (st.codingErrorAction == CodingErrorAction.REPORT)
          cr.throwException()
        else {
          Assert.invariant(st.codingErrorAction == CodingErrorAction.REPLACE)
          //
          // it's possible bytes were consumed. Rewind those.
          setBitPos0b(bitPos0bBefore)
          st.decoder.onMalformedInput(CodingErrorAction.REPLACE)
          st.decoder.onUnmappableCharacter(CodingErrorAction.REPLACE)
          decodeIt // try again, error should get replaced and then decode may go further
        }
      } else {
        // we got a decode error, but some characters were transferred before
        // the error. So we're going to return with what we have.
        // If the caller wants more characters, and they call this method again
        // then they'll get either the throw or a replacement character.
        cr = CoderResult.OVERFLOW // setting this gets us a "normal" return that provides characters back.
      }
    }
    if (cr.isUnderflow && nCharsTransferred == 0) {
      // couldn't decode even one character
      //
      // it's possible bytes were consumed. Rewind those.
      setBitPos0b(bitPos0bBefore)
      Nope
    } else {
      Assert.invariant(nBytesConsumed == bbRemainingBefore - data.remaining())
      val lengthInBits = stringLengthInbits(nBytesConsumed, nCharsTransferred)
      setBitPos0b(bitPos0bBefore + lengthInBits)
      One(nCharsTransferred)
    }
  }

  def skipChars(nChars: Long): Boolean = {
    // threadCheck()
    Assert.usage(nChars <= skipCharBuf.capacity())
    skipCharBuf.clear
    skipCharBuf.limit(nChars.toInt)
    var maybeN: Maybe[Long] = One(0L)
    var total = 0L
    while (maybeN.isDefined && total < nChars) {
      maybeN = fillCharBuffer(skipCharBuf)
      if (maybeN.isDefined) {
        total += maybeN.get
      }
    }
    if (total < nChars) false
    else true
  }

  private def needMoreData(bufPos: Int): Boolean = {
    val existingLimit = regexMatchBuffer.limit
    if (bufPos < existingLimit) {
      // we didn't fill the buffer with data last time
      // we can just try again and fillCharBuffer may
      // deliver more. 
      return false // we are not done.
    }
    val existingCapacity = regexMatchBuffer.capacity

    val isMatchDone =
      if (existingLimit < existingCapacity) {
        // can enlarge it in place by increasing the limit.
        regexMatchBuffer.position(existingLimit)
        st.adaptedRegexMatchBufferLimit = math.min(st.adaptedRegexMatchBufferLimit * 2, existingCapacity)
        // println("regex match buffer size increased to " + st.adaptedRegexMatchBufferLimit)
        regexMatchBuffer.limit(st.adaptedRegexMatchBufferLimit)
        false // not done, try the match again
      } else {
        Assert.invariant(existingLimit == existingCapacity)
        // need more but we can't enlarge it any more
        // match is done. It might be successful or might have failed to match
        // but either way we have a decision now.
        true // we are done
      }
    isMatchDone
  }

  def lookingAt(matcher: java.util.regex.Matcher, initialRegexMatchLimitInChars: Long = limits.defaultInitialRegexMatchLimitInChars): Boolean = {
    // threadCheck()
    val initialBitPos0b = bitPos0b
    var isMatchDone = false
    var isAMatch: Boolean = false
    st.adaptedRegexMatchBufferLimit = math.max(initialRegexMatchLimitInChars.toInt, st.adaptedRegexMatchBufferLimit)
    regexMatchBuffer.clear
    regexMatchBuffer.limit(st.adaptedRegexMatchBufferLimit)
    var positionAfterLastFill = regexMatchBuffer.position()
    var limitAfterLastFill = regexMatchBuffer.limit()
    while (!isMatchDone) {
      //
      // a new or resized buffer means we have to reset the matcher
      // 
      // Unfortunately, there appears to be no way to restart a matcher
      // part way through a match of a regex. The match succeeds or fails.
      // Resuming matchers is for find() where you then proceed to find
      // another match, not for resuming part way through a match.
      // 
      // This means the matcher will have to start from position 0 of the buffer again
      // however, we don't have to fillCharBuffer again starting from
      // position 0, we can add more characters to the enlarged buffer.
      // if we choose the initial capacity large enough, this won't happen often.
      //
      regexMatchBuffer.position(positionAfterLastFill)
      regexMatchBuffer.limit(math.max(limitAfterLastFill, regexMatchBuffer.limit))
      val ml = fillCharBuffer(regexMatchBuffer)
      val bufPosAfter = regexMatchBuffer.position()
      val limitAfter = regexMatchBuffer.limit()
      positionAfterLastFill = bufPosAfter
      limitAfterLastFill = limitAfter
      // back up so matcher can read from the start through what was added.
      regexMatchBuffer.position(0)
      regexMatchBuffer.limit(bufPosAfter)
      if (!ml.isDefined) {
        // no more data can be had. 
        // so this iteration gets the final answer.
        isMatchDone = true // it will be done after this iteration.
      } else {
        matcher.reset(regexMatchBuffer)
        val isMatch = matcher.lookingAt()
        isAMatch = isMatch // redundant val here is so we can see it in debugger of eclipse
        val hitEnd = matcher.hitEnd()
        val requireEnd = matcher.requireEnd()
        isMatchDone =
          (isMatch, hitEnd, requireEnd) match {
            case (_, true, _) if !isMatchDone => needMoreData(bufPosAfter) // need more data to know if it will match or not
            case (true, _, true) if !isMatchDone => needMoreData(bufPosAfter) // match underway but might be invalidated by more data
            case (false, false, _) => true // there is no match, but more data can't help, so we are done
            case (true, false, false) => true // match, but not at end, so more data can't invalidate it.
            case _ => {
              //
              // Proof that this is an impossible case
              //           TTT case 1
              //           TTF case 1
              //           TFT case 2
              //           TFF case 4
              //           FTT case 1
              //           FTF case 1
              //           FFT case 3
              //           FFF case 3
              //
              Assert.impossibleCase()
            }
          }
      }
    }
    if (!isAMatch) {
      setBitPos0b(initialBitPos0b) // no match, then we don't move the position
    } else {
      //
      // now we have to figure out how long in bytes the match is.
      //
      val nCharsInMatch = matcher.group().length
      if (nCharsInMatch < st.adaptedRegexMatchBufferLimit) {
        val newLimit = math.max(nCharsInMatch, limits.defaultInitialRegexMatchLimitInChars.toInt)
        //        if (newLimit > limits.defaultInitialRegexMatchLimitInChars.toInt)
        //          println("regex match buffer size decreased to " + newLimit)
        st.adaptedRegexMatchBufferLimit = newLimit // set the prior value as our adapted length
      }
      val nBitsConsumed: Long =
        if (this.isFixedWidthEncoding) {
          // that means the characters are fixed width
          Assert.notYetImplemented(st.maybeUTF16Width.getOrElse(UTF16Width.Fixed) == UTF16Width.Variable)
          // TODO: utf16 width variable just changes the way we count chars slightly. Have to scan the string
          // for surrogate pairs, and subtract 1 from the length in chars for each pair found.
          // not bothering for now.
          val bitsPerChar = st.maybeCharWidthInBits.get
          nCharsInMatch * bitsPerChar
        } else {
          //
          // variable width encoding (e.g, like utf-8, shift-JIS)
          // We measure the length by re-executing the fillCharBuffer
          // but this time we know how many characters to stop it at,
          // (length of the match), and then we can compare bitPos
          // to find out how long that was in bits.
          // 
          // We use a different char buffer for this, because the regexMatchBuffer
          // is currently holding things like the groups of the match, we don't want
          // to mess with its position.
          //
          lengthDeterminationBuffer.position(0)
          lengthDeterminationBuffer.limit(nCharsInMatch) // exactly this many chars
          setBitPos0b(initialBitPos0b)
          //
          // looping call to fill this lengthDeterminationBuffer is
          // needed because fillCharBuffer stops on decode errors even if it 
          // will just replace them with the unicode replacement character on the next
          // call. That's an efficiency hack to enable matching to not scan ahead the full
          // size of the regex match buffer while just crashing into and substituting for tons
          // of what really is binary data. If the format forces it to do that it will,
          // but it first returns shy of having substituted unicode replacement characters
          // for decode errors so as to give things like lookingAt a chance to match against the data up to that
          // point, which might be successful, and thereby eliminate the need to 
          // push forward into filling the buffer with unicode replacement characters corresponding
          // to decode errors that are only encountered because the regex match buffer is big.
          //
          // In other words, if hitEnd is false after an attempted match, then we don't need to 
          // keep pushing ahead decoding (and substituting for errors) into the data.
          //
          val isFilled = fillCharBufferLoop(lengthDeterminationBuffer)
          Assert.invariant(isFilled)
          val nbits = bitPos0b - initialBitPos0b
          nbits
        }
      setBitPos0b(initialBitPos0b + nBitsConsumed)
    }
    isAMatch // if true, then the matcher contains details about the match.
  }

  def asIteratorChar: DataInputStream.CharIterator = st.charIterator

  def pastData(nBytesRequested: Int): ByteBuffer = {
    // threadCheck()
    if (!areDebugging) throw new IllegalStateException("Must be debugging.")
    Assert.usage(nBytesRequested >= 0)
    val bb = data.duplicate()
    val posOffset = math.min(bytePos0b, nBytesRequested).toInt
    bb.limit(bb.position)
    bb.position(bytePos0b.toInt - posOffset)
    bb.asReadOnlyBuffer()
  }

  def futureData(nBytesRequested: Int): ByteBuffer = {
    // threadCheck()
    if (!areDebugging) throw new IllegalStateException("Must be debugging.")
    Assert.usage(nBytesRequested >= 0)
    if (nBytesRequested == 0) return ByteBuffer.allocate(0)
    val bb = data.duplicate()
    if (bytePos0b + nBytesRequested < bb.limit()) {
      bb.limit(bb.position() + nBytesRequested)
    } else {
      // ok. The position and limit bb already has are good.
    }
    bb.asReadOnlyBuffer()
  }

}

private class CharIteratorState { // CharIterator state
  var cb = CharBuffer.allocate(2) // allow for 2 in case of surrogate pair
  var deltaBits: Int = 0
  var isFetched = false
  var bitPos0bAtLastFetch = 0L
  // any members added here must be added to assignFrom below.
}

class BBDISCharIterator(st: MarkState, dis: ByteBufferDataInputStream) extends DataInputStream.CharIterator {

  private lazy val myFirstThread = Thread.currentThread()

  protected final def threadCheck() {
    Assert.invariant(Thread.currentThread eq myFirstThread)
  }

  private val ist = new CharIteratorState

  def assignFrom(other: BBDISCharIterator) {
    // threadCheck()
    this.ist.cb = other.ist.cb
    this.ist.deltaBits = other.ist.deltaBits
    this.ist.isFetched = other.ist.isFetched
    this.ist.bitPos0bAtLastFetch = other.ist.bitPos0bAtLastFetch
  }

  def reset() {
    // threadCheck()
    ist.isFetched = false
    ist.cb.clear()
    ist.deltaBits = 0
    st.decoder.reset()
    ()
  }

  /**
   * returns false if unable to fetch a character
   * returns true if it is able.
   * Stores number of bits consumed by the character
   * in deltaBits
   */
  private def fetch(): Boolean = {
    // threadCheck()
    val dataBitPosBefore0b = dis.bitPos0b
    if (!dis.align(st.encodingMandatoryAlignmentInBits)) return false
    ist.bitPos0bAtLastFetch = dis.bitPos0b // keep track of where we start trying to fetch a character
    ist.cb.clear()
    ist.cb.limit(1)
    st.decoder.reset()
    val maybeNumChars = dis.fillCharBuffer(ist.cb) // throws if a decode error and encodingErrorPolicy="error"
    if (maybeNumChars.isEmpty) {
      // Couldn't get one character
      ist.deltaBits = 0
      Assert.invariant(dis.bitPos0b =#= dataBitPosBefore0b)
      return false
    }
    // got 1 character 
    val dataBitPosAfter0b = dis.bitPos0b
    ist.deltaBits = (dataBitPosAfter0b - dataBitPosBefore0b).toInt
    dis.setBitPos0b(dataBitPosBefore0b) // restore data position so we aren't advancing if called by hasNext
    true
  }

  def peek(): Char = {
    // threadCheck()
    if (!hasNext) return -1.toChar
    Assert.invariant(ist.isFetched)
    val c = ist.cb.get(0)
    c
  }

  def peek2(): Char = {
    // threadCheck()
    if (!hasNext) return -1.toChar
    val m = dis.mark
    val c1 = next()
    val c2 =
      if (!hasNext) -1.toChar
      else next()
    dis.reset(m)
    c2
  }

  def hasNext(): Boolean = {
    // threadCheck()
    if (ist.bitPos0bAtLastFetch != dis.bitPos0b) {
      // something moved the position between the last
      // call to hasNext, and this call to next.
      // (or between two calls to hasNext()
      // so we have to invalidate any character
      // cached by hasNext.
      ist.isFetched = false
    }
    if (!ist.isFetched) ist.isFetched = fetch()

    Assert.invariant(dis.bitPos0b <= dis.bitLimit0b.get)
    ist.isFetched
  }

  def next(): Char = {
    // threadCheck()
    if (ist.bitPos0bAtLastFetch != dis.bitPos0b) {
      // something moved the position between the last
      // call to hasNext, and this call to next.
      // (or between two calls to hasNext()
      // so we have to invalidate any character
      // cached by hasNext.
      ist.isFetched = false
    }
    if (!ist.isFetched) ist.isFetched = fetch()
    if (!ist.isFetched) throw new NoSuchElementException()
    val dataBitPosBefore0b = dis.bitPos0b
    val c = ist.cb.get(0)
    ist.isFetched = false
    val newBitPos0b = dis.bitPos0b + ist.deltaBits
    dis.setBitPos0b(newBitPos0b)
    Assert.invariant(dis.bitPos0b <= dis.bitLimit0b.get)
    c
  }
}

private sealed trait LongConverter {

  def getSignedLong(bitLengthFrom1To64: Int, dis: ByteBufferDataInputStream): Maybe[Long]

  /**
   * Allows for 9 bytes. which we may need if there is a bit-offset such that the
   * bits straddle 9 bytes. E.g., at offset 7, a 59 bit length will straddle 9 bytes, using
   * 1 bit from the first byte, and 2 bits from the last byte.
   */
  protected val smallBuf = ByteBuffer.allocate(9)

  protected final def wrapUp(bitLengthFrom1To64: Int, cooked: Long, dis: ByteBufferDataInputStream): Long = {
    dis.setBitPos0b(dis.bitPos0b + bitLengthFrom1To64)
    val cookedFor1Bit = if (bitLengthFrom1To64 == 1 && cooked == -1) 1 else cooked
    cookedFor1Bit
  }

  private def haveEnoughBits(bitLengthFrom1To64: Int, dis: ByteBufferDataInputStream): Boolean = {
    Assert.invariant(dis.bitLimit0b.isDefined)
    val bitLim = dis.bitLimit0b.get
    if (dis.bitPos0b + bitLengthFrom1To64 > bitLim) false
    else true
  }

  protected final def populateSmallBuf(bitLengthFrom1To64: Int, dis: ByteBufferDataInputStream): Maybe[Int] = {
    if (!haveEnoughBits(bitLengthFrom1To64, dis)) return Nope
    val nBytesNeeded = dis.computeNBytesNeeded(bitLengthFrom1To64, dis.st.bitOffset0b)
    val savedDataLimit = dis.data.limit()
    dis.data.limit(dis.data.position() + nBytesNeeded)
    val savedBytePos0b = dis.data.position()
    smallBuf.clear()
    smallBuf.put(dis.data).flip // puts until data runs out of bytes (hits limit) or smallBuf is full.
    dis.data.position(savedBytePos0b)
    dis.data.limit(savedDataLimit) // restore data limit
    One(nBytesNeeded)
  }

  protected final def bigEndianBytesToSignedLong(bb: ByteBuffer, lengthInBytes: Int,
    startingByteOffset0b: Int, bitLengthFrom1To64: Int, dis: ByteBufferDataInputStream): Long = {
    var accumulator = 0L
    var i = 0
    while (i < lengthInBytes) {
      accumulator <<= 8
      accumulator += Bits.asUnsignedByte(smallBuf.get(i + startingByteOffset0b))
      i += 1
    }
    val value = dis.unSignExtend(accumulator, bitLengthFrom1To64) // ?? Redundant ??
    val signed = dis.signExtend(value, bitLengthFrom1To64)
    wrapUp(bitLengthFrom1To64, signed, dis)
  }
}

private class Converter_BE_MSBFirst extends LongConverter {

  final def getSignedLong(bitLengthFrom1To64: Int, dis: ByteBufferDataInputStream): Maybe[Long] = {
    val maybeNBytesNeeded = populateSmallBuf(bitLengthFrom1To64, dis)
    val result =
      maybeNBytesNeeded.map {
        nBytesNeeded =>
          //
          // smallBuf now contains the bytes we need to create the value
          // and at this point we know we will successfully create a value.
          //
          val numBitsInLastByte = (dis.bitPos0b + bitLengthFrom1To64) % 8
          val lastByteShift = if (numBitsInLastByte == 0) 0 else 8 - numBitsInLastByte
          Bits.shiftRight(smallBuf, lastByteShift.toInt)
          val lim = smallBuf.limit
          val numBitsInFirstByte = math.max(8 - dis.st.bitOffset0b - lastByteShift, 0)
          //
          // The shift above, if done, may have shifted all the bits
          // out of the left-most byte of the smallBuf. If so we
          // don't want to include that byte in computing the value
          //
          val (numBytesRemaining, offset) =
            if (numBitsInFirstByte > 0) {
              // we didn't shift every bit out of the first byte
              (nBytesNeeded, 0)
            } else {
              // we shifted every bit out of the first byte
              (nBytesNeeded - 1, 1)
            }
          //
          // Now we just accumulate the bytes into the result value.
          //
          bigEndianBytesToSignedLong(smallBuf, numBytesRemaining, offset, bitLengthFrom1To64, dis)
      }
    result
  }

}

private class Converter_LE_MSBFirst extends LongConverter {

  final def getSignedLong(bitLengthFrom1To64: Int, dis: ByteBufferDataInputStream): Maybe[Long] = {
    val maybeNBytesNeeded = populateSmallBuf(bitLengthFrom1To64, dis)

    val result = maybeNBytesNeeded.map { nBytesNeeded =>
      //
      // smallBuf now contains the bytes we need to create the value
      // and at this point we know we will successfully create a value.
      //
      if (smallBuf.limit() == 1) {
        //
        // only 1 byte holds the entire representation
        //
        // It is worth special case code for this, because many bit fields
        // are just one or a small handful of bits, so many will live in 
        // just one byte.
        //
        val bitLimitOffset0b = (dis.bitPos0b + bitLengthFrom1To64) % 8
        val numUnusedBits = if (bitLimitOffset0b == 0) 0 else 8 - bitLimitOffset0b
        val theByte = Bits.asUnsignedByte(smallBuf.get(0))
        val mask = -1.toByte >>> dis.st.bitOffset0b
        val maskedByte = (theByte >>> numUnusedBits) & mask
        val result = dis.signExtend(maskedByte, bitLengthFrom1To64)
        wrapUp(bitLengthFrom1To64, result, dis)
      } else {
        //
        // There are two or more bytes holding the representation. 
        //
        // The bits of interest start somewhere in the first byte of smallBuf
        // (bitOffset0b tells us exactly where)
        // and end also in that byte or in some later byte at farthest in the 9th byte
        // of smallBuf. (bitOffset0b + bitLengthFrom1To64) tell us exactly where
        // it ends and how far away that is.
        //
        // The order of bytes in smallBuf is exactly as those bytes appeared in 
        // the data stream.
        // 
        // Per the DFDL spec, the start position cannot affect the value. So 
        // we can shift left (because bit order is MSBFirst) until the first bit
        // is the most significant bit of the first byte.
        //
        Bits.shiftLeft(smallBuf, dis.st.bitOffset0b)
        //
        // Now, there's a possibility that this shift shifted all bits of interest
        // out of the last byte of smallBuf
        //
        val limitingBitPos0b = dis.bitPos0b + bitLengthFrom1To64 // last bit position + 1 is the 'limiting' bit.
        val numBitsInLastByte = {
          val m = limitingBitPos0b % 8
          if (m == 0) 8 else m
        }
        if (dis.st.bitOffset0b >= numBitsInLastByte) {
          // the shift left above will leave no bits in the final byte. 
          Assert.invariant(smallBuf.limit() > 1)
          smallBuf.limit(smallBuf.limit - 1) // shorten it by 1 byte.
        }
        //
        Bits.reverseBytes(smallBuf)
        //
        // now bytes of smallBuf are such that the most significant bits are in 
        // the first byte, and least significant are in the last byte.
        // 
        // However, the most significant byte has the problem that the bits in it 
        // are now in the most-significant end of that byte. They need to be shifted right
        // to occupy the least significant bit locations of that byte.
        //
        val numBitsInNewFirstByte =
          if (dis.st.bitOffset0b >= numBitsInLastByte) {
            8 - (dis.st.bitOffset0b - numBitsInLastByte)
          } else {
            numBitsInLastByte - dis.st.bitOffset0b
          }

        val firstByte = smallBuf.get(0)

        if (numBitsInNewFirstByte < 8) {
          // there are some unused bits
          val bitsUnused = 8 - numBitsInNewFirstByte
          val newFirstByte = firstByte >> bitsUnused
          smallBuf.put(0, newFirstByte.toByte)
        }
        //
        // Now the smallBuf bytes contain our big-endian value, 
        //
        bigEndianBytesToSignedLong(smallBuf, smallBuf.remaining(), 0, bitLengthFrom1To64, dis)
      }
    }
    result
  }

}

private class Converter_LE_LSBFirst extends LongConverter {

  final def getSignedLong(bitLengthFrom1To64: Int, dis: ByteBufferDataInputStream): Maybe[Long] = {
    val maybeNBytesNeeded = populateSmallBuf(bitLengthFrom1To64, dis)
    val result = maybeNBytesNeeded.map { nBytesNeeded =>
      //
      // smallBuf now contains the bytes we need to create the value
      // and at this point we know we will successfully create a value.
      //
      // The bits of interest start somewhere in the first byte of smallBuf
      // (bitOffset0b tells us exactly where, though because bit order is 
      // LSBFirst, those bits occupy the most-significant bits of that first byte
      // 
      // and end also in that byte or in some later byte at farthest in the 9th byte
      // of smallBuf. (bitOffset0b + bitLengthFrom1To64) tell us exactly where
      // it ends and how far away that is, again bit order tells us that
      // those bits in the last byte are in the least significant bits of that byte
      //
      // The order of bytes in smallBuf is exactly as those bytes appeared in 
      // the data stream.
      //
      // in order to be able to shift the bits around we must reverse the bits 
      // within the bytes
      Bits.reverseBitsWithinBytes(smallBuf)
      //
      // The bit-string is now properly continuous across all the byte boundaries
      // The bits of interest in the first byte are the least significant bits
      // and the bits of interest in the last byte are the most significant bits
      //
      val numBitsInFirstByte = 8 - dis.st.bitOffset0b
      val numUnusedBitsInFirstByte = 8 - numBitsInFirstByte
      Bits.shiftLeft(smallBuf, numUnusedBitsInFirstByte)
      //
      // Did we shift away all bits in the last byte?
      val numBitsInLastByteInitially = {
        val m = (dis.bitPos0b + bitLengthFrom1To64) % 8
        if (m == 0) 8 else m
      }
      val numUnusedBitsInLastByteInitially = 8 - numBitsInLastByteInitially
      // 
      // The left shift above, if done, may have shifted all the bits
      // out of the right-most byte of the smallBuf. If so we
      // don't want to include that byte in computing the value
      //
      val numBitsRemainingInLastByte = math.max(8 - numUnusedBitsInLastByteInitially - numUnusedBitsInFirstByte, 0)
      val numBytesRemaining =
        if (numBitsRemainingInLastByte > 0) {
          // we didn't shift every bit out of the last byte
          nBytesNeeded
        } else {
          // we shifted every bit out of the last byte
          nBytesNeeded - 1
        }
      Assert.invariant(numBytesRemaining == smallBuf.limit || numBytesRemaining == (smallBuf.limit - 1))
      smallBuf.limit(numBytesRemaining)

      // Now we can reverse the bits back to their original bit order
      // and then reverse the bytes to reflect little-endian

      Bits.reverseBytesAndReverseBits(smallBuf)
      //
      // We know now that any unused bits of the first byte are the most-significant
      // bits and all other bytes are full (all 8 bits in use).
      //
      // So we have ordinary big-endian bytes now and can construct the result value
      //
      bigEndianBytesToSignedLong(smallBuf, numBytesRemaining, 0, bitLengthFrom1To64, dis)
    }
    result
  }
}

