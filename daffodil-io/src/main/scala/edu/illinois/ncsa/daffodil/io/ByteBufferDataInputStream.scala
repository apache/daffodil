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

object ByteBufferDataInputStream {
  def apply(byteArray: Array[Byte]): DataInputStream = new ByteBufferDataInputStream(ByteBuffer.wrap(byteArray))
  def apply(byteBuffer: ByteBuffer): DataInputStream = new ByteBufferDataInputStream(byteBuffer)
  def apply(in: InputStream): DataInputStream = new ByteBufferDataInputStream(in)
}

/**
 * Simple realization of the DataInputStream API
 *
 * Underlying representation is just a ByteBuffer containing all input data.
 */
class ByteBufferDataInputStream private (data: ByteBuffer) extends DataInputStream {

  private def this(inputStream: java.io.InputStream) = this({
    val bos = new ByteArrayOutputStream
    IOUtils.copy(inputStream, bos)
    bos.flush()
    bos.close()
    val ba = bos.toByteArray
    val bb = ByteBuffer.wrap(ba)
    bb
  })

  private def this(file: java.io.File) = this({
    val path = file.toPath()
    val channel = FileChannel.open(path)
    val bbuf = channel.map(FileChannel.MapMode.READ_ONLY, 0L, file.length())
    bbuf
  })

  /*
   * Hack that must go away soon.
   * 
   * This lets us keep the old Input Layer (InStream), but keep in sync with 
   * this new DataInputStream layer. 
   * 
   * For use only during the gap while we have TWO copies of the input.
   * 
   * This is only here because it is the DataInputStream that supports 
   * the pastData and futureData that the data dump stuff needs for debug/trace.
   */

  def notifyNewBitPos0b(newBitPos0b: Long) {
    setBitPos0b(newBitPos0b)
  }

  /*
   * The state that must be saved and restored by mark/reset calls
   */
  class State extends DataInputStream.Mark {
    var savedBytePosition0b: Int = 0
    var savedByteLimit0b: Int = 0
    var bitOffset0b: Int = 0
    var binaryFloatRep: BinaryFloatRep = BinaryFloatRep.Ieee
    var maybeBitLimitOffset0b: Maybe[Long] = One(0)
    var bitOrder: BitOrder = BitOrder.MostSignificantBitFirst
    var savedByteOrder: java.nio.ByteOrder = java.nio.ByteOrder.BIG_ENDIAN
    var maybeCharWidthInBits: Maybe[Int] = Nope
    var codingErrorAction: CodingErrorAction = defaultCodingErrorAction
    var encodingMandatoryAlignment: Int = 8
    var maybeUTF16Width: Maybe[UTF16Width] = Maybe(UTF16Width.Fixed)
    var decoder: CharsetDecoder = {
      val dec = StandardCharsets.UTF_8.newDecoder()
      dec.onMalformedInput(defaultCodingErrorAction)
      dec.onUnmappableCharacter(defaultCodingErrorAction)
      dec
    }
    var debugging: Boolean = false

    object ciState { // CharIterator state
      var cb = CharBuffer.allocate(1)
      var deltaBytes: Int = 0
      var isFetched = false
    }

    def assignFrom(other: State): Unit = {
      this.savedBytePosition0b = other.savedBytePosition0b
      this.savedByteLimit0b = other.savedByteLimit0b
      this.bitOffset0b = other.bitOffset0b
      this.binaryFloatRep = other.binaryFloatRep
      this.maybeBitLimitOffset0b = other.maybeBitLimitOffset0b
      this.bitOrder = other.bitOrder
      this.savedByteOrder = other.savedByteOrder
      this.maybeCharWidthInBits = other.maybeCharWidthInBits
      this.encodingMandatoryAlignment = other.encodingMandatoryAlignment
      this.maybeUTF16Width = other.maybeUTF16Width
      this.decoder = other.decoder
      this.ciState.cb = other.ciState.cb
      this.ciState.deltaBytes = other.ciState.deltaBytes
      this.ciState.isFetched = other.ciState.isFetched
    }
  }

  private object st extends State

  def isFixedWidthEncoding = st.maybeCharWidthInBits.isDefined

  object SSDISLimits extends Limits {
    def maximumSimpleElementSizeInBytes: Long = 1024
    def maximumSimpleElementSizeInCharacters: Long = 1024
    def maximumForwardSpeculationLengthInBytes: Long = 1024 * 1024
    def maximumRegexMatchLengthInCharacters: Long = 1024
  }

  def limits: Limits = SSDISLimits

  private def bytePos0b_ = data.position

  def bitPos0b: Long = (bytePos0b_ << 3) + st.bitOffset0b

  def setBitPos0b(newBitPos0b: Long) {
    Assert.invariant(newBitPos0b < Int.MaxValue)
    Assert.invariant(newBitPos0b >= 0)
    val n: Int = newBitPos0b.toInt
    val newBitOffset0b = n & 0x7
    val newBytePos0b = n >> 3
    if (newBytePos0b <= data.limit)
      data.position(newBytePos0b)
    else {
      data.position(data.limit)
    }
    st.bitOffset0b = newBitOffset0b
  }

  def setBinaryFloatRep(binaryFloatRep: BinaryFloatRep): Unit = {
    if (binaryFloatRep != BinaryFloatRep.Ieee) ???
    st.binaryFloatRep = binaryFloatRep
  }

  def bitLimit0b: Maybe[Long] = st.maybeBitLimitOffset0b.map { offset => (data.limit << 3) + offset }

  def setBitLimit0b(bitLimit0b: Maybe[Long]): Unit = {
    st.maybeBitLimitOffset0b = bitLimit0b.map { bitLimit =>
      Assert.invariant(bitLimit < Int.MaxValue)
      Assert.invariant(bitLimit >= bitPos0b)
      val bitLim = bitLimit.toInt
      val newBitLimitOffset0b = bitLim & 0x7
      val newByteLimit = bitLim >> 3
      data.limit(newByteLimit)
      newBitLimitOffset0b
    }
  }

  def setBitOrder(bitOrder: BitOrder): Unit = { st.bitOrder = bitOrder }

  private def byteOrder = data.order()

  def setByteOrder(byteOrder: ByteOrder): Unit = {
    byteOrder match {
      case ByteOrder.BigEndian => data.order(java.nio.ByteOrder.BIG_ENDIAN)
      case ByteOrder.LittleEndian => data.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    }
    st.savedByteOrder = data.order()
  }

  def setCharWidthInBits(charWidthInBits: Maybe[Int]): Unit = { st.maybeCharWidthInBits = charWidthInBits }

  def setDecoder(decoder: CharsetDecoder): Unit = {
    st.decoder = decoder
    st.decoder.onMalformedInput(st.codingErrorAction)
    st.decoder.onUnmappableCharacter(st.codingErrorAction)
    val cs = decoder.charset()
    val encoder = cs.newEncoder()
    st.maybeCharWidthInBits = {
      if (cs == StandardCharsets.UTF_16 || cs == StandardCharsets.UTF_16BE || cs == StandardCharsets.UTF_16LE)
        if (st.maybeUTF16Width.isDefined && st.maybeUTF16Width.get == UTF16Width.Fixed) One(16)
        else Nope
      else if (encoder.maxBytesPerChar() == encoder.averageBytesPerChar()) One((encoder.maxBytesPerChar() * 8).toInt)
      else Nope
    }
  }

  private def defaultCodingErrorAction = CodingErrorAction.REPLACE

  def setEncodingErrorPolicy(eep: EncodingErrorPolicy): Unit = {
    st.codingErrorAction = eep match {
      case EncodingErrorPolicy.Replace => CodingErrorAction.REPLACE
      case EncodingErrorPolicy.Error => CodingErrorAction.REPORT
    }
    st.decoder.onMalformedInput(st.codingErrorAction)
    st.decoder.onUnmappableCharacter(st.codingErrorAction)
  }

  def setEncodingMandatoryAlignment(bitAlignment: Int): Unit = { st.encodingMandatoryAlignment = bitAlignment }

  def setMaybeUTF16Width(maybeUTF16Width: Maybe[UTF16Width]): Unit = { st.maybeUTF16Width = maybeUTF16Width }

  def withBitLengthLimit[T](lengthLimitInBits: Long)(body: => T): T = {
    val savedLengthLimit = bitLimit0b
    setBitLimit0b(Maybe(lengthLimitInBits))
    val res =
      try {
        body
      } finally {
        setBitLimit0b(savedLengthLimit)
      }
    res
  }

  def fillByteBuffer(bb: java.nio.ByteBuffer): Maybe[Int] = {
    Assert.usage(isAligned(8))
    var i = 0
    val delta = math.min(bb.remaining(), data.remaining())
    while (i < delta) {
      i += 1
      bb.put(data.get())
    }
    if (i == 0) Nope else One(i)
  }

  def getBinaryDouble(): Maybe[Double] = {
    Assert.usage(isAligned(8))
    val db = data.slice().asDoubleBuffer()
    if (db.remaining() < 1) Nope
    else {
      val d = db.get()
      data.position(data.position + 8)
      setBitPos0b(bitPos0b + 64)
      One(d)
    }
  }

  def getBinaryFloat(): Maybe[Float] = {
    Assert.usage(isAligned(8))
    val db = data.slice().asFloatBuffer()
    if (db.remaining() < 1) Nope
    else {
      val d = db.get()
      data.position(data.position + 4)
      setBitPos0b(bitPos0b + 32)
      One(d)
    }
  }

  /**
   * Everything about bit order and byte order is centralized here.
   */
  private def convertRawLongToLong(rawLong: Long, bitLength: Int, bitOffset: Int, rawNinthByte: Maybe[Int]): Long = {
    if (st.bitOrder == BitOrder.MostSignificantBitFirst) {
      if (byteOrder == java.nio.ByteOrder.BIG_ENDIAN) {
        val ninthContribution =
          if (rawNinthByte.isDefined) {
            val ninthShift = 8 - bitOffset
            val ninthByte = rawNinthByte.get
            val shifted = ((ninthByte >>> ninthShift) & (0xFF >>> ninthShift))
            shifted
          } else 0
        val leftLong = (rawLong << bitOffset) + ninthContribution
        val rightShift = 64 - bitLength
        val rightLong = leftLong >> rightShift // sign-extending shift
        rightLong
      } else {
        Assert.invariant(byteOrder == java.nio.ByteOrder.LITTLE_ENDIAN)
        ???
      }
    } else {
      Assert.invariant(st.bitOrder == BitOrder.LeastSignificantBitFirst)
      Assert.invariant(byteOrder == java.nio.ByteOrder.LITTLE_ENDIAN)
      Assert.invariant(st.savedByteOrder == byteOrder)
      ???
    }
  }

  private def computeNBytesNeeded(bitLength: Int, bitOffset0b: Int) = {
    val nBitsRemainingInFirstByte = (8 - bitOffset0b)
    val nAdditionalBitsNeeded = bitLength - nBitsRemainingInFirstByte
    val nAdditionalBytesNeeded = (nAdditionalBitsNeeded >> 3) +
      (if ((nAdditionalBitsNeeded & 0x7) > 0) 1 else 0)
    val needed = 1 + nAdditionalBytesNeeded
    needed
  }

  /**
   * Allows for 9 bytes. which we may need if there is a bit-offset such that the
   * bits straddle 9 bytes. E.g., at offset 7, a 58 bit length will straddle 9 bytes, using
   * 1 bit from the first byte, and 1 bit from the last byte.
   */
  private val smallBuf = ByteBuffer.allocate(9)
  private val smallLongViewBuf = smallBuf.asLongBuffer()

  /**
   * There are 8 long 'view' buffers that are overlaid on the actual
   * byte buffer. They let us access an 8-byte long integer starting on
   * any byte boundary with one access operation.
   */
  private val viewAslongBufs = {
    val ar = new ArrayList[LongBuffer](8)
    0 to 7 foreach { i =>
      if (data.limit > i) {
        data.position(i)
        ar.add(i, data.asLongBuffer())
      }
    }
    data.position(0)
    ar
  }

  /**
   * This is the thing we can do fast with one tap on the data
   *
   * No allocation of objects occurs when calling this.
   */
  def getSignedLong(bitLengthFrom1To64: Int): Maybe[Long] = {
    Assert.usage(bitLengthFrom1To64 >= 1)
    Assert.usage(bitLengthFrom1To64 <= 64)
    val cooked =
      if (data.remaining() >= 8 && (bitLengthFrom1To64 + st.bitOffset0b) <= 64) {
        //
        // best case. We grab one long from the data only, and 
        // we know there are 8 bytes to grab a long from.
        //
        val longBufNum = data.position() % 8
        val lb = viewAslongBufs.get(longBufNum)
        lb.position(data.position() / 8)
        val rawLong = lb.get(0)
        val cookedLong = convertRawLongToLong(rawLong, bitLengthFrom1To64, st.bitOffset0b, Nope)
        cookedLong
      } else {
        val nBytesNeeded =
          if (data.remaining() >= 8) 9 // has to be 9 because  
          else computeNBytesNeeded(bitLengthFrom1To64, st.bitOffset0b)
        if (data.remaining() < nBytesNeeded) return Nope
        val savedDataLimit = data.limit()
        data.limit(data.position() + nBytesNeeded)
        val savedBytePos0b = data.position()
        smallBuf.clear()
        smallBuf.put(data).flip // puts until data runs out of bytes (hits limit)
        data.position(savedBytePos0b)
        data.limit(savedDataLimit) // restore data limit
        val lb = smallLongViewBuf
        val raw = lb.get(0)
        val cookedLong = if (nBytesNeeded < 9) {
          convertRawLongToLong(raw, bitLengthFrom1To64, st.bitOffset0b, Nope)
        } else {
          val rawNinthByte = Bits.asUnsignedByte(smallBuf.get(8)) // last one
          convertRawLongToLong(raw, bitLengthFrom1To64, st.bitOffset0b, One(rawNinthByte))
        }
        cookedLong
      }
    setBitPos0b(bitPos0b + bitLengthFrom1To64)
    val cookedFor1Bit = if (bitLengthFrom1To64 == 1 && cooked == -1) 1 else cooked
    One(cookedFor1Bit)
  }

  //  def get(bytePos0b: Int): Byte = {
  //    val savedPos = bitPos0b
  //    data.position(bytePos0b)
  //    val byte = data.get()
  //    setBitPos0b(savedPos)
  //    byte
  //  }

  def getUnsignedLong(bitLengthFrom1To64: Int): Maybe[ULong] = {
    Assert.usage(bitLengthFrom1To64 >= 1 && bitLengthFrom1To64 <= 64)
    val maybeSigned = getSignedLong(bitLengthFrom1To64)
    if (maybeSigned.isEmpty) return Nope
    val signed = maybeSigned.get
    val mask = (1L << (bitLengthFrom1To64)) - 1
    val unsignedLong = signed & mask
    val unsigned = ULong(unsignedLong)
    One(unsigned)
  }

  def getSignedBigInt(bitLengthFrom1: Int): Maybe[BigInt] = {
    Assert.usage(bitLengthFrom1 >= 1)
    if (bitLengthFrom1 <= 64) {
      getSignedLong(bitLengthFrom1).map { long => BigInt(long) }
    } else {
      val nBytesNeeded = computeNBytesNeeded(bitLengthFrom1, st.bitOffset0b)
      if (data.remaining() < nBytesNeeded) return Nope
      val savedDataLimit = data.limit()
      // data.limit(data.position() + nBytesNeeded) // Not necessary since we're allocating the rawBytes array below.
      val rawBytes = new Array[Byte](nBytesNeeded)
      data.get(rawBytes)
      // data.limit(savedDataLimit) // restore data limit - not necessary per above
      val raw =
        if (byteOrder == java.nio.ByteOrder.BIG_ENDIAN) {
          Assert.invariant(st.bitOrder == BitOrder.MostSignificantBitFirst)
          BigInt(rawBytes)
        } else ???
      val secondShift = 8 - ((bitLengthFrom1 + st.bitOffset0b) % 8)
      val cooked = (raw << st.bitOffset0b) >> secondShift
      One(cooked)
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

  def isAligned(bitAlignment1b: Int): Boolean = {
    Assert.usage(bitAlignment1b >= 1)
    val alignment = bitPos0b % bitAlignment1b
    val res = alignment == 0
    res
  }

  def align(bitAlignment1b: Int): Boolean = {
    if (isAligned(bitAlignment1b)) return true
    val deltaBits = bitAlignment1b - (bitPos0b % bitAlignment1b)
    skip(deltaBits)
  }

  def skip(nBits: Long): Boolean = {
    Assert.usage(nBits <= Int.MaxValue)
    val n = nBits.toInt
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
        getSignedBigInt(rest).isDefined
      } else res
    }
  }

  private val markStack = mutable.Stack[State]()
  private val markPool = mutable.Stack[State]()

  private def getFromPool: State = {
    if (markPool.isEmpty) new State
    else markPool.pop
  }
  private def releaseToPool(st: State) {
    markPool.push(st)
  }

  def mark: DataInputStream.Mark = {
    val m = getFromPool
    m.assignFrom(st)
    m.savedBytePosition0b = data.position()
    m.savedByteLimit0b = data.limit()
    m.savedByteOrder = data.order()
    markStack.push(m)
    m
  }

  private def releaseUntilMark(mark: DataInputStream.Mark) = {
    Assert.usage(!markStack.isEmpty)
    var current = markStack.pop
    while (!(markStack.isEmpty) && (current ne mark)) {
      releaseToPool(current)
      current = markStack.pop
    }
    if (current ne mark) Assert.invariantFailed("mark not found in mark stack")
    current
  }

  def reset(mark: DataInputStream.Mark): Unit = {
    val current = releaseUntilMark(mark)
    Assert.invariant(current eq mark)
    st.assignFrom(current)
    data.position(st.savedBytePosition0b)
    data.limit(st.savedByteLimit0b)
    data.order(st.savedByteOrder)
    releaseToPool(current)

    CharIterator.reset()
    st.decoder.reset()
  }

  def discard(mark: DataInputStream.Mark): Unit = {
    val current = releaseUntilMark(mark)
    Assert.invariant(current eq mark)
    releaseToPool(current)
  }

  def fillCharBuffer(cb: java.nio.CharBuffer): Maybe[Long] = {
    Assert.usage(isAligned(8))
    Assert.usage(st.encodingMandatoryAlignment == 8)
    val cbRemainingBefore = cb.remaining()
    val bbRemainingBefore = data.remaining()
    val bitPos0bBefore = bitPos0b
    //
    // irrespective of whether the user wants REPLACE semantics
    // we need REPORT semantics here.
    st.decoder.onMalformedInput(CodingErrorAction.REPORT)
    st.decoder.onUnmappableCharacter(CodingErrorAction.REPORT)
    var cr: CoderResult = null
    var nCharsTransferred: Int = 0
    var nBytesConsumed: Int = 0
    def decodeIt = {
      cr = st.decoder.decode(data, cb, true)
      nCharsTransferred = cbRemainingBefore - cb.remaining()
      nBytesConsumed = bbRemainingBefore - data.remaining()
    }
    decodeIt
    //
    // If successful, then this already has advanced the
    // bitPos because bitPos is computed from the data
    // buffer position
    //
    if (cr.isError) {
      if (nCharsTransferred == 0) {
        // we got an error on the very first character
        Assert.invariant(nBytesConsumed == 0)
        if (st.codingErrorAction == CodingErrorAction.REPORT)
          cr.throwException()
        else {
          Assert.invariant(st.codingErrorAction == CodingErrorAction.REPLACE)
          st.decoder.onMalformedInput(CodingErrorAction.REPLACE)
          st.decoder.onUnmappableCharacter(CodingErrorAction.REPLACE)
          decodeIt // try again
        }
      } else {
        // we got a decode error, but some characters were transferred before
        // the error. So we're going to return with what we have.
        // If the caller wants more characters, and they call this method again
        // then they'll get either the throw or a replacement character.
        cr = CoderResult.OVERFLOW
      }
    }
    if (cr.isUnderflow && nCharsTransferred == 0) {
      // couldn't decode even one character
      Assert.invariant(bitPos0b == bitPos0bBefore) // have not moved
      Nope
    } else {
      val nBytesConsumed = bbRemainingBefore - data.remaining()
      Assert.invariant(bitPos0b == (bitPos0bBefore + (8 * nBytesConsumed)))
      One(nCharsTransferred)
    }
  }

  // not part of state because it is only used locally by lookingAt.
  // it's only a data member to avoid allocating these repeatedly.
  private val regexMatchBuffer = CharBuffer.allocate(limits.maximumRegexMatchLengthInCharacters.toInt)
  private val lengthDeterminationBuffer = CharBuffer.allocate(limits.maximumRegexMatchLengthInCharacters.toInt)
  private val initialRegexMatchLimit = 32

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
        val newLimit = math.min(existingLimit * 2, existingCapacity)
        regexMatchBuffer.limit(newLimit)
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

  def lookingAt(matcher: java.util.regex.Matcher): Boolean = {
    val initialBitPos0b = bitPos0b
    var isMatchDone = false
    var isAMatch: Boolean = false
    regexMatchBuffer.clear
    regexMatchBuffer.limit(initialRegexMatchLimit)
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
      val ml = fillCharBuffer(regexMatchBuffer)
      val bufPosAfter = regexMatchBuffer.position()
      // back up so matcher can read from the start through what was added.
      regexMatchBuffer.position(0)
      regexMatchBuffer.limit(bufPosAfter)
      if (!ml.isDefined) {
        // no more data can be had. 
        // so this iteration gets the final answer.
        isMatchDone = true // it will be done after this iteration.
      } else {
        matcher.reset(regexMatchBuffer)
        isAMatch = matcher.lookingAt()
        val hitEnd = matcher.hitEnd()
        val requireEnd = matcher.requireEnd()
        isMatchDone =
          (isAMatch, hitEnd, requireEnd) match {
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
          var total = 0L
          while (total < nCharsInMatch) {
            //
            // while loop needed because fillCharBuffer stops on decode errors even if it 
            // will just replace them with the unicode replacement character on the next
            // call. That's an efficiency hack to enable matching to not scan ahead the full
            // size of the regex match buffer while just crashing into and substituting for tons
            // of what really is binary data. If the format forces it to do that it will,
            // but it first returns shy of having substituted unicode replacement characters
            // for decode errors so as to give things like lookingAt a chance to match against the data up to that
            // point, which might be successful, and thereby eliminate the need to 
            // push forward into filling the buffer with unicode replacement characters corresponding
            // to decode erros that are only encountered because the regex match buffer is big.
            //
            val ml = fillCharBuffer(lengthDeterminationBuffer)
            Assert.invariant(ml.isDefined) // will always be defined as we're re-reading data already read.
            val n = ml.get
            total += n
          }
          Assert.invariant(total == nCharsInMatch)
          val nbits = bitPos0b - initialBitPos0b
          nbits
        }
      setBitPos0b(initialBitPos0b + nBitsConsumed)
    }
    isAMatch // if true, then the matcher contains details about the match.
  }

  private object CharIterator extends Iterator[Char] {

    private def ist = st.ciState

    def reset() {
      ist.isFetched = false
      ist.cb.clear()
      ist.deltaBytes = 0
      st.decoder.reset()
    }

    /**
     * returns false if unable to fetch a character
     * returns true if it is able.
     * Stores number of bytes consumed by the character
     * in deltaBytes
     */
    private def fetch(): Boolean = {
      Assert.usage(isAligned(st.encodingMandatoryAlignment))
      ist.cb.clear()
      st.decoder.reset()
      val dataPosBefore = data.position()
      val cr = st.decoder.decode(data, ist.cb, true)
      if (cr.isError()) {
        if (ist.cb.position == 0) cr.throwException()
        else {
          Assert.invariant(ist.cb.position == 1)
        }
      }
      val dataPosAfter = data.position()
      ist.deltaBytes = dataPosAfter - dataPosBefore
      data.position(dataPosBefore)

      if (cr.isUnderflow() && ist.cb.position() == 0) {
        // Not enough data for a character
        false
      } else true
    }

    def hasNext(): Boolean = {
      if (!ist.isFetched) ist.isFetched = fetch()
      ist.isFetched
    }

    def next(): Char = {
      if (!ist.isFetched) ist.isFetched = fetch()
      if (!ist.isFetched) throw new NoSuchElementException()
      val dataPosBefore = data.position()
      val c = ist.cb.get(0)
      ist.isFetched = false
      setBitPos0b(bitPos0b + (ist.deltaBytes * 8))
      c
    }
  }

  def asIteratorChar: Iterator[Char] = CharIterator

  /*
   * Debugger support
   */

  def areDebugging = st.debugging

  def setDebugging(setting: Boolean) {
    if (bitPos0b > 0) throw new IllegalStateException("Must call before any access to data")
    st.debugging = setting
  }

  def pastData(nBytesRequested: Int): ByteBuffer = {
    if (!areDebugging) throw new IllegalStateException("Must be debugging.")
    Assert.usage(nBytesRequested >= 0)
    val bb = data.duplicate()
    val posOffset = math.min(bytePos0b, nBytesRequested).toInt
    bb.limit(bb.position)
    bb.position(bytePos0b.toInt - posOffset)
    bb.asReadOnlyBuffer()
  }

  def futureData(nBytesRequested: Int): ByteBuffer = {
    if (!areDebugging) throw new IllegalStateException("Must be debugging.")
    Assert.usage(nBytesRequested >= 1)
    val bb = data.duplicate()
    if (bytePos0b + nBytesRequested < bb.limit()) {
      bb.limit(bb.position() + nBytesRequested)
    } else {
      // ok. The position and limit bb already has are good.
    }
    bb.asReadOnlyBuffer()
  }
}