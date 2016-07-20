package edu.illinois.ncsa.daffodil.io

import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.MaybeInt
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import java.nio.charset.CharsetEncoder
import java.nio.charset.StandardCharsets
import java.nio.charset.CodingErrorAction
import passera.unsigned.ULong
import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.nio.charset.CoderResult
import java.nio.channels.Channels
import java.io.ByteArrayOutputStream
import org.apache.commons.io.output.TeeOutputStream
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.util.Bits

class DataOutputStreamState extends DataStreamCommonState {

  def defaultCodingErrorAction: CodingErrorAction = CodingErrorAction.REPLACE

  private val initialEncoder = {
    val enc = StandardCharsets.UTF_8.newEncoder
    enc.onMalformedInput(defaultCodingErrorAction)
    enc.onUnmappableCharacter(defaultCodingErrorAction)
    enc
  }

  private var fillByte_ : Int = 0
  def fillByte = fillByte_

  /**
   * The fillLong is a long that has been pre-filled with the fill byte.
   *
   * If the fillByte changes, then the fillLong is updated to track it.
   */
  private var fillLong_ : Long = 0L
  def fillLong = fillLong_

  def setFillByte(newFillByte: Int) {
    Assert.usage(newFillByte <= 255 && newFillByte >= 0)
    fillByte_ = newFillByte
    fillLong_ = {
      if (fillByte_ == 0) 0L
      else {
        var fl: Long = 0L
        var i = 0
        while (i < 8) {
          fl = fl << 8
          fl = fl | fillByte_
          i += 1
        }
        fl
      }
    }
  }

  var encoder: CharsetEncoder = initialEncoder

  var codingErrorAction: CodingErrorAction = defaultCodingErrorAction

  /**
   * Absolute bit position zero based.
   * Absolute as in relative the the start of the output data stream.
   *
   * This is a Maybe type because we might not know this value, but still need
   * to do unparsing into a buffer.
   */
  private var maybeAbsBitPos0b_ : MaybeULong = MaybeULong.Nope
  def maybeAbsBitPos0b = maybeAbsBitPos0b_
  def setMaybeAbsBitPos0b(newMaybeAbsPos0b: MaybeULong) {
    maybeAbsBitPos0b_ = newMaybeAbsPos0b
  }

  /**
   * Relative bit position zero based.
   * Relative to the start of the current buffer.
   */
  private var relBitPos0b_ : ULong = ULong(0)
  def relBitPos0b = relBitPos0b_

  /**
   * Besides setting the relBitPos, it also maintains the value of
   * the absolute bit pos, if it is known.
   */
  def setRelBitPos0b(newRelBitPos0b: ULong) {
    val delta: Long = (newRelBitPos0b - relBitPos0b_).toLong
    if (maybeAbsBitPos0b.isDefined) {
      setMaybeAbsBitPos0b(MaybeULong(maybeAbsBitPos0b.get + delta))
    }
    relBitPos0b_ = ULong(relBitPos0b_ + delta)
  }

  /**
   * Absolute bit limit zero based
   *
   * If defined it is the position 1 bit past the last bit location that can be written.
   * So if we at starting at bitPos0b of 0, and we allow only 100 bits, then the bit positions are
   * 0 to 99, and the bit limit is 100.
   */
  var maybeAbsBitLimit0b: MaybeULong = MaybeULong.Nope

  /**
   * Relative bit limit zero based
   */
  private var maybeRelBitLimit0b_ : MaybeULong = MaybeULong.Nope
  def maybeRelBitLimit0b = maybeRelBitLimit0b_

  /**
   * sets, but also maintains the absolute bit limit, if that is defined.
   *
   * Returns false if the set was unsuccessful, meaning one is setting a limit that
   * extends past a pre-existing limit.
   */
  def setMaybeRelBitLimit0b(newMaybeRelBitLimit0b: MaybeULong, reset: Boolean = false): Boolean = {
    Assert.invariant((maybeAbsBitLimit0b.isDefined && maybeRelBitLimit0b.isDefined) || maybeAbsBitLimit0b.isEmpty)
    if (newMaybeRelBitLimit0b.isEmpty) {
      maybeRelBitLimit0b_ = MaybeULong.Nope
      maybeAbsBitLimit0b = MaybeULong.Nope
    } else if (maybeRelBitLimit0b.isEmpty) {
      maybeRelBitLimit0b_ = newMaybeRelBitLimit0b
      // absolute limit doesn't getchanged (it must be undefined)
    } else {
      val delta = maybeRelBitLimit0b.get - newMaybeRelBitLimit0b.get // this is how much the relative value is changing.
      if (!reset && delta < 0) {
        // we're trying to lengthen past an existing limit. This isn't allowed, unless we are forcing a reset
        return false
      }
      maybeRelBitLimit0b_ = MaybeULong(maybeRelBitLimit0b.get - delta)
      if (maybeAbsBitLimit0b.isDefined) {
        // adjust absolute value by the same amount.
        maybeAbsBitLimit0b = MaybeULong(maybeAbsBitLimit0b.get - delta)
      }
    }
    true // in all other cases, we return true because we're successful.
  }

  /**
   * Offset within the first byte to the first bit. This is the number of bits to skip.
   * Value is from 0 to 7.
   */
  var bitStartOffset0b: Int = 0

  final def remainingBits: MaybeULong = {
    if (maybeRelBitLimit0b.isEmpty) MaybeULong.Nope else MaybeULong(maybeRelBitLimit0b.get - relBitPos0b.toLong)
  }

  var byteOrder: ByteOrder = ByteOrder.BigEndian
  var debugOutputStream: Maybe[ByteArrayOutputStream] = Nope

  /**
   * When there is a partial byte on the end that is also to be
   * considered part of the output, but as a partial byte only it
   * can't be written to the underlying java.io.OutputStream, that
   * partial byte is held here.
   */
  private var fragmentLastByte_ : Int = 0
  def fragmentLastByte = fragmentLastByte_

  /**
   * Within the fragmentLastByte, how many bits are in use.
   *
   * Always between 0 and 7 inclusive.
   */
  private var fragmentLastByteLimit_ : Int = 0
  def fragmentLastByteLimit = fragmentLastByteLimit_

  def setFragmentLastByte(newFragmentByte: Int, nBitsInUse: Int) {
    Assert.usage(nBitsInUse >= 0 && nBitsInUse <= 7)
    Assert.usage(newFragmentByte >= 0 && newFragmentByte <= 255) // no bits above first byte are in use.
    if (nBitsInUse == 0) {
      Assert.usage(newFragmentByte == 0)
    } else {
      // nBitsInUse is 1..7
      // based on the bitOrder, create a mask for the bits we are using.
      val usedBitsMask = if (bitOrder eq BitOrder.MostSignificantBitFirst) {
        128.toByte >> (nBitsInUse - 1)
      } else {
        // LeastSignificantBitFirst
        (1 << nBitsInUse) - 1
      }
      // create mask for bits we're not using
      val unusedBitsMask = ~usedBitsMask
      Assert.usage((newFragmentByte & unusedBitsMask) == 0) // all unused bits must be zero.
    }
    fragmentLastByte_ = newFragmentByte
    fragmentLastByteLimit_ = nBitsInUse
  }

  def assignFrom(other: DataOutputStreamState) = {
    super.assignFrom(other)
    this.fillByte_ = other.fillByte_
    this.fillLong_ = other.fillLong_
    // DO NOT SET THIS IT IS SET IN THE CALLER ; this.encoder = other.encoder
    // DO NOT SET THIS IT IS SET IN THE CALLER ; this.codingErrorAction = other.codingErrorAction
    this.maybeAbsBitPos0b_ = other.maybeAbsBitPos0b_
    this.relBitPos0b_ = other.relBitPos0b_
    this.maybeAbsBitLimit0b = other.maybeAbsBitLimit0b
    this.maybeRelBitLimit0b_ = other.maybeRelBitLimit0b_
    this.byteOrder = other.byteOrder
    this.debugOutputStream = other.debugOutputStream
    //this.setFragmentLastByte(other.fragmentLastByte, other.fragmentLastByteLimit)
  }

}

sealed trait DOSState
private[io] case object Active extends DOSState
private[io] case object Finished extends DOSState
private[io] case object Uninitialized extends DOSState

trait DataOutputStreamImplMixin extends DataOutputStream
    with DataStreamCommonImplMixin
    with LocalBufferMixin {

  def isEndOnByteBoundary = st.fragmentLastByteLimit == 0

  private var _dosState: DOSState = Active // active when new.

  @inline final def dosState = _dosState

  @inline protected final def setDOSState(newState: DOSState) { _dosState = newState }

  private[io] def isBuffering: Boolean

  @inline private[io] final def isDirect = !isBuffering

  @inline private[io] final def isDead = { _dosState =:= Uninitialized }
  @inline override final def isFinished = { _dosState =:= Finished }
  @inline override def setFinished() { _dosState = Finished }
  @inline private[io] final def isActive = { _dosState =:= Active }
  @inline private[io] final def isReadOnly = { isFinished && isBuffering }
  @inline private[io] final def isWritable = {
    isActive ||
      // This can happen if merging streams A and B and C, where A, the direct stream) is being setFinished.
      // if B is finished, then the result of merging A into B is a finished stream (now direct), but we still
      // want to keep merging B into C, and that involves calling this putBytes call to merge C's data into B's stream.
      // Hence, the stream could be writable or finished.
      (isFinished && isDirect)
  }
  @inline private[io] final def isReadable = { !isDead }

  final def encoder = {
    Assert.usage(isReadable)
    st.encoder
  }

  final def setEncoder(encoder: CharsetEncoder): Unit = {
    Assert.usage(isWritable)
    if (this.encoder == encoder) return
    st.encoder = encoder
    st.encoder.onMalformedInput(st.codingErrorAction)
    st.encoder.onUnmappableCharacter(st.codingErrorAction)
    val cs = encoder.charset()
    val (mCharWidthInBits: MaybeInt, mandatoryAlignInBits) = {
      if (cs == StandardCharsets.UTF_16 || cs == StandardCharsets.UTF_16BE || cs == StandardCharsets.UTF_16LE)
        if (st.maybeUTF16Width.isDefined && st.maybeUTF16Width.get == UTF16Width.Fixed) (MaybeInt(16), 8)
        else (MaybeInt.Nope, 8)
      else {
        cs match {
          case encoderWithBits: NonByteSizeCharset =>
            (MaybeInt(encoderWithBits.bitWidthOfACodeUnit), 1)
          case _ => {
            val maxBytes = encoder.maxBytesPerChar()
            if (maxBytes == encoder.averageBytesPerChar())
              (MaybeInt((maxBytes * 8).toInt), 8)
            else (MaybeInt.Nope, 8)
          }
        }
      }
    }
    st.maybeCharWidthInBits = mCharWidthInBits
    st.encodingMandatoryAlignmentInBits = mandatoryAlignInBits
  }

  final def encodingErrorPolicy = {
    Assert.usage(isReadable)
    st.codingErrorAction match {
      case CodingErrorAction.REPLACE => EncodingErrorPolicy.Replace
      case CodingErrorAction.REPORT => EncodingErrorPolicy.Error
    }
  }

  final def setEncodingErrorPolicy(eep: EncodingErrorPolicy): Unit = {
    Assert.usage(isWritable)
    st.codingErrorAction = eep match {
      case EncodingErrorPolicy.Replace => CodingErrorAction.REPLACE
      case EncodingErrorPolicy.Error => CodingErrorAction.REPORT
    }
    st.encoder.onMalformedInput(st.codingErrorAction)
    st.encoder.onUnmappableCharacter(st.codingErrorAction)
    ()
  }

  final def setFillByte(fillByte: Int) {
    Assert.usage(isWritable)
    st.setFillByte(fillByte)
  }

  protected def setJavaOutputStream(newOutputStream: java.io.OutputStream): Unit

  protected def getJavaOutputStream(): java.io.OutputStream

  protected val st: DataOutputStreamState = new DataOutputStreamState
  final protected def cst = st

  def assignFrom(other: DataOutputStreamImplMixin) {
    Assert.usage(isWritable)
    this.st.assignFrom(other.st)
    this.setEncoder(other.encoder)
    this.setEncodingErrorPolicy(other.encodingErrorPolicy)
  }

  /**
   * just a synonym
   */
  protected final def realStream = getJavaOutputStream()

  final override def setDebugging(setting: Boolean) {
    Assert.usage(isWritable)
    if (setting) {
      Assert.usage(!areDebugging)
      val dataCopyStream = new ByteArrayOutputStream()
      st.debugOutputStream = One(dataCopyStream)
      val teeStream = new TeeOutputStream(realStream, dataCopyStream)
      setJavaOutputStream(teeStream)
      this.cst.debugging = true
    } else {
      // turn debugging off
      this.cst.debugging = false
      st.debugOutputStream = Nope
      setJavaOutputStream(new ByteArrayOutputStream())
    }
  }

  def putBigInt(bigInt: BigInt, bitLengthFrom1: Int): Boolean = {
    Assert.usage(isWritable)
    if (bitLengthFrom1 <= 64) {
      // output as a long
      val longInt = bigInt.toLong
      putLong(longInt, bitLengthFrom1)
    } else {
      ???
      //
      // TODO: DELETE THIS COMMENT ONCE THIS IS IMPLEMENTED
      //
      // The code below is "a good try", but far from correct.
      // For now, just leaving it here with the ??? above.
      //
      // The API is actually wrong. We need to distinguish signed from unsigned here
      // because BigInts are stored sign and magnitude, so there's really no way for us
      // to acertain whether to add another bit for the sign bit, or not.
      //
      val numWholeBytes = bitLengthFrom1 / 8
      val numBitsLastByte = bitLengthFrom1 % 8

      val bytes = {
        //
        // We have to pad the bigInt out to the number of bytes required for bitLengthFrom1
        // because a bigInt in principle extends infinitely to the left (most significant bits of 0, or extended sign 1 bits)
        //
        val byteCapacity = numWholeBytes + (if (numBitsLastByte > 0) 1 else 0)
        val numBytes = bigInt.toByteArray
        Assert.notYetImplemented(numBytes.length > byteCapacity)
        val bytes = new Array[Byte](byteCapacity)
        numBytes.copyToArray(bytes, byteCapacity - numBytes.length, numBytes.length)
        bytes
      }
      //
      // TODO: we have to truncate off bits if the bitLengthFrom1 does not take all the bits
      //
      if (st.byteOrder eq ByteOrder.LittleEndian) {
        Bits.reverseBytes(bytes)
        if (st.bitOrder eq BitOrder.LeastSignificantBitFirst) {
          val nBits = putBits(bytes, 0, bitLengthFrom1)
          Assert.invariant(nBits == bitLengthFrom1)
        } else {
          val lastByte = bytes(bytes.length - 1)
          val numBitsLastByte = bitLengthFrom1 % 8
          val lastByteShiftLeft = 8 - numBitsLastByte
          val newLastByte = (lastByte << lastByteShiftLeft).toByte
          bytes(bytes.length - 1) = newLastByte
          val nBits = putBits(bytes, 0, bitLengthFrom1)
          Assert.invariant(nBits == bitLengthFrom1)
        }
      } else {
        // BigEndian case
        Assert.invariant(st.bitOrder eq BitOrder.MostSignificantBitFirst)
        val wholeNumShiftLeft = 8 - numBitsLastByte
        val bigIntShifted = bigInt << wholeNumShiftLeft // this can be a byte bigger
        val bytes = {
          //
          // We have to pad the bigInt out to the number of bytes required for bitLengthFrom1
          // because a bigInt in principle extends infinitely to the left (most significant bits of 0, or extended sign 1 bits)
          //
          val numBytes = bigIntShifted.toByteArray
          val byteCapacity = numWholeBytes + (if (numBitsLastByte > 0) 1 else 0)
          Assert.notYetImplemented(numBytes.length > byteCapacity)
          val bytes = new Array[Byte](byteCapacity)
          numBytes.copyToArray(bytes, byteCapacity - numBytes.length, numBytes.length)
          bytes
        }
        val nBits = putBits(bytes, 0, bitLengthFrom1)
        Assert.invariant(nBits == bitLengthFrom1)
      }
      //
      // TODO: PERFORMANCE: algorithm idea. To avoid heap allocated things, you could do this
      // by recursively calling, putting each 64-bit chunk of the bigInt into
      // a local val chunk: Long = ...least significant 64 bit chunk ...
      // If little endian, then the chunk is output before we recurse (on the way down)
      // If big endian, then the chunk is output after we recurse (on the way back up)
      //
      true
    }
  }

  private def exceedsBitLimit(lengthInBytes: Long): Boolean = {
    val relEndBitPos0b = relBitPos0b + ULong(lengthInBytes * 8)
    if (maybeRelBitLimit0b.isDefined &&
      (relEndBitPos0b > maybeRelBitLimit0b.getULong)) true
    else false
  }

  def putBits(ba: Array[Byte], byteStartOffset0b: Int, lengthInBits: Long): Long = {
    Assert.usage((ba.length - byteStartOffset0b) * 8 >= lengthInBits)
    val nWholeBytes = (lengthInBits / 8).toInt
    val nFragBits = (lengthInBits % 8).toInt
    val nBytesWritten = putBytes(ba, byteStartOffset0b, nWholeBytes)
    val nBitsWritten = nBytesWritten * 8
    if (nBytesWritten < nWholeBytes) {
      nBytesWritten * 8
    } else {
      val isFragWritten =
        if (nFragBits > 0) {
          var fragByte: Long = ba(byteStartOffset0b + nWholeBytes)
          if (st.bitOrder == BitOrder.MostSignificantBitFirst) {
            // we need to shift the bits. We want the most significant bits of the byte
            fragByte = fragByte >>> (8 - nFragBits)
          }
          putLong(fragByte, nFragBits)
        } else
          true
      if (isFragWritten) nBitsWritten + nFragBits
      else nBitsWritten
    }
  }

  def putBytes(ba: Array[Byte], byteStartOffset0b: Int, lengthInBytes: Int): Long = {
    Assert.usage(isWritable)
    if (isEndOnByteBoundary) {
      val nBytes =
        if (exceedsBitLimit(lengthInBytes)) {
          val n = (maybeRelBitLimit0b.getULong - relBitPos0b) / 8
          Assert.invariant(n >= 0)
          n
        } else {
          lengthInBytes
        }
      realStream.write(ba, byteStartOffset0b, nBytes.toInt)
      st.setRelBitPos0b(st.relBitPos0b + ULong(nBytes * 8))
      nBytes
    } else {
      // the data currently ends with some bits in the fragment byte.
      //
      // rather than duplicate all this shifting logic here, we're going to output
      // each byte separately as an 8-bit chunk using putLong
      //
      var i = 0
      var continue = true
      while ((i < lengthInBytes) && continue) {
        continue = putLong(Bits.asUnsignedByte(ba(i)), 8) // returns false if we hit the limit.
        if (continue) i += 1
      }
      i
    }
  }

  def putBytes(ba: Array[Byte]): Long = putBytes(ba, 0, ba.length)

  def putBitBuffer(bb: java.nio.ByteBuffer, lengthInBits: Long): Long = {
    Assert.usage(bb.remaining() * 8 >= lengthInBits)
    val nWholeBytes = (lengthInBits / 8).toInt
    val nFragBits = (lengthInBits % 8).toInt
    //
    // The position and limit of the buffer must be consistent with lengthInBits
    //
    val numBytesForLengthInBits = nWholeBytes + (if (nFragBits > 0) 1 else 0)

    Assert.usage(bb.remaining() == numBytesForLengthInBits)

    if (nFragBits > 0) bb.limit(bb.limit - 1) // last byte is the frag byte
    val nBytesWritten = putByteBuffer(bb) // output all but the frag byte if there is one.
    val nBitsWritten = nBytesWritten * 8
    if (nBytesWritten < nWholeBytes) {
      nBytesWritten * 8
    } else {
      val isFragWritten =
        if (nFragBits > 0) {
          bb.limit(bb.limit + 1)
          var fragByte: Long = Bits.asUnsignedByte(bb.get(bb.limit - 1))
          if (st.bitOrder == BitOrder.MostSignificantBitFirst) {
            // we need to shift the bits. We want the most significant bits of the byte
            fragByte = fragByte >>> (8 - nFragBits)
          }
          putLong(fragByte, nFragBits)
        } else
          true
      if (isFragWritten) nBitsWritten + nFragBits
      else nBitsWritten
    }
  }

  def putByteBuffer(bb: java.nio.ByteBuffer): Long = {
    Assert.usage(isWritable)
    val nTransferred =
      if (bb.hasArray) {
        putBytes(bb.array, bb.arrayOffset + bb.position, bb.remaining())
      } else {
        if (isEndOnByteBoundary) {
          val lengthInBytes = bb.remaining
          if (exceedsBitLimit(lengthInBytes)) return 0
          val chan = Channels.newChannel(realStream) // supposedly, this will not allocate if the outStream is a FileOutputStream
          st.setRelBitPos0b(st.relBitPos0b + ULong(lengthInBytes * 8))
          chan.write(bb)
        } else {
          // not on a byte boundary
          //
          // rather than duplicate all this shifting logic here, we're going to output
          // each byte separately as an 8-bit chunk using putLong
          //
          var i = 0
          var continue = true
          val limit = bb.remaining()
          while ((i < limit) && continue) {
            continue = putLong(Bits.asUnsignedByte(bb.get(i)), 8) // returns false if we hit the limit.
            if (continue) i += 1
          }
          i
        }
      }
    nTransferred
  }

  final def putString(str: String): Long = {
    Assert.usage(isWritable)
    // must respect bitLimit0b if defined
    // must not get encoding errors until a char is being written to the output
    // otherwise we can't get precise encodingErrorPolicy='error' behavior.
    withLocalCharBuffer { lcb =>
      val cb = lcb.getBuf(str.length)
      cb.append(str) // one copying hop
      cb.flip
      putCharBuffer(cb)
    }
  }

  def putCharBuffer(cb: java.nio.CharBuffer): Long = {
    Assert.usage(isWritable)
    val nToTransfer = cb.remaining()
    //
    // TODO: restore mandatory alignment functionality.
    // It can't sipmly be here, as we might be writing to a buffering 
    // stream, so unless we know the absoluteStartPos, we have to 
    // suspend until it is known
    //
    // if (!align(st.encodingMandatoryAlignmentInBits)) return 0

    // must respect bitLimit0b if defined
    // must not get encoding errors until a char is being written to the output
    // otherwise we can't get precise encodingErrorPolicy='error' behavior.
    // must handle non-byte-sized characters (e.g., 7-bit), so that bit-granularity positioning
    // works.
    val res = withLocalByteBuffer { lbb =>
      //
      // TODO: data size limit. For utf-8, this will limit the max number of chars
      // in cb to below 1/4 the max number of bytes in a bytebuffer, because the maxBytesPerChar
      // is 4, even though it is going to be on average much closer to 1 or 2.
      //
      val nBytes = math.ceil(cb.remaining * st.encoder.maxBytesPerChar()).toLong
      val bb = lbb.getBuf(nBytes)
      val encoder = st.encoder
      // Note that encode reads the charbuffer and encodes the character into a
      // local byte buffer. The contents of the local byte buffer are then
      // copied to the DOS byte buffer using putBitBuffer, which handles
      // bitPosiition/bitLimit/fragment bytes. So because we encode to a
      // temporary output buffer, the encode loop does not need to be aware of
      // bit position/limit.
      val cr = encoder.encode(cb, bb, true)
      cr match {
        case CoderResult.UNDERFLOW => //ok. Normal termination
        case CoderResult.OVERFLOW => Assert.invariantFailed("byte buffer wasn't big enough to accomodate the string")
        case _ if cr.isMalformed() => cr.throwException()
        case _ if cr.isUnmappable() => cr.throwException()
      }
      bb.flip

      val bitsToWrite = encoder match {
        case encoderWithBits: NonByteSizeCharsetEncoder => encoderWithBits.bitWidthOfACodeUnit * nToTransfer
        case _ => bb.remaining * 8
      }
      if (putBitBuffer(bb, bitsToWrite) == 0) 0 else nToTransfer
    }
    res
  }

  def putULong(unsignedLong: ULong, bitLengthFrom1To64: Int): Boolean = putLong(unsignedLong.longValue, bitLengthFrom1To64)

  def putLong(signedLong: Long, bitLengthFrom1To64: Int): Boolean = {
    Assert.usage(bitLengthFrom1To64 >= 1 && bitLengthFrom1To64 <= 64)
    Assert.usage(isWritable)
    //
    // do we have enough room for ALL the bits?
    // if not return false.
    //
    if (st.maybeRelBitLimit0b.isDefined) {
      if (st.maybeRelBitLimit0b.get < st.relBitPos0b + bitLengthFrom1To64) return false
    }
    //
    // based on bit and byte order, dispatch to specific code
    //
    val res =
      if (st.byteOrder eq ByteOrder.BigEndian) {
        Assert.invariant(st.bitOrder eq BitOrder.MostSignificantBitFirst)
        putLong_BE_MSBFirst(signedLong, bitLengthFrom1To64)
      } else {
        Assert.invariant(st.byteOrder eq ByteOrder.LittleEndian)
        if (st.bitOrder eq BitOrder.MostSignificantBitFirst) {
          putLong_LE_MSBFirst(signedLong, bitLengthFrom1To64)
        } else {
          Assert.invariant(st.bitOrder eq BitOrder.LeastSignificantBitFirst)
          putLong_LE_LSBFirst(signedLong, bitLengthFrom1To64)
        }
      }

    if (res) {
      st.setRelBitPos0b(st.relBitPos0b + ULong(bitLengthFrom1To64))
    }
    res
  }

  protected def putLong_BE_MSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean
  protected def putLong_LE_MSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean
  protected def putLong_LE_LSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean

  /*
   * These members are like a C language union. We write as float, we get back same storage
   * as int. Ditto double to long.
   */
  protected val unionByteBuffer = ByteBuffer.allocate(8)
  private val unionDoubleBuffer = unionByteBuffer.asDoubleBuffer()
  private val unionFloatBuffer = unionByteBuffer.asFloatBuffer()
  private val unionIntBuffer = unionByteBuffer.asIntBuffer()
  protected val unionLongBuffer = unionByteBuffer.asLongBuffer()

  def putBinaryFloat(v: Float): Boolean = {
    unionFloatBuffer.put(0, v)
    val i = unionIntBuffer.get(0)
    putLong(i, 32)
  }

  def putBinaryDouble(v: Double): Boolean = {
    unionDoubleBuffer.put(0, v)
    val l = unionLongBuffer.get(0)
    putLong(l, 64)
  }

  def skip(nBits: Long): Boolean = {
    Assert.usage(isWritable)
    if (maybeRelBitLimit0b.isDefined) {
      val lim = maybeRelBitLimit0b.getULong
      if (relBitPos0b + ULong(nBits) > lim) return false
    }
    if (nBits <= 64) {
      putLong(st.fillLong, nBits.toInt)
    } else {
      // more than 64 bits to skip
      //
      // break into 3 skips
      //
      // first is the small skip that fills in any remaining bits of the fragment byte
      //
      // second is a skip of whole bytes done by writing whole fillbytes
      //
      // third is a skip that re-creates the fragment byte to hold whatever part of that fragment
      // is part of the skipped bits.
      //
      // often there will be no fragment byte at the beginning or at the end

      var nBitsRemaining = nBits

      if (st.fragmentLastByteLimit > 0) {
        // there is a fragment byte.
        val numRemainingFragmentBits = 8 - st.fragmentLastByteLimit
        val isInitialFragDone = putLong(st.fillByte, numRemainingFragmentBits)
        Assert.invariant(isInitialFragDone)
        nBitsRemaining -= numRemainingFragmentBits
        Assert.invariant(st.fragmentLastByteLimit == 0) // no longer is a fragment byte on the end
      }
      //
      // now the whole bytes
      //
      var nBytes = nBitsRemaining / 8 // computes floor
      while (nBytes > 0) {
        nBytes -= 1
        st.setRelBitPos0b(st.relBitPos0b + ULong(8))
        realStream.write(st.fillByte)
        nBitsRemaining -= 8
      }
      //
      // now any final fragment
      //
      Assert.invariant(nBitsRemaining < 8)
      if (nBitsRemaining > 0) {
        val isFinalFragDone = putLong(st.fillByte, nBitsRemaining.toInt)
        Assert.invariant(isFinalFragDone)
      }
      true
    }
  }

  override def maybeRelBitLimit0b: MaybeULong = {
    st.maybeRelBitLimit0b
  }

  override def maybeAbsBitLimit0b: MaybeULong = st.maybeAbsBitLimit0b
  override def setMaybeRelBitLimit0b(newMaybeRelBitLimit0b: MaybeULong): Boolean = st.setMaybeRelBitLimit0b(newMaybeRelBitLimit0b)

  override def relBitPos0b: ULong = {
    st.relBitPos0b
  }

  override def maybeAbsBitPos0b: MaybeULong = {
    st.maybeAbsBitPos0b
  }

  override def setMaybeAbsBitPos0b(maybeNewAbsBitPos0b: MaybeULong) {
    st.setMaybeAbsBitPos0b(maybeNewAbsBitPos0b)
  }

  override def maybeAbsBitPos1b: MaybeULong = {
    if (maybeAbsBitPos0b.isDefined) MaybeULong(maybeAbsBitPos0b.get.toLong + 1)
    else MaybeULong.Nope
  }

  override def setRelBitPos0b(newRelBitPos0b: ULong) = {
    Assert.usage(isWritable)
    st.setRelBitPos0b(newRelBitPos0b)
  }

  def futureData(nBytesRequested: Int): ByteBuffer = {
    Assert.usage(isReadable)
    ByteBuffer.allocate(0)
  }

  def pastData(nBytesRequested: Int): ByteBuffer = {
    Assert.usage(isReadable)
    if (!areDebugging) throw new IllegalStateException("Must be debugging.")
    Assert.usage(nBytesRequested >= 0)
    if (st.debugOutputStream == Nope) {
      ByteBuffer.allocate(0)
    } else {
      val arr = st.debugOutputStream.get.toByteArray()
      val bb = ByteBuffer.wrap(arr)
      val sz = math.max(0, arr.length - nBytesRequested)
      bb.limit(arr.length)
      bb.position(sz)
      bb
    }
  }

  override final def resetMaybeRelBitLimit0b(savedBitLimit0b: MaybeULong): Unit = {
    Assert.usage(isWritable)
    st.setMaybeRelBitLimit0b(savedBitLimit0b, true)
  }

  def setByteOrder(byteOrder: ByteOrder): Unit = {
    Assert.usage(isWritable)
    st.byteOrder = byteOrder
  }

  def byteOrder: ByteOrder = {
    Assert.usage(isReadable)
    st.byteOrder
  }

  def validateFinalStreamState {
    // nothing to validate
  }

  final override def isAligned(bitAlignment1b: Int): Boolean = {
    Assert.usage(bitAlignment1b >= 1)
    if (bitAlignment1b =#= 1) true
    else {
      Assert.usage(st.maybeAbsBitPos0b.isDefined)
      val alignment = st.maybeAbsBitPos0b.get % bitAlignment1b
      val res = alignment == 0
      res
    }
  }

  final override def align(bitAlignment1b: Int): Boolean = {
    if (isAligned(bitAlignment1b)) true
    else {
      val deltaBits = bitAlignment1b - (st.maybeAbsBitPos0b.get % bitAlignment1b)
      skip(deltaBits)
    }
  }

  final override def remainingBits = st.remainingBits
}
