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

class DataOutputStreamState extends DataStreamCommonState {

  def defaultCodingErrorAction: CodingErrorAction = CodingErrorAction.REPLACE

  private val initialEncoder = {
    val enc = StandardCharsets.UTF_8.newEncoder
    enc.onMalformedInput(defaultCodingErrorAction)
    enc.onUnmappableCharacter(defaultCodingErrorAction)
    enc
  }

  var fillByte: Int = 0
  var encoder: CharsetEncoder = initialEncoder

  var codingErrorAction: CodingErrorAction = defaultCodingErrorAction

  /**
   * Absolute bit position zero based.
   * Absolute as in relative the the start of the output data stream.
   *
   * This is a Maybe type because we might not know this value, but still need
   * to do unparsing into a buffer.
   */
  var maybeAbsBitPos0b: MaybeULong = MaybeULong.Nope

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
      maybeAbsBitPos0b = MaybeULong(maybeAbsBitPos0b.get + delta)
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
  def setMaybeRelBitLimit0b(newMaybeRelBitLimit0b: MaybeULong): Boolean = {
    Assert.invariant((maybeAbsBitLimit0b.isDefined && maybeRelBitLimit0b.isDefined) || maybeAbsBitLimit0b.isEmpty)
    if (newMaybeRelBitLimit0b.isEmpty) {
      maybeRelBitLimit0b_ = MaybeULong.Nope
      maybeAbsBitLimit0b = MaybeULong.Nope
    } else if (maybeRelBitLimit0b.isEmpty) {
      maybeRelBitLimit0b_ = newMaybeRelBitLimit0b
      // absolute limit doesn't getchanged (it must be undefined)
    } else {
      val delta = maybeRelBitLimit0b.get - newMaybeRelBitLimit0b.get // this is how much the relative value is changing.
      if (delta < 0) {
        // we're trying to lengthen past an existing limit. This isn't allowed.
        return false
      }
      maybeRelBitLimit0b_ = MaybeULong(maybeRelBitLimit0b.get + delta)
      if (maybeAbsBitLimit0b.isDefined) {
        // adjust absolute value by the same amount.
        maybeAbsBitLimit0b = MaybeULong(maybeAbsBitLimit0b.get + delta)
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

  def assignFrom(other: DataOutputStreamState) = {
    super.assignFrom(other)
    this.fillByte = other.fillByte
    // DO NOT SET THIS IT IS SET IN THE CALLER ; this.encoder = other.encoder
    // DO NOT SET THIS IT IS SET IN THE CALLER ; this.codingErrorAction = other.codingErrorAction
    this.maybeAbsBitPos0b = other.maybeAbsBitPos0b
    this.relBitPos0b_ = other.relBitPos0b_
    this.maybeAbsBitLimit0b = other.maybeAbsBitLimit0b
    this.maybeRelBitLimit0b_ = other.maybeRelBitLimit0b_
    this.byteOrder = other.byteOrder
    this.debugOutputStream = other.debugOutputStream
  }

}

sealed trait DOSState
private[io] case object Active extends DOSState
private[io] case object Finished extends DOSState
private[io] case object Uninitialized extends DOSState

trait DataOutputStreamImplMixin extends DataOutputStream
  with DataStreamCommonImplMixin
  with LocalBufferMixin {

  private var _dosState: DOSState = Active // active when new.

  @inline protected def dosState = _dosState

  @inline protected final def setDOSState(newState: DOSState) { _dosState = newState }

  private[io] def isBuffering: Boolean

  @inline private[io] final def isDead = { _dosState =:= Uninitialized }
  @inline override final def isFinished = { _dosState =:= Finished }
  @inline override def setFinished() { _dosState = Finished }
  @inline private[io] final def isActive = { _dosState =:= Active }
  @inline private[io] final def isReadOnly = { isFinished && isBuffering }
  @inline private[io] final def isWritable = { isActive }
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
          case encoderWithBits: NonByteSizeCharsetEncoderDecoder =>
            (MaybeInt(encoderWithBits.bitWidthOfACodeUnit), 1)
          case _ => {
            val encoder = cs.newEncoder()
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
    st.fillByte = fillByte
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
  private final def realStream = getJavaOutputStream()

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
    ???
  }

  private def exceedsBitLimit(lengthInBytes: Long): Boolean = {
    val relEndBitPos0b = relBitPos0b + ULong(lengthInBytes * 8)
    if (maybeRelBitLimit0b.isDefined &&
      (relEndBitPos0b > maybeRelBitLimit0b.getULong)) true
    else false
  }

  def putBytes(ba: Array[Byte], byteStartOffset0b: Int, lengthInBytes: Int): Long = {
    Assert.usage(isWritable)
    Assert.notYetImplemented(st.relBitPos0b % 8 != 0, "non-byte-aligned data")
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
  }

  def putBytes(ba: Array[Byte]): Long = putBytes(ba, 0, ba.length)

  def putByteBuffer(bb: java.nio.ByteBuffer): Long = {
    Assert.usage(isWritable)
    val nTransferred =
      if (bb.hasArray) {
        putBytes(bb.array, bb.arrayOffset + bb.position, bb.remaining())
      } else {
        Assert.notYetImplemented(st.relBitPos0b % 8 != 0, "non-byte-aligned data")
        val lengthInBytes = bb.remaining
        if (exceedsBitLimit(lengthInBytes)) return 0
        val chan = Channels.newChannel(realStream) // supposedly, this will not allocate if the outStream is a FileOutputStream
        st.setRelBitPos0b(st.relBitPos0b + ULong(lengthInBytes * 8))
        chan.write(bb)
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
    if (!align(st.encodingMandatoryAlignmentInBits)) return 0
    // must respect bitLimit0b if defined
    // must not get encoding errors until a char is being written to the output
    // otherwise we can't get precise encodingErrorPolicy='error' behavior.
    // must handle non-byte-sized characters (e.g., 7-bit), so that bit-granularity positioning
    // works.
    withLocalByteBuffer { lbb =>
      val nBytes = math.ceil(cb.remaining * st.encoder.maxBytesPerChar()).toLong
      val bb = lbb.getBuf(nBytes)
      Assert.notYetImplemented(st.encoder.charset.isInstanceOf[NonByteSizeCharsetEncoderDecoder])
      val cr = st.encoder.encode(cb, bb, true) // unavoidable copying/encoding of characters
      cr match {
        case CoderResult.UNDERFLOW => //ok. Normal termination
        case CoderResult.OVERFLOW => Assert.invariantFailed("byte buffer wasn't big enough to accomodate the string")
        case _ if cr.isMalformed() => cr.throwException()
        case _ if cr.isUnmappable() => cr.throwException()
      }
      bb.flip
      if (putByteBuffer(bb) == 0) return 0 // byte buffer length exceeded bit limit
    }
    nToTransfer
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
  }

  protected def putLong_BE_MSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean
  protected def putLong_LE_MSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean
  protected def putLong_LE_LSBFirst(signedLong: Long, bitLengthFrom1To64: Int): Boolean

  def skip(nBits: Long): Boolean = {
    Assert.usage(isWritable)
    Assert.notYetImplemented(nBits % 8 != 0, "skips that aren't a multiple of 8 bits")
    if (maybeRelBitLimit0b.isDefined) {
      val lim = maybeRelBitLimit0b.getULong
      if (relBitPos0b + ULong(nBits) > lim) return false
    }
    var nBytes = nBits / 8
    while (nBytes > 0) {
      nBytes -= 1
      st.setRelBitPos0b(st.relBitPos0b + ULong(8))
      realStream.write(st.fillByte)
    }
    true
  }

  override def maybeRelBitLimit0b: MaybeULong = {
    Assert.usage(isReadable)
    st.maybeRelBitLimit0b
  }

  override def maybeAbsBitLimit0b: MaybeULong = st.maybeAbsBitLimit0b
  override def setMaybeRelBitLimit0b(newMaybeRelBitLimit0b: MaybeULong): Boolean = st.setMaybeRelBitLimit0b(newMaybeRelBitLimit0b)

  override def relBitPos0b: ULong = {
    Assert.usage(isReadable)
    st.relBitPos0b
  }

  override def maybeAbsBitPos0b: MaybeULong = {
    st.maybeAbsBitPos0b
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
    st.setMaybeRelBitLimit0b(savedBitLimit0b)
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
      val deltaBits = bitAlignment1b - (relBitPos0b.toLong % bitAlignment1b)
      skip(deltaBits)
    }
  }

  final override def remainingBits = st.remainingBits
}
