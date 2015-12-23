package edu.illinois.ncsa.daffodil.io

import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.MaybeInt
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BinaryFloatRep
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
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
import java.nio.CharBuffer
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

  var bitPos0b: Long = 0
  var bitLimit0b: MaybeULong = MaybeULong.Nope

  var byteOrder: ByteOrder = ByteOrder.BigEndian
  var debugOutputStream: Maybe[ByteArrayOutputStream] = Nope

  def assignFrom(other: DataOutputStreamState) = {
    super.assignFrom(other)
    this.fillByte = other.fillByte
    // DO NOT SET THIS IT IS SET IN THE CALLER ; this.encoder = other.encoder
    // DO NOT SET THIS IT IS SET IN THE CALLER ; this.codingErrorAction = other.codingErrorAction
    this.bitPos0b = other.bitPos0b
    this.bitLimit0b = other.bitLimit0b
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
    Assert.usage(!areDebugging)
    super.setDebugging(setting)
    val dataCopyStream = new ByteArrayOutputStream()
    st.debugOutputStream = One(dataCopyStream)
    val teeStream = new TeeOutputStream(realStream, dataCopyStream)
    setJavaOutputStream(teeStream)
  }

  def putBigInt(bigInt: BigInt, bitLengthFrom1: Int): Boolean = {
    Assert.usage(isWritable)
    ???
  }

  private def exceedsBitLimit(lengthInBytes: Long): Boolean = {
    val endBitPos0b = bitPos0b + (lengthInBytes * 8)
    if (bitLimit0b.isDefined &&
      endBitPos0b > bitLimit0b.get) true
    else false
  }

  def putBytes(ba: Array[Byte], byteStartOffset0b: Int, lengthInBytes: Int): Long = {
    Assert.usage(isWritable)
    Assert.notYetImplemented(st.bitPos0b % 8 != 0, "non-byte-aligned data")
    val nBytes =
      if (exceedsBitLimit(lengthInBytes)) {
        val n = (bitLimit0b.get - bitPos0b) / 8
        Assert.invariant(n >= 0)
        n
      } else {
        lengthInBytes
      }
    realStream.write(ba, byteStartOffset0b, nBytes.toInt)
    st.bitPos0b += nBytes * 8
    nBytes
  }

  def putBytes(ba: Array[Byte]): Long = putBytes(ba, 0, ba.length)

  def putByteBuffer(bb: java.nio.ByteBuffer): Long = {
    Assert.usage(isWritable)
    val nTransferred =
      if (bb.hasArray) {
        putBytes(bb.array, bb.arrayOffset + bb.position, bb.remaining())
      } else {
        Assert.notYetImplemented(st.bitPos0b % 8 != 0, "non-byte-aligned data")
        val lengthInBytes = bb.remaining
        if (exceedsBitLimit(lengthInBytes)) return 0
        val chan = Channels.newChannel(realStream) // supposedly, this will not allocate if the outStream is a FileOutputStream
        st.bitPos0b += lengthInBytes * 8
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
    // must respect bitLimit0b if defined
    Assert.usage(isWritable)
    ???
  }

  def skip(nBits: Long): Boolean = {
    Assert.usage(isWritable)
    Assert.notYetImplemented(nBits % 8 != 0, "skips that aren't a multiple of 8 bits")
    if (bitLimit0b.isDefined) {
      val lim = bitLimit0b.get
      if (bitPos0b + nBits > lim) return false
    }
    var nBytes = nBits / 8
    while (nBytes > 0) {
      nBytes -= 1
      st.bitPos0b += 8
      realStream.write(st.fillByte)
    }
    true
  }

  // Members declared in DataStreamCommon
  def bitLimit0b: MaybeULong = {
    Assert.usage(isReadable)
    st.bitLimit0b
  }
  def bitPos0b: Long = {
    Assert.usage(isReadable)
    st.bitPos0b
  }
  def setBitPos0b(bitPos0b: Long) = {
    Assert.usage(isWritable)
    st.bitPos0b = bitPos0b
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

  def setBitLimit0b(bitLimit0b: MaybeULong): Boolean = {
    Assert.usage(isWritable)
    if (st.bitLimit0b.isDefined && bitLimit0b.isDefined)
      if (bitLimit0b.get > st.bitLimit0b.get) return false
    st.bitLimit0b = bitLimit0b
    true
  }

  final override def resetBitLimit0b(savedBitLimit0b: MaybeULong): Unit = {
    Assert.usage(isWritable)
    st.bitLimit0b = savedBitLimit0b
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
}
