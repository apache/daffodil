package edu.illinois.ncsa.daffodil.io

import edu.illinois.ncsa.daffodil.util.Maybe
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

trait DataOutputStreamStateImplMixin extends DataStreamCommonState {

  def defaultCodingErrorAction: CodingErrorAction = CodingErrorAction.REPLACE

  var fillByte: Int = 0
  var encoder: CharsetEncoder = {
    val enc = StandardCharsets.UTF_8.newEncoder
    enc.onMalformedInput(defaultCodingErrorAction)
    enc.onUnmappableCharacter(defaultCodingErrorAction)
    enc
  }
  var codingErrorAction: CodingErrorAction = defaultCodingErrorAction

  def assignFrom(other: DataOutputStreamStateImplMixin) = {
    super.assignFrom(other)
    this.fillByte = other.fillByte
    this.encoder = other.encoder
    this.codingErrorAction = other.codingErrorAction
  }
}

class BasicDataOutputStreamState extends DataOutputStreamStateImplMixin {

  var bitPos0b: Long = 0
  var bitLimit0b: Maybe[Long] = Nope

  var byteOrder: ByteOrder = ByteOrder.BigEndian
  var debugOutputStream: ByteArrayOutputStream = null

  def assignFrom(other: BasicDataOutputStreamState) = {
    super.assignFrom(other)
    this.bitPos0b = other.bitPos0b
    this.bitLimit0b = other.bitLimit0b
    this.byteOrder = other.byteOrder
    this.debugOutputStream = other.debugOutputStream
  }
}

object BasicDataOutputStream {
  def apply(chan: java.nio.channels.WritableByteChannel): DataOutputStream = {
    val os = java.nio.channels.Channels.newOutputStream(chan)
    BasicDataOutputStream(os)
  }

  def apply(os: java.io.OutputStream): DataOutputStream = new BasicDataOutputStream(os)
}

/**
 * Mixin to implement character-related methods in terms of more primitive
 * character-related methods.
 */
private[io] trait DataOutputStreamCharImplMixin extends DataStreamCommonImplMixin {
  import DataStreamCommon._

  protected def st: DataOutputStreamStateImplMixin

  def putCharBuffer(cb: CharBuffer): Long

  final def putString(str: String): Long = {
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

  final def setFillByte(fillByte: Int) {
    st.fillByte = fillByte
  }

  final def encoder = st.encoder
  final def setEncoder(encoder: CharsetEncoder): Unit = {
    if (st.encoder == encoder) return
    st.encoder = encoder
    st.encoder.onMalformedInput(st.codingErrorAction)
    st.encoder.onUnmappableCharacter(st.codingErrorAction)
    val cs = encoder.charset()
    val (mCharWidthInBits, mandatoryAlignInBits) = {
      if (cs == StandardCharsets.UTF_16 || cs == StandardCharsets.UTF_16BE || cs == StandardCharsets.UTF_16LE)
        if (st.maybeUTF16Width.isDefined && st.maybeUTF16Width.get == UTF16Width.Fixed) (One(16), 8)
        else (Nope, 8)
      else {
        cs match {
          case encoderWithBits: NonByteSizeCharsetEncoderDecoder =>
            (One(encoderWithBits.bitWidthOfACodeUnit), 1)
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

  final def setEncodingErrorPolicy(eep: EncodingErrorPolicy): Unit = {
    st.codingErrorAction = eep match {
      case EncodingErrorPolicy.Replace => CodingErrorAction.REPLACE
      case EncodingErrorPolicy.Error => CodingErrorAction.REPORT
    }
    st.encoder.onMalformedInput(st.codingErrorAction)
    st.encoder.onUnmappableCharacter(st.codingErrorAction)
    ()
  }

}

class BasicDataOutputStream private (realStream: java.io.OutputStream,
  protected final val st: BasicDataOutputStreamState = new BasicDataOutputStreamState)
  extends DataOutputStream with DataOutputStreamCharImplMixin {
  import DataStreamCommon._

  private lazy val outStream = {
    if (areDebugging) {
      if (st.debugOutputStream == null) {
        val dataCopyStream = new ByteArrayOutputStream()
        st.debugOutputStream = dataCopyStream
      }
      val teeStream = new TeeOutputStream(realStream, st.debugOutputStream)
      teeStream
    } else realStream
  }

  protected final def cst: DataStreamCommonState = st

  @deprecated("2015-07-14", "Don't copy these.")
  def copyOutStream: BasicDataOutputStream = {
    val newState = new BasicDataOutputStreamState
    newState.assignFrom(st)
    val newOS = new BasicDataOutputStream(outStream, newState)
    newOS
  }

  def putBigInt(bigInt: BigInt, bitLengthFrom1: Int): Boolean = ???

  private def exceedsBitLimit(lengthInBytes: Long): Boolean = {
    val endBitPos0b = bitPos0b + (lengthInBytes * 8)
    if (bitLimit0b.isDefined &&
      endBitPos0b > bitLimit0b.get) true
    else false
  }

  def putBytes(ba: Array[Byte], byteStartOffset0b: Int, lengthInBytes: Int): Long = {
    Assert.notYetImplemented(st.bitPos0b % 8 != 0, "non-byte-aligned data")
    if (exceedsBitLimit(lengthInBytes)) return 0
    outStream.write(ba, byteStartOffset0b, lengthInBytes)
    st.bitPos0b += lengthInBytes * 8
    lengthInBytes
  }

  def putBytes(ba: Array[Byte]) = putBytes(ba, 0, ba.length)

  def putByteBuffer(bb: java.nio.ByteBuffer): Long = {
    val nTransferred =
      if (bb.hasArray) {
        putBytes(bb.array, bb.arrayOffset + bb.position, bb.remaining())
      } else {
        Assert.notYetImplemented(st.bitPos0b % 8 != 0, "non-byte-aligned data")
        val lengthInBytes = bb.remaining
        if (exceedsBitLimit(lengthInBytes)) return 0
        val chan = Channels.newChannel(outStream) // supposedly, this will not allocate if the outStream is a FileOutputStream
        st.bitPos0b += lengthInBytes * 8
        chan.write(bb)
      }
    nTransferred
  }

  def putCharBuffer(cb: java.nio.CharBuffer): Long = {
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
    ???
  }

  def skip(nBits: Long): Boolean = {
    Assert.notYetImplemented(nBits % 8 != 0, "skips that aren't a multiple of 8 bits")
    bitLimit0b.foreach {
      lim => if (bitPos0b + nBits > lim) return false
    }
    var nBytes = nBits / 8
    while (nBytes > 0) {
      nBytes -= 1
      st.bitPos0b += 8
      outStream.write(st.fillByte)
    }
    true
  }

  // Members declared in DataStreamCommon   
  def bitLimit0b: Maybe[Long] = st.bitLimit0b
  def bitPos0b: Long = st.bitPos0b
  def setBitPos0b(bitPos0b: Long) = st.bitPos0b = bitPos0b

  def futureData(nBytesRequested: Int): ByteBuffer = ByteBuffer.allocate(0)

  def pastData(nBytesRequested: Int): ByteBuffer = {
    if (!areDebugging) throw new IllegalStateException("Must be debugging.")
    Assert.usage(nBytesRequested >= 0)
    if (st.debugOutputStream == null) {
        ByteBuffer.allocate(0)
    } else {
      val arr = st.debugOutputStream.toByteArray()
      val bb = ByteBuffer.wrap(arr)
      val sz = math.max(0, arr.length - nBytesRequested)
      bb.limit(arr.length)
      bb.position(sz)
      bb
    }
  }

  def setBitLimit0b(bitLimit0b: Maybe[Long]): Boolean = {
    if (st.bitLimit0b.isDefined && bitLimit0b.isDefined)
      if (bitLimit0b.get > st.bitLimit0b.get) return false
    st.bitLimit0b = bitLimit0b
    true
  }

  final private[io] override def resetBitLimit0b(savedBitLimit0b: Maybe[Long]): Unit = {
    st.bitLimit0b = savedBitLimit0b
  }

  def setByteOrder(byteOrder: ByteOrder): Unit = st.byteOrder = byteOrder

  def byteOrder: ByteOrder = st.byteOrder

  def validateFinalStreamState {
    // nothing to validate
  }
}
