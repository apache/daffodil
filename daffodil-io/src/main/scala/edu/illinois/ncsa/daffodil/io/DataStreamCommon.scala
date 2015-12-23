package edu.illinois.ncsa.daffodil.io

import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset.CharsetDecoder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import java.util.regex.Matcher
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BinaryFloatRep
import passera.unsigned.ULong
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.LocalStack
import edu.illinois.ncsa.daffodil.util.MaybeInt
import edu.illinois.ncsa.daffodil.exceptions.ThinThrowable
import edu.illinois.ncsa.daffodil.util.MaybeULong

object DataStreamCommon {
  /*
   * These limits will come from tunables, or just hard implementation-specific
   * thresholds.
   */
  trait Limits {
    def maximumSimpleElementSizeInBytes: Long
    def maximumSimpleElementSizeInCharacters: Long
    def maximumForwardSpeculationLengthInBytes: Long
    def maximumRegexMatchLengthInCharacters: Long
    def defaultInitialRegexMatchLimitInChars: Long
  }

}

/**
 * This is an interface trait, and it defines methods shared by
 * both DataInputStream and DataOutputStream.
 *
 * Implementation (partial) is in DataStreamCommonImplMixin.
 *
 */
trait DataStreamCommon {
  import DataStreamCommon._
  def limits: Limits

  /**
   * Allow tuning of these thresholds and starting values. These could,
   * in principle, be tuned differently for different elements, thereby
   * keeping limits small when the schema component can be determined to
   * only require small space, but enabling larger limits/starting values
   * when a component has larger needs.
   *
   * These could be cached on, say,
   * the ElementRuntimeData object for each element, or some other kind
   * of dynamic cache.
   */
  def setLimits(newLimits: Limits): Unit

  // def setEncodingMandatoryAlignment(bitAlignment: Int): Unit
  def setEncodingErrorPolicy(eep: EncodingErrorPolicy): Unit

  /**
   * Use Nope for variable-width encodings.
   */
  def setMaybeUTF16Width(maybeUTF16Width: Maybe[UTF16Width]): Unit
  def setBinaryFloatRep(binaryFloatRep: BinaryFloatRep): Unit

  /*
   * Note that when character encodings are not byte-centric (e.g., 7, 6, 5, or 4 bits)
   * then the bit order *is* used by the character decoding to determine which
   * side of a byte is first.
   */
  def setBitOrder(bitOrder: BitOrder): Unit

  /* Note that the byte order for UTF-16 and UTF-32 encodings is
   * not taken from this setByteOrder call, but by use of the
   * UTF-16BE, UTF-16LE, UTF-32BE and UTF-32LE encodings, or
   * by use of the dfdl:unicodeByteOrderMark property.
   * <p>
   * Note that when character encodings are not byte-centric (e.g., 7, 6, 5, or 4 bits)
   * then the byte order *is* used by the character decoding when a character
   * code unit spans a byte boundary.
   */
  def setByteOrder(byteOrder: ByteOrder): Unit
  def byteOrder: ByteOrder

  /**
   * The position is maintained at bit granularity.
   */
  def bitPos0b: Long
  final def bitPos1b: Long = bitPos0b + 1

  /**
   * The byte position excludes any partial byte. So if the bit position
   * is not on a byte boundary, then the byte position is as if the bit position
   * was rounded down to the next byte boundary.
   * <p>
   * These are convenience methods only.
   */
  final def bytePos0b: Long = bitPos0b >> 3
  final def bytePos1b: Long = bitPos1b >> 3

  /**
   * The bit limit is Nope if there is no imposed limit other than end of data.
   * <p>
   * The bitLimit1b is the value of the first bitPos1b beyond the end of the data.
   * Valid bit positions are less than, but not equal to, the bit limit.
   * <p>
   * If bitLimit0b is defined, then there IS that much data available at least.
   */
  def bitLimit0b: MaybeULong
  final def bitLimit1b: MaybeULong = if (bitLimit0b.isEmpty) MaybeULong.Nope else MaybeULong(bitLimit0b.get + 1)

  /**
   * Returns number of bits remaining (if a limit is defined). Nope if not defined.
   */

  def remainingBits: MaybeULong

  /**
   * Convenience methods that temporarily set and (reliably) restore the bitLimit.
   * The argument gives the limit length. Note this is a length, not a bit position.
   *
   * This is added to the current bit position to get the limiting bit position
   * which is then set as the bitLimit when
   * the body is evaluated. On return the bit limit is restored to its
   * prior value.
   * <p>
   * The return value is false if the new bit limit is beyond the existing bit limit range.
   * Otherwise the return value is true.
   * <p>
   * The prior value is restored even if an Error/Exception is thrown. (ie., via a try-finally)
   * <p>
   * These are intended for use implementing specified-length types (simple or complex).
   * <p>
   * Note that length limits in lengthUnits Characters are not implemented
   * this way. See fillCharBuffer(cb) method.
   */
  final def withBitLengthLimit(self: DataStreamCommon, lengthLimitInBits: Long)(body: => Unit): Boolean = macro IOMacros.withBitLengthLimitMacro

  def resetBitLimit0b(savedBitLimit0b: MaybeULong): Unit

  /**
   * Sets the bit limit to an absolute value and returns true.
   * Returns false if the new bit limit is beyond the existing bit limit range.
   */
  def setBitLimit0b(bitLimit0b: MaybeULong): Boolean
  final def setBitLimit1b(bitLimit1b: MaybeULong): Boolean = {
    val newLimit = if (bitLimit1b.isDefined) MaybeULong(bitLimit1b.get - 1) else MaybeULong.Nope
    setBitLimit0b(newLimit)
  }

  /*
   * Methods for moving through data.
   */

  /**
   * advances the bit position to the specified alignment.
   * <p>
   * Note that the bitAlignment1b argument is 1-based.
   * <p>
   * Passing 0 as the argument is a usage error.
   * <p>
   * Passing 1 as the argument performs no alignment, as any bit position
   * is 1-bit aligned.
   * <p>
   * For any other value, the bit position (1-based) is advanced to
   * the next multiple of that argument value.
   * <p>
   * False is returned if there are insufficient available bits to achieve
   * the alignment.
   */

  def align(bitAlignment1b: Int): Boolean

  /**
   * For assertion checking really. Optimizations should remove the need for most
   * alignment operations. This can be used in assertions that check that this
   * is working properly.
   * <p>
   * Note that the bitAlignment1b argument is 1-based.
   * <p>
   * Passing 0 as the argument is a usage error.
   * <p>
   * Passing 1 as the argument performs no alignment, as any bit position
   * is 1-bit aligned.
   */
  def isAligned(bitAlignment1b: Int): Boolean

  /**
   * Advances the bit position by nBits. If nBits aren't available this
   * returns false. Otherwise it returns true.
   */
  def skip(nBits: Long): Boolean

  /**
   * Debugging flag. If set then performance may be reduced, but
   * historic and upcoming data may be viewed using the pastData and futureData
   * methods.
   *
   * This should be set at the beginning of execution. If it is set after data has
   * been accessed then IllegalStateException is thrown.
   */
  def areDebugging: Boolean
  def setDebugging(setting: Boolean): Unit

  /**
   * Access to historic (past data) and upcoming data for
   * purposes of display in a trace or debugger.
   *
   * If areDebugging is false, these throw IllegalStateException
   */
  def pastData(nBytesRequested: Int): ByteBuffer
  def futureData(nBytesRequested: Int): ByteBuffer

  /**
   * Called once after each parse operation to verify final invariants for
   * the implementation.
   *
   * Use to perform checks such as that data structures held in pools are
   * all returned before end of parse.
   */
  def validateFinalStreamState: Unit
}
