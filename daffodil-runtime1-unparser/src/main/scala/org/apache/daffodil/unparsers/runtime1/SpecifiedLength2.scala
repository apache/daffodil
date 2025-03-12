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

package org.apache.daffodil.unparsers.runtime1

import java.nio.charset.MalformedInputException
import java.nio.charset.UnmappableCharacterException

import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.io.ZeroLengthStatus
import org.apache.daffodil.io.processors.charset.BitsCharset
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.lib.util.MaybeChar
import org.apache.daffodil.lib.util.MaybeJULong
import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.runtime1.infoset.DIComplex
import org.apache.daffodil.runtime1.infoset.DIElement
import org.apache.daffodil.runtime1.infoset.DISimple
import org.apache.daffodil.runtime1.processors.CharsetEv
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Evaluatable
import org.apache.daffodil.runtime1.processors.LengthEv
import org.apache.daffodil.runtime1.processors.ModelGroupRuntimeData
import org.apache.daffodil.runtime1.processors.RuntimeData
import org.apache.daffodil.runtime1.processors.SuspendableOperation
import org.apache.daffodil.runtime1.processors.UnparseTargetLengthInBitsEv
import org.apache.daffodil.runtime1.processors.unparsers._

import passera.unsigned.ULong

/*
 * Notes on variable-width characters with lengthUnits 'characters'
 *
 * (Not worth doing for complex types, at least initially)
 *
 * (Similarly, don't implement dfdl:contentLength nor dfdl:valueLength
 * functions in units 'characters' for complex types if the encoding is
 * variable width).
 *
 * Simple types:
 *
 * For parsing, we must decode characters one by one until we accumulate
 * the number of characters.
 *
 * The dfdl:contentLength in characters is this number of characters. This requires
 * specific unparsers for all the textual data types for this case.
 *
 * If we are not trimming off pad characters, then the dfdl:valueLength is the
 * same as the dfdl:contentLength.
 *
 * A single-pass algorithm that does trimming on left/right, and filling of the
 * rightFill region would be:
 *
 * If we are trimming on the left (justification right or center) we must
 * (1) record bit position as start of dfdl:contentLength in bits.
 * (2) decode characters, counting up to N, keeping count also of the number of
 * pad characters encountered (and discarding those pad characters). When a
 * non-pad-character is encountered, the start position (in bits) is the start
 * of the dfdl:valueLength in bits.
 * (3) decode characters, counting up to N, and this part is tricky.
 * If we are trimming on the right (justification center and left) then keeping
 * track of the position of the last non-pad character, and the length of a
 * current run of adjacent pad characters since the last non-pad character. When
 * we reach N characters, the end of value position (in bits) is the position after the last
 * non-pad character, the final position after N characters is the dfdl:contentLength
 * in bits.  The dfdl:valueLength in characters is the dfdl:valueLength in characters
 * minus the length of the run of pad characters on the right, and the number of
 * pad characters counted on the left.
 *
 * For unparsing, we must encode characters one by one until we have
 * encoded the number of characters, or we run out of characters.
 * If we run out of characters, then if we are padding, the length of the left
 * and right padding regions is computed, and that many are encoded into
 * those. If we are not padding and we run out of characters it is an error.
 * If we have too many characters, then if simpleType string and
 * truncateVariableLengthString, then we discard any excess characters, otherwise
 * it is an error.
 *
 * The contentLength and valueLength in characters need to be stored on the infoset
 * node explicitly by these processors.
 *
 * The infoset value is NOT modified by truncation nor padding.
 * The fn:stringLength of the value is constant throughout this.
 *
 * Complex Types:
 *
 * (Not worth doing. Should SDE - not implemented by Daffodil - complex types with
 * specified length with length units characters, with variable length encoding.)
 *
 * For parsing, we record the start of content bit position (and start of value bit
 * position is the same), then we decode N characters, and the new bit position
 * is the end of content bit position. Behavior on a decode error is controlled by
 * dfdl:encodingErrorPolicy. So dfdl:contentLength in 'characters' is N,
 * and we have the positions to enable us to compute dfdl:contentLength in bits.
 * Then we backup to the start of content bit position and recursively parse
 * the complex type body, in an environment where the data limit is set to prevent
 * parsing beyond the content length in bits. When this recursive parse
 * returns, the bit position is the end of value bit position, and we then
 * skip to the content end position.
 *
 * For unparsing, we record the start of content bit position and start of value
 * bit position is the same. Then we recursively unparse the complex type body into
 * a buffer. Then we scan and decode this counting the characters. If a decode
 * error occurs, dfdl:encodingErrorPolicy is used to decide whether to error, or
 * count 1 for the unicodeReplacementCharacter that is the replacement.
 *
 * This makes length of a complex type in characters fundamentally unreliable if
 * decode errors are possible. User beware. Use length in bytes or bits instead.
 *
 * When the recursive unparse completes, we block on the end bit pos
 * and the ElementUnused region is filled with the number of characters to reach total N.
 * If the number of characters is greater than N it is an error.
 */

/*
 * Notes on variable-width characters when length units are bits
 *
 * (Really does need to be implemented, examples like 80-byte records, but where
 * the characters can be utf-8 not just ascii = would be a typical Unicode
 * upgrade to a legacy 80-byte oriented application.)
 *
 * In this case we know the number of bits, we don't know how many characters
 * will be parsed or unparsed.
 *
 * Content and value length regions in bits/bytes are computed by the framework,
 * but content and/or value length in characters must be determined by keeping
 * count of the number of characters parsed by the pad/trim processors, and the
 * value processor. These counts need to be stored on the infoset node.
 *
 */

/*
 * non-text or fixed-width characters, units are bits
 *
 * For parsing or unparsing, we know the exact length in bits.
 *
 * However, if textual, the number of bits does not necessarily divide by
 * the width of a character in bits. There may be a fragment of a character
 * at the end.
 *
 * An example would be if there are 56 bits (7 bytes), but utf-16 characters
 * which are 16 bits each, will hold 3 characters with 8 bits left over.
 *
 * For unparsing, in the case where all the characters do not fit, we may
 * discard the extra characters, or we may fail.
 *
 * We don't have 'bytes' here because that is always converted to bits.
 *
 * Note that the dfdl:contentLength and dfdl:valueLength can be requested in 'characters'
 * and in that case, we can just divide by the character set width to convert
 * the number of bits to characters.
 */

class SimpleTypeRetryUnparserSuspendableOperation(
  override val rd: ElementRuntimeData,
  maybeUnparserTargetLengthInBitsEv: Maybe[UnparseTargetLengthInBitsEv],
  vUnparser: Unparser
) extends SuspendableOperation {

  override protected def maybeKnownLengthInBits(ustate: UState): MaybeULong = {
    if (maybeUnparserTargetLengthInBitsEv.isDefined) {
      // maybeUnparserTargetLengthInBitsEv should only be defined if we know at
      // schema compile time that it will evaluate to a value, and that value
      // will match the actual unparsed length (e.g. there will not be any
      // padding/fill). Here we assert that if the target length evaluatable was
      // passed into this class, then it must evaluate to a value. When we
      // deliver buffered content, we will also assert that the starting bit
      // position of the buffered DOS that results from this suspension matches
      // the direct DOS (i.e. this length is used correctly)
      val maybeLen = maybeUnparserTargetLengthInBitsEv.get.evaluate(ustate)
      Assert.invariant(maybeLen.isDefined)
      maybeLen.toMaybeULong
    } else {
      MaybeULong.Nope
    }
  }

  protected def test(state: UState) = {
    state.currentInfosetNode.asSimple.hasValue
  }

  protected def continuation(state: UState): Unit = {
    vUnparser.unparse1(state)
  }
}

class SimpleTypeRetryUnparser(
  override val context: ElementRuntimeData,
  maybeUnparserTargetLengthInBitsEv: Maybe[UnparseTargetLengthInBitsEv],
  vUnparser: Unparser
) extends PrimUnparser
  with SuspendableUnparser {

  override final def runtimeDependencies = maybeUnparserTargetLengthInBitsEv.toSeq.toVector

  final override def childProcessors = Vector(vUnparser)

  def suspendableOperation = new SimpleTypeRetryUnparserSuspendableOperation(
    context,
    maybeUnparserTargetLengthInBitsEv,
    vUnparser
  )

}

class CaptureStartOfContentLengthUnparser(override val context: ElementRuntimeData)
  extends PrimUnparser {

  override def runtimeDependencies = Vector()

  override def unparse(state: UState): Unit = {
    val dos = state.getDataOutputStream
    val elem = state.currentInfosetNode.asInstanceOf[DIElement]
    if (dos.maybeAbsBitPos0b.isDefined) {
      elem.contentLength.setAbsStartPos0bInBits(dos.maybeAbsBitPos0b.getULong)
    } else {
      elem.contentLength.setRelStartPos0bInBits(dos.relBitPos0b, dos)
    }
  }
}

class CaptureEndOfContentLengthUnparser(
  override val context: ElementRuntimeData,
  maybeFixedLengthInBits: MaybeULong
) extends PrimUnparser {

  override def runtimeDependencies = Vector()

  override def unparse(state: UState): Unit = {
    val dos = state.getDataOutputStream
    val elem = state.currentInfosetNode.asInstanceOf[DIElement]

    if (
      elem.contentLength.isStartAbsolute && dos.maybeAbsBitPos0b.isEmpty && maybeFixedLengthInBits.isDefined
    ) {
      // If this element has an absolute starting bit position, but the current
      // DOS bit position is only known relatively, that means there was some
      // suspension related to this element that had an unknown length.
      // However, if this is a fixed length element, we can calculate the
      // absolute position of this DOS based on its absolute starting position
      // position, its length, and the relative position of the current DOS
      val startAbsBitPos0b: ULong = elem.contentLength.maybeStartPos0bInBits.getULong
      val currentAbsPos0b = startAbsBitPos0b + maybeFixedLengthInBits.getULong
      dos.setAbsStartingBitPos0b(currentAbsPos0b - dos.relBitPos0b)
    }

    if (dos.maybeAbsBitPos0b.isDefined) {
      elem.contentLength.setAbsEndPos0bInBits(dos.maybeAbsBitPos0b.getULong)
    } else {
      elem.contentLength.setRelEndPos0bInBits(dos.relBitPos0b, dos)
    }
  }
}

class CaptureStartOfValueLengthUnparser(override val context: ElementRuntimeData)
  extends PrimUnparser {

  override def runtimeDependencies = Vector()

  override def unparse(state: UState): Unit = {
    val dos = state.getDataOutputStream
    val elem = state.currentInfosetNode.asInstanceOf[DIElement]
    if (dos.maybeAbsBitPos0b.isDefined) {
      elem.valueLength.setAbsStartPos0bInBits(dos.maybeAbsBitPos0b.getULong)
    } else {
      elem.valueLength.setRelStartPos0bInBits(dos.relBitPos0b, dos)
    }
  }
}

class CaptureEndOfValueLengthUnparser(override val context: ElementRuntimeData)
  extends PrimUnparser {

  override def runtimeDependencies = Vector()

  override def unparse(state: UState): Unit = {
    val dos = state.getDataOutputStream
    val elem = state.currentInfosetNode.asInstanceOf[DIElement]
    if (dos.maybeAbsBitPos0b.isDefined) {
      elem.valueLength.setAbsEndPos0bInBits(dos.maybeAbsBitPos0b.getULong)
    } else {
      elem.valueLength.setRelEndPos0bInBits(dos.relBitPos0b, dos)
    }
  }
}

/**
 * Carries out computation of the target length for a specified-length element.
 *
 * This is not a SuspendableExpression because the dfdl:length property cannot
 * be forward referencing. However, it can refer backward to elements that have
 * dfdl:outputValueCalc or variables that have not yet been computed. So we have
 * to retry this in order to get the target length used to compute the amount of
 * padding or the amount of unused space.
 */
class TargetLengthOperation(
  override val rd: ElementRuntimeData,
  targetLengthEv: UnparseTargetLengthInBitsEv
) extends SuspendableOperation {

  override val isReadOnly = true

  override def toString =
    "target length for " + rd.diagnosticDebugName + " expr " + targetLengthEv.lengthInBitsEv.lengthEv
      .toBriefXML()

  /**
   * This override indicates that this operation itself doesn't correspond
   * to any bits in the unparsed data stream. It's just a computation.
   */
  override protected def maybeKnownLengthInBits(ustate: UState): MaybeULong = MaybeULong(0L)

  override def test(ustate: UState): Boolean = {
    //
    // regular evaluation - can only look backwards
    //
    targetLengthEv.evaluate(
      ustate
    ) // can we successfully evaluate without blocking (blocking would throw)
    true
  }

  override def continuation(state: UState): Unit = {
    // once we have evaluated the targetLengthEv, nothing else to do
    // here
  }
}

/**
 * Several sub-unparsers need to have the value length, and the target length
 * in order to compute their own length.
 */
sealed trait NeedValueAndTargetLengthMixin {

  def targetLengthEv: Evaluatable[MaybeJULong]
  def maybeLengthEv: Maybe[LengthEv]
  def maybeCharsetEv: Maybe[CharsetEv]
  def maybeLiteralNilEv: Maybe[NilStringLiteralForUnparserEv]

  protected final def hasTargetLength(ustate: UState): Boolean = {
    targetLengthEv.evaluate(ustate)
    true
  }

  protected def test(ustate: UState): Boolean = {
    hasTargetLength(ustate) && {
      val e = ustate.currentInfosetNode.asInstanceOf[DIElement]
      val hasValueLength = e.valueLength.maybeLengthInBits().isDefined
      hasValueLength
    }
  }

  /**
   * returns the value as a string, or if nillable and isNilled, the nil value
   * for use when unparsing.
   */
  private def valueString(s: DISimple, ustate: UState) = {
    val vs =
      if (s.erd.isNillable && s.isNilled) {
        Assert.invariant(this.maybeLiteralNilEv.isDefined)
        maybeLiteralNilEv.get.evaluate(ustate)
      } else {
        s.dataValueAsString
      }
    vs
  }

  /**
   * Returns number of bits to skip.
   *
   * This can be negative if the unparsed data was too big.
   *
   * It is up to the caller to determine if this is an error or not.
   */
  protected def getSkipBits(ustate: UState): Long = {
    val e = ustate.currentInfosetNode.asInstanceOf[DIElement]
    val mtl = targetLengthEv.evaluate(ustate)
    if (mtl.isDefined) {
      val tl = mtl.get
      val vl = e.valueLength.lengthInBits.longValue
      val skipInBits = tl - vl
      skipInBits
    } else {
      // it's measured in variable width characters
      if (maybeLengthEv.isDefined) {
        val lengthEv = maybeLengthEv.get
        val tlChars = lengthEv.evaluate(ustate)
        e match {
          case s: DISimple => {
            val v = valueString(s, ustate)
            val vlChars = v.length
            val nPadChars = tlChars - vlChars // negative if data too long for available space.
            val cs: BitsCharset = maybeCharsetEv.get.evaluate(ustate)
            val paddingLengthInBits = cs.padCharWidthInBits * nPadChars
            paddingLengthInBits
          }
          case c: DIComplex =>
            ??? // FIXME: This code was left incomplete - JIRA DAFFODIL-1952
        }
      } else {
        0L // must be delimited, so we don't pad unless there's a minLength, which
        // is not this case.
      }
    }
  }
}

trait SkipTheBits { self: SuspendableOperation =>

  protected val rd: RuntimeData

  protected final def skipTheBits(ustate: UState, skipInBits: Long): Unit = {
    if (skipInBits > 0) {
      val dos = ustate.getDataOutputStream
      if (!dos.skip(skipInBits, ustate))
        UE(ustate, "Unable to skip %s(bits).", skipInBits)
    }
    if (skipInBits == 0) {
      Logger.log.debug(
        s"${Misc.getNameFromClass(this)} no fill for ${rd.diagnosticDebugName} DOS ${ustate.getDataOutputStream}."
      )
    } else {
      Logger.log.debug(
        s"${Misc.getNameFromClass(this)} filled ${skipInBits} bits for ${rd.diagnosticDebugName} DOS ${ustate.getDataOutputStream}."
      )
    }
  }
}

class ElementUnusedUnparserSuspendableOperation(
  override val rd: ElementRuntimeData,
  override val targetLengthEv: UnparseTargetLengthInBitsEv,
  override val maybeLengthEv: Maybe[LengthEv],
  override val maybeCharsetEv: Maybe[CharsetEv],
  override val maybeLiteralNilEv: Maybe[NilStringLiteralForUnparserEv]
) extends SuspendableOperation
  with SkipTheBits
  with NeedValueAndTargetLengthMixin {

  /**
   * determine delta between value length and target length
   *
   * and skip that many bits.
   */
  override def continuation(ustate: UState): Unit = {
    val skipInBits = getSkipBits(ustate)
    if (skipInBits < 0)
      UE(ustate, "Data too long by %s bits. Unable to truncate.", -skipInBits)
    skipTheBits(ustate, skipInBits)
  }

}

class ElementUnusedUnparser(
  override val context: ElementRuntimeData,
  targetLengthEv: UnparseTargetLengthInBitsEv,
  maybeLengthEv: Maybe[LengthEv],
  maybeCharsetEv: Maybe[CharsetEv],
  maybeLiteralNilEv: Maybe[NilStringLiteralForUnparserEv]
) extends PrimUnparser
  with SuspendableUnparser {

  override def runtimeDependencies = Vector(targetLengthEv)

  override def suspendableOperation =
    new ElementUnusedUnparserSuspendableOperation(
      context,
      targetLengthEv,
      maybeLengthEv,
      maybeCharsetEv,
      maybeLiteralNilEv
    )

}

class ChoiceUnusedUnparserSuspendableOperation(
  override val rd: ModelGroupRuntimeData,
  targetLengthInBits: Long
) extends SuspendableOperation
  with StreamSplitter
  with SkipTheBits {

  private var zlStatus_ : ZeroLengthStatus = ZeroLengthStatus.Unknown

  private var maybeDOSStart: Maybe[DataOutputStream] = Maybe.Nope
  private var maybeDOSEnd: Maybe[DataOutputStream] = Maybe.Nope

  def captureDOSStartForChoiceUnused(state: UState): Unit = {
    val splitter = new RegionSplitUnparser(rd)
    splitter.unparse(state)
    maybeDOSStart = Maybe(splitter.dataOutputStream)
  }

  def captureDOSEndForChoiceUnused(state: UState): Unit = {
    val splitter = new RegionSplitUnparser(rd)
    splitter.unparse(state)
    maybeDOSEnd = Maybe(splitter.dataOutputStream)
  }

  private lazy val dosToCheck_ = {
    Assert.usage(maybeDOSStart.isDefined)
    val dosForStart = maybeDOSStart.get
    val dosForEnd = maybeDOSEnd.get
    val primaryDOSList = getDOSFromAtoB(dosForStart, dosForEnd)

    primaryDOSList
  }

  override def test(ustate: UState): Boolean = {
    if (zlStatus_ ne ZeroLengthStatus.Unknown)
      true
    else if (maybeDOSStart.isEmpty)
      false
    else {
      Assert.invariant(maybeDOSStart.isDefined)
      if (
        dosToCheck_.exists { dos =>
          val dosZLStatus = dos.zeroLengthStatus
          dosZLStatus eq ZeroLengthStatus.NonZero
        }
      ) {
        zlStatus_ = ZeroLengthStatus.NonZero
        true
      } else if (
        dosToCheck_.forall { dos =>
          val dosZLStatus = dos.zeroLengthStatus
          dosZLStatus eq ZeroLengthStatus.Zero
        }
      ) {
        zlStatus_ = ZeroLengthStatus.Zero
        true
      } else {
        Assert.invariant(zlStatus_ eq ZeroLengthStatus.Unknown)
        false
      }
    }
  }

  /**
   * determine delta between value length and target length
   *
   * and skip that many bits.
   */
  override def continuation(ustate: UState): Unit = {
    val startPos0b = dosToCheck_(0).relBitPos0b
    val endPos0b = dosToCheck_.last.relBitPos0b + startPos0b
    val vl = (endPos0b - startPos0b).toLong
    val skipInBits = targetLengthInBits - vl
    if (skipInBits < 0)
      UE(
        ustate,
        "Data too long for dfdl:choiceLength (%s bits) by %s bits. Unable to truncate.",
        targetLengthInBits,
        -skipInBits
      )
    skipTheBits(ustate, skipInBits)
  }
}

class ChoiceUnusedUnparser(
  override val context: ModelGroupRuntimeData,
  targetLengthInBits: Long,
  suspendableOp: SuspendableOperation
) extends PrimUnparser
  with SuspendableUnparser {

  override def runtimeDependencies = Vector()

  override def suspendableOperation = suspendableOp
}

trait PaddingUnparserMixin extends NeedValueAndTargetLengthMixin { self: SuspendableOperation =>

  protected def charsKind = "pad"

  protected def maybePadChar: MaybeChar

  override def test(ustate: UState): Boolean = {
    super.test(ustate) && {
      // we know there is a charset. We can't have a padChar without one
      val charsetEv = maybeCharsetEv.get
      charsetEv.evaluate(ustate)
      true
    }
  }

  protected def numPadChars(skipInBits: Long, charWidthInBits: Long) =
    skipInBits / charWidthInBits // discarding any fragment of a character

  protected final def charset(state: UState) =
    maybeCharsetEv.get.evaluate(state)

  protected final def charWidthInBits(charset: BitsCharset) = {
    val res = charset.maybeFixedWidth.get
    res
  }

  override def continuation(state: UState): Unit = {
    val skipInBits = getSkipBits(state)
    if (skipInBits <= 0)
      return // padding doesn't worry about data too long. RightFill and ElementUnused do.
    val cs = charset(state)

    val nChars = numPadChars(skipInBits, cs.padCharWidthInBits)
    if (nChars > 0) {
      val dos = state.getDataOutputStream
      var i = 0
      val padChar = maybePadChar.get
      val padString = padChar.toString
      while (i < nChars) {
        try {
          if (dos.putString(padString, state) != 1)
            UE(state, "Unable to output %s %s characters.", nChars, charsKind)
        } catch {
          case m: MalformedInputException => {
            UnparseError(
              One(self.rd.schemaFileLocation),
              One(state.currentLocation),
              "MalformedInputException: \n%s",
              m.getMessage()
            )
          }
          case u: UnmappableCharacterException => {
            UnparseError(
              One(self.rd.schemaFileLocation),
              One(state.currentLocation),
              "UnmappableCharacterException: \n%s",
              u.getMessage()
            )
          }
        }
        i += 1
      }
    }
  }
}

class OnlyPaddingUnparserSuspendableOperation(
  override val rd: ElementRuntimeData,
  override val targetLengthEv: Evaluatable[MaybeJULong],
  override val maybeLengthEv: Maybe[LengthEv],
  override val maybeCharsetEv: Maybe[CharsetEv],
  override val maybeLiteralNilEv: Maybe[NilStringLiteralForUnparserEv],
  override val maybePadChar: MaybeChar
) extends SuspendableOperation
  with PaddingUnparserMixin

/**
 * Doesn't matter if we're left or right padding if we're the only padding
 */
class OnlyPaddingUnparser(
  override val context: ElementRuntimeData,
  targetLengthEv: Evaluatable[MaybeJULong],
  maybeLengthEv: Maybe[LengthEv],
  maybeCharsetEv: Maybe[CharsetEv],
  maybeLiteralNilEv: Maybe[NilStringLiteralForUnparserEv],
  maybePadChar: MaybeChar
) extends TextPrimUnparser
  with SuspendableUnparser {

  override def runtimeDependencies = Vector(targetLengthEv)

  override def suspendableOperation =
    new OnlyPaddingUnparserSuspendableOperation(
      context,
      targetLengthEv,
      maybeLengthEv,
      maybeCharsetEv,
      maybeLiteralNilEv,
      maybePadChar
    )
}

class NilLiteralCharacterUnparserSuspendableOperation(
  override val rd: ElementRuntimeData,
  override val targetLengthEv: UnparseTargetLengthInBitsEv,
  override val maybeLengthEv: Maybe[LengthEv],
  override val maybeCharsetEv: Maybe[CharsetEv],
  literalNilChar: Char
) extends SuspendableOperation
  with PaddingUnparserMixin {

  override def maybeLiteralNilEv: Maybe[NilStringLiteralForUnparserEv] = Nope

  override def charsKind = "dfdl:nilKind 'literalCharacter'"

  override val maybePadChar: MaybeChar = MaybeChar(literalNilChar)

  //
  // We don't wait for the valueLength, because the unparsed
  // nil is part of the valueLength
  //
  override def test(state: UState) =
    hasTargetLength(state) && {
      val e = state.currentInfosetNode.asInstanceOf[DISimple]
      val isNilled = e.isNilled
      isNilled
    }

  override protected def getSkipBits(ustate: UState): Long = {
    val mtl = targetLengthEv.evaluate(ustate)
    val tl = mtl.get
    val skipInBits = tl
    skipInBits
  }

}

class NilLiteralCharacterUnparser(
  override val context: ElementRuntimeData,
  val targetLengthEv: UnparseTargetLengthInBitsEv,
  val maybeLengthEv: Maybe[LengthEv],
  val maybeCharsetEv: Maybe[CharsetEv],
  literalNilChar: Char
) extends TextPrimUnparser
  with SuspendableUnparser {

  override def runtimeDependencies = Vector(targetLengthEv)

  override def suspendableOperation = new NilLiteralCharacterUnparserSuspendableOperation(
    context,
    targetLengthEv,
    maybeLengthEv,
    maybeCharsetEv,
    literalNilChar
  )

}

class RightCenteredPaddingUnparserSuspendaableOperation(
  rd: ElementRuntimeData,
  targetLengthEv: Evaluatable[MaybeJULong],
  maybeLengthEv: Maybe[LengthEv],
  maybeCharsetEv: Maybe[CharsetEv],
  maybeLiteralNilEv: Maybe[NilStringLiteralForUnparserEv],
  maybePadChar: MaybeChar
) extends OnlyPaddingUnparserSuspendableOperation(
    rd,
    targetLengthEv,
    maybeLengthEv,
    maybeCharsetEv,
    maybeLiteralNilEv,
    maybePadChar
  ) {

  override def numPadChars(skipInBits: Long, charWidthInBits: Long) = {
    val numChars = super.numPadChars(skipInBits, charWidthInBits)
    numChars / 2
  }
}

class RightCenteredPaddingUnparser(
  rd: ElementRuntimeData,
  targetLengthEv: Evaluatable[MaybeJULong],
  maybeLengthEv: Maybe[LengthEv],
  maybeCharsetEv: Maybe[CharsetEv],
  maybeLiteralNilEv: Maybe[NilStringLiteralForUnparserEv],
  maybePadChar: MaybeChar
) extends OnlyPaddingUnparser(
    rd,
    targetLengthEv,
    maybeLengthEv,
    maybeCharsetEv,
    maybeLiteralNilEv,
    maybePadChar
  ) {

  override def suspendableOperation =
    new RightCenteredPaddingUnparserSuspendaableOperation(
      rd,
      targetLengthEv,
      maybeLengthEv,
      maybeCharsetEv,
      maybeLiteralNilEv,
      maybePadChar
    )
}

class LeftCenteredPaddingUnparserSuspendableOperation(
  override val rd: ElementRuntimeData,
  targetLengthEv: Evaluatable[MaybeJULong],
  maybeLengthEv: Maybe[LengthEv],
  maybeCharsetEv: Maybe[CharsetEv],
  maybeLiteralNilEv: Maybe[NilStringLiteralForUnparserEv],
  maybePadChar: MaybeChar
) extends OnlyPaddingUnparserSuspendableOperation(
    rd,
    targetLengthEv,
    maybeLengthEv,
    maybeCharsetEv,
    maybeLiteralNilEv,
    maybePadChar
  ) {

  override def numPadChars(skipInBits: Long, charWidthInBits: Long) = {
    val numChars = super.numPadChars(skipInBits, charWidthInBits)
    if ((numChars & 1) == 0)
      numChars / 2
    else
      (numChars / 2) + 1
  }
}

class LeftCenteredPaddingUnparser(
  rd: ElementRuntimeData,
  targetLengthEv: Evaluatable[MaybeJULong],
  maybeLengthEv: Maybe[LengthEv],
  maybeCharsetEv: Maybe[CharsetEv],
  maybeLiteralNilEv: Maybe[NilStringLiteralForUnparserEv],
  maybePadChar: MaybeChar
) extends OnlyPaddingUnparser(
    rd,
    targetLengthEv,
    maybeLengthEv,
    maybeCharsetEv,
    maybeLiteralNilEv,
    maybePadChar
  ) {

  override def suspendableOperation =
    new LeftCenteredPaddingUnparserSuspendableOperation(
      rd,
      targetLengthEv,
      maybeLengthEv,
      maybeCharsetEv,
      maybeLiteralNilEv,
      maybePadChar
    )
}

class RightFillUnparserSuspendableOperation(
  rd: ElementRuntimeData,
  targetLengthEv: UnparseTargetLengthInBitsEv,
  maybeLengthEv: Maybe[LengthEv],
  maybeCharsetEv: Maybe[CharsetEv],
  maybeLiteralNilEv: Maybe[NilStringLiteralForUnparserEv],
  override val maybePadChar: MaybeChar
) extends ElementUnusedUnparserSuspendableOperation(
    rd,
    targetLengthEv,
    maybeLengthEv,
    maybeCharsetEv,
    maybeLiteralNilEv
  )
  with PaddingUnparserMixin {

  override def continuation(state: UState): Unit = {
    val skipInBits = getSkipBits(state)
    if (skipInBits == 0L) return
    if (skipInBits > 0) {
      val cs = charset(state)
      val skipInBitsMinusPadding =
        if (maybePadChar.isDefined) {
          skipInBits % cs.padCharWidthInBits
        } else {
          skipInBits
        }

      skipTheBits(state, skipInBitsMinusPadding)
    } else {
      UE(state, "Data too long by %s bits. Unable to truncate.", -skipInBits)
    }
  }

}

class RightFillUnparser(
  rd: ElementRuntimeData,
  targetLengthEv: UnparseTargetLengthInBitsEv,
  maybeLengthEv: Maybe[LengthEv],
  maybeCharsetEv: Maybe[CharsetEv],
  maybeLiteralNilEv: Maybe[NilStringLiteralForUnparserEv],
  val maybePadChar: MaybeChar
) extends ElementUnusedUnparser(
    rd,
    targetLengthEv,
    maybeLengthEv,
    maybeCharsetEv,
    maybeLiteralNilEv
  ) {

  override def suspendableOperation =
    new RightFillUnparserSuspendableOperation(
      rd,
      targetLengthEv,
      maybeLengthEv,
      maybeCharsetEv,
      maybeLiteralNilEv,
      maybePadChar
    )

}

class PrefixLengthSuspendableOperation(
  override val rd: ElementRuntimeData,
  elem: DIElement,
  plElem: DISimple,
  override val lengthUnits: LengthUnits,
  override val prefixedLengthAdjustmentInUnits: Long
) extends SuspendableOperation
  with CalculatedPrefixedLengthUnparserMixin {

  override val isReadOnly = true

  override def toString = "prefix length for " + rd.diagnosticDebugName

  /**
   * This override indicates that this operation itself doesn't correspond
   * to any bits in the unparsed data stream. It's just a computation.
   */
  override protected def maybeKnownLengthInBits(ustate: UState): MaybeULong = MaybeULong(0L)

  override def test(ustate: UState): Boolean = {
    elem.contentLength.maybeLengthInBits().isDefined
  }

  override def continuation(state: UState): Unit = {
    val len = elem.contentLength.maybeLengthInBits().isDefined
    assignPrefixLength(state, elem, plElem)
  }
}
