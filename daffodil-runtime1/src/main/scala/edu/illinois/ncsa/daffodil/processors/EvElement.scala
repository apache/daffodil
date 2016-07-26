/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom._
import java.lang.{ Long => JLong }
import edu.illinois.ncsa.daffodil.util.MaybeJULong
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.dpath.UnparserBlocking
import edu.illinois.ncsa.daffodil.dpath.EvalMode
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthUnits
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthKind

sealed trait LengthEv extends EvaluatableBase[JLong]

class ExplicitLengthEv(expr: CompiledExpression[JLong], rd: ElementRuntimeData)
    extends EvaluatableExpression[JLong](
      expr,
      rd)
    with LengthEv
    with InfosetCachedEvaluatable[JLong] {
  override lazy val runtimeDependencies = Nil

  /**
   * Length is special. For the dfdl:length property, when it's an expression
   * we use blocking mode when unparsing.
   *
   * Most (all?) other Evaluatables for runtime-valued properties will use
   * UnparserNonBlocking when unparsing.
   */
  override protected final def maybeUseUnparserMode: Maybe[EvalMode] = Maybe(UnparserBlocking)
}

class ImplicitLengthEv(lengthValue: Long, rd: ElementRuntimeData)
    extends Evaluatable[JLong](rd)
    with LengthEv
    with NoCacheEvaluatable[JLong] {

  override val runtimeDependencies = Nil

  private val jLength = new JLong(lengthValue)

  override protected def compute(state: State): JLong = {
    jLength
  }
}

/*
 * There really are these cases for specified length unparsing 
 * (Which means lengthKind implicit (simple types only), and length kind 
 * explicit - for simple or complex types)
 * 
 * # variable width characters, units are characters
 * 
 * (Not worth doing, at least initially, don't implement for Daffodil - should SDE)
 * (Similarly, don't implement dfdl:contentLength nor dfdl:valueLength in units
 * characters for complex types if the encoding is variable width).
 * 
 * Simple types:
 * 
 * For parsing, we must decode characters one by one until we accumulate
 * the number of characters.
 * 
 * The dfdl:contentLength in characters is this number of characters.
 * The dfdl:contentLength in bits is determined by the starting position when 
 * we begin decoding, and the ending position after decoding the number of characters.
 * 
 * If we are not trimming off pad characters, then the dfdl:valueLength is the
 * same as the dfdl:contentLength.
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
 * 
 * # variable width characters, length units are bits
 * 
 * (Really does need to be implemented, examples like 80-byte records, but where
 * the characters can be utf-8 not just ascii = would be a typical Unicode 
 * upgrade to a legacy 80-byte oriented application.)
 * 
 * For Simple types:
 * 
 * For parsing, we must decode characters one by one until we run into the 
 * end of the number of bits available. A fragment of a character may have
 * to be skipped at the end. 
 * 
 * If triming, then to identify content and value regions, we have to
 * decode character by character, count runs of initial pad characters, 
 * count runs of final pad characters, but discard the pad characters.
 * Keep track of bit position of most recently 
 * decoded non-pad character (gives value region end in bits). The value length
 * in characters is the number of non-pad characters when we are done.
 * The value length in bits is based on the start bit pos of the first non-pad
 * character, and the end bit pos of the last non-pad character.  
 * 
 * For unparsing, WRONG: assumes we know N. We don't.
 * We must encode characters into a buffer stream one by one until we either 
 * run out of characters, or the next character would put us at or past the 
 * number of bits. In the case where all the characters do not fit, we 
 * may (depending on simple type string and dfdl:truncaseVariableLengthString)
 * discard the extra characters, or we may fail. If we run out of characters, 
 * then if not padding it is an error. If we are padding, then we compute the
 * number of additional characters needed on the left and right, and unparse
 * pad chars to those buffer stream regions. If there are left over bits still,
 * then we unparse the fill byte for that many bits into that region.
 * 
 * For complex types:
 * 
 * For parsing we know the content length in bits is N.
 * We setup the bit limit, and then parse recursively. If we run into the end
 * it is an error. If we do not run into the end limit, then we record the end
 * value length in bits, and skip to the bit limit position.
 * 
 * The content length in characters - DO NOT IMPLEMENT.
 * 
 * The value length in characters - DO NOT IMPLEMENT. 
 * 
 * For unparsing, we don't know the content length in bits. 
 * 
 * We record the complex value start
 * position in bits, then we setup the bit limit and unparse recursively. 
 * If we run into the end, then it is an error. 
 * If not then when  
 * 
 * # non-text or fixed-width characters, units are bits
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
 */
sealed trait LengthCase
case object LengthInBits extends LengthCase
case object LengthInBitsVariableWidthCharacters extends LengthCase
case object LengthInCharactersVariableWidthCharacters extends LengthCase

/**
 * Used when lengthKind='explicit', computes the length in bits or Nope if
 * the length depends on encoding, and the encoding is variable width.
 *
 * Handles the case of LengthUnits.Characters by returning Nope if
 * the charset is variable width. Otherwise it computes the number of
 * bits using the width.
 */
sealed abstract class LengthInBitsEv(rd: ElementRuntimeData,
  val lengthUnits: LengthUnits,
  val lengthKind: LengthKind)
    extends Evaluatable[MaybeJULong](rd)
    with InfosetCachedEvaluatable[MaybeJULong] {

  protected def maybeCharsetEv: Maybe[CharsetEv]
  protected def lengthInLengthUnits(state: ParseOrUnparseState): Long

  override protected def compute(state: ParseOrUnparseState): MaybeJULong = {
    val lengthV = lengthInLengthUnits(state)
    val bitsLength: Long =
      lengthUnits match {
        case LengthUnits.Bits => lengthV
        case LengthUnits.Bytes => lengthV * 8
        case LengthUnits.Characters => {
          val maybeWidth = maybeCharsetEv.get.evaluate(state).maybeFixedWidth
          if (maybeWidth.isEmpty)
            return MaybeJULong.Nope
          else {
            val width = maybeWidth.get
            lengthV * width
          }
        }
      }
    MaybeJULong(bitsLength)
  }

}

/**
 * Provides the length of the element, in bits, for Explicit or Implicit
 * length elements, including deals with LengthUnits.Characters and fixed
 * width encodings.
 *
 * Does NOT take into account minLength and maxLength or
 * textOutputMinLength for unparsing of Explicit length elements.
 * See ElementTargetLengthInBitsEv.
 */
class ElementLengthInBitsEv(lengthUnits: LengthUnits,
  lengthKind: LengthKind,
  override val maybeCharsetEv: Maybe[CharsetEv],
  val lengthEv: LengthEv,
  rd: ElementRuntimeData)
    extends LengthInBitsEv(rd, lengthUnits, lengthKind) {

  override lazy val runtimeDependencies = maybeCharsetEv.toList :+ lengthEv

  override protected def lengthInLengthUnits(state: ParseOrUnparseState) =
    lengthEv.evaluate(state).longValue()
}

/**
 * Since the minimum text length might be specified in LengthUnits.Characters
 * for the minLength facet, and for the textOutputMinLength property, then
 * converting those to bits requires possibly not knowing the
 * encoding until runtime.
 *
 * Hence, we have to compute this similarly at runtime.
 */
class MinLengthInBitsEv(lengthUnits: LengthUnits,
  lengthKind: LengthKind,
  override val maybeCharsetEv: Maybe[CharsetEv],
  minLen: Long, rd: ElementRuntimeData)
    extends LengthInBitsEv(rd, lengthUnits, lengthKind) {

  override lazy val runtimeDependencies = maybeCharsetEv.toList

  override protected def lengthInLengthUnits(state: ParseOrUnparseState) = minLen
}

/**
 * Used for unparsing, to determine whether we have a target length
 * from the dfdl:length property and Explicit length, or for fixed length
 * from facets or textOutputMinLength.
 *
 * Evaluates to Nope if variable-width encoding prevents computing the
 * targetLengthInBits.
 */
class UnparseTargetLengthInBitsEv(
  val lengthInBitsEv: ElementLengthInBitsEv,
  minLengthInBitsEv: MinLengthInBitsEv,
  rd: ElementRuntimeData)
    extends Evaluatable[MaybeJULong](rd)
    with InfosetCachedEvaluatable[MaybeJULong] {

  override lazy val runtimeDependencies = List(this.lengthInBitsEv, this.minLengthInBitsEv)

  override protected def compute(state: ParseOrUnparseState): MaybeJULong = {
    val maybeLen = lengthInBitsEv.evaluate(state)
    val maybeMin = minLengthInBitsEv.evaluate(state)
    if (maybeLen.isEmpty) {
      // Assert.invariant(maybeMin.isEmpty)
      //      val encName = encodingEv.evaluate(state)
      //      state.SDE("Elements of dfdl:lengthKind '%s' with dfdl:lengthUnits '%s', " + 
      //          "cannot be used with variable-width character encoding '%s'.",
      //        lengthKind.toString,
      //        lengthUnits.toString,
      //        encName)
      MaybeJULong.Nope
    } else {
      Assert.invariant(maybeMin.isDefined)
      val lenInBits = maybeLen.get
      val minInBits = maybeMin.get
      val targetLenInBits = scala.math.max(lenInBits, minInBits)
      MaybeJULong(targetLenInBits)
    }
  }
}

class OccursCountEv(expr: CompiledExpression[JLong], rd: ElementRuntimeData)
    extends EvaluatableExpression[JLong](
      expr,
      rd)
    with InfosetCachedEvaluatable[JLong] {
  override lazy val runtimeDependencies = Nil
}

class OutputNewLineEv(expr: CompiledExpression[String], rd: TermRuntimeData)
    extends EvaluatableConvertedExpression[String, String](
      expr,
      OutputNewLineCooker,
      rd)
    with InfosetCachedEvaluatable[String] {
  override lazy val runtimeDependencies = Nil
}
