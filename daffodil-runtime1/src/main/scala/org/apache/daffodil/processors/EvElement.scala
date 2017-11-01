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

package org.apache.daffodil.processors

import org.apache.daffodil.dsom._
import java.lang.{ Long => JLong }
import org.apache.daffodil.util.MaybeJULong
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.dpath.UnparserBlocking
import org.apache.daffodil.dpath.EvalMode
import org.apache.daffodil.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.cookers.OutputNewLineCooker
import org.apache.daffodil.cookers.ChoiceDispatchKeyCooker

sealed trait LengthEv extends Evaluatable[JLong]

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

  override def compute(state: State): JLong = {
    val v: JLong = super.compute(state)
    if (v < 0) {
      state.SDE("dfdl:length expression result must be non-negative, but was: %d", v)
    }
    v
  }
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
 * We need Evaluatables that let us tease apart the different algorithmic cases
 * for determining length.
 *
 * Specified-length means elements with simple types with lengthKind 'implicit', or
 * lengthKind 'explicit' - for simple or complex types.
 *
 * There really are these cases for specified length unparsing:
 *
 * # length convertible to N bits. This case includes binary data as well as textual data
 * when the encoding is fixed width. In this case, even if the lengthUnits is
 * 'characters' we can convert to a number of bits.
 *
 * # length in variable-width characters, but lengthUnits is 'bits' or 'bytes'.
 * In this case, we must unparse only until we run out of space when space is
 * limited. Truncation, in the one case it is allowed (type string with
 * dfdl:truncateSpecifiedLenghtString='yes') requires care, as the number of
 * characters where to truncate isn't clear.
 *
 * # variable width characters, units are characters. In this case we need
 * entirely different algorithms as we must count the number of characters.
 */

/**
 * Used when lengthKind='explicit', computes the length in bits or Nope if
 * the length depends on encoding, and the encoding is variable width.
 *
 * Handles the case of LengthUnits.Characters by returning Nope if
 * the charset is variable width. Otherwise it computes the number of
 * bits using the width.
 *
 * Nope does NOT mean that we cannot yet compute the length expression. It
 * means the length depends on variable-width character encoding (and since
 * encoding is runtime-valued sometimes, we need an Ev for this.
 *
 * Sometimes a length cannot be computed yet - happens during unparsing, for
 * an element whose length expression refers back to an element with a
 * dfdl:outputValueCalc that has not yet been computed, or a variable that
 * has not yet been computed.
 *
 * In that case, the access to the infoset throws particular exceptions
 * descended from the RetryableException trait.
 */
sealed abstract class LengthInBitsEvBase(rd: ElementRuntimeData,
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
class LengthInBitsEv(lengthUnits: LengthUnits,
  lengthKind: LengthKind,
  override val maybeCharsetEv: Maybe[CharsetEv],
  val lengthEv: LengthEv,
  rd: ElementRuntimeData)
  extends LengthInBitsEvBase(rd, lengthUnits, lengthKind) {

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
  extends LengthInBitsEvBase(rd, lengthUnits, lengthKind) {

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
 *
 * Nope does NOT MEAN the length cannot be computed yet. It means variable-width
 * encoding prevents computing the target length in bits units.
 */
class UnparseTargetLengthInBitsEv(
  val lengthInBitsEv: LengthInBitsEv,
  minLengthInBitsEv: MinLengthInBitsEv,
  rd: ElementRuntimeData)
  extends Evaluatable[MaybeJULong](rd)
  with InfosetCachedEvaluatable[MaybeJULong] {

  override lazy val runtimeDependencies = List(this.lengthInBitsEv, this.minLengthInBitsEv)

  /**
   * Note: use of MaybeJULong type. New Maybe type added which can be stored in
   * a generic data structure. (I.e., a MaybeJULong is an AnyRef, not an AnyVal
   * like Long and MaybeULong).
   *
   * Ev values that are runtime dependent get stored in a generic cache of
   * AnyRef values, so this gives us the benefits of a maybe object on top of
   * the boxing that is required to store a number in in the cache.
   *
   */

  override protected def compute(state: ParseOrUnparseState): MaybeJULong = {
    val maybeLen = lengthInBitsEv.evaluate(state)
    val maybeMin = minLengthInBitsEv.evaluate(state)
    if (maybeLen.isEmpty) {
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

class UnparseTargetLengthInCharactersEv(
  val lengthEv: LengthEv,
  val charsetEv: CharsetEv,
  minLen: Long,
  rd: ElementRuntimeData)
  extends Evaluatable[MaybeJULong](rd)
  with InfosetCachedEvaluatable[MaybeJULong] {

  override lazy val runtimeDependencies = List(this.lengthEv, charsetEv)

  /**
   * Note: use of MaybeJULong type. New Maybe type added which can be stored in
   * a generic data structure. (I.e., a MaybeJULong is an AnyRef, not an AnyVal
   * like Long and MaybeULong).
   *
   * Ev values that are runtime dependent get stored in a generic cache of
   * AnyRef values, so this gives us the benefits of a maybe object on top of
   * the boxing that is required to store a number in in the cache.
   *
   */

  override protected def compute(state: ParseOrUnparseState): MaybeJULong = {
    Assert.usage(charsetEv.evaluate(state).maybeFixedWidth.isEmpty) // must be variable-width-chars
    val len = lengthEv.evaluate(state)
    val targetLen = scala.math.max(len, minLen)
    MaybeJULong(targetLen)
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

class ChoiceDispatchKeyEv(expr: CompiledExpression[String], rd: TermRuntimeData)
  extends EvaluatableConvertedExpression[String, String](
    expr,
    ChoiceDispatchKeyCooker,
    rd)
  with InfosetCachedEvaluatable[String] {
  override lazy val runtimeDependencies = Nil
}
